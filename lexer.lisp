;;;; lexer.lisp
;;;; DEFLEXER: a parameterized tokenizer. Each fantasy CPU can declare its
;;;; own surface syntax (comment styles, number-literal prefixes, label
;;;; suffix, identifier character class, line continuation) rather than
;;;; inheriting one hard-coded assembly dialect -- see LASM-plan.md sec. 3.5.
;;;;
;;;; This mirrors machine.lisp's shape: parse-*-clause functions build a
;;;; descriptor struct, and a thin macro registers it by name in a hash
;;;; table (*LEXERS*, alongside machine.lisp's *MACHINES*). Unlike DEFMACHINE,
;;;; registration does not need to happen inside an EVAL-WHEN: nothing
;;;; consumes a lexer descriptor at macroexpansion time -- TOKENIZE runs at
;;;; ordinary runtime, unlike M1's DEFINSTRUCTION resolving storage names
;;;; against a DEFMACHINE descriptor at compile time.

(in-package #:lasm)

;;; Token representation

(defstruct token
  type    ; :identifier :number :string :punctuation :label-suffix :newline :eof
  value   ; parsed value: string (identifier/string), integer (number),
          ; keyword (punctuation/label-suffix)
  text    ; verbatim source text
  line
  column
  localp) ; :identifier only -- T if TEXT starts with the lexer descriptor's
          ; LOCAL-LABEL-PREFIX (#16); NIL for every other token type

;;; Descriptor structures

(defstruct number-format
  name        ; e.g. :hex :bin :dec :char -- a label, not interpreted beyond lookup
  prefixes    ; list of prefix strings (nil for the default/no-prefix format)
  defaultp    ; t for the format matched by bare digits with no prefix
  radix)      ; integer radix, or nil for :char (handled specially)

;; Known format names map to a fixed radix. A :char format is handled
;; specially in the tokenizer (single character literal, not a radix number)
;; and never appears in this table.
(defparameter *number-format-radixes*
  '((:hex . 16) (:bin . 2) (:oct . 8) (:dec . 10)))

(defstruct lexer-descriptor
  name
  comment-styles        ; list of (start end kind), kind one of :line :block
  number-formats        ; list of number-format
  label-suffix          ; string, or nil to disable labels
  local-label-prefix    ; string, or nil
  string-delim          ; string, or nil to disable string literals
  ident-extra-chars     ; string of non-alphanumeric chars allowed in identifiers
  line-continuation)    ; string, or nil to disable line continuation

;; Registry of defined lexer descriptors, keyed by name -- mirrors *MACHINES*
;; in storage.lisp.
(defvar *lexers* (make-hash-table :test 'eq))

(defun find-lexer-descriptor (name)
  (or (gethash name *lexers*)
      (error "No lexer named ~S has been defined with DEFLEXER" name)))

;;; DEFLEXER clause parsing

(defun parse-comment-styles-clause (specs)
  ;; Each spec is (start kind) for a line comment or (start end kind) for a
  ;; block comment, e.g. (";" :line) or ("/*" "*/" :block).
  (mapcar (lambda (spec)
            (case (length spec)
              (2 (list (first spec) nil (second spec)))
              (3 (list (first spec) (second spec) (third spec)))
              (t (error "Malformed comment-styles entry ~S" spec))))
          specs))

(defun parse-number-format-clause (clause)
  ;; (name prefix...) or (name :default)
  (destructuring-bind (name &rest specs) clause
    (if (equal specs '(:default))
        (make-number-format :name name :defaultp t
                             :radix (or (cdr (assoc name *number-format-radixes*)) 10))
        (make-number-format :name name :prefixes specs
                             :radix (cdr (assoc name *number-format-radixes*))))))

(defun parse-ident-chars-clause (args)
  ;; (ident-chars :alnum "_.") -- :alnum is currently the only supported
  ;; base class; the string lists additional allowed characters.
  (destructuring-bind (class extra) args
    (unless (eq class :alnum)
      (error "Unsupported ident-chars class ~S (only :alnum is implemented)" class))
    extra))

(defun build-lexer-descriptor (name clauses)
  (let (comment-styles number-formats label-suffix local-label-prefix
        string-delim (ident-extra-chars "") line-continuation)
    (dolist (clause clauses)
      (case (first clause)
        (comment-styles (setf comment-styles (parse-comment-styles-clause (rest clause))))
        (number-formats (setf number-formats (mapcar #'parse-number-format-clause (rest clause))))
        (label-suffix (setf label-suffix (second clause)))
        (local-label-prefix (setf local-label-prefix (second clause)))
        (string-delim (setf string-delim (second clause)))
        (ident-chars (setf ident-extra-chars (parse-ident-chars-clause (rest clause))))
        (line-continuation (setf line-continuation (second clause)))
        (t (error "Unknown DEFLEXER clause head ~S in ~S" (first clause) clause))))
    (make-lexer-descriptor :name name :comment-styles comment-styles
                            :number-formats number-formats
                            :label-suffix label-suffix
                            :local-label-prefix local-label-prefix
                            :string-delim string-delim
                            :ident-extra-chars ident-extra-chars
                            :line-continuation line-continuation)))

(defmacro deflexer (name &body clauses)
  "Define a surface syntax named NAME from CLAUSES, each one of:
     (comment-styles (start [end] kind)...)   kind is :line or :block
     (number-formats (name prefix...)... (name :default))
     (label-suffix string)
     (local-label-prefix string)
     (string-delim string)
     (ident-chars :alnum extra-chars-string)
     (line-continuation string)

Registers a LEXER-DESCRIPTOR under NAME in *LEXERS*, retrievable with
FIND-LEXER-DESCRIPTOR and usable as the :LEXER argument to TOKENIZE/PARSE."
  `(progn
     (setf (gethash ',name *lexers*) (build-lexer-descriptor ',name ',clauses))
     ',name))

;; A ready-to-use default syntax so callers need not define their own lexer
;; for a conventional assembly dialect.
(deflexer default
  (comment-styles (";" :line))
  (number-formats (:hex "$" "0x") (:bin "%" "0b") (:dec :default) (:char "'"))
  (label-suffix ":")
  (local-label-prefix ".")
  (string-delim "\"")
  (ident-chars :alnum "_.")
  (line-continuation "\\"))

;;; Tokenizer

(defstruct lex-state
  string
  (pos 0)
  (line 1)
  (col 1))

(defun %lex-error (state fmt &rest args)
  (error 'lex-error :message (apply #'format nil fmt args)
                     :line (lex-state-line state) :column (lex-state-col state)))

(defun %peek (state &optional (offset 0))
  (let ((i (+ (lex-state-pos state) offset)))
    (when (< i (length (lex-state-string state)))
      (char (lex-state-string state) i))))

(defun %advance (state &optional (n 1))
  (dotimes (i n)
    (let ((c (%peek state)))
      (when c
        (if (char= c #\Newline)
            (progn (incf (lex-state-line state)) (setf (lex-state-col state) 1))
            (incf (lex-state-col state)))
        (incf (lex-state-pos state))))))

(defun %looking-at (state str)
  (let ((s (lex-state-string state)) (pos (lex-state-pos state)))
    (and (<= (+ pos (length str)) (length s))
         (string= str s :start2 pos :end2 (+ pos (length str))))))

;; Each %MATCH-* function attempts one lexical rule at the current position.
;; It returns :SKIP if it consumed input but produced no token (whitespace,
;; comments, line continuation), a TOKEN if it produced one, or NIL if the
;; rule does not apply here -- TOKENIZE tries each in turn.

(defun %match-whitespace (state descriptor)
  (declare (ignore descriptor))
  (when (member (%peek state) '(#\Space #\Tab #\Return))
    (%advance state)
    :skip))

(defun %match-line-continuation (state descriptor)
  (let ((lc (lexer-descriptor-line-continuation descriptor)))
    (when (and lc (%looking-at state lc))
      (%advance state (length lc))
      (loop while (member (%peek state) '(#\Space #\Tab)) do (%advance state))
      (when (eql (%peek state) #\Newline) (%advance state))
      :skip)))

(defun %match-comment (state descriptor)
  (dolist (style (lexer-descriptor-comment-styles descriptor))
    (destructuring-bind (start end kind) style
      (when (%looking-at state start)
        (%advance state (length start))
        (ecase kind
          (:line
           (loop while (and (%peek state) (not (char= (%peek state) #\Newline)))
                 do (%advance state)))
          (:block
           (loop
             (when (null (%peek state))
               (%lex-error state "Unterminated block comment"))
             (when (%looking-at state end)
               (%advance state (length end))
               (return))
             (%advance state))))
        (return-from %match-comment :skip))))
  nil)

(defun %match-newline (state descriptor)
  (declare (ignore descriptor))
  (when (eql (%peek state) #\Newline)
    (let ((tok (make-token :type :newline :text (string #\Newline)
                            :line (lex-state-line state) :column (lex-state-col state))))
      (%advance state)
      tok)))

(defun %match-string (state descriptor)
  (let ((delim (lexer-descriptor-string-delim descriptor)))
    (when (and delim (%looking-at state delim))
      (let ((line (lex-state-line state)) (col (lex-state-col state))
            (chars (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
        (%advance state (length delim))
        (loop
          (let ((c (%peek state)))
            (cond
              ((null c) (%lex-error state "Unterminated string literal"))
              ((%looking-at state delim) (%advance state (length delim)) (return))
              ((char= c #\\)
               (%advance state)
               (let ((esc (%peek state)))
                 (unless esc (%lex-error state "Unterminated string literal"))
                 (vector-push-extend (case esc (#\n #\Newline) (#\t #\Tab) (t esc)) chars)
                 (%advance state)))
              (t (vector-push-extend c chars) (%advance state)))))
        (let ((text (coerce chars 'simple-string)))
          (make-token :type :string :value text :text text :line line :column col))))))

(defun %digit-run (state radix)
  (let ((chars (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
    (loop for c = (%peek state)
          while (and c (digit-char-p c radix))
          do (vector-push-extend c chars) (%advance state))
    (coerce chars 'simple-string)))

(defun %match-number (state descriptor)
  (let ((line (lex-state-line state)) (col (lex-state-col state)))
    (dolist (fmt (lexer-descriptor-number-formats descriptor))
      (unless (number-format-defaultp fmt)
        (dolist (prefix (sort (copy-list (number-format-prefixes fmt)) #'> :key #'length))
          (when (%looking-at state prefix)
            (%advance state (length prefix))
            (return-from %match-number
              (if (eq (number-format-name fmt) :char)
                  (let ((c (%peek state)))
                    (unless c (%lex-error state "Unterminated character literal"))
                    (%advance state)
                    (make-token :type :number :value (char-code c) :text (string c)
                                :line line :column col))
                  (let ((digits (%digit-run state (number-format-radix fmt))))
                    (when (zerop (length digits))
                      (%lex-error state "Expected digits after numeric prefix ~S" prefix))
                    (make-token :type :number
                                :value (parse-integer digits :radix (number-format-radix fmt))
                                :text digits :line line :column col))))))))
    (let ((default-fmt (find-if #'number-format-defaultp (lexer-descriptor-number-formats descriptor))))
      (when (and default-fmt (%peek state) (digit-char-p (%peek state) (number-format-radix default-fmt)))
        (let ((digits (%digit-run state (number-format-radix default-fmt))))
          (make-token :type :number
                      :value (parse-integer digits :radix (number-format-radix default-fmt))
                      :text digits :line line :column col))))))

(defun %ident-initial-p (c descriptor)
  (or (alpha-char-p c) (find c (lexer-descriptor-ident-extra-chars descriptor))))

(defun %ident-char-p (c descriptor)
  (or (alphanumericp c) (find c (lexer-descriptor-ident-extra-chars descriptor))))

(defun %match-identifier (state descriptor)
  (let ((c (%peek state)))
    (when (and c (%ident-initial-p c descriptor))
      (let ((line (lex-state-line state)) (col (lex-state-col state))
            (chars (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
        (loop for ch = (%peek state)
              while (and ch (%ident-char-p ch descriptor))
              do (vector-push-extend ch chars) (%advance state))
        (let* ((text (coerce chars 'simple-string))
               (prefix (lexer-descriptor-local-label-prefix descriptor)))
          (make-token :type :identifier :value text :text text :line line :column col
                      :localp (and prefix (plusp (length prefix))
                                   (>= (length text) (length prefix))
                                   (string= prefix text :end2 (length prefix)))))))))

(defun %match-label-suffix (state descriptor)
  (let ((suf (lexer-descriptor-label-suffix descriptor)))
    (when (and suf (%looking-at state suf))
      (let ((line (lex-state-line state)) (col (lex-state-col state)))
        (%advance state (length suf))
        (make-token :type :label-suffix :value :label-suffix :text suf :line line :column col)))))

;; Fixed punctuator table, checked in this order (two-char operators before
;; their single-char prefixes, so "<<"/">>" win maximal munch over "<"/">").
;; "#" has no meaning to the lexer or expression parser -- it is here so
;; addressing-mode literal patterns (LASM-plan.md sec. 3.4's immediate mode,
;; "#" expr) have a token to match against once #9 implements DEFMODE. "="
;; likewise has no meaning to the expression parser (it's absent from both
;; *BINARY-PRECEDENCE* and *UNARY-OPS*, parser.lisp) -- it exists only so
;; %PARSE-LINE (parser.lisp, #35) can recognize "name = value" as sugar for
;; ".equ name, value".
(defparameter *punctuators*
  '(("<<" . :shl) (">>" . :shr)
    ("|" . :pipe) ("^" . :caret) ("&" . :amp)
    ("+" . :plus) ("-" . :minus) ("*" . :star) ("/" . :slash) ("~" . :tilde)
    ("(" . :lparen) (")" . :rparen) ("," . :comma) ("#" . :hash)
    ("<" . :lt) (">" . :gt) ("=" . :equals)))

(defun %match-punctuation (state descriptor)
  (declare (ignore descriptor))
  (let ((line (lex-state-line state)) (col (lex-state-col state)))
    (dolist (entry *punctuators*)
      (when (%looking-at state (car entry))
        (%advance state (length (car entry)))
        (return-from %match-punctuation
          (make-token :type :punctuation :value (cdr entry) :text (car entry)
                      :line line :column col))))))

(defun tokenize (string &key (lexer 'default))
  "Tokenize STRING with the syntax registered under LEXER (default 'DEFAULT).
Returns a SIMPLE-VECTOR of TOKEN structs, terminated by a single :EOF token.
Signals LEX-ERROR on malformed input."
  (let ((descriptor (find-lexer-descriptor lexer))
        (state (make-lex-state :string string))
        (tokens '()))
    (loop
      (when (null (%peek state)) (return))
      (let ((result (or (%match-whitespace state descriptor)
                         (%match-line-continuation state descriptor)
                         (%match-comment state descriptor)
                         (%match-newline state descriptor)
                         (%match-string state descriptor)
                         (%match-number state descriptor)
                         (%match-identifier state descriptor)
                         (%match-label-suffix state descriptor)
                         (%match-punctuation state descriptor))))
        (cond
          ((eq result :skip))
          ((token-p result) (cl:push result tokens))
          (t (%lex-error state "Unexpected character ~S" (%peek state))))))
    (cl:push (make-token :type :eof :text "" :line (lex-state-line state) :column (lex-state-col state))
             tokens)
    (coerce (nreverse tokens) 'simple-vector)))
