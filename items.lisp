;;;; items.lisp
;;;; #113: structured assembly items -- a program as data rather than source
;;;; text. Each item becomes the tokens of one source line, which
;;;; %PARSE-LINE turns into a STATEMENT for ASSEMBLE-STATEMENTS. Operands are
;;;; built from addressing-mode patterns (mode.lisp), named through the
;;;; operand kinds of a backend (backend.lisp) or directly with (:mode ...).
;;;; The same tokens render as the ordinary source text an ASSEMBLY retains,
;;;; so listings, diagnostics and snapshots work unchanged.
;;;;
;;;; Items:  (:label NAME)  (:directive NAME expr...)  (:op NAME arg...)
;;;;         (MNEMONIC operand...)
;;;; Operands: an expression, (KIND value...) or (:mode MODE value...).
;;;; Expressions: an integer, a name, or (OPERATOR expr...).
;;;; A symbol names by its downcased name, a string by itself.

(in-package #:lasm)

(define-condition items-error (lasm-error)
  ((detail :initarg :detail :reader items-error-detail)
   (item :initarg :item :initform nil :reader items-error-item))
  (:report (lambda (c s)
             (format s "Item error: ~A~@[ (in ~S)~]" (items-error-detail c) (items-error-item c)))))

(define-condition items-malformed (items-error) ())
(define-condition items-operand-mismatch (items-error) ())

(defun %items-fail (type item control &rest args)
  (error type :detail (apply #'format nil control args) :item item))

(defstruct items-program
  items backend machine origin memory lexer)

;;; Context

(defvar *items-lexer* 'default)
(defvar *items-lexer-descriptor* nil)
(defvar *items-machine* nil)
(defvar *items-backend* nil)
(defvar *items-literals* nil "Literal text -> its tokens, for the assembly in progress.")
(defvar *items-pickables* nil "Mnemonic -> its (VARIANT-MODE-NAMES . ALTERNATIVE-NAMES), for the assembly in progress.")

(defun %source-name (designator item)
  "The source spelling of DESIGNATOR: a string as is, a symbol downcased unless it has mixed case."
  (typecase designator
    (string designator)
    (symbol (let ((name (symbol-name designator)))
              (if (string= name (string-upcase name)) (string-downcase name) name)))
    (t (%items-fail 'items-malformed item "~S is not a name" designator))))

(defun %same-name-p (a b)
  (let ((a (%designator-name a)) (b (%designator-name b)))
    (and a b (string= a b))))

(defun %local-name-p (name)
  (let ((prefix (lexer-descriptor-local-label-prefix *items-lexer-descriptor*)))
    (and prefix (plusp (length prefix))
         (>= (length name) (length prefix))
         (string= prefix name :end2 (length prefix)))))

;;; Tokens

(defun %name-token (name)
  (make-token :type :identifier :value name :text name :localp (%local-name-p name)))

(defun %number-token (n)
  (make-token :type :number :value n :text (format nil "~D" n)))

(defun %string-token (string)
  (make-token :type :string :value string :text string))

(defun %punct-token (keyword)
  (make-token :type :punctuation :value keyword :text (car (rassoc keyword *punctuators*))))

(defun %literal-tokens (text)
  (mapcar #'copy-token
          (or (gethash text *items-literals*)
              (setf (gethash text *items-literals*)
                    (remove-if (lambda (token) (member (token-type token) '(:eof :newline)))
                               (coerce (tokenize text :lexer *items-lexer*) 'list))))))

(defun %token-source (token)
  (if (eq (token-type token) :string)
      (with-output-to-string (out)
        (let ((delim (or (lexer-descriptor-string-delim *items-lexer-descriptor*) "\"")))
          (write-string delim out)
          (loop for char across (token-text token)
                do (when (or (char= char #\\) (string= (string char) delim))
                     (write-char #\\ out))
                   (write-char char out))
          (write-string delim out)))
      (token-text token)))

;;; Expressions

(defun %expression-tokens (value item)
  (typecase value
    (integer (if (minusp value)
                 (list (%punct-token :minus) (%number-token (- value)))
                 (list (%number-token value))))
    (string (list (%name-token value)))
    (symbol (if (keywordp value)
                (%items-fail 'items-malformed item "~S is not a value" value)
                (list (%name-token (%source-name value item)))))
    (cons (%operator-tokens value item))
    (t (%items-fail 'items-malformed item "~S is not a value" value))))

(defun %function-operator (head)
  (and (or (symbolp head) (stringp head))
       (cdr (assoc (string head) (lexer-descriptor-function-operators *items-lexer-descriptor*)
                   :test #'string-equal))))

(defun %expression-head-p (head)
  (or (%expression-operator head) (%function-operator head)))

(defun %operator-tokens (form item)
  (destructuring-bind (head &rest args) form
    (let ((operator (%expression-operator head))
          (function-keyword (%function-operator head)))
      (cond
        ((and operator args (null (rest args)) (assoc operator *unary-ops*))
         (append (list (%punct-token :lparen) (%punct-token operator))
                 (%expression-tokens (first args) item)
                 (list (%punct-token :rparen))))
        ((and operator (rest args) (assoc operator *binary-precedence*))
         (append (list (%punct-token :lparen))
                 (loop for (arg . more) on args
                       append (%expression-tokens arg item)
                       when more append (list (%punct-token operator)))
                 (list (%punct-token :rparen))))
        (function-keyword
         (append (list (make-token :type :function-operator :value function-keyword
                                   :text (string-downcase (string head)))
                       (%punct-token :lparen))
                 (loop for (arg . more) on args
                       append (%expression-tokens arg item)
                       when more append (list (%punct-token :comma)))
                 (list (%punct-token :rparen))))
        (t (%items-fail 'items-malformed item "~S is not an expression" form))))))

;;; Operands

(defun %fill-pattern (elements values item)
  "The tokens of a mode pattern with VALUES in its holes, and a claim
(ALTERNATIVE START END) for each ONE-OF alternative named in VALUES, its span
counted in tokens from the pattern's start."
  (let ((tokens '()) (claims '()))
    (loop while elements
          do (let ((element (cl:pop elements)))
               (ecase (first element)
                 (:literal (setf tokens (append tokens (%literal-tokens (second element)))))
                 (:expr (when (null values)
                          (%items-fail 'items-malformed item "too few values for the operand"))
                        (setf tokens (append tokens (%expression-tokens (cl:pop values) item))))
                 (:end-of (cl:push (list (second element) (third element) (length tokens)) claims))
                 (:one-of
                  (let* ((name (cl:pop values))
                         (alternative (find-if (lambda (alternative) (%same-name-p alternative name))
                                               (%one-of-alternatives element))))
                    (unless alternative
                      (%items-fail 'items-malformed item "~S is not one of ~{~A~^, ~}"
                                   name (%one-of-alternatives element)))
                    (setf elements (append (mode-descriptor-pattern
                                            (find-mode-descriptor alternative *items-machine*))
                                           (cons (list :end-of alternative (length tokens))
                                                 elements))))))))
    (when values
      (%items-fail 'items-malformed item "too many values for the operand: ~S" values))
    (values tokens (nreverse claims))))

(defun %operand-tokens (operand item)
  "The tokens of OPERAND, the addressing mode it names, if any, and its claims
(MODE START END): the named mode over the whole operand, then its alternatives."
  (if (and (consp operand) (not (%expression-head-p (first operand))))
      (let* ((head (first operand))
             (mode (cond ((and (keywordp head) (string= (symbol-name head) "MODE"))
                          (or (%find-mode-by-name (second operand) *items-machine*)
                              (%items-fail 'items-malformed item "~S is not an addressing mode"
                                           (second operand))))
                         ((keywordp head)
                          (%items-fail 'items-malformed item "unknown operand form ~S" operand))
                         (t
                          (unless *items-backend*
                            (%items-fail 'items-malformed item "operand kind ~A needs a backend" head))
                          (let ((entry (assoc (%designator-name head)
                                              (backend-descriptor-operands *items-backend*)
                                              :test #'equal)))
                            (unless entry
                              (%items-fail 'items-malformed item "backend ~A has no operand kind ~A"
                                           (backend-descriptor-name *items-backend*) head))
                            (find-mode-descriptor (cdr entry) *items-machine*))))))
        (multiple-value-bind (tokens claims)
            (%fill-pattern (mode-descriptor-pattern mode)
                           (if (keywordp head) (cddr operand) (rest operand))
                           item)
          (values tokens mode
                  (cons (list (mode-descriptor-name mode) 0 (length tokens)) claims))))
      (values (%expression-tokens operand item) nil nil)))

;;; Operands that another alternative would win

(defun %check-operand-syntax (tokens mode item)
  (unless (nth-value 1 (try-match-operand-mode (coerce tokens 'simple-vector) mode))
    (%items-fail 'items-operand-mismatch item "the operand does not match its mode ~A"
                 (mode-descriptor-name mode))))

(defun %mode-pickables (variants)
  "The names of VARIANTS' modes, and of every ONE-OF alternative reachable from them."
  (let ((modes '()) (alternatives '()) (seen '()))
    (labels ((visit (mode)
               (unless (member mode seen)
                 (cl:push mode seen)
                 (dolist (element (mode-descriptor-pattern mode))
                   (when (eq (first element) :one-of)
                     (dolist (name (%one-of-alternatives element))
                       (cl:pushnew name alternatives :test #'%same-name-p)
                       (visit (find-mode-descriptor name *items-machine*))))))))
      (dolist (variant variants)
        (let ((mode (instruction-descriptor-mode variant)))
          (when mode
            (cl:pushnew (mode-descriptor-name mode) modes :test #'%same-name-p)
            (visit mode)))))
    (cons modes alternatives)))

(defun %mnemonic-pickables (mnemonic)
  (let ((key (string-upcase mnemonic)))
    (or (gethash key *items-pickables*)
        (setf (gethash key *items-pickables*)
              (%mode-pickables (find-instruction-variants *items-machine* mnemonic))))))

(defun %score-of (mode name tokens)
  (nth-value 6 (try-match-operand-mode tokens (or mode (find-mode-descriptor name *items-machine*)))))

(defun %claim-rival (claim line statement entry)
  "The mode the assembler chose over CLAIM's, or NIL when CLAIM holds. LINE is
the item line, STATEMENT its parsed statement and ENTRY its listing line.
A claim holds when the assembler picked that alternative for its span, or its
mode is the one the instruction's variant uses. Otherwise it loses to another
alternative picked for the same span, or, for the whole operand, to a variant
whose syntax matches more specifically."
  (destructuring-bind (name start end) claim
    (let* ((picks (listing-line-choices entry))
           (mode (instruction-descriptor-mode (listing-line-descriptor entry)))
           (tokens (statement-operand-tokens statement))
           (pickables (%mnemonic-pickables (item-line-mnemonic line))))
      (flet ((span-p (pick) (and (= (second pick) start) (= (third pick) end))))
        (cond
          ((find-if (lambda (pick) (and (span-p pick) (%same-name-p (first pick) name))) picks) nil)
          ((and mode (%same-name-p (mode-descriptor-name mode) name) (= start 0) (= end (length tokens))) nil)
          ((and (member name (cdr pickables) :test #'%same-name-p)
                (find-if #'span-p picks))
           (first (find-if #'span-p picks)))
          ((and mode (= start 0) (= end (length tokens))
                (member name (car pickables) :test #'%same-name-p)
                (%score> (%score-of mode nil tokens) (%score-of nil name tokens)))
           (mode-descriptor-name mode)))))))

(defun %check-choices (assembly lines statements unit)
  "Signal ITEMS-OPERAND-MISMATCH for an operand whose named mode lost to another
alternative of the assembled instruction (see %CLAIM-RIVAL)."
  (let ((lines (coerce lines 'simple-vector))
        (statements (coerce statements 'simple-vector)))
    (dolist (entry (assembly-listing assembly))
      (when (and (eq (listing-line-kind entry) :instruction)
                 (eq (listing-line-source-unit entry) unit)
                 (null (listing-line-definition-line entry)))
        (let* ((index (1- (listing-line-line entry)))
               (line (aref lines index)))
          (dolist (claim (item-line-claims line))
            (let ((rival (%claim-rival claim line (aref statements index) entry)))
              (when rival
                (%items-fail 'items-operand-mismatch (item-line-item line)
                             "the operand for mode ~A also matches ~A, which the assembler chose"
                             (first claim) rival)))))))))

;;; Backend operations

(defun %substitute-params (tree bindings)
  (cond ((and (symbolp tree) tree (not (keywordp tree)))
         (let ((binding (assoc (symbol-name tree) bindings :test #'string=)))
           (if binding (cdr binding) tree)))
        ((consp tree) (cons (%substitute-params (car tree) bindings)
                            (%substitute-params (cdr tree) bindings)))
        (t tree)))

(defun backend-expand-op (backend name args)
  "The instruction forms of BACKEND's operation NAME with ARGS in place of its
parameters. Signals ITEMS-MALFORMED for an unknown operation or a wrong argument count."
  (let* ((backend (find-backend backend))
         (item (list* :op name args))
         (entry (assoc (%designator-name name) (backend-descriptor-ops backend) :test #'equal)))
    (unless entry
      (%items-fail 'items-malformed item "backend ~A has no operation ~A" (backend-descriptor-name backend) name))
    (destructuring-bind (params &rest forms) (rest entry)
      (unless (= (length params) (length args))
        (%items-fail 'items-malformed item "operation ~A takes ~D argument~:P, got ~D"
                     name (length params) (length args)))
      (let ((bindings (mapcar #'cons params args)))
        (mapcar (lambda (form)
                  (cons (first form)
                        (mapcar (lambda (operand) (%substitute-params operand bindings)) (rest form))))
                forms)))))

;;; Items to lines

(defstruct item-line
  label       ; string, or NIL
  mnemonic    ; string, or NIL
  operands    ; list of token lists
  item        ; the item it came from
  claims)     ; (MODE START END) per named mode, as token indices into the operands

(defun %instruction-line (form item)
  (unless (and (consp form) (or (stringp (first form)) (and (symbolp (first form)) (not (keywordp (first form))))))
    (%items-fail 'items-malformed item "~S is not an instruction" form))
  (let ((offset 0) (claims '()) (operands '()))
    (dolist (operand (rest form))
      (multiple-value-bind (tokens mode operand-claims) (%operand-tokens operand item)
        (when mode
          (%check-operand-syntax tokens mode item))
        (dolist (claim operand-claims)
          (cl:push (list (first claim) (+ offset (second claim)) (+ offset (third claim))) claims))
        (cl:push tokens operands)
        (incf offset (1+ (length tokens)))))
    (make-item-line :mnemonic (%source-name (first form) item) :operands (nreverse operands)
                    :item item :claims (nreverse claims))))

(defun %directive-line (item)
  (destructuring-bind (name &rest args) (rest item)
    (let ((name (%source-name name item)))
      (make-item-line :mnemonic (if (and (plusp (length name)) (char= (char name 0) #\.))
                                    name
                                    (concatenate 'string "." name))
                      :operands (mapcar (lambda (arg)
                                          (if (stringp arg)
                                              (list (%string-token arg))
                                              (%expression-tokens arg item)))
                                        args)))))

(defun %item-lines (item)
  (unless (and (consp item) (listp (cdr item)) (or (symbolp (first item)) (stringp (first item))))
    (%items-fail 'items-malformed item "~S is not an item" item))
  (let ((head (first item)))
    (cond
      ((and (keywordp head) (string= (symbol-name head) "LABEL"))
       (unless (and (= (length item) 2) (or (stringp (second item)) (symbolp (second item))))
         (%items-fail 'items-malformed item "expected (:label NAME)"))
       (list (make-item-line :label (%source-name (second item) item))))
      ((and (keywordp head) (string= (symbol-name head) "DIRECTIVE"))
       (unless (rest item)
         (%items-fail 'items-malformed item "expected (:directive NAME expr...)"))
       (list (%directive-line item)))
      ((and (keywordp head) (string= (symbol-name head) "OP"))
       (unless (and (rest item) *items-backend*)
         (%items-fail 'items-malformed item "(:op NAME arg...) needs a backend"))
       (mapcar (lambda (form) (%instruction-line form item))
               (backend-expand-op *items-backend* (second item) (cddr item))))
      ((keywordp head)
       (%items-fail 'items-malformed item "unknown item ~S" head))
      (t (list (%instruction-line item item))))))

(defun %layout-line (line number)
  "The tokens of LINE, placed on source line NUMBER, and that line's text."
  (let ((tokens '()) (column 1) (out (make-string-output-stream)) (start t))
    (labels ((emit (token &optional (space t))
               (when (and space (not start))
                 (write-char #\Space out)
                 (incf column))
               (setf start nil
                     (token-line token) number
                     (token-column token) column)
               (let ((text (%token-source token)))
                 (write-string text out)
                 (incf column (length text)))
               (cl:push token tokens)))
      (when (item-line-label line)
        (let ((suffix (lexer-descriptor-label-suffix *items-lexer-descriptor*)))
          (unless suffix
            (%items-fail 'items-malformed nil "lexer ~A has no label syntax" *items-lexer*))
          (emit (%name-token (item-line-label line)))
          (emit (make-token :type :label-suffix :value :label-suffix :text suffix) nil)))
      (when (item-line-mnemonic line)
        (emit (%name-token (item-line-mnemonic line))))
      (loop for (operand . more) on (item-line-operands line)
            do (loop for token in operand
                     do (emit token))
               (when more
                 (emit (%punct-token :comma) nil))))
    (values (nreverse tokens) (get-output-stream-string out))))

;;; Assembling

(defun %find-by-name (designator table)
  "The key of hash TABLE named DESIGNATOR."
  (and designator
       (loop for key being the hash-keys of table
             when (%same-name-p key designator) return key)))

(defun %items-context (backend machine lexer)
  "Values BACKEND, MACHINE and LEXER resolved."
  (let* ((backend (and backend (find-backend backend)))
         (machine (cond ((and backend machine
                              (not (%same-name-p (backend-descriptor-machine backend) machine)))
                         (%signal-usage-error 'usage-error "backend ~A targets machine ~A, not ~A"
                                              (backend-descriptor-name backend)
                                              (backend-descriptor-machine backend) machine))
                        (backend (backend-descriptor-machine backend))
                        (machine (or (%find-machine-name machine)
                                     (%lookup-error 'unknown-machine machine "No machine named ~S" machine)))
                        (t (%signal-usage-error 'usage-error "items need a :machine or a :backend"))))
         (lexer (or (%find-by-name lexer *lexers*)
                    (%lookup-error 'unknown-lexer lexer "No lexer named ~S" lexer))))
    (values backend machine lexer)))

(defmacro %with-items-context ((backend machine lexer) &body body)
  `(multiple-value-bind (backend* machine* lexer*) (%items-context ,backend ,machine ,lexer)
     (let* ((*items-backend* backend*)
            (*items-machine* machine*)
            (*items-lexer* lexer*)
            (*items-lexer-descriptor* (find-lexer-descriptor lexer*))
            (*items-literals* (make-hash-table :test 'equal))
            (*items-pickables* (make-hash-table :test 'equal))
            (*mode-scope* machine*)
            (*register-alias-elements*
              (machine-descriptor-register-alias-elements (find-machine-descriptor machine*))))
       ,@body)))

(defun %items-source (items)
  "The statements ITEMS make, the source text they render as, its unit and the item lines."
  (let* ((lines (loop for item in items append (%item-lines item)))
         (descriptor *items-lexer-descriptor*)
         (text (make-string-output-stream))
         (token-lines (loop for line in lines
                            for number from 1
                            collect (multiple-value-bind (tokens source) (%layout-line line number)
                                      (write-line source text)
                                      tokens))))
    (let* ((text (get-output-stream-string text))
           (unit (make-source-unit :text text))
           (statements (mapcar (lambda (tokens)
                                 (let ((statement (%parse-line tokens
                                                               :mode-suffix-separator
                                                               (lexer-descriptor-mode-suffix-separator descriptor)
                                                               :hole-prefix-separator
                                                               (lexer-descriptor-hole-prefix-separator descriptor))))
                                   (setf (statement-source-unit statement) unit)
                                   statement))
                               token-lines)))
      (values statements text unit lines))))

(defun render-items (items &key backend machine (lexer 'default))
  "The assembly source text ITEMS render as. Assembling it gives the cells
ASSEMBLE-ITEMS gives."
  (%with-items-context (backend machine lexer)
    (nth-value 1 (%items-source items))))

(defun assemble-items (items &key backend machine (lexer 'default) (origin 0) memory file)
  "Assemble ITEMS, a list of items, for the machine of BACKEND (a name or
BACKEND-DESCRIPTOR) or for MACHINE. Returns an ASSEMBLY like ASSEMBLE, whose
source is the text RENDER-ITEMS gives; LEXER, ORIGIN and MEMORY are ASSEMBLE's.
FILE names the program in diagnostics. Signals ITEMS-MALFORMED for an item that
is not well formed and ITEMS-OPERAND-MISMATCH for an operand that does not
match its mode, or that the assembler read as another alternative of the
instruction's modes; the assembler's own conditions otherwise."
  (%with-items-context (backend machine lexer)
    (multiple-value-bind (statements text unit lines) (%items-source items)
      (setf (source-unit-file unit) (and file (namestring (pathname file))))
      (let ((assembly (with-source-unit unit
                        (assemble-statements statements
                                             :machine *items-machine* :lexer *items-lexer* :origin origin
                                             :memory (and memory
                                                          (or (%find-element-name *items-machine* memory)
                                                              (%signal-usage-error 'usage-error "No memory named ~S" memory)))
                                             :source text :source-unit unit))))
        (%check-choices assembly lines statements unit)
        assembly))))

(defun %find-element-name (machine designator)
  (let ((element (find-if (lambda (element) (%same-name-p (storage-element-name element) designator))
                          (machine-descriptor-elements (find-machine-descriptor machine)))))
    (and element (storage-element-name element))))

;;; Files

(defun %program-fail (control &rest args)
  (%items-fail 'items-malformed nil "~?" control args))

(defun %parse-program (form)
  (unless (and (consp form) (listp (cdr form)) (keywordp (first form)) (string= (symbol-name (first form)) "PROGRAM")
               (listp (second form)))
    (%program-fail "expected (:program (option...) item...)"))
  (let ((program (make-items-program :items (cddr form))) (options (second form)))
    (unless (evenp (length options))
      (%program-fail "the :program options must be keyword/value pairs"))
    (loop for (key value) on options by #'cddr
          do (let ((name (and (keywordp key) (symbol-name key))))
               (cond ((equal name "BACKEND") (setf (items-program-backend program) value))
                     ((equal name "MACHINE") (setf (items-program-machine program) value))
                     ((equal name "MEMORY") (setf (items-program-memory program) value))
                     ((equal name "LEXER") (setf (items-program-lexer program) value))
                     ((equal name "ORIGIN")
                      (unless (typep value '(integer 0))
                        (%program-fail ":origin must be a non-negative integer, got ~S" value))
                      (setf (items-program-origin program) value))
                     (t (%program-fail "unknown :program option ~S" key)))))
    program))

(defun read-items (path)
  "The ITEMS-PROGRAM in the file PATH: one (:program (option...) item...) form,
with options :backend, :machine, :memory, :lexer and :origin. The file is
untrusted: it is read without evaluation and without interning symbols.
Signals ITEMS-MALFORMED for an unreadable or malformed file."
  (%parse-program
   (with-open-file (in path)
     (read-restricted-form in (lambda (control &rest args)
                                (apply #'%program-fail control args))
                           path :bare :uninterned))))

(defun read-items-from-string (string)
  "Like READ-ITEMS, for the text STRING."
  (%parse-program
   (with-input-from-string (in string)
     (read-restricted-form in (lambda (control &rest args)
                                (apply #'%program-fail control args))
                           "items" :bare :uninterned))))

(defun assemble-items-file (path &key backend machine lexer origin memory)
  "Read the items program at PATH and assemble it. The keys override the
program's own options."
  (let* ((program (read-items path))
         (truename (truename path))
         (*include-directory* (%file-directory truename))
         (*include-chain* (list truename))
         (assembly (assemble-items (items-program-items program)
                                   :backend (or backend (items-program-backend program))
                                   :machine (or machine (items-program-machine program))
                                   :lexer (or lexer (items-program-lexer program) 'default)
                                   :origin (or origin (items-program-origin program) 0)
                                   :memory (or memory (items-program-memory program))
                                   :file path)))
    (setf (source-unit-path (assembly-source-unit assembly)) (namestring truename))
    assembly))
