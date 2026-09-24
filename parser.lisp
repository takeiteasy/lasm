;;;; parser.lisp
;;;; A fixed line/statement grammar shared across all target machines, and a
;;;; precedence-climbing (Pratt) expression parser reusable at every operand
;;;; `expr` hole an addressing mode declares (M2's DEFMODE / M1's built-in
;;;; IMMEDIATE and ABSOLUTE modes, instruction.lisp).
;;;;
;;;; Scope: this stops at the AST. Label references stay symbolic
;;;; (EXPR-LABEL, unresolved) and operands are handed back as raw token runs
;;;; -- no symbol table and no expression evaluation here; those belong to
;;;; instruction.lisp (EVAL-EXPR) and assembler.lisp (label resolution), and
;;;; addressing-mode matching / operand encoding belong to instruction.lisp.
;;;; The location-counter symbol "*" (EXPR-LOCATION, #15) is likewise left
;;;; unresolved here -- EVAL-EXPR folds it against the assembler's current
;;;; address.
;;;;
;;;; Grammar (one STATEMENT per source line):
;;;;   line      := [label-def] [mnemonic [operands]]
;;;;             |  [label-def] identifier "=" expr-tokens
;;;;   label-def := identifier label-suffix
;;;;   mnemonic  := identifier [mode-suffix-separator identifier]
;;;;   operands  := operand ("," operand)*
;;;; Blank and comment-only lines produce no statement.
;;;;
;;;; A mnemonic's trailing "separator identifier" piece (#40, e.g. the ".w"
;;;; in "lda.w") is a forced addressing-mode suffix, not part of the
;;;; mnemonic proper -- %SPLIT-MNEMONIC-SUFFIX below splits it off (using
;;;; the active lexer's MODE-SUFFIX-SEPARATOR, lexer.lisp) into STATEMENT's
;;;; own MODE-SUFFIX slot, so mode.lisp/assembler.lisp never see a dotted
;;;; mnemonic string. Only the ordinary-mnemonic line form does this split;
;;;; the "identifier = expr-tokens" sugar and label/symbol-name positions
;;;; are untouched.
;;;;
;;;; The second line form ("name = value", #35) is pure surface sugar for
;;;; ".equ name, value" -- %PARSE-LINE below recognizes an identifier
;;;; followed by "=" and rewrites it to a statement whose mnemonic is
;;;; +ASSIGNMENT-DIRECTIVE-NAME+ with two operands (the name, then whatever
;;;; follows "="), so the assembler (assembler.lisp, #35) has exactly one
;;;; .EQU code path regardless of which spelling a program uses.

(in-package #:lasm)

;;; AST structures

(defstruct statement
  label           ; string, or nil
  label-localp    ; T if LABEL starts with the lexer's local-label prefix --
                   ; the assembler (assembler.lisp, #16) scopes a local label
                   ; definition to the nearest preceding non-local one.
  mnemonic        ; string, or nil (label-only line)
  operands        ; list of OPERAND, split on top-level commas -- a general
                   ; statement-grammar product; a multi-operand instruction
                   ; (instruction.lisp) instead reaches its operands through
                   ; a multi-hole addressing-mode pattern (mode.lisp), whose
                   ; own literal commas would be unmatchable if this split
                   ; were applied first, so the assembler doesn't read this
                   ; field to match an addressing mode (below). A directive
                   ; statement (directive.lisp, #14) reads this field instead
                   ; -- e.g. .byte 1, 2, 3's three comma-separated operands.
  (operand-tokens #() :type simple-vector)  ; every token after the mnemonic,
                   ; commas included -- addressing-mode matching (mode.lisp)
                   ; needs the whole run uncommitted to any comma split,
                   ; since a mode's own pattern may include a literal ","
                   ; (e.g. INDEXED-X: expr "," "X"); empty when there is no
                   ; mnemonic or no operand tokens
  mode-suffix     ; string, or nil -- a gas-style forced addressing-mode
                   ; suffix split off the mnemonic by %SPLIT-MNEMONIC-SUFFIX
                   ; below (e.g. "w" from "lda.w", #40), naming the mode
                   ; (mode.lisp's DEFMODE :SUFFIX) the assembler must use for
                   ; this statement's operand regardless of what it folds
                   ; to -- see assembler.lisp's %CHOOSE-VARIANT. NIL when
                   ; the mnemonic has no suffix, or when the active lexer
                   ; disables mode-suffix syntax entirely (its
                   ; MODE-SUFFIX-SEPARATOR is NIL).
  line            ; source line number, for diagnostics
  source-unit
  definition-line ; macro body line, or NIL outside an expansion
  definition-unit)

(defstruct source-unit file text (children (make-hash-table)))

(defstruct operand
  tokens)     ; simple-vector of raw tokens for this operand -- MATCH-OPERAND-MODE
              ; (instruction.lisp) matches an addressing-mode pattern against
              ; this and calls PARSE-EXPRESSION on the pattern's `expr` hole(s)

(defstruct expr-number value)
(defstruct expr-label name localp line column) ; NAME unresolved; LOCALP set from
                                             ; the lexer's LOCAL-LABEL-PREFIX
                                             ; (lexer.lisp's TOKEN-LOCALP) --
                                             ; scoping the reference to its
                                             ; enclosing global label is the
                                             ; assembler's job (assembler.lisp,
                                             ; #16).
(defstruct expr-location)                   ; the location-counter symbol "*"
                                             ; in operand position -- folds to
                                             ; the current statement's address
                                             ; (#15). No slots; it IS the
                                             ; value, resolved by EVAL-EXPR's
                                             ; :PC argument (instruction.lisp).
(defstruct expr-unary op operand)           ; OP one of :neg :pos :lognot :not :lo :hi
                                             ;          :bank :lowcell :highcell
                                             ; (:bank's operand is an EXPR-LABEL or EXPR-LOCATION)
(defstruct expr-binary op left right)       ; OP one of :pipe :caret :amp :shl :shr
                                             ;          :plus :minus :star :slash :percent
                                             ;          :lt :gt :le :ge :eq :ne :andand :oror

;;; Shared error helper

(defun %parse-error (tok fmt &rest args)
  (error 'parse-failure :message (apply #'format nil fmt args)
                        :line (and tok (token-line tok))
                        :column (and tok (token-column tok))))

;;; Expression parser (precedence climbing / Pratt)

;; Left-associative binary operator precedence, lowest-binding first. All
;; unary operators bind tighter than any binary operator.
(defparameter *binary-precedence*
  '((:oror . 1) (:andand . 2)
    (:lt . 3) (:gt . 3) (:le . 3) (:ge . 3) (:eq . 3) (:ne . 3)
    (:pipe . 4) (:caret . 5) (:amp . 6) (:shl . 7) (:shr . 7)
    (:plus . 8) (:minus . 8) (:star . 9) (:slash . 9) (:percent . 9)))

(defparameter *unary-ops*
  '((:minus . :neg) (:plus . :pos) (:tilde . :lognot) (:bang . :not) (:lt . :lo) (:gt . :hi)))

(defun %tok (tokens i end)
  (when (< i end) (aref tokens i)))

(defun %punct-value (tok)
  (and tok (eq (token-type tok) :punctuation) (token-value tok)))

(defun %parse-primary (tokens i end)
  (let ((tok (%tok tokens i end)))
    (cond
      ((null tok) (%parse-error tok "Unexpected end of expression"))
      ((eq (token-type tok) :number)
       (values (make-expr-number :value (token-value tok)) (1+ i)))
      ((eq (token-type tok) :identifier)
       (values (make-expr-label :name (token-value tok) :localp (token-localp tok)
                                :line (token-line tok) :column (token-column tok))
               (1+ i)))
      ((eq (token-type tok) :location-counter)
       (values (make-expr-location) (1+ i)))
      ((eq (token-type tok) :function-operator)
       (unless (eq (%punct-value (%tok tokens (1+ i) end)) :lparen)
         (%parse-error tok "Expected \"(\" after ~A" (token-text tok)))
       (multiple-value-bind (inner next-i) (%parse-binary tokens (+ i 2) end 0)
         (when (and (eq (token-value tok) :bank)
                    (not (or (expr-label-p inner) (expr-location-p inner))))
           (%parse-error tok "~A() takes a label or *" (token-text tok)))
         (let ((close (%tok tokens next-i end)))
           (unless (eq (%punct-value close) :rparen)
             (%parse-error close "Expected closing parenthesis, found ~:[end of expression~;~:*~S~]"
                            (and close (token-text close))))
           (values (make-expr-unary :op (token-value tok) :operand inner) (1+ next-i)))))
      ((eq (%punct-value tok) :star)
       ;; The location-counter symbol (#15): "*" in operand/primary position
       ;; is the current address, not multiplication -- precedence climbing
       ;; only reaches %PARSE-PRIMARY where an operand is expected, so this
       ;; never shadows "*" as the binary multiply operator once a left
       ;; operand exists (2 * 3 still multiplies; "*+2" and "lda *" don't).
       (values (make-expr-location) (1+ i)))
      ((eq (%punct-value tok) :lparen)
       (multiple-value-bind (inner next-i) (%parse-binary tokens (1+ i) end 0)
         (let ((close (%tok tokens next-i end)))
           (unless (eq (%punct-value close) :rparen)
             (%parse-error close "Expected closing parenthesis, found ~:[end of expression~;~:*~S~]"
                            (and close (token-text close))))
           (values inner (1+ next-i)))))
      (t (%parse-error tok "Unexpected token in expression: ~S" (token-text tok))))))

(defun %parse-unary (tokens i end)
  (let* ((tok (%tok tokens i end))
         (op (assoc (%punct-value tok) *unary-ops*)))
    (if op
        (multiple-value-bind (operand next-i) (%parse-unary tokens (1+ i) end)
          (values (make-expr-unary :op (cdr op) :operand operand) next-i))
        (%parse-primary tokens i end))))

(defun %parse-binary (tokens i end min-prec)
  (multiple-value-bind (left i) (%parse-unary tokens i end)
    (loop
      (let* ((tok (%tok tokens i end))
             (prec (cdr (assoc (%punct-value tok) *binary-precedence*))))
        (unless (and prec (>= prec min-prec))
          (return (values left i)))
        (multiple-value-bind (right next-i)
            (%parse-binary tokens (1+ i) end (1+ prec))
          (setf left (make-expr-binary :op (token-value tok) :left left :right right)
                i next-i))))))

(defun parse-expression (tokens &key (start 0) (end (length tokens)))
  "Parse a single expression from the SIMPLE-VECTOR TOKENS between START and
END. Returns (VALUES ast next-index) so a caller (e.g. MATCH-OPERAND-MODE,
matching an addressing-mode pattern) can parse one `expr` hole out of a
longer token run and continue from NEXT-INDEX. Signals PARSE-FAILURE on
malformed input."
  (%parse-binary tokens start end 0))

;;; Statement grammar

(defparameter +assignment-directive-name+ ".equ"
  "The directive mnemonic %PARSE-LINE rewrites \"name = value\" (#35) to --
kept as its own name (rather than a literal string at the call site) so the
one place that couples the \"=\" sugar to the .EQU directive is visible from
its name. Matched case-insensitively by FIND-DIRECTIVE-DESCRIPTOR
(directive.lisp) like any other mnemonic, so this need not match whatever
case a program's own \".equ\" spelling uses.")

(defun %split-operands (tokens)
  "Split a list of TOKENS on top-level commas (commas nested inside
parentheses or brackets do not split) into a list of token-lists, one per
operand. Bracket depth (#103) shares DEPTH with paren depth -- a mode
pattern's own literal \"[\"/\"]\" (e.g. an indirect \"[\" expr \"]\" hole) is
just as entitled to hide a comma as a literal \"(\"/\")\" already is."
  (let (groups current (depth 0))
    (dolist (tok tokens)
      (case (%punct-value tok)
        ((:lparen :lbracket) (incf depth) (cl:push tok current))
        ((:rparen :rbracket) (decf depth) (cl:push tok current))
        (:comma (if (zerop depth)
                    (progn (cl:push (nreverse current) groups) (setf current nil))
                    (cl:push tok current)))
        (t (cl:push tok current))))
    (cl:push (nreverse current) groups)
    (nreverse groups)))

(defun %split-mnemonic-suffix (mnemonic-text separator)
  "Split MNEMONIC-TEXT on the last occurrence of SEPARATOR (a non-empty
string, or NIL to disable mode-suffix syntax entirely, #40), returning
(VALUES base suffix) -- SUFFIX is NIL and BASE is MNEMONIC-TEXT unchanged
when SEPARATOR is NIL, doesn't occur, or occurs only at position 0 (an
empty base is never a suffix split -- there is no bare mnemonic to its
left). :FROM-END T picks the *last* occurrence, so a hypothetical dotted
base mnemonic name still yields the rightmost dot-separated piece as the
suffix rather than the whole tail after the first dot."
  (if (null separator)
      (values mnemonic-text nil)
      (let ((pos (search separator mnemonic-text :from-end t)))
        (if (and pos (plusp pos))
            (values (subseq mnemonic-text 0 pos)
                    (subseq mnemonic-text (+ pos (length separator))))
            (values mnemonic-text nil)))))

(defun %collapse-hole-prefixes (tokens start separator)
  "Replace each identifier token immediately followed by a SEPARATOR token,
from START on, with one :HOLE-PREFIX token whose value is the identifier."
  (if (null separator)
      tokens
      (let ((out (coerce (subseq tokens 0 start) 'list)) (i start) (len (length tokens)))
        (loop while (< i len)
              do (let ((tok (aref tokens i)) (next (and (< (1+ i) len) (aref tokens (1+ i)))))
                   (if (and next (eq (token-type tok) :identifier)
                            (string= (token-text next) separator))
                       (progn (cl:push (make-token :type :hole-prefix :value (token-value tok)
                                                   :text (concatenate 'string (token-text tok) separator)
                                                   :line (token-line tok) :column (token-column tok))
                                       out)
                              (incf i 2))
                       (progn (cl:push tok out) (incf i)))))
        (coerce (nreverse out) 'simple-vector))))

(defun %parse-line (line-tokens &key mode-suffix-separator hole-prefix-separator)
  (let* ((tokens (coerce line-tokens 'simple-vector))
         (len (length tokens))
         (pos 0) label label-localp mnemonic operands (operand-tokens #())
         mode-suffix)
    (when (and (< (1+ pos) len)
               (eq (token-type (aref tokens pos)) :identifier)
               (eq (token-type (aref tokens (1+ pos))) :label-suffix))
      (setf label (token-value (aref tokens pos))
            label-localp (token-localp (aref tokens pos)))
      (incf pos 2))
    (when (and (< pos len) (eq (token-type (aref tokens pos)) :function-operator))
      (setf (token-type (aref tokens pos)) :identifier
            (token-value (aref tokens pos)) (token-text (aref tokens pos))))
    (setf tokens (%collapse-hole-prefixes tokens pos hole-prefix-separator)
          len (length tokens))
    (cond
      ;; "name = value" (#35): sugar for ".equ name, value" -- checked before
      ;; the ordinary mnemonic case below, since an identifier followed by
      ;; "=" would otherwise be read as a bare mnemonic with a stray "="
      ;; operand token.
      ((and (< (1+ pos) len)
            (eq (token-type (aref tokens pos)) :identifier)
            (eq (%punct-value (aref tokens (1+ pos))) :equals))
       (let ((name-tok (aref tokens pos)))
         (when (= (+ pos 2) len)
           (%parse-error (aref tokens (1+ pos)) "Expected expression after \"=\""))
         (setf mnemonic +assignment-directive-name+
               operand-tokens (subseq tokens pos len)
               operands (list (make-operand :tokens (vector name-tok))
                               (make-operand :tokens (subseq tokens (+ pos 2) len))))))
      ((and (< pos len) (eq (token-type (aref tokens pos)) :identifier))
       (let ((mnemonic-tok (aref tokens pos)))
         (multiple-value-setq (mnemonic mode-suffix)
           (%split-mnemonic-suffix (token-value mnemonic-tok) mode-suffix-separator))
         (incf pos 1)
         (when (< pos len)
           (setf operand-tokens (subseq tokens pos len))
           (setf operands
                 (mapcar (lambda (group)
                           (when (null group)
                             ;; #74: a stray comma (e.g. "lda 1,,2") leaves no
                             ;; token of its own to point at -- the mnemonic
                             ;; token is the closest anchor this line has.
                             (%parse-error mnemonic-tok
                                           "~A: empty operand" (token-text mnemonic-tok)))
                           (make-operand :tokens (coerce group 'simple-vector)))
                         (%split-operands (coerce (subseq tokens pos len) 'list))))))))
    (when (and (< pos len) (null mnemonic))
      (%parse-error (aref tokens pos) "Expected mnemonic, found ~S" (token-text (aref tokens pos))))
    (make-statement :label label :label-localp label-localp
                     :mnemonic mnemonic :operands operands
                     :operand-tokens operand-tokens
                     :mode-suffix mode-suffix
                     :line (token-line (aref tokens 0)))))

(defun %split-lines (tokens)
  "Split a token vector (as returned by TOKENIZE, ending in :EOF) into a
list of non-empty lists of non-newline tokens, one per source line."
  (let (lines current)
    (loop for tok across tokens
          do (cond
               ((eq (token-type tok) :eof)
                (when current (cl:push (nreverse current) lines)))
               ((eq (token-type tok) :newline)
                (when current (cl:push (nreverse current) lines))
                (setf current nil))
               (t (cl:push tok current))))
    (nreverse lines)))

(defun parse (string &key (lexer 'default) file)
  "Tokenize STRING with LEXER and parse it into a list of STATEMENT structs,
one per non-blank source line. FILE, when supplied, names the source in
diagnostics. The second value is its source unit, used by include expansion
and listings. Signals LEX-ERROR or PARSE-FAILURE with source context."
  (let ((unit (make-source-unit :file (and file (namestring (pathname file))) :text string)))
    (with-source-unit unit
      (let* ((descriptor (find-lexer-descriptor lexer))
             (separator (lexer-descriptor-mode-suffix-separator descriptor))
             (prefix-separator (lexer-descriptor-hole-prefix-separator descriptor)))
        (values
         (mapcar (lambda (line-tokens)
                   (let ((statement (%parse-line line-tokens :mode-suffix-separator separator
                                                   :hole-prefix-separator prefix-separator)))
                     (setf (statement-source-unit statement) unit)
                     statement))
                 (%split-lines (tokenize string :lexer lexer)))
         unit)))))

(defun %conditional-mnemonic (statement)
  "The conditional-assembly keyword STATEMENT's mnemonic spells (:IF :ELSEIF
:ELSE :ENDIF), or NIL."
  (let ((mnemonic (statement-mnemonic statement)))
    (and mnemonic
         (cdr (assoc mnemonic '((".if" . :if) (".elseif" . :elseif)
                                (".else" . :else) (".endif" . :endif))
                     :test #'string-equal)))))
