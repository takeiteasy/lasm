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
;;;;   label-def := identifier label-suffix
;;;;   operands  := operand ("," operand)*
;;;; Blank and comment-only lines produce no statement.

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
  line)           ; source line number, for diagnostics

(defstruct operand
  tokens)     ; simple-vector of raw tokens for this operand -- MATCH-OPERAND-MODE
              ; (instruction.lisp) matches an addressing-mode pattern against
              ; this and calls PARSE-EXPRESSION on the pattern's `expr` hole(s)

(defstruct expr-number value)
(defstruct expr-label name localp)          ; NAME unresolved; LOCALP set from
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
(defstruct expr-unary op operand)           ; OP one of :neg :pos :lognot :lo :hi
(defstruct expr-binary op left right)       ; OP one of :pipe :caret :amp :shl :shr
                                             ;          :plus :minus :star :slash

;;; Shared error helper

(defun %parse-error (tok fmt &rest args)
  (error 'parse-failure :message (apply #'format nil fmt args)
                        :line (and tok (token-line tok))
                        :column (and tok (token-column tok))))

;;; Expression parser (precedence climbing / Pratt)

;; Left-associative binary operator precedence, lowest-binding first. All
;; unary operators bind tighter than any binary operator.
(defparameter *binary-precedence*
  '((:pipe . 1) (:caret . 2) (:amp . 3) (:shl . 4) (:shr . 4)
    (:plus . 5) (:minus . 5) (:star . 6) (:slash . 6)))

(defparameter *unary-ops*
  '((:minus . :neg) (:plus . :pos) (:tilde . :lognot) (:lt . :lo) (:gt . :hi)))

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
       (values (make-expr-label :name (token-value tok) :localp (token-localp tok))
               (1+ i)))
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
             (%parse-error close "Expected closing parenthesis"))
           (values inner (1+ next-i)))))
      (t (%parse-error tok "Unexpected token in expression")))))

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

(defun %split-operands (tokens)
  "Split a list of TOKENS on top-level commas (commas nested inside
parentheses do not split) into a list of token-lists, one per operand."
  (let (groups current (depth 0))
    (dolist (tok tokens)
      (case (%punct-value tok)
        (:lparen (incf depth) (cl:push tok current))
        (:rparen (decf depth) (cl:push tok current))
        (:comma (if (zerop depth)
                    (progn (cl:push (nreverse current) groups) (setf current nil))
                    (cl:push tok current)))
        (t (cl:push tok current))))
    (cl:push (nreverse current) groups)
    (nreverse groups)))

(defun %parse-line (line-tokens)
  (let* ((tokens (coerce line-tokens 'simple-vector))
         (len (length tokens))
         (pos 0) label label-localp mnemonic operands (operand-tokens #()))
    (when (and (< (1+ pos) len)
               (eq (token-type (aref tokens pos)) :identifier)
               (eq (token-type (aref tokens (1+ pos))) :label-suffix))
      (setf label (token-value (aref tokens pos))
            label-localp (token-localp (aref tokens pos)))
      (incf pos 2))
    (when (and (< pos len) (eq (token-type (aref tokens pos)) :identifier))
      (setf mnemonic (token-value (aref tokens pos)))
      (incf pos 1)
      (when (< pos len)
        (setf operand-tokens (subseq tokens pos len))
        (setf operands
              (mapcar (lambda (group)
                        (when (null group)
                          (%parse-error nil "Empty operand"))
                        (make-operand :tokens (coerce group 'simple-vector)))
                      (%split-operands (coerce (subseq tokens pos len) 'list))))))
    (when (and (< pos len) (null mnemonic))
      (%parse-error (aref tokens pos) "Expected mnemonic"))
    (make-statement :label label :label-localp label-localp
                     :mnemonic mnemonic :operands operands
                     :operand-tokens operand-tokens
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

(defun parse (string &key (lexer 'default))
  "Tokenize STRING with LEXER and parse it into a list of STATEMENT structs,
one per non-blank source line. Signals LEX-ERROR or PARSE-FAILURE on
malformed input."
  (mapcar #'%parse-line (%split-lines (tokenize string :lexer lexer))))
