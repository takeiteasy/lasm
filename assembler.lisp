;;;; assembler.lisp
;;;; The M1 assembler pass: turns a STATEMENT list (parser.lisp) into encoded
;;;; bytes, resolving labels along the way.
;;;;
;;;; Two passes, not the two-pass assembly M2 defers (LASM-plan.md sec. 2):
;;;; M2's two-pass is about *mode selection* needing label values first
;;;; (e.g. choosing zero-page vs absolute once an address is known). Here,
;;;; in M1, every instruction has exactly one mode and a fixed operand
;;;; width, so an instruction's size never depends on a label value -- a
;;;; layout pass can size and place every statement before any operand is
;;;; evaluated. Pass 1 (layout) binds every label to an address; pass 2
;;;; (encode) evaluates operands against the complete symbol table and
;;;; emits bytes. This is what buys forward references (`jmp end` ... `end:`)
;;;; for free.
;;;;
;;;; Local labels (a name starting with a non-alphanumeric prefix char, e.g.
;;;; ".loop") are NOT scoped to an enclosing global label in M1 -- they are
;;;; ordinary global names, sharing one flat symbol table with everything
;;;; else. Binding a local label to its enclosing label is M2 (#16).

(in-package #:lasm)

;;; Conditions

(define-condition assembly-error (lasm-syntax-error) ()
  (:documentation "Signalled by ASSEMBLE-STATEMENTS on a malformed program:
a duplicate label, or an operand count that does not match an instruction's
declared addressing mode. Undefined labels are not this condition -- they
surface as UNRESOLVED-LABEL from EVAL-EXPR, since that condition already
names exactly this failure."))

(defun %assembly-error (line fmt &rest args)
  (error 'assembly-error :message (apply #'format nil fmt args) :line line))

;;; Result

(defstruct assembly
  (bytes nil :type (or null (vector (unsigned-byte 8))))
  (origin 0 :type (integer 0))
  (symbols nil :type (or null hash-table)))  ; string -> address

;;; Pass 1: layout -- size every statement and bind every label

(defun %check-operand-count (statement descriptor)
  (let* ((mode (instruction-descriptor-mode descriptor))
         (n (length (statement-operands statement))))
    (cond
      ((> n 1)
       (%assembly-error (statement-line statement)
                         "~A takes at most one operand in M1, got ~D"
                         (statement-mnemonic statement) n))
      ((and mode (= n 0))
       (%assembly-error (statement-line statement)
                         "~A requires an operand (~(~A~) addressing mode)"
                         (statement-mnemonic statement) mode))
      ((and (not mode) (= n 1))
       (%assembly-error (statement-line statement)
                         "~A takes no operand" (statement-mnemonic statement))))))

(defun %layout (statements machine origin)
  "Returns (VALUES symbols sized-statements) where SYMBOLS is a string ->
address hash table and SIZED-STATEMENTS pairs each mnemonic-bearing
statement with its address and INSTRUCTION-DESCRIPTOR, in order."
  (let ((symbols (make-hash-table :test 'equal))
        (address origin)
        sized)
    (dolist (statement statements)
      (when (statement-label statement)
        (when (nth-value 1 (gethash (statement-label statement) symbols))
          (%assembly-error (statement-line statement)
                            "Duplicate label ~S" (statement-label statement)))
        (setf (gethash (statement-label statement) symbols) address))
      (when (statement-mnemonic statement)
        (let ((descriptor (find-instruction machine (statement-mnemonic statement))))
          (%check-operand-count statement descriptor)
          (cl:push (list address descriptor statement) sized)
          (incf address (1+ (or (instruction-descriptor-operand-width descriptor) 0))))))
    (values symbols (nreverse sized))))

;;; Pass 2: encode -- evaluate operands against the completed symbol table

(defun %encode (sized-statements symbols)
  (let (bytes)
    (dolist (entry sized-statements)
      (destructuring-bind (address descriptor statement) entry
        (declare (ignore address))
        (let ((value (when (instruction-descriptor-mode descriptor)
                       (eval-expr (match-operand-mode (first (statement-operands statement))
                                                       (instruction-descriptor-mode descriptor))
                                  :symbols symbols))))
          (dolist (byte (encode-instruction descriptor value))
            (cl:push byte bytes)))))
    (coerce (nreverse bytes) '(vector (unsigned-byte 8)))))

;;; Entry points

(defun assemble-statements (statements &key machine (origin 0))
  "Assemble a STATEMENT list (parser.lisp) targeting MACHINE into an
ASSEMBLY. Signals ASSEMBLY-ERROR on a duplicate label or a mismatched
operand count, UNKNOWN-INSTRUCTION on an unregistered mnemonic, and
UNRESOLVED-LABEL (via EVAL-EXPR) on a reference to a label that is never
defined anywhere in STATEMENTS."
  (multiple-value-bind (symbols sized) (%layout statements machine origin)
    (make-assembly :bytes (%encode sized symbols) :origin origin :symbols symbols)))

(defun assemble (source &key machine (lexer 'default) (origin 0))
  "Tokenize and parse SOURCE with LEXER (lexer.lisp/parser.lisp), then
ASSEMBLE-STATEMENTS the result targeting MACHINE. See ASSEMBLE-STATEMENTS
for the conditions this can signal, plus LEX-ERROR/PARSE-FAILURE from the
front end."
  (assemble-statements (parse source :lexer lexer) :machine machine :origin origin))
