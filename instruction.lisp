;;;; instruction.lisp
;;;; DEFINSTRUCTION: an instruction descriptor carrying an addressing mode,
;;;; an encoding form (opcode + operand width), and a semantics body
;;;; expanded through WITH-MACHINE-BINDINGS (semantics.lisp) rather than a
;;;; parallel evaluator -- so instruction semantics and the standalone M0
;;;; examples share one vocabulary.
;;;;
;;;; Also implements matching one parsed OPERAND (parser.lisp) against a
;;;; single addressing mode and turning it into encoded bytes. M1 keeps mode
;;;; resolution trivial -- exactly one mode per instruction, IMMEDIATE or
;;;; ABSOLUTE, from a small built-in table (*BUILTIN-MODE-PREFIXES*) rather
;;;; than a user-declarative DEFMODE, which is M2's job. The table is kept
;;;; separate from the call sites specifically so DEFMODE can replace it
;;;; later without touching MATCH-OPERAND-MODE's callers.
;;;;
;;;; Scope: this stops at "one instruction + one already-evaluated operand ->
;;;; bytes / executed effect". There is no statement-list driver, no symbol
;;;; table, and no label resolution here -- EVAL-EXPR-CONSTANT folds constant
;;;; expressions only and signals UNRESOLVED-LABEL on an EXPR-LABEL; full
;;;; expression evaluation against a resolved symbol table belongs to the
;;;; assembler pass.

(in-package #:lasm)

;;; Conditions

(define-condition unresolved-label (lasm-error)
  ((name :initarg :name :reader unresolved-label-name))
  (:report (lambda (c s)
             (format s "Cannot fold constant expression: unresolved label ~S"
                     (unresolved-label-name c)))))

(define-condition unknown-instruction (lasm-error)
  ((machine :initarg :machine :reader unknown-instruction-machine)
   (mnemonic :initarg :mnemonic :initform nil :reader unknown-instruction-mnemonic)
   (opcode :initarg :opcode :initform nil :reader unknown-instruction-opcode))
  (:documentation "Signalled by FIND-INSTRUCTION or FIND-INSTRUCTION-BY-OPCODE
on an unregistered mnemonic or opcode -- distinguishable from an unrelated
crash, which matters once an emulator loop wants to handle \"no such
opcode\" as a decode failure rather than let it propagate as a generic
error.")
  (:report (lambda (c s)
             (if (unknown-instruction-mnemonic c)
                 (format s "No instruction ~S registered on machine ~S"
                         (unknown-instruction-mnemonic c) (unknown-instruction-machine c))
                 (format s "No instruction with opcode ~S registered on machine ~S"
                         (unknown-instruction-opcode c) (unknown-instruction-machine c))))))

;;; Instruction descriptor

(defstruct instruction-descriptor
  (name nil :type string)      ; mnemonic, upcased
  (machine nil :type symbol)
  (mode nil :type (or null (member :immediate :absolute)))  ; nil = no operand
  (opcode nil :type (integer 0))
  (operand-width nil :type (or null (integer 1)))  ; bytes, nil = no operand
  (semantics-fn nil :type (or null function))
  ;; Parsed and stored, not used (#19) -- there is no timing model yet.
  ;; Accepted because users will copy LASM-plan.md sec. 3.2's (cycles n)
  ;; verbatim.
  (cycles nil :type (or null (integer 0))))

;;; Addressing modes
;;;
;;; Each M1 mode is a literal prefix token (or none) followed by exactly one
;;; `expr` hole, matching LASM-plan.md sec. 3.4's DEFMODE shape without the
;;; declarative macro. "#" already lexes to :HASH (lexer.lisp) for exactly
;;; this purpose.

(defparameter *builtin-mode-prefixes*
  '((:immediate . :hash)
    (:absolute . nil)))

(defun %mode-keyword (sym)
  (let ((name (string-upcase (symbol-name sym))))
    (cond
      ((string= name "IMMEDIATE") :immediate)
      ((string= name "ABSOLUTE") :absolute)
      (t (error "Unknown addressing mode ~S -- M1 only supports IMMEDIATE and ~
ABSOLUTE; multi-mode resolution (DEFMODE) is M2" sym)))))

(defun match-operand-mode (op mode)
  "Match OPERAND struct OP's token run (parser.lisp) against the M1
built-in addressing MODE (:IMMEDIATE or :ABSOLUTE): consume MODE's literal
prefix token if it has one, then parse the remaining tokens as a single
expression. Returns the EXPR-* AST. Signals PARSE-FAILURE if the tokens
don't match MODE or leave an unconsumed trailing token."
  (let* ((tokens (operand-tokens op))
         (end (length tokens))
         (prefix (cdr (assoc mode *builtin-mode-prefixes*)))
         (start 0))
    (when prefix
      (let ((tok (%tok tokens 0 end)))
        (unless (eq (%punct-value tok) prefix)
          (%parse-error tok "Operand does not match ~(~A~) addressing mode" mode))
        (setf start 1)))
    (multiple-value-bind (ast next-i) (parse-expression tokens :start start :end end)
      (when (< next-i end)
        (%parse-error (%tok tokens next-i end) "Unexpected trailing token in operand"))
      ast)))

;;; Constant folding (the evaluated-operand slice of full expression evaluation)

(defun eval-expr-constant (ast)
  "Fold the EXPR-* AST node AST (parser.lisp) to an integer. Signals
UNRESOLVED-LABEL on an EXPR-LABEL -- full evaluation against a resolved
symbol table belongs to the assembler pass; this is the constant-only
piece the assembler pass will call once labels are bound."
  (etypecase ast
    (expr-number (expr-number-value ast))
    (expr-label (error 'unresolved-label :name (expr-label-name ast)))
    (expr-unary
     (let ((v (eval-expr-constant (expr-unary-operand ast))))
       (ecase (expr-unary-op ast)
         (:neg (- v))
         (:pos v)
         (:lognot (lognot v))
         (:lo (logand v #xff))
         (:hi (logand (ash v -8) #xff)))))
    (expr-binary
     (let ((l (eval-expr-constant (expr-binary-left ast)))
           (r (eval-expr-constant (expr-binary-right ast))))
       (ecase (expr-binary-op ast)
         (:pipe (logior l r))
         (:caret (logxor l r))
         (:amp (logand l r))
         (:shl (ash l r))
         (:shr (ash l (- r)))
         (:plus (+ l r))
         (:minus (- l r))
         (:star (* l r))
         (:slash (truncate l r)))))))

;;; DEFINSTRUCTION registration

(defun register-instruction! (machine-name descriptor)
  (let* ((md (find-machine-descriptor machine-name))
         (name (instruction-descriptor-name descriptor))
         (old (gethash name (machine-descriptor-instructions md))))
    ;; A redefinition under the same mnemonic with a different opcode must
    ;; not leave the old opcode pointing at this descriptor too -- opcode
    ;; lookup (FIND-INSTRUCTION-BY-OPCODE, for an emulator's decode step)
    ;; would otherwise resolve both the old and new opcode to one
    ;; instruction.
    (when (and old (/= (instruction-descriptor-opcode old) (instruction-descriptor-opcode descriptor)))
      (remhash (instruction-descriptor-opcode old) (machine-descriptor-opcodes md)))
    (setf (gethash name (machine-descriptor-instructions md)) descriptor)
    (setf (gethash (instruction-descriptor-opcode descriptor) (machine-descriptor-opcodes md))
          descriptor)
    descriptor))

(defun find-instruction (machine-name mnemonic)
  "Look up the INSTRUCTION-DESCRIPTOR registered under MNEMONIC (a string or
symbol, matched case-insensitively) on machine MACHINE-NAME. Signals
UNKNOWN-INSTRUCTION if none is registered."
  (let ((md (find-machine-descriptor machine-name))
        (key (string-upcase (string mnemonic))))
    (or (gethash key (machine-descriptor-instructions md))
        (error 'unknown-instruction :machine machine-name :mnemonic mnemonic))))

(defun find-instruction-by-opcode (machine-name opcode)
  "Look up the INSTRUCTION-DESCRIPTOR registered under OPCODE on machine
MACHINE-NAME -- the decode direction an emulator loop needs. Signals
UNKNOWN-INSTRUCTION if none is registered."
  (let ((md (find-machine-descriptor machine-name)))
    (or (gethash opcode (machine-descriptor-opcodes md))
        (error 'unknown-instruction :machine machine-name :opcode opcode))))

;; Absolute mode's default operand width: the machine's sole memory
;; element's address width, rounded up to whole (8-bit) bytes,
;; little-endian on encode. When a machine declares more than one memory
;; element, DEFINSTRUCTION requires (operand :width n) explicitly rather
;; than guessing which one an absolute operand addresses.
(defun %default-absolute-width (machine-name)
  (let* ((descriptor (find-machine-descriptor machine-name))
         (mem-elements (remove-if-not (lambda (e) (eq (storage-element-kind e) :memory))
                                       (machine-descriptor-elements descriptor))))
    (cond
      ((null mem-elements)
       (error "DEFINSTRUCTION on machine ~S: ABSOLUTE mode needs a memory ~
element to size its operand, but none is declared" machine-name))
      ((> (length mem-elements) 1)
       (error "DEFINSTRUCTION on machine ~S: more than one memory element ~
declared (~S) -- specify (operand :width n) explicitly instead of (operand :mode)"
              machine-name (mapcar #'storage-element-name mem-elements)))
      (t (ceiling (storage-element-addr-width (first mem-elements)) 8)))))

(defun %operand-width (mode spec machine-name)
  ;; SPEC is the tail of an (operand ...) encoding subclause: (:mode) or
  ;; (:width n).
  (destructuring-bind (spec-head &optional spec-arg) spec
    (cond
      ((eq spec-head :mode)
       (ecase mode
         (:immediate 1)
         (:absolute (%default-absolute-width machine-name))))
      ((eq spec-head :width) spec-arg)
      (t (error "Malformed operand encoding spec ~S -- expected (operand :mode) or (operand :width n)" spec)))))

(defmacro definstruction (machine name &body clauses)
  "Define an instruction named NAME on machine MACHINE from CLAUSES, each
one of:
  (modes MODE)                       -- 0 or 1 addressing mode in M1
                                         (IMMEDIATE or ABSOLUTE); more than
                                         one is M2 (DEFMODE)
  (encoding (opcode n) [(operand :mode) | (operand :width n)])
  (semantics form...)                -- expanded via WITH-MACHINE-BINDINGS,
                                         with MACHINE bound to the runtime
                                         machine instance (for explicit
                                         memory/stack access, e.g. (mref
                                         machine 'ram operand)) and OPERAND
                                         bound to the already-evaluated
                                         operand integer (or NIL for a
                                         no-operand instruction)
  (cycles n)                         -- parsed and stored, not yet used

Registers the resulting INSTRUCTION-DESCRIPTOR on MACHINE's descriptor,
by mnemonic and by opcode, inside an EVAL-WHEN so it is available at
macroexpansion time like DEFMACHINE (machine.lisp)."
  (let (modes-clause encoding-clause semantics-clause cycles-clause)
    (dolist (clause clauses)
      (case (first clause)
        (modes (setf modes-clause clause))
        (encoding (setf encoding-clause clause))
        (semantics (setf semantics-clause clause))
        (cycles (setf cycles-clause clause))
        (t (error "Unknown DEFINSTRUCTION clause head ~S in ~S" (first clause) clause))))
    (unless encoding-clause
      (error "DEFINSTRUCTION ~S ~S requires an (encoding ...) clause" machine name))
    (unless semantics-clause
      (error "DEFINSTRUCTION ~S ~S requires a (semantics ...) clause" machine name))
    (let* ((mode-syms (rest modes-clause))
           (mode (cond
                   ((null mode-syms) nil)
                   ((= (length mode-syms) 1) (%mode-keyword (first mode-syms)))
                   (t (error "DEFINSTRUCTION ~S ~S: only one addressing mode is ~
allowed in M1 (multi-mode resolution is M2), got ~S" machine name mode-syms))))
           (opcode-subclause (find 'opcode (rest encoding-clause) :key #'first))
           (operand-subclause (find 'operand (rest encoding-clause) :key #'first)))
      (unless opcode-subclause
        (error "DEFINSTRUCTION ~S ~S: (encoding ...) requires an (opcode n) subclause"
               machine name))
      (when (and mode (not operand-subclause))
        (error "DEFINSTRUCTION ~S ~S: (modes ~A) declares an addressing mode but ~
(encoding ...) has no (operand ...) subclause" machine name mode))
      (when (and operand-subclause (not mode))
        (error "DEFINSTRUCTION ~S ~S: (encoding ...) has an (operand ...) subclause ~
but no (modes ...) clause declares an addressing mode" machine name))
      (let ((opcode (second opcode-subclause))
            (operand-width (and operand-subclause
                                 (%operand-width mode (rest operand-subclause) machine))))
        ;; The semantics body has no WITH-MACHINE form of its own to name its
        ;; machine variable (unlike the M0 standalone examples), so
        ;; DEFINSTRUCTION fixes it to the literal symbol MACHINE -- used
        ;; explicitly for memory/stack access, e.g. (mref machine 'ram
        ;; operand) -- and the operand integer to the literal symbol OPERAND,
        ;; matching the exact names used throughout the design mockups.
        `(eval-when (:compile-toplevel :load-toplevel :execute)
           (register-instruction!
            ',machine
            (make-instruction-descriptor
             :name ,(string-upcase (symbol-name name))
             :machine ',machine
             :mode ,mode
             :opcode ,opcode
             :operand-width ,operand-width
             :cycles ,(and cycles-clause (second cycles-clause))
             :semantics-fn (lambda (machine operand)
                              (declare (ignorable operand))
                              (with-machine-bindings (machine ,machine)
                                ,@(rest semantics-clause)))))
           ',name)))))

;;; Encoding / execution

(defun encode-instruction (descriptor value)
  "Encode one use of instruction DESCRIPTOR with operand VALUE (an
already-evaluated integer, or NIL for a no-operand instruction) into a list
of (unsigned-byte 8) bytes: the opcode, followed by VALUE's bytes
little-endian if the instruction takes an operand."
  (let ((width (instruction-descriptor-operand-width descriptor)))
    (cons (wrap-value (instruction-descriptor-opcode descriptor) 8)
          (when width
            (loop for i below width
                  collect (wrap-value (ash value (* -8 i)) 8))))))

(defun execute-instruction (descriptor machine value)
  "Execute instruction DESCRIPTOR against a live MACHINE instance, passing
VALUE (an already-evaluated integer, or NIL) as OPERAND to its semantics."
  (funcall (instruction-descriptor-semantics-fn descriptor) machine value))
