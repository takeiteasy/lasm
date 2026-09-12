;;;; instruction.lisp
;;;; DEFINSTRUCTION: an instruction descriptor carrying an addressing mode,
;;;; an encoding form (opcode + operand width), and a semantics body
;;;; expanded through WITH-MACHINE-BINDINGS (semantics.lisp) rather than a
;;;; parallel evaluator -- so instruction semantics and the standalone M0
;;;; examples share one vocabulary.
;;;;
;;;; M2: an instruction may declare several addressing modes (mode.lisp's
;;;; DEFMODE), each becoming its own INSTRUCTION-DESCRIPTOR with its own
;;;; opcode, operand width, and (optionally) its own semantics -- a
;;;; mnemonic's variants are registered together and looked up as a list
;;;; (FIND-INSTRUCTION-VARIANTS). Choosing which variant a given operand
;;;; actually uses is the assembler's job (assembler.lisp), since it depends
;;;; on operand syntax and, for constants, operand value.
;;;;
;;;; Scope: this stops at "one instruction + one already-evaluated operand ->
;;;; bytes / executed effect". There is no statement-list driver or label
;;;; resolution here -- that is assembler.lisp, which calls EVAL-EXPR (below)
;;;; with its completed label table. EVAL-EXPR-CONSTANT is the same folder
;;;; with no label support at all, for callers with no symbol table to give
;;;; it.

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
  (mode nil :type (or null mode-descriptor))  ; nil = no operand
  (opcode nil :type (integer 0))
  ;; One byte width per operand encoding field, in the mode's hole order --
  ;; NIL for a no-operand instruction, a one-element list for the common
  ;; single-hole case. INSTRUCTION-DESCRIPTOR-TOTAL-OPERAND-WIDTH below sums
  ;; these for callers (assembler layout, PC advance) that only care about
  ;; the statement's total size.
  (operand-widths nil :type list)
  ;; One symbol (or NIL for an unnamed field) per operand encoding field,
  ;; parallel to OPERAND-WIDTHS -- a named (operand NAME ...) subclause binds
  ;; NAME in the semantics body; an unnamed one only ever binds OPERAND
  ;; (which aliases the first field, named or not).
  (operand-names nil :type list)
  (semantics-fn nil :type (or null function))
  ;; Parsed and stored, not used (#19) -- there is no timing model yet.
  ;; Accepted because users will copy LASM-plan.md sec. 3.2's (cycles n)
  ;; verbatim.
  (cycles nil :type (or null (integer 0))))

(defun instruction-descriptor-total-operand-width (descriptor)
  "Sum of DESCRIPTOR's OPERAND-WIDTHS -- the byte count its operand encoding
occupies as a whole, regardless of how many fields it's split across. 0 for
a no-operand instruction."
  (reduce #'+ (instruction-descriptor-operand-widths descriptor) :initial-value 0))

;;; Constant folding (the evaluated-operand slice of full expression evaluation)

(defun eval-expr (ast &key symbols)
  "Fold the EXPR-* AST node AST (parser.lisp) to an integer. SYMBOLS, when
given, is a hash table (string -> address) resolving EXPR-LABEL nodes --
the assembler pass (assembler.lisp) calls this with its completed layout
symbol table. Signals UNRESOLVED-LABEL on an EXPR-LABEL whose name is not
in SYMBOLS (or when SYMBOLS is NIL)."
  (etypecase ast
    (expr-number (expr-number-value ast))
    (expr-label
     (multiple-value-bind (value foundp)
         (and symbols (gethash (expr-label-name ast) symbols))
       (unless foundp (error 'unresolved-label :name (expr-label-name ast)))
       value))
    (expr-unary
     (let ((v (eval-expr (expr-unary-operand ast) :symbols symbols)))
       (ecase (expr-unary-op ast)
         (:neg (- v))
         (:pos v)
         (:lognot (lognot v))
         (:lo (logand v #xff))
         (:hi (logand (ash v -8) #xff)))))
    (expr-binary
     (let ((l (eval-expr (expr-binary-left ast) :symbols symbols))
           (r (eval-expr (expr-binary-right ast) :symbols symbols)))
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

(defun eval-expr-constant (ast)
  "Fold AST to an integer with no symbol table -- the constant-only case of
EVAL-EXPR, kept as its own name since callers throughout the codebase (and
this docstring's own examples) use it to mean \"no labels allowed here\"."
  (eval-expr ast :symbols nil))

;;; DEFINSTRUCTION registration
;;;
;;; A mnemonic registers as a list of variants -- one INSTRUCTION-DESCRIPTOR
;;; per addressing mode it accepts (or a single one-element list for a
;;; no-operand or single-mode instruction). Choosing which variant a parsed
;;; operand actually uses is the assembler's job (assembler.lisp): it depends
;;; on operand syntax and, for a constant operand, its value.

(defun register-instruction-variants! (machine-name descriptors)
  "Register DESCRIPTORS -- one or more INSTRUCTION-DESCRIPTORs sharing one
mnemonic -- on machine MACHINE-NAME, replacing any previous registration
under that mnemonic. Every old opcode not reused by DESCRIPTORS is dropped
from the opcode table first, so a redefinition that drops a mode's opcode
does not leave FIND-INSTRUCTION-BY-OPCODE (an emulator's decode step)
resolving it to a now-stale descriptor."
  (let* ((md (find-machine-descriptor machine-name))
         (name (instruction-descriptor-name (first descriptors)))
         (old (gethash name (machine-descriptor-instructions md)))
         (new-opcodes (mapcar #'instruction-descriptor-opcode descriptors)))
    (dolist (old-descriptor old)
      (unless (member (instruction-descriptor-opcode old-descriptor) new-opcodes)
        (remhash (instruction-descriptor-opcode old-descriptor) (machine-descriptor-opcodes md))))
    (setf (gethash name (machine-descriptor-instructions md)) descriptors)
    (dolist (descriptor descriptors)
      (setf (gethash (instruction-descriptor-opcode descriptor) (machine-descriptor-opcodes md))
            descriptor))
    descriptors))

(defun find-instruction-variants (machine-name mnemonic)
  "Look up the list of INSTRUCTION-DESCRIPTOR variants registered under
MNEMONIC (a string or symbol, matched case-insensitively) on machine
MACHINE-NAME. Signals UNKNOWN-INSTRUCTION if none is registered."
  (let ((md (find-machine-descriptor machine-name))
        (key (string-upcase (string mnemonic))))
    (or (gethash key (machine-descriptor-instructions md))
        (error 'unknown-instruction :machine machine-name :mnemonic mnemonic))))

(defun find-instruction (machine-name mnemonic &key mode)
  "Look up one INSTRUCTION-DESCRIPTOR variant registered under MNEMONIC on
machine MACHINE-NAME. MODE (a MODE-DESCRIPTOR, or a symbol naming one) picks
which variant when the mnemonic has more than one; omitted, the first
declared variant is returned (the common case: a no-operand or single-mode
instruction has exactly one). Signals UNKNOWN-INSTRUCTION if the mnemonic is
unregistered, or if MODE names none of its variants."
  (let ((variants (find-instruction-variants machine-name mnemonic)))
    (if mode
        (let ((mode-name (if (mode-descriptor-p mode) (mode-descriptor-name mode) mode)))
          (or (find mode-name variants
                    :key (lambda (d) (and (instruction-descriptor-mode d)
                                           (mode-descriptor-name (instruction-descriptor-mode d)))))
              (error 'unknown-instruction :machine machine-name :mnemonic mnemonic)))
        (first variants))))

(defun find-instruction-by-opcode (machine-name opcode)
  "Look up the INSTRUCTION-DESCRIPTOR registered under OPCODE on machine
MACHINE-NAME -- the decode direction an emulator loop needs. Signals
UNKNOWN-INSTRUCTION if none is registered."
  (let ((md (find-machine-descriptor machine-name)))
    (or (gethash opcode (machine-descriptor-opcodes md))
        (error 'unknown-instruction :machine machine-name :opcode opcode))))

;; A mode's default operand width, when neither the mode itself nor the
;; instruction gives one explicitly: the machine's sole memory element's
;; address width, rounded up to whole (8-bit) bytes, little-endian on
;; encode. When a machine declares more than one memory element, this is
;; ambiguous and DEFINSTRUCTION requires (operand :width n) explicitly
;; instead of guessing which memory element an address-shaped operand
;; addresses. Named for what it does now that ABSOLUTE is an ordinary
;; DEFMODE with no special standing (formerly %DEFAULT-ABSOLUTE-WIDTH).
(defun %default-address-width (machine-name)
  (let* ((descriptor (find-machine-descriptor machine-name))
         (mem-elements (remove-if-not (lambda (e) (eq (storage-element-kind e) :memory))
                                       (machine-descriptor-elements descriptor))))
    (cond
      ((null mem-elements)
       (error "DEFINSTRUCTION on machine ~S: this addressing mode needs a ~
memory element to size its operand, but none is declared" machine-name))
      ((> (length mem-elements) 1)
       (error "DEFINSTRUCTION on machine ~S: more than one memory element ~
declared (~S) -- specify (operand :width n) explicitly instead of (operand :mode)"
              machine-name (mapcar #'storage-element-name mem-elements)))
      (t (ceiling (storage-element-addr-width (first mem-elements)) 8)))))

(defun %mode-operand-width (mode machine-name)
  "MODE's own default width, falling back to %DEFAULT-ADDRESS-WIDTH."
  (or (mode-descriptor-width mode) (%default-address-width machine-name)))

(defun %operand-width (mode spec machine-name)
  ;; SPEC is the tail of an (operand ...) encoding subclause, with any
  ;; leading field name already stripped off by %PARSE-OPERAND-SUBCLAUSE:
  ;; (:mode) or (:width n).
  (destructuring-bind (spec-head &optional spec-arg) spec
    (cond
      ((eq spec-head :mode) (%mode-operand-width mode machine-name))
      ((eq spec-head :width) spec-arg)
      (t (error "Malformed operand encoding spec ~S -- expected (operand :mode) or (operand :width n)" spec)))))

(defun %parse-operand-subclause (subclause)
  "SUBCLAUSE is one whole (operand ...) form. Returns (VALUES name spec)
where NAME is the symbol from an (operand NAME :mode) / (operand NAME
:width n) field, or NIL for the unnamed (operand :mode) / (operand :width
n) form, and SPEC is the remaining (:mode) or (:width n) tail as
%OPERAND-WIDTH expects."
  (let ((rest (cdr subclause)))
    (if (keywordp (first rest))
        (values nil rest)
        (values (first rest) (rest rest)))))

(defun %scalar-bindable-names (machine-name)
  "The set of names WITH-MACHINE-BINDINGS (semantics.lisp) binds as
symbol-macros for MACHINE-NAME: every scalar (:count 1) register, plus
every flag. An operand field name colliding with one of these would be
silently shadowed inside (semantics ...) -- see %CHECK-OPERAND-NAMES."
  (let ((descriptor (find-machine-descriptor machine-name)))
    (loop for element in (machine-descriptor-elements descriptor)
          when (or (eq (storage-element-kind element) :flag)
                   (and (eq (storage-element-kind element) :register)
                        (= (storage-element-count element) 1)))
            collect (storage-element-name element))))

(defun %check-operand-names (names machine name mode-name)
  "Signal an error naming instruction NAME (on MACHINE) and addressing mode
MODE-NAME if NAMES (this variant's OPERAND-NAMES, NIL entries excluded)
contains a duplicate, or a name also bound by WITH-MACHINE-BINDINGS
(a scalar register or flag) -- either would leave (semantics ...) reading
the wrong thing silently: a duplicate NAME collapses into one LET binding
overwriting the other, and a register/flag NAME shadows (or is shadowed by,
depending on binding order) the storage element of the same name."
  (let ((given (remove nil names)))
    (let ((dup (loop for (n . rest) on given when (member n rest) return n)))
      (when dup
        (error "DEFINSTRUCTION ~S ~S: addressing mode ~S names the operand ~S ~
more than once" machine name mode-name dup)))
    (dolist (n given)
      (when (member n (%scalar-bindable-names machine))
        (error "DEFINSTRUCTION ~S ~S: addressing mode ~S names an operand ~S, ~
which is also a register or flag on ~S -- (semantics ...) can only see one ~
of them" machine name mode-name n machine)))))

(defun %parse-operand-subclauses (mode subclauses machine name mode-name machine-name)
  "SUBCLAUSES is every (operand ...) form declared for one variant of
instruction NAME (on MACHINE) using addressing MODE (named MODE-NAME in
diagnostics), in declaration order. Returns (VALUES widths names), one
entry per subclause -- their count must equal MODE's EXPR hole count
exactly, since each hole needs somewhere to put its parsed value and each
operand subclause needs a hole to size itself against; mismatch in either
direction is an error. Named fields are also checked for collisions
(%CHECK-OPERAND-NAMES)."
  (let ((holes (%mode-hole-count mode))
        (n (length subclauses)))
    (unless (= holes n)
      (error "DEFINSTRUCTION ~S ~S: addressing mode ~S has ~D EXPR hole~:P ~
but ~D (operand ...) subclause~:P ~:[were~;was~] given -- one is required ~
per hole" machine name mode-name holes n (= n 1))))
  (multiple-value-bind (widths names)
      (loop for subclause in subclauses
            collect (multiple-value-bind (name spec) (%parse-operand-subclause subclause)
                      (cons name (%operand-width mode spec machine-name))) into pairs
            finally (return (values (mapcar #'cdr pairs) (mapcar #'car pairs))))
    (%check-operand-names names machine name mode-name)
    (values widths names)))

(defun %check-relative-mode-holes (mode machine name)
  ;; A RELATIVE mode (mode.lisp) marks its *whole* pattern's operand as a
  ;; PC-relative offset -- there is no way to say "only this hole is
  ;; relative" yet (a per-hole attribute is a follow-up), so a RELATIVE mode
  ;; with more than one hole has no coherent meaning and is rejected here
  ;; rather than silently relative-adjusting the wrong (or every) field.
  (when (and (mode-descriptor-relativep mode) (> (%mode-hole-count mode) 1))
    (error "DEFINSTRUCTION ~S ~S: addressing mode ~S is :RELATIVE and has ~
more than one EXPR hole -- a RELATIVE mode's offset applies to its whole ~
operand, so per-hole relative marking is not supported"
           machine name (mode-descriptor-name mode))))

;; The semantics body has no WITH-MACHINE form of its own to name its machine
;; variable (unlike the M0 standalone examples), so DEFINSTRUCTION fixes it
;; to the literal symbol MACHINE -- used explicitly for memory/stack access,
;; e.g. (mref machine 'ram operand). OPERAND-NAMES gives one entry per
;; operand encoding field (parallel to OPERAND-WIDTHS), NIL for an unnamed
;; field: OPERAND is always bound to the first field's value (the only field
;; in the common single-hole case), and any non-NIL name gets its own
;; binding to its field's value, so a two-hole (operand dst :width 1)
;; (operand src :width 1) instruction can write DST/SRC directly instead of
;; indexing into a list.
;;
;; The LET establishing these bindings goes *inside* WITH-MACHINE-BINDINGS's
;; body, not around the whole form -- WITH-MACHINE-BINDINGS expands to a
;; SYMBOL-MACROLET, and a SYMBOL-MACROLET's own scope always shadows an
;; enclosing LET of the same name, so an outer LET would leave a register-
;; or flag-named operand silently reading the storage element instead
;; (never observed, since %CHECK-OPERAND-NAMES rejects that combination at
;; DEFINSTRUCTION time -- this ordering is what makes REJECTING it, rather
;; than just documenting it, actually sufficient).
(defun %semantics-fn-form (semantics-forms machine operand-names)
  (let ((named-bindings (loop for name in operand-names
                               for i from 0
                               when name
                                 collect `(,name (nth ,i operands)))))
    `(lambda (machine operands)
       (declare (ignorable operands))
       (with-machine-bindings (machine ,machine)
         (let ((operand (first operands))
               ,@named-bindings)
           (declare (ignorable operand ,@(remove nil operand-names)))
           ,@semantics-forms)))))

(defun %descriptor-form (machine name mode-form opcode operand-widths operand-names cycles semantics-forms)
  `(make-instruction-descriptor
    :name ,(string-upcase (symbol-name name))
    :machine ',machine
    :mode ,mode-form
    :opcode ,opcode
    :operand-widths ',operand-widths
    :operand-names ',operand-names
    :cycles ,cycles
    :semantics-fn ,(%semantics-fn-form semantics-forms machine operand-names)))

(defun %resolve-operand-fields (mode operand-subclauses machine name mode-name machine-name)
  "Resolve the (operand ...) subclauses (zero or more whole forms, in
declaration order) given for one addressing-mode use into (VALUES widths
names), one entry per MODE hole. With no subclauses at all, MODE must have
exactly one hole (a bare width can't be inferred for more) -- its default
width (%MODE-OPERAND-WIDTH) is used, unnamed. With one or more subclauses,
their count must match MODE's hole count exactly (%PARSE-OPERAND-
SUBCLAUSES)."
  (if operand-subclauses
      (%parse-operand-subclauses mode operand-subclauses machine name mode-name machine-name)
      (if (= (%mode-hole-count mode) 1)
          (values (list (%mode-operand-width mode machine-name)) (list nil))
          (error "DEFINSTRUCTION ~S ~S: addressing mode ~S has ~D EXPR holes ~
-- an (operand ...) subclause is required per hole" machine name mode-name
                 (%mode-hole-count mode)))))

(defun %parse-mode-variant-clause (variant-form machine name default-semantics-forms cycles-form)
  "VARIANT-FORM is one element of a multi-mode (modes ...) clause:
(MODE-NAME (opcode n) (operand ...)* [(semantics form...)]). Returns a
%DESCRIPTOR-FORM for this variant."
  (destructuring-bind (mode-sym &rest body) variant-form
    (let* ((mode (find-mode-descriptor mode-sym))
           (opcode-subclause (find 'opcode body :key #'first))
           (operand-subclauses (remove-if-not (lambda (c) (eq (first c) 'operand)) body))
           (semantics-subclause (find 'semantics body :key #'first)))
      (%check-relative-mode-holes mode machine name)
      (unless opcode-subclause
        (error "DEFINSTRUCTION ~S ~S: mode ~S requires an (opcode n) subclause"
               machine name mode-sym))
      (multiple-value-bind (operand-widths operand-names)
          (%resolve-operand-fields mode operand-subclauses machine name mode-sym machine)
        (let ((opcode (second opcode-subclause))
              (semantics-forms (cond
                                  (semantics-subclause (rest semantics-subclause))
                                  (default-semantics-forms default-semantics-forms)
                                  (t (error "DEFINSTRUCTION ~S ~S: mode ~S has no ~
(semantics ...) of its own and no shared top-level (semantics ...) default"
                                            machine name mode-sym)))))
          (%descriptor-form machine name `(find-mode-descriptor ',mode-sym)
                             opcode operand-widths operand-names cycles-form semantics-forms))))))

(defmacro definstruction (machine name &body clauses)
  "Define an instruction named NAME on machine MACHINE from CLAUSES, each
one of:
  (modes MODE)                       -- 0 or 1 addressing mode, sharing the
                                         top-level (encoding ...) below
  (modes (MODE (opcode n)
               [(operand [NAME] :mode)
                | (operand [NAME] :width n)]*
               [(semantics form...)])
         ...)                        -- 2+ addressing modes, each with its
                                         own opcode and (optionally) its own
                                         operand field(s) and semantics; a
                                         mode with no (semantics ...) of its
                                         own uses the shared (semantics ...)
                                         below as its default
  (encoding (opcode n)
            [(operand [NAME] :mode)
             | (operand [NAME] :width n)]*)
                                      -- required with the bare-symbol
                                         (modes MODE) form above; not
                                         allowed with the multi-mode form,
                                         since each mode supplies its own
  (semantics form...)                -- expanded via WITH-MACHINE-BINDINGS,
                                         with MACHINE bound to the runtime
                                         machine instance (for explicit
                                         memory/stack access, e.g. (mref
                                         machine 'ram operand)) and OPERAND
                                         bound to the first operand field's
                                         already-evaluated value (or NIL for
                                         a no-operand instruction). Required
                                         unless every mode in a multi-mode
                                         (modes ...) supplies its own.
  (cycles n)                         -- parsed and stored, not yet used

A mode with more than one EXPR hole (mode.lisp) needs one (operand ...)
subclause per hole, in hole order -- (operand :mode)/(operand :width n) for
an unnamed field bound only through OPERAND above, or (operand NAME :mode)/
(operand NAME :width n) to also bind NAME to that field's value in
(semantics ...). A single-hole mode may omit (operand ...) entirely in the
multi-mode form (its default width applies, unnamed); the (encoding ...)
form always requires it explicitly. Declaring more or fewer (operand ...)
subclauses than the mode has holes is an error.

MODE names are resolved against DEFMODE's registry (mode.lisp) at
macroexpansion time, like MACHINE is resolved against DEFMACHINE's.
Registers the resulting variant(s) on MACHINE's descriptor, by mnemonic and
by opcode, inside an EVAL-WHEN so they are available at macroexpansion time
like DEFMACHINE itself."
  (let (modes-clause encoding-clause semantics-clause cycles-clause)
    (dolist (clause clauses)
      (case (first clause)
        (modes (setf modes-clause clause))
        (encoding (setf encoding-clause clause))
        (semantics (setf semantics-clause clause))
        (cycles (setf cycles-clause clause))
        (t (error "Unknown DEFINSTRUCTION clause head ~S in ~S" (first clause) clause))))
    (let* ((mode-forms (rest modes-clause))
           (cycles-form (and cycles-clause (second cycles-clause))))
      (cond
        ;; No addressing mode -- no operand.
        ((null mode-forms)
         (unless encoding-clause
           (error "DEFINSTRUCTION ~S ~S requires an (encoding ...) clause" machine name))
         (unless semantics-clause
           (error "DEFINSTRUCTION ~S ~S requires a (semantics ...) clause" machine name))
         (let* ((opcode-subclause (find 'opcode (rest encoding-clause) :key #'first))
                (operand-subclause (find 'operand (rest encoding-clause) :key #'first)))
           (unless opcode-subclause
             (error "DEFINSTRUCTION ~S ~S: (encoding ...) requires an (opcode n) subclause"
                    machine name))
           (when operand-subclause
             (error "DEFINSTRUCTION ~S ~S: (encoding ...) has an (operand ...) subclause ~
but no (modes ...) clause declares an addressing mode" machine name))
           `(eval-when (:compile-toplevel :load-toplevel :execute)
              (register-instruction-variants!
               ',machine
               (list ,(%descriptor-form machine name nil (second opcode-subclause) nil nil
                                         cycles-form (rest semantics-clause))))
              ',name)))
        ;; Multi-mode form: (modes (MODE ...) (MODE ...) ...).
        ((consp (first mode-forms))
         (when encoding-clause
           (error "DEFINSTRUCTION ~S ~S: a multi-mode (modes ...) clause gives ~
each mode its own (opcode n) -- a top-level (encoding ...) clause is not allowed"
                  machine name))
         (unless (rest mode-forms)
           (error "DEFINSTRUCTION ~S ~S: a multi-mode (modes ...) clause needs ~
at least two modes -- use (modes MODE) with (encoding ...) for just one" machine name))
         (let ((default-semantics-forms (and semantics-clause (rest semantics-clause))))
           `(eval-when (:compile-toplevel :load-toplevel :execute)
              (register-instruction-variants!
               ',machine
               (list ,@(mapcar (lambda (variant-form)
                                  (%parse-mode-variant-clause variant-form machine name
                                                              default-semantics-forms cycles-form))
                                mode-forms)))
              ',name)))
        ;; Sugar: (modes MODE), one bare mode symbol, opcode/width/semantics
        ;; all shared with the rest of the instruction -- the M1 shape.
        (t
         (when (rest mode-forms)
           (error "DEFINSTRUCTION ~S ~S: more than one bare addressing-mode ~
symbol in (modes ...) requires the multi-mode list form, e.g. (modes (~A ~
(opcode ...)) (~A (opcode ...)))" machine name (first mode-forms) (second mode-forms)))
         (unless encoding-clause
           (error "DEFINSTRUCTION ~S ~S requires an (encoding ...) clause" machine name))
         (unless semantics-clause
           (error "DEFINSTRUCTION ~S ~S requires a (semantics ...) clause" machine name))
         (let* ((mode-sym (first mode-forms))
                (mode (find-mode-descriptor mode-sym))
                (opcode-subclause (find 'opcode (rest encoding-clause) :key #'first))
                (operand-subclauses (remove-if-not (lambda (c) (eq (first c) 'operand))
                                                    (rest encoding-clause))))
           (%check-relative-mode-holes mode machine name)
           (unless opcode-subclause
             (error "DEFINSTRUCTION ~S ~S: (encoding ...) requires an (opcode n) subclause"
                    machine name))
           (unless operand-subclauses
             (error "DEFINSTRUCTION ~S ~S: (modes ~A) declares an addressing mode but ~
(encoding ...) has no (operand ...) subclause" machine name mode-sym))
           (multiple-value-bind (operand-widths operand-names)
               (%parse-operand-subclauses mode operand-subclauses machine name mode-sym machine)
             `(eval-when (:compile-toplevel :load-toplevel :execute)
                (register-instruction-variants!
                 ',machine
                 (list ,(%descriptor-form machine name `(find-mode-descriptor ',mode-sym)
                                           (second opcode-subclause)
                                           operand-widths operand-names
                                           cycles-form (rest semantics-clause))))
                ',name))))))))

;;; Encoding / execution

(defun %encode-value-bytes (value width)
  "Split (already-evaluated integer) VALUE into WIDTH little-endian
(unsigned-byte 8) bytes, wrapping each with WRAP-VALUE (storage.lisp) like
every other encoded quantity in this codebase. Shared by ENCODE-INSTRUCTION
below and the assembler's .BYTE/.WORD directive encoding (assembler.lisp,
#14), so instruction operands and directive data can't drift apart in how
they lay bytes down."
  (loop for i below width collect (wrap-value (ash value (* -8 i)) 8)))

(defun encode-instruction (descriptor values)
  "Encode one use of instruction DESCRIPTOR with operand VALUES (a list of
already-evaluated integers, one per DESCRIPTOR's OPERAND-WIDTHS entry, in
the same order -- NIL for a no-operand instruction) into a list of
(unsigned-byte 8) bytes: the opcode, followed by each value's bytes
little-endian in turn. VALUES shorter than OPERAND-WIDTHS silently encodes
fewer fields than DESCRIPTOR declares, rather than erroring -- every caller
in this codebase (%ENCODE, assembler.lisp) always supplies exactly one
value per width, so this is unreachable internally, but a caller of this
exported function on its own should supply the same."
  (cons (wrap-value (instruction-descriptor-opcode descriptor) 8)
        (loop for value in values
              for width in (instruction-descriptor-operand-widths descriptor)
              append (%encode-value-bytes value width))))

(defun execute-instruction (descriptor machine values)
  "Execute instruction DESCRIPTOR against a live MACHINE instance, passing
VALUES (a list of already-evaluated integers, one per operand encoding
field, or NIL for a no-operand instruction) to its semantics -- OPERAND is
bound to the first (or only) value, and any named field to its own value
(see %SEMANTICS-FN-FORM)."
  (funcall (instruction-descriptor-semantics-fn descriptor) machine values))
