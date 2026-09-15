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

(define-condition unresolved-location (lasm-error) ()
  (:documentation "Signalled by EVAL-EXPR on an EXPR-LOCATION node (the \"*\"
location-counter symbol, #15) when no PC is given to resolve it against --
e.g. EVAL-EXPR-CONSTANT's default (no :PC), used where no address is known
yet.")
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "Cannot fold constant expression: \"*\" (location counter) used where no address is known"))))

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

(define-condition opcode-conflict (lasm-error)
  ((machine :initarg :machine :reader opcode-conflict-machine)
   (opcode :initarg :opcode :reader opcode-conflict-opcode)
   (mnemonic :initarg :mnemonic :reader opcode-conflict-mnemonic)
   (other-mnemonic :initarg :other-mnemonic :reader opcode-conflict-other-mnemonic))
  (:documentation "Signalled by REGISTER-INSTRUCTION-VARIANTS! when a
descriptor's opcode is already claimed by a *different* mnemonic on the same
machine (#26) -- without this check the later DEFINSTRUCTION silently wins
the opcode-table entry, and a later redefinition of the earlier mnemonic can
then delete the winner's entry outright as an apparently orphaned opcode.")
  (:report (lambda (c s)
             (format s "Opcode ~S for instruction ~S on machine ~S is already registered to ~S"
                     (opcode-conflict-opcode c) (opcode-conflict-mnemonic c)
                     (opcode-conflict-machine c) (opcode-conflict-other-mnemonic c)))))

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
  ;; #75: this variant's cycle cost. NIL (no (cycles n) clause given) means
  ;; the default of 1 -- resolved by %DESCRIPTOR-CYCLE-COST (emulator.lisp),
  ;; the one place that default lives, rather than by every caller
  ;; separately defaulting a NIL.
  (cycles nil :type (or null (integer 0)))
  ;; #20 (M4): non-NIL only on a word-encoded machine (MACHINE-DESCRIPTOR-
  ;; INSTRUCTION-WORD non-NIL, storage.lisp). WORD-FIELDS is *this*
  ;; descriptor's chosen field encoding -- one WORD-FIELD-CHOICE per operand,
  ;; parallel to OPERAND-NAMES, in hole order -- which ENCODE-INSTRUCTION
  ;; uses directly to build the instruction word.
  (word-fields nil :type list)
  ;; The full per-operand variant menu -- one list of WORD-FIELD-CHOICE per
  ;; operand, in hole order -- shared by every sibling descriptor expanded
  ;; from the same DEFINSTRUCTION variant clause (#20). Decode (emulator.lisp)
  ;; needs every alternative, not just the one combo that happens to occupy
  ;; the opcode table, to tell an inline value from an escaped extra-word
  ;; marker apart by comparing against the actually fetched bits.
  (word-alternatives nil :type list)
  ;; Count of :EXTRA-WORD fields in WORD-FIELDS -- this combo's extra encoded
  ;; words, each one INSTRUCTION-WORD-LAYOUT-WIDTH-BYTES wide. 0 for a
  ;; byte-encoded descriptor and for an all-inline word combo alike.
  (extra-words 0 :type (integer 0)))

(defun instruction-descriptor-total-operand-width (descriptor)
  "Sum of DESCRIPTOR's OPERAND-WIDTHS -- the cell count its operand encoding
occupies as a whole, regardless of how many fields it's split across. 0 for
a no-operand instruction, and always 0 for a word-encoded descriptor (#20),
whose OPERAND-WIDTHS is NIL by construction -- see INSTRUCTION-DESCRIPTOR-SIZE
for the accessor that covers both encoding schemes."
  (reduce #'+ (instruction-descriptor-operand-widths descriptor) :initial-value 0))

(defun instruction-descriptor-word-layout (descriptor)
  "DESCRIPTOR's machine's INSTRUCTION-WORD-LAYOUT (storage.lisp), or NIL on
an ordinary byte-encoded machine. Looked up via DESCRIPTOR's own MACHINE
slot rather than cached on the descriptor, so it can't drift from the
machine descriptor it names."
  (machine-descriptor-instruction-word (find-machine-descriptor (instruction-descriptor-machine descriptor))))

(defun instruction-descriptor-cell-width (descriptor)
  "DESCRIPTOR's machine's code cell width in bits (#53) -- looked up via
%MACHINE-CELL-WIDTH (machine.lisp) rather than cached on the descriptor, same
rationale as INSTRUCTION-DESCRIPTOR-WORD-LAYOUT."
  (%machine-cell-width (instruction-descriptor-machine descriptor)))

(defun instruction-descriptor-size (descriptor)
  "Total encoded cells for one use of DESCRIPTOR -- 1 (opcode cell) plus
operand cell widths on an ordinary byte/cell-encoded machine, or
INSTRUCTION-WORD-LAYOUT-WIDTH-CELLS * (1 + EXTRA-WORDS) on a word-encoded one
(#20). Centralizes what used to be five separate \"1 + operand width\"
computations scattered across the assembler's layout/relaxation, its
relative-branch offset arithmetic, and the emulator's fetch loop, so a
word-encoded descriptor's size is computed identically everywhere rather than
each caller assuming a byte opcode."
  (let ((layout (instruction-descriptor-word-layout descriptor)))
    (if layout
        (* (instruction-word-layout-width-cells layout) (1+ (instruction-descriptor-extra-words descriptor)))
        (1+ (instruction-descriptor-total-operand-width descriptor)))))

;;; Constant folding (the evaluated-operand slice of full expression evaluation)

(defun eval-expr (ast &key symbols pc)
  "Fold the EXPR-* AST node AST (parser.lisp) to an integer. SYMBOLS, when
given, is a hash table (string -> value -- a label's address, or an .EQU's
folded value, #35) resolving EXPR-LABEL nodes -- the assembler pass
(assembler.lisp) calls this with its completed layout symbol table. PC, when
given, is the integer address EXPR-LOCATION (the \"*\" location-counter
symbol, #15) folds to. Signals UNRESOLVED-LABEL on an EXPR-LABEL whose name
is not in SYMBOLS (or when SYMBOLS is NIL), and UNRESOLVED-LOCATION on an
EXPR-LOCATION when PC is NIL."
  (etypecase ast
    (expr-number (expr-number-value ast))
    (expr-label
     (multiple-value-bind (value foundp)
         (and symbols (gethash (expr-label-name ast) symbols))
       (unless foundp (error 'unresolved-label :name (expr-label-name ast)))
       value))
    (expr-location
     (unless pc (error 'unresolved-location))
     pc)
    (expr-unary
     (let ((v (eval-expr (expr-unary-operand ast) :symbols symbols :pc pc)))
       (ecase (expr-unary-op ast)
         (:neg (- v))
         (:pos v)
         (:lognot (lognot v))
         (:lo (logand v #xff))
         (:hi (logand (ash v -8) #xff)))))
    (expr-binary
     (let ((l (eval-expr (expr-binary-left ast) :symbols symbols :pc pc))
           (r (eval-expr (expr-binary-right ast) :symbols symbols :pc pc)))
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

(defun eval-expr-constant (ast &key pc)
  "Fold AST to an integer with no symbol table -- the constant-only case of
EVAL-EXPR, kept as its own name since callers throughout the codebase (and
this docstring's own examples) use it to mean \"no labels allowed here\". PC,
when given, still resolves an EXPR-LOCATION node (#15) -- a location-counter
reference is not a label, so it's independent of \"no labels allowed here\"."
  (eval-expr ast :symbols nil :pc pc))

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
resolving it to a now-stale descriptor.

A word-encoded machine's variant expansion (#20, instruction.lisp's
%EXPAND-WORD-COMBOS) can hand this several sibling DESCRIPTORS that all
share one opcode value (one mnemonic, encoded differently by operand size,
not by opcode) -- whichever ends up in the opcode table below (the last one
processed wins, same as any other same-opcode overwrite here) is fine for
decode: every sibling carries an equivalent WORD-ALTERNATIVES menu, so
%STEP-WORD-MACHINE (emulator.lisp) reconstructs the actual encoding from the
fetched bits regardless of which specific combo it's looking at."
  (let* ((md (find-machine-descriptor machine-name))
         (name (instruction-descriptor-name (first descriptors)))
         (old (gethash name (machine-descriptor-instructions md)))
         (new-opcodes (mapcar #'instruction-descriptor-opcode descriptors)))
    (dolist (descriptor descriptors)
      (let ((claimant (gethash (instruction-descriptor-opcode descriptor) (machine-descriptor-opcodes md))))
        (when (and claimant (not (string= (instruction-descriptor-name claimant) name)))
          (error 'opcode-conflict :machine machine-name
                                   :opcode (instruction-descriptor-opcode descriptor)
                                   :mnemonic name
                                   :other-mnemonic (instruction-descriptor-name claimant)))))
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
;; address width, rounded up to whole cells of that same element's own
;; CELL-WIDTH (#53), little-endian on encode. When a machine declares more
;; than one memory element, this is ambiguous and DEFINSTRUCTION requires
;; (operand :width n) explicitly instead of guessing which memory element an
;; address-shaped operand addresses. Named for what it does now that
;; ABSOLUTE is an ordinary DEFMODE with no special standing (formerly
;; %DEFAULT-ABSOLUTE-WIDTH).
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
      (t (ceiling (storage-element-addr-width (first mem-elements))
                  (storage-element-cell-width (first mem-elements)))))))

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
  "The set of names WITH-MACHINE-BINDINGS (semantics.lisp) binds for
MACHINE-NAME: every register (scalar as a symbol-macro, banked (#13) as a
macrolet taking an index) plus every flag. An operand field name colliding
with one of these would be silently shadowed inside (semantics ...) -- see
%CHECK-OPERAND-NAMES. Despite the name (kept for history), this now covers
banked registers too -- a macrolet binding shadows exactly as silently as a
symbol-macrolet one."
  (let ((descriptor (find-machine-descriptor machine-name)))
    (loop for element in (machine-descriptor-elements descriptor)
          when (member (storage-element-kind element) '(:flag :register))
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
  ;; single PC-relative offset -- there is no way to say "only this hole is
  ;; the offset" yet (a per-hole attribute is a follow-up), so a RELATIVE
  ;; mode with more than one hole has no coherent meaning and is rejected
  ;; here rather than silently offset-adjusting the wrong (or every) field.
  ;; This restriction is specific to RELATIVE's offset computation, not to
  ;; signedness in general -- a plain SIGNED mode (#30) may have any number
  ;; of holes; each is sign-extended independently (emulator.lisp).
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

;;; Word-encoded instructions (#20, M4) -- DCPU-16-shaped bitfield/variant
;;; operand encoding, kept as its own code path parallel to the byte-encoded
;;; (operand :mode)/(operand :width n) machinery above rather than threaded
;;; through it: a word-encoded operand can expand into *several*
;;; INSTRUCTION-DESCRIPTORs sharing one mnemonic, mode, and opcode value (one
;;; per value-range variant, e.g. "fits inline" vs. "needs an extra word"),
;;; which the byte path's "exactly one descriptor per mode use" shape has no
;;; room for. Selecting between them per statement is still %CHOOSE-VARIANT's
;;; job (assembler.lisp) -- these descriptors just give it more to choose
;;; from, via the same syntax/floor/value filter pipeline, generalized to
;;; INSTRUCTION-DESCRIPTOR-SIZE (below) instead of assuming byte widths.

(defstruct word-variant
  (kind nil :type (member :inline :extra-word))
  (bias 0 :type integer)                 ; :inline only
  (range nil :type (or null cons))       ; :inline only, pre-bias (lo . hi)
  (escape nil :type (or null integer))   ; :extra-word only
  ;; #104: non-NIL only for a (CHOICE M) selector -- the ONE-OF alternative
  ;; mode-name symbol M that must be this hole's matched alternative
  ;; (mode.lisp's hole-aligned CHOICES) for this variant to apply, rather
  ;; than the operand's own folded VALUE choosing between a (RANGE LO HI)
  ;; variant and an :ELSE one. A field's variants are either all CHOICE-
  ;; selected or all value-selected (%CHECK-WORD-VARIANTS rejects mixing) --
  ;; never both on the same operand.
  (choice nil :type (or null symbol)))

(defstruct word-operand-spec
  (name nil)                  ; operand field name, or NIL for unnamed
  (field nil :type symbol)    ; instruction-word field name
  (width nil :type (integer 1))
  (shift nil :type (integer 0))
  (variants nil :type list))  ; list of WORD-VARIANT, declaration order

;; One operand's *chosen* (or, in an INSTRUCTION-DESCRIPTOR's WORD-ALTERNATIVES,
;; one *candidate*) field encoding -- WIDTH/SHIFT locate its bits in the
;; instruction word; KIND says whether VALUE packs in biased by BIAS or is
;; replaced by ESCAPE with VALUE following in its own word. RANGE (pre-bias)
;; is kept alongside BIAS so decode (emulator.lisp) can test a fetched raw
;; field value for membership without redoing DEFINSTRUCTION-time arithmetic.
(defstruct word-field-choice
  (width nil :type (integer 1))
  (shift nil :type (integer 0))
  (kind nil :type (member :inline :extra-word))
  (bias 0 :type integer)
  (range nil :type (or null cons))
  (escape nil :type (or null integer))
  ;; #104: mirrors WORD-VARIANT-CHOICE -- non-NIL only for a variant
  ;; selected by matched ONE-OF alternative rather than by value. Carried
  ;; through to every descriptor's WORD-FIELDS/WORD-ALTERNATIVES so
  ;; %CHOOSE-VARIANT (assembler.lisp) can filter combos by the operand's
  ;; actually-matched alternative, and so DECODE-INSTRUCTION-AT's matched
  ;; choice (decoder.lisp) gives the disassembler (disassembler.lisp, #117)
  ;; a record of which alternative was really encoded, instead of always
  ;; rendering a ONE-OF's first alternative.
  (choice nil :type (or null symbol)))

(defun %word-machine-p (machine-name)
  "T if MACHINE-NAME's DEFMACHINE declared an (instruction-word ...) clause
(machine.lisp, #20) -- DEFINSTRUCTION branches on this to pick the
word-field/variant encoding path below instead of the byte-encoded
(operand :mode)/(operand :width n) one."
  (and (machine-descriptor-instruction-word (find-machine-descriptor machine-name)) t))

(defun %parse-word-variant-form (form field-name)
  "Parse one (variant selector kind...) form (DEFINSTRUCTION's docstring)
into a WORD-VARIANT. SELECTOR is (range LO HI) for a value-selected :INLINE
variant (optionally :BIAS N, default 0), :ELSE for the value-selected
:EXTRA-WORD fallback (kind form (extra-word :escape n)), or (choice M) (#104)
for a variant selected by hole M matching mode.lisp's hole-aligned CHOICES
instead of by the operand's folded value -- kind form INLINE (requiring its
own :RANGE (lo hi), since unlike (range lo hi) a CHOICE selector carries no
range to double as one; optionally :BIAS N, default 0) or (extra-word
:escape n), the latter an *unconditional* trailing word once M is the
matched alternative, not a value-triggered fallback."
  (destructuring-bind (head selector &rest tail) form
    (unless (eq head 'variant)
      (error "DEFINSTRUCTION: field ~S: malformed variant form ~S -- expected ~
(variant selector kind)" field-name form))
    (cond
      ((and (consp selector) (eq (first selector) 'range))
       (destructuring-bind (range-kw lo hi) selector
         (declare (ignore range-kw))
         (unless (eq (first tail) 'inline)
           (error "DEFINSTRUCTION: field ~S: a (range ...) variant must be ~
INLINE, got ~S" field-name tail))
         (destructuring-bind (inline-sym &key (bias 0)) tail
           (declare (ignore inline-sym))
           (make-word-variant :kind :inline :bias bias :range (cons lo hi)))))
      ((eq selector :else)
       (unless (and (consp (first tail)) (eq (first (first tail)) 'extra-word))
         (error "DEFINSTRUCTION: field ~S: an :ELSE variant must be ~
(extra-word :escape n), got ~S" field-name tail))
       (destructuring-bind (extra-word-kw &key escape) (first tail)
         (declare (ignore extra-word-kw))
         (unless escape
           (error "DEFINSTRUCTION: field ~S: (extra-word ...) requires :escape n" field-name))
         (make-word-variant :kind :extra-word :escape escape)))
      ((and (consp selector) (eq (first selector) 'choice))
       (destructuring-bind (choice-kw choice-name) selector
         (declare (ignore choice-kw))
         (cond
           ((and (consp (first tail)) (eq (first (first tail)) 'extra-word))
            (destructuring-bind (extra-word-kw &key escape) (first tail)
              (declare (ignore extra-word-kw))
              (unless escape
                (error "DEFINSTRUCTION: field ~S: (extra-word ...) requires :escape n" field-name))
              (make-word-variant :kind :extra-word :escape escape :choice choice-name)))
           ((eq (first tail) 'inline)
            (destructuring-bind (inline-sym &key range (bias 0)) tail
              (declare (ignore inline-sym))
              (unless range
                (error "DEFINSTRUCTION: field ~S: a (choice ~S) INLINE variant requires its ~
own :range (lo hi) -- unlike (range lo hi), a CHOICE selector carries no range of its own"
                       field-name choice-name))
              (destructuring-bind (lo hi) range
                (make-word-variant :kind :inline :bias bias :range (cons lo hi) :choice choice-name))))
           (t (error "DEFINSTRUCTION: field ~S: a (choice ~S) variant must be INLINE (with ~
:range) or (extra-word :escape n), got ~S" field-name choice-name tail)))))
      (t (error "DEFINSTRUCTION: field ~S: variant selector must be (range lo hi), :else, ~
or (choice mode), got ~S" field-name selector)))))

(defun %check-word-variants (variants field-width field-name)
  "Signal an error if any of VARIANTS (one FIELD-NAME operand's declared
variant list, already parsed) doesn't fit FIELD-WIDTH bits; if an
:EXTRA-WORD variant's escape value falls inside another variant's biased
inline range; if two :INLINE variants' biased ranges overlap; if two
:EXTRA-WORD variants share one escape value; or if VARIANTS mixes CHOICE-
selected (#104) and value-selected (RANGE/:ELSE) variants on one operand --
every one of these is an ambiguity a decoder reading a raw field value could
never resolve (the RANGE/:ELSE-only versions of the first two checks predate
#104; a field with only one value-selected :INLINE and one :ELSE, the only
shape possible before #104, could never trigger the overlap/duplicate-escape
cases, so this doesn't change any existing DEFINSTRUCTION's validity)."
  (let ((max (1- (ash 1 field-width))) inline-ranges escapes)
    (dolist (v variants)
      (ecase (word-variant-kind v)
        (:inline
         (let* ((lo (+ (car (word-variant-range v)) (word-variant-bias v)))
                (hi (+ (cdr (word-variant-range v)) (word-variant-bias v))))
           (when (or (< lo 0) (> hi max))
             (error "DEFINSTRUCTION: field ~S: biased inline range ~D..~D does ~
not fit its ~D-bit field" field-name lo hi field-width))
           (cl:push (cons lo hi) inline-ranges)))
        (:extra-word
         (let ((e (word-variant-escape v)))
           (when (or (< e 0) (> e max))
             (error "DEFINSTRUCTION: field ~S: escape ~D does not fit its ~D-bit field"
                    field-name e field-width))
           (cl:push e escapes)))))
    (dolist (e escapes)
      (dolist (r inline-ranges)
        (when (<= (car r) e (cdr r))
          (error "DEFINSTRUCTION: field ~S: escape value ~D is inside inline ~
range ~D..~D -- an encoded field value of ~D can never be told apart from a ~
genuine inline value" field-name e (car r) (cdr r) e))))
    ;; #104: reachable now that several CHOICE-selected :INLINE variants can
    ;; share one field -- unreachable before, when a field had at most one
    ;; value-selected :INLINE variant.
    (loop for (r . later) on inline-ranges
          do (dolist (r2 later)
               (when (<= (max (car r) (car r2)) (min (cdr r) (cdr r2)))
                 (error "DEFINSTRUCTION: field ~S: inline ranges ~D..~D and ~D..~D overlap -- ~
an encoded field value in the overlap could never be told apart"
                        field-name (car r) (cdr r) (car r2) (cdr r2)))))
    ;; #104: reachable now that several CHOICE-selected :EXTRA-WORD variants
    ;; can share one field -- unreachable before, when a field had at most
    ;; one :ELSE.
    (let ((dup (loop for (e . later) on escapes when (member e later) return e)))
      (when dup
        (error "DEFINSTRUCTION: field ~S: escape value ~D is used by more than one variant"
               field-name dup)))
    ;; #104: a field is either all CHOICE-selected or all value-selected --
    ;; never both, so %CHOOSE-VARIANT's eligibility filter never has to
    ;; reason about a mix.
    (let ((choice-count (count-if #'word-variant-choice variants)))
      (when (and (plusp choice-count) (/= choice-count (length variants)))
        (error "DEFINSTRUCTION: field ~S: CHOICE-selected and value-selected ~
(RANGE/:ELSE) variants may not be mixed on the same operand" field-name)))))

(defun %check-word-variant-choices! (variants field-name hole-alternatives)
  "Signal an error if any of VARIANTS' non-NIL WORD-VARIANT-CHOICE (#104)
names a mode not registered (FIND-MODE-DESCRIPTOR signals), or one not among
HOLE-ALTERNATIVES -- this operand hole's actual ONE-OF alternatives, per
mode.lisp's %MODE-HOLE-ALTERNATIVES (NIL when the hole isn't a ONE-OF at
all, which makes any (CHOICE M) on it an error unconditionally)."
  (dolist (v variants)
    (let ((choice (word-variant-choice v)))
      (when choice
        (find-mode-descriptor choice)
        (unless hole-alternatives
          (error "DEFINSTRUCTION: field ~S: (choice ~S) given for an operand hole that is ~
not a ONE-OF pattern element -- CHOICE only selects between ONE-OF alternatives"
                 field-name choice))
        (unless (member choice hole-alternatives)
          (error "DEFINSTRUCTION: field ~S: (choice ~S) is not one of this hole's ONE-OF ~
alternatives ~S" field-name choice hole-alternatives))))))

(defun %parse-word-operand-subclause (subclause machine-name hole-alternatives)
  "SUBCLAUSE is one whole (operand [NAME] :field FIELD-NAME (variant ...)*)
form on a word-encoded machine. Returns a WORD-OPERAND-SPEC. With no
(variant ...) forms at all, the operand is plain inline over the field's
full unsigned range (bias 0) -- the word-encoded equivalent of a byte-encoded
(operand :mode)'s implicit default. HOLE-ALTERNATIVES (#104) is this hole's
own ONE-OF alternative mode-name list (mode.lisp's %MODE-HOLE-ALTERNATIVES),
or NIL for a plain EXPR hole -- validated against any (CHOICE M) variant
here (%CHECK-WORD-VARIANT-CHOICES!)."
  (multiple-value-bind (name spec) (%parse-operand-subclause subclause)
    (destructuring-bind (field-kw field-name &rest variant-forms) spec
      (unless (eq field-kw :field)
        (error "DEFINSTRUCTION: malformed word operand spec ~S -- expected ~
(operand [name] :field f ...)" subclause))
      (let ((field (instruction-word-field
                    (machine-descriptor-instruction-word (find-machine-descriptor machine-name))
                    field-name)))
        (unless field
          (error "DEFINSTRUCTION: no instruction-word field named ~S on machine ~S"
                 field-name machine-name))
        (destructuring-bind (fname fwidth fshift) field
          (declare (ignore fname))
          (let ((variants (if variant-forms
                               (mapcar (lambda (f) (%parse-word-variant-form f field-name)) variant-forms)
                               (list (make-word-variant :kind :inline :bias 0
                                                         :range (cons 0 (1- (ash 1 fwidth))))))))
            (%check-word-variants variants fwidth field-name)
            (%check-word-variant-choices! variants field-name hole-alternatives)
            (make-word-operand-spec :name name :field field-name :width fwidth :shift fshift
                                     :variants variants)))))))

(defun %parse-word-operand-subclauses (mode subclauses machine name mode-name machine-name)
  "Like %PARSE-OPERAND-SUBCLAUSES but for a word-encoded machine -- one
WORD-OPERAND-SPEC per MODE hole, in hole order. Threads MODE's own
hole-by-hole ONE-OF alternatives (mode.lisp's %MODE-HOLE-ALTERNATIVES, #104)
through to each subclause so a (CHOICE M) variant can be checked against
what its hole can actually match."
  (let ((holes (%mode-hole-count mode)) (n (length subclauses)))
    (unless (= holes n)
      (error "DEFINSTRUCTION ~S ~S: addressing mode ~S has ~D EXPR hole~:P ~
but ~D (operand ...) subclause~:P ~:[were~;was~] given -- one is required ~
per hole" machine name mode-name holes n (= n 1))))
  (let* ((hole-alternatives (%mode-hole-alternatives mode))
         (specs (mapcar (lambda (s alts) (%parse-word-operand-subclause s machine-name alts))
                         subclauses hole-alternatives)))
    (%check-operand-names (mapcar #'word-operand-spec-name specs) machine name mode-name)
    specs))

(defun %word-variant-extra-p (v) (eq (word-variant-kind v) :extra-word))

(defun %expand-word-combos (specs)
  "Cartesian product of SPECS' (WORD-OPERAND-SPEC) variant lists -- one combo
per element, each a list of (SPEC . VARIANT) pairs parallel to SPECS. Ordered
by ascending total :EXTRA-WORD count, ties in declaration order -- matching
%CHOOSE-VARIANT's documented \"narrower before wider\" convention
(assembler.lisp) so an all-inline combo is always tried before one needing an
extra word."
  (let ((combos (list nil)))
    (dolist (spec specs)
      (setf combos
            (loop for combo in combos
                  append (loop for variant in (word-operand-spec-variants spec)
                               collect (append combo (list (cons spec variant)))))))
    (stable-sort combos #'<
                 :key (lambda (combo) (count-if (lambda (p) (%word-variant-extra-p (cdr p))) combo)))))

(defun %word-field-choice-form (spec variant)
  `(make-word-field-choice
    :width ,(word-operand-spec-width spec)
    :shift ,(word-operand-spec-shift spec)
    :kind ,(word-variant-kind variant)
    :bias ,(word-variant-bias variant)
    :range ',(word-variant-range variant)
    :escape ,(word-variant-escape variant)
    :choice ',(word-variant-choice variant)))

(defun %word-alternatives-form (specs)
  "One (quoted) form building SPECS' full per-operand variant menu -- shared
by every sibling combo of one word-field operand list, since decode
(emulator.lisp's %STEP-WORD-MACHINE) needs every alternative, not just
whichever combo happens to occupy the opcode table, to tell an inline value
from an escaped extra-word marker apart by comparing against the actually
fetched bits."
  `(list ,@(mapcar (lambda (spec)
                      `(list ,@(mapcar (lambda (variant) (%word-field-choice-form spec variant))
                                       (word-operand-spec-variants spec))))
                    specs)))

(defun %word-descriptor-form (machine name mode-form opcode alternatives-form combo cycles semantics-forms)
  "One INSTRUCTION-DESCRIPTOR form for word-field COMBO (a list of (SPEC
. VARIANT) pairs from %EXPAND-WORD-COMBOS, in hole order)."
  (let* ((operand-names (mapcar (lambda (p) (word-operand-spec-name (car p))) combo))
         (word-fields-form `(list ,@(mapcar (lambda (p) (%word-field-choice-form (car p) (cdr p))) combo)))
         (extra-words (count-if (lambda (p) (%word-variant-extra-p (cdr p))) combo)))
    `(make-instruction-descriptor
      :name ,(string-upcase (symbol-name name))
      :machine ',machine
      :mode ,mode-form
      :opcode ,opcode
      :operand-widths nil
      :operand-names ',operand-names
      :word-fields ,word-fields-form
      :word-alternatives ,alternatives-form
      :extra-words ,extra-words
      :cycles ,cycles
      :semantics-fn ,(%semantics-fn-form semantics-forms machine operand-names))))

(defun %word-mode-descriptor-forms (machine name mode-form opcode operand-subclauses mode mode-name machine-name
                                     cycles semantics-forms)
  "Every INSTRUCTION-DESCRIPTOR form for one word-encoded addressing-mode
use -- one per %EXPAND-WORD-COMBOS combo, or a single no-operand descriptor
if OPERAND-SUBCLAUSES is empty and MODE has no holes. Unlike the byte-encoded
multi-mode form, a single-hole MODE may NOT omit (operand ...) here even
though the mode itself has only one hole to fill -- there is no
\"default field\" a word-encoded operand could fall back to the way a
byte-encoded one falls back to %MODE-OPERAND-WIDTH, so silently accepting
zero subclauses against a mode with holes would drop that hole's value on
the floor instead of encoding it anywhere."
  (when (and (null operand-subclauses) (plusp (%mode-hole-count mode)))
    (error "DEFINSTRUCTION ~S ~S: addressing mode ~S has ~D EXPR hole~:P but no ~
(operand ...) subclause was given -- a word-encoded operand has no default ~
field to fall back to" machine name mode-name (%mode-hole-count mode)))
  (if (null operand-subclauses)
      (list `(make-instruction-descriptor
              :name ,(string-upcase (symbol-name name))
              :machine ',machine
              :mode ,mode-form
              :opcode ,opcode
              :operand-widths nil
              :operand-names nil
              :word-fields nil
              :word-alternatives nil
              :extra-words 0
              :cycles ,cycles
              :semantics-fn ,(%semantics-fn-form semantics-forms machine nil)))
      (let* ((specs (%parse-word-operand-subclauses mode operand-subclauses machine name mode-name machine-name))
             (alternatives-form (%word-alternatives-form specs))
             (combos (%expand-word-combos specs)))
        (mapcar (lambda (combo)
                  (%word-descriptor-form machine name mode-form opcode alternatives-form combo
                                          cycles semantics-forms))
                combos))))

(defun %check-word-opcode (machine name opcode)
  "Signal an error if OPCODE doesn't fit MACHINE's instruction-word OPCODE
field. Registration keys the opcode table by this *declared* value
(REGISTER-INSTRUCTION-VARIANTS!), while %ENCODE-WORD-INSTRUCTION writes it
through WRAP-VALUE against the field's own width -- without this check, an
opcode too wide for its field would register under one value but encode (and
so decode) as a different, silently wrapped one, an ambiguity of exactly the
kind %CHECK-WORD-VARIANTS already guards against for operand fields."
  (when (%word-machine-p machine)
    (let ((width (second (instruction-word-field (machine-descriptor-instruction-word
                                                    (find-machine-descriptor machine))
                                                  'opcode))))
      (when (or (minusp opcode) (>= opcode (ash 1 width)))
        (error "DEFINSTRUCTION ~S ~S: opcode ~D does not fit the ~D-bit OPCODE field"
               machine name opcode width)))))

(defun %check-word-relative (mode machine name machine-name)
  "A :RELATIVE mode's offset arithmetic (%RELATIVE-OFFSET, assembler.lisp)
assumes a byte operand width -- rejected outright on a word-encoded machine
rather than silently computing nonsense; a follow-up ticket tracks lifting
this once relative branching on a word machine has a design."
  (when (and (mode-descriptor-relativep mode) (%word-machine-p machine-name))
    (error "DEFINSTRUCTION ~S ~S: a :RELATIVE addressing mode is not yet ~
supported on word-encoded machine ~S" machine name machine-name)))

(defun %parse-mode-variant-clause-forms (variant-form machine name default-semantics-forms cycles-form)
  "VARIANT-FORM is one element of a multi-mode (modes ...) clause:
(MODE-NAME (opcode n) (operand ...)* [(semantics form...)] [(cycles n)]).
Returns a list of INSTRUCTION-DESCRIPTOR forms for this variant -- more than
one only on a word-encoded machine (#20), where a variant-bearing operand
field expands into several descriptors sharing this one mode/opcode.

#75: a variant's own (cycles n) subclause overrides the shared top-level
CYCLES-FORM for this mode alone -- e.g. a zero-page mode costing less than
its absolute-mode sibling."
  (destructuring-bind (mode-sym &rest body) variant-form
    (let* ((mode (find-mode-descriptor mode-sym))
           (opcode-subclause (find 'opcode body :key #'first))
           (operand-subclauses (remove-if-not (lambda (c) (eq (first c) 'operand)) body))
           (semantics-subclause (find 'semantics body :key #'first))
           ;; NOTE (#92): like OPCODE-SUBCLAUSE/OPERAND-SUBCLAUSES/SEMANTICS-
           ;; SUBCLAUSE above, this FINDs known subclause heads out of BODY
           ;; and silently drops anything unrecognized -- a typo'd
           ;; (cycle 2) vanishes with no error. Pre-existing, not specific to
           ;; CYCLES; #92 tracks rejecting unknown subclauses here instead.
           (cycles-subclause (find 'cycles body :key #'first)))
      (%check-relative-mode-holes mode machine name)
      (%check-word-relative mode machine name machine)
      (unless opcode-subclause
        (error "DEFINSTRUCTION ~S ~S: mode ~S requires an (opcode n) subclause"
               machine name mode-sym))
      (let ((opcode (second opcode-subclause))
            (cycles-form (if cycles-subclause (second cycles-subclause) cycles-form))
            (semantics-forms (cond
                                (semantics-subclause (rest semantics-subclause))
                                (default-semantics-forms default-semantics-forms)
                                (t (error "DEFINSTRUCTION ~S ~S: mode ~S has no ~
(semantics ...) of its own and no shared top-level (semantics ...) default"
                                          machine name mode-sym)))))
        (%check-word-opcode machine name opcode)
        (if (%word-machine-p machine)
            (%word-mode-descriptor-forms machine name `(find-mode-descriptor ',mode-sym)
                                          opcode operand-subclauses mode mode-sym machine
                                          cycles-form semantics-forms)
            (multiple-value-bind (operand-widths operand-names)
                (%resolve-operand-fields mode operand-subclauses machine name mode-sym machine)
              (list (%descriptor-form machine name `(find-mode-descriptor ',mode-sym)
                                       opcode operand-widths operand-names cycles-form semantics-forms))))))))

(defmacro definstruction (machine name &body clauses)
  "Define an instruction named NAME on machine MACHINE from CLAUSES, each
one of:
  (modes MODE)                       -- 0 or 1 addressing mode, sharing the
                                         top-level (encoding ...) below
  (modes (MODE (opcode n)
               [(operand [NAME] :mode)
                | (operand [NAME] :width n)]*
               [(semantics form...)]
               [(cycles n)])
         ...)                        -- 2+ addressing modes, each with its
                                         own opcode and (optionally) its own
                                         operand field(s), semantics, and
                                         cycle cost; a mode with no
                                         (semantics ...)/(cycles ...) of its
                                         own uses the shared (semantics ...)/
                                         (cycles ...) below as its default
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
  (cycles n)                         -- this instruction's cycle cost (#75),
                                         accumulated by the emulator's step
                                         loop (STEP-MACHINE, emulator.lisp).
                                         Optional; defaults to 1 when
                                         omitted. A multi-mode variant's own
                                         (cycles n) subclause overrides this
                                         shared default for that mode alone.

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
like DEFMACHINE itself.

On a machine declaring an (instruction-word ...) clause (machine.lisp, #20),
every (operand ...) subclause above instead reads
  (operand [NAME] :field FIELD-NAME
    [(variant (range LO HI) inline [:bias N])
     (variant :else (extra-word :escape N))
     (variant (choice MODE) inline :range (LO HI) [:bias N])
     (variant (choice MODE) (extra-word :escape N))]*)
binding NAME's value to instruction-word field FIELD-NAME rather than to a
byte-width encoding. With no (variant ...) forms, the field holds the value
directly (biased by 0) over its full unsigned range. With one or more, a
value-selected INLINE variant's (biased) range or :ELSE fallback works as
described above.

A (choice MODE) selector (#104) instead selects by *syntax*, not value: MODE
must be one of the addressing-mode alternatives named by the ONE-OF pattern
element (mode.lisp) that produced this hole, and the variant applies only
when MODE is the alternative that hole actually matched (mode.lisp's
TRY-MATCH-OPERAND-MODE/MATCH-OPERAND-MODE CHOICES, hole-aligned per #104).
A CHOICE-selected INLINE variant packs its own value into the field's range
exactly like a value-selected one, but requires an explicit :RANGE (LO HI)
of its own -- unlike (range LO HI), the selector itself carries no range.
A CHOICE-selected (extra-word :escape N) variant writes N into the field and
the value into its own following word *unconditionally* once MODE is
matched, regardless of what value the hole folds to -- unlike :ELSE, which
only triggers when no INLINE variant's range fits. Declaring a (choice M)
variant for a hole that is not a ONE-OF, or naming a mode that is not one of
that ONE-OF's own alternatives, is a DEFINSTRUCTION-time error
(%CHECK-WORD-VARIANT-CHOICES!). A field's variants must be either all
CHOICE-selected or all value-selected (RANGE/:ELSE) -- never a mix.

Either way, declaring variants at all makes DEFINSTRUCTION register one
INSTRUCTION-DESCRIPTOR per combination of variants across all of a mode's
fields, sharing one mnemonic, mode, and opcode value -- the assembler's
existing relaxation and #104's new CHOICE-eligibility filter
(%CHOOSE-VARIANT, assembler.lisp) pick between them per statement, all-inline
tried before any needing an extra word among whichever combos a CHOICE-
selected field's matched alternative left eligible. A CHOICE-selected field
whose matched alternative's own range doesn't fit the folded value is an
ASSEMBLY-ERROR at assemble time (there being no wider CHOICE-selected
sibling to relax into, unlike the value-selected case's silent-wrap
fallback). Every variant's range and every escape value must fit FIELD-NAME's
declared bit width; no two INLINE variants' (biased) ranges may overlap; no
two :EXTRA-WORD variants may share an escape value; and no escape value may
fall inside any INLINE variant's biased range -- all checked here, at
DEFINSTRUCTION time, since any of them would make the field undecodable. A
:RELATIVE addressing mode is not supported on a word-encoded machine."
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
           (%check-word-opcode machine name (second opcode-subclause))
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
               (list ,@(mapcan (lambda (variant-form)
                                  (%parse-mode-variant-clause-forms variant-form machine name
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
           (%check-word-relative mode machine name machine)
           (unless opcode-subclause
             (error "DEFINSTRUCTION ~S ~S: (encoding ...) requires an (opcode n) subclause"
                    machine name))
           (unless operand-subclauses
             (error "DEFINSTRUCTION ~S ~S: (modes ~A) declares an addressing mode but ~
(encoding ...) has no (operand ...) subclause" machine name mode-sym))
           (%check-word-opcode machine name (second opcode-subclause))
           (if (%word-machine-p machine)
               `(eval-when (:compile-toplevel :load-toplevel :execute)
                  (register-instruction-variants!
                   ',machine
                   (list ,@(%word-mode-descriptor-forms machine name `(find-mode-descriptor ',mode-sym)
                                                         (second opcode-subclause) operand-subclauses
                                                         mode mode-sym machine
                                                         cycles-form (rest semantics-clause))))
                  ',name)
               (multiple-value-bind (operand-widths operand-names)
                   (%parse-operand-subclauses mode operand-subclauses machine name mode-sym machine)
                 `(eval-when (:compile-toplevel :load-toplevel :execute)
                    (register-instruction-variants!
                     ',machine
                     (list ,(%descriptor-form machine name `(find-mode-descriptor ',mode-sym)
                                               (second opcode-subclause)
                                               operand-widths operand-names
                                               cycles-form (rest semantics-clause))))
                    ',name)))))))))

;;; Encoding / execution

(defun %encode-value-cells (value width cell-width)
  "Split (already-evaluated integer) VALUE into WIDTH little-endian
(unsigned-byte CELL-WIDTH) cells, wrapping each with WRAP-VALUE
(storage.lisp) like every other encoded quantity in this codebase. Shared by
ENCODE-INSTRUCTION below and the assembler's .BYTE/.WORD directive encoding
(assembler.lisp, #14), so instruction operands and directive data can't
drift apart in how they lay cells down."
  (loop for i below width collect (wrap-value (ash value (* (- cell-width) i)) cell-width)))

(defun %encode-word-instruction (descriptor layout values)
  "ENCODE-INSTRUCTION's word-encoded path (#20): OR DESCRIPTOR's opcode and
each operand's chosen WORD-FIELD-CHOICE (WORD-FIELDS, parallel to VALUES)
into one LAYOUT-WIDTH-bit word by shift, then emit that word little-endian
(%ENCODE-VALUE-CELLS, at LAYOUT's own CELL-WIDTH) followed by each
:EXTRA-WORD operand's own value, also little-endian, in operand declaration
order."
  (let ((word 0) extra-word-values (cell-width (instruction-word-layout-cell-width layout)))
    (destructuring-bind (opcode-width opcode-shift)
        (rest (instruction-word-field layout 'opcode))
      (setf word (ash (wrap-value (instruction-descriptor-opcode descriptor) opcode-width) opcode-shift)))
    (loop for choice in (instruction-descriptor-word-fields descriptor)
          for value in values
          do (ecase (word-field-choice-kind choice)
               (:inline
                (setf word (logior word (ash (wrap-value (+ value (word-field-choice-bias choice))
                                                          (word-field-choice-width choice))
                                              (word-field-choice-shift choice)))))
               (:extra-word
                (setf word (logior word (ash (word-field-choice-escape choice)
                                              (word-field-choice-shift choice))))
                (cl:push value extra-word-values))))
    (append (%encode-value-cells word (instruction-word-layout-width-cells layout) cell-width)
            (loop for value in (nreverse extra-word-values)
                  append (%encode-value-cells value (instruction-word-layout-width-cells layout) cell-width)))))

(defun encode-instruction (descriptor values)
  "Encode one use of instruction DESCRIPTOR with operand VALUES (a list of
already-evaluated integers, one per operand encoding field, in the same
order -- NIL for a no-operand instruction) into a list of
(unsigned-byte cell-width) cells, CELL-WIDTH being DESCRIPTOR's machine's own
code cell width (#53, INSTRUCTION-DESCRIPTOR-CELL-WIDTH). On an ordinary
cell-encoded machine: the opcode, followed by each value's cells
little-endian in turn, per DESCRIPTOR's OPERAND-WIDTHS. On a word-encoded
machine (#20, INSTRUCTION-DESCRIPTOR-WORD-LAYOUT non-NIL): one instruction
word packing the opcode and every inline operand's biased value or
extra-word escape by bit field, little-endian, followed by each extra-word
operand's own value, also little-endian, in declaration order
(%ENCODE-WORD-INSTRUCTION). VALUES shorter than DESCRIPTOR declares silently
encodes fewer fields, rather than erroring -- every caller in this codebase
(%ENCODE, assembler.lisp) always supplies exactly one value per field, so
this is unreachable internally, but a caller of this exported function on
its own should supply the same."
  (let ((layout (instruction-descriptor-word-layout descriptor)))
    (if layout
        (%encode-word-instruction descriptor layout values)
        (let ((cell-width (instruction-descriptor-cell-width descriptor)))
          (cons (wrap-value (instruction-descriptor-opcode descriptor) cell-width)
                (loop for value in values
                      for width in (instruction-descriptor-operand-widths descriptor)
                      append (%encode-value-cells value width cell-width)))))))

(defun execute-instruction (descriptor machine values)
  "Execute instruction DESCRIPTOR against a live MACHINE instance, passing
VALUES (a list of already-evaluated integers, one per operand encoding
field, or NIL for a no-operand instruction) to its semantics -- OPERAND is
bound to the first (or only) value, and any named field to its own value
(see %SEMANTICS-FN-FORM)."
  (funcall (instruction-descriptor-semantics-fn descriptor) machine values))
