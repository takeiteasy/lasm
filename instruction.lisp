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

(define-condition no-matching-choice (lasm-error)
  ((machine :initarg :machine :reader no-matching-choice-machine)
   (instruction :initarg :instruction :reader no-matching-choice-instruction)
   (operand :initarg :operand :reader no-matching-choice-operand)
   (choice :initarg :choice :reader no-matching-choice-choice))
  (:documentation "Signalled by a (SEMANTICS ...) body's CHOICE-CASE (#73)
when OPERAND's matched alternative -- CHOICE, a mode-name symbol, or NIL when
none was recorded (a hole with no hole-selected sub-opcode selector on a
cell-encoded machine, or EXECUTE-INSTRUCTION called directly with no
CHOICES) -- names none of CHOICE-CASE's own clauses and it declares no
OTHERWISE clause to fall back to. Distinguishable from any other
error a semantics body might signal, the same rationale as UNKNOWN-
INSTRUCTION being its own condition rather than a generic error. MACHINE and
INSTRUCTION are kept as separate slots, like UNKNOWN-INSTRUCTION's own
MACHINE/MNEMONIC, so a handler can read either programmatically rather than
parsing them back out of a combined report string.")
  (:report (lambda (c s)
             (format s "Instruction ~S ~S: operand ~S matched alternative ~S, ~
which no CHOICE-CASE clause names, and no OTHERWISE clause was given"
                     (no-matching-choice-machine c) (no-matching-choice-instruction c)
                     (no-matching-choice-operand c) (no-matching-choice-choice c)))))

(define-condition opcode-conflict (lasm-error)
  ((machine :initarg :machine :reader opcode-conflict-machine)
   (opcode :initarg :opcode :reader opcode-conflict-opcode)
   (mnemonic :initarg :mnemonic :reader opcode-conflict-mnemonic)
   (other-mnemonic :initarg :other-mnemonic :reader opcode-conflict-other-mnemonic)
   ;; #105: NIL for the original "different mnemonic, same opcode" case
   ;; (below); :UNDECODABLE-BYTE-MACHINE or :INDISTINGUISHABLE otherwise --
   ;; see REGISTER-INSTRUCTION-VARIANTS!/%CHECK-OPCODE-DECODABLE!. #125 adds
   ;; two more, both byte-machine-only: :SUB-OPCODE-REQUIRED (one co-tenant
   ;; declares (opcode n :sub s), the other doesn't -- decode could not tell
   ;; whether cell+1 is a sub-opcode or an operand) and :DUPLICATE-SUB-OPCODE
   ;; (both declare a :SUB, but the same value, so cell+1 still can't tell
   ;; them apart).
   (reason :initarg :reason :initform nil :reader opcode-conflict-reason))
  (:documentation "Signalled by REGISTER-INSTRUCTION-VARIANTS! when a
descriptor's opcode is already claimed by another descriptor on the same
machine and the two cannot coexist there: a *different* mnemonic (#26) --
without this check the later DEFINSTRUCTION silently wins the opcode-table
entry, and a later redefinition of the earlier mnemonic can then delete the
winner's entry outright as an apparently orphaned opcode -- or, on a
byte-encoded machine, even the *same* mnemonic under a different mode (#105:
a byte encoding carries no per-field discriminator to decode two modes
apart, unlike a word-encoded machine's operand fields, unless every co-tenant
declares its own distinct sub-opcode -- #125), or, on a word-encoded machine,
two descriptors whose operand fields accept overlapping raw bit patterns at
every field they share bits with (#105's %CHECK-OPCODE-DECODABLE!, which is
what REASON :INDISTINGUISHABLE names) -- co-tenants may name different
instruction-word layouts (#64/#140) or even different fields, as long as
some field either occupies disagrees at the bits it actually sits at.")
  (:report (lambda (c s)
             (case (opcode-conflict-reason c)
               (:undecodable-byte-machine
                (format s "Opcode ~S for instruction ~S on machine ~S is already registered to ~S -- ~
a byte-encoded machine has no per-field discriminator to decode two modes ~
of one mnemonic apart, so they may not share an opcode unless every mode ~
declares its own distinct (opcode ~S :sub s)"
                        (opcode-conflict-opcode c) (opcode-conflict-mnemonic c)
                        (opcode-conflict-machine c) (opcode-conflict-other-mnemonic c)
                        (opcode-conflict-opcode c)))
               (:indistinguishable
                (format s "Opcode ~S for instruction ~S on machine ~S is already registered to ~S with ~
an indistinguishable encoding -- no operand field's raw bits tell the two apart at decode time"
                        (opcode-conflict-opcode c) (opcode-conflict-mnemonic c)
                        (opcode-conflict-machine c) (opcode-conflict-other-mnemonic c)))
               (:sub-opcode-required
                (format s "Opcode ~S for instruction ~S on machine ~S is already registered to ~S -- ~
one of the two declares a :SUB sub-opcode and the other doesn't, so decode ~
could not tell whether the cell after the opcode is a sub-opcode or an operand; ~
give both a distinct (opcode ~S :sub s)"
                        (opcode-conflict-opcode c) (opcode-conflict-mnemonic c)
                        (opcode-conflict-machine c) (opcode-conflict-other-mnemonic c)
                        (opcode-conflict-opcode c)))
               (:duplicate-sub-opcode
                (format s "Opcode ~S for instruction ~S on machine ~S is already registered to ~S ~
under the same sub-opcode -- two co-tenants at one opcode need pairwise distinct :SUB values"
                        (opcode-conflict-opcode c) (opcode-conflict-mnemonic c)
                        (opcode-conflict-machine c) (opcode-conflict-other-mnemonic c)))
               (t
                (format s "Opcode ~S for instruction ~S on machine ~S is already registered to ~S"
                        (opcode-conflict-opcode c) (opcode-conflict-mnemonic c)
                        (opcode-conflict-machine c) (opcode-conflict-other-mnemonic c)))))))

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
  ;; #143: one storage-element name (or NIL) per operand hole, parallel to
  ;; OPERAND-NAMES/OPERAND-WIDTHS -- a hole whose (operand ... :register ELEM)
  ;; subclause named ELEM indexes that banked register's bank, so the
  ;; disassembler (%RENDER-OPERAND-TEXT, disassembler.lisp) can render its
  ;; decoded value as ELEM's own #72 :NAMES alias instead of a bare integer.
  ;; NIL throughout for a descriptor with no :REGISTER hole.
  (operand-registers nil :type list)
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
  ;; #135: total cells every :EXTRA-WORD field in WORD-FIELDS spills into --
  ;; the sum of each such field's own WORD-FIELD-CHOICE-EXTRA-CELLS, which
  ;; may now differ per field (formerly EXTRA-WORDS, a plain field count,
  ;; each one implicitly INSTRUCTION-WORD-LAYOUT-WIDTH-CELLS wide, #53). 0
  ;; for a byte-encoded descriptor and for an all-inline word combo alike.
  (extra-cells 0 :type (integer 0))
  ;; #125 (M4): non-NIL only on a byte-encoded machine, and only when this
  ;; descriptor's own (opcode n :sub s) subclause -- or, per #126 below, a
  ;; hole-selected (variant (choice m) (sub s)) -- gave one. Lets several
  ;; DESCRIPTORs share one OPCODE on a byte-encoded machine -- normally
  ;; impossible there (#105: a byte encoding has no per-field discriminator
  ;; the way a word-encoded machine's operand fields give it one) -- by
  ;; reserving the cell right after the opcode as a second, purely
  ;; discriminating value REGISTER-INSTRUCTION-VARIANTS! requires every
  ;; co-tenant at that opcode to declare distinctly.
  (sub-opcode nil :type (or null (integer 0)))
  ;; #126/#128 (M4): non-NIL only on a byte-encoded machine, and only when
  ;; one or more of this descriptor's operand holes carries a hole-selected
  ;; sub-opcode selector -- a single-hole (variant (choice m) (sub s)), or
  ;; several holes jointly selected by a (sub-opcode ...) table (#128) --
  ;; the byte-machine analogue of WORD-FIELDS' CHOICE, and of
  ;; %DECODE-CELL-INSTRUCTION's fourth CHOICES return value. Hole-aligned,
  ;; parallel to OPERAND-NAMES/OPERAND-WIDTHS: one or more entries may be
  ;; non-NIL, naming the ONE-OF alternative *this* descriptor was expanded
  ;; for at that hole -- %DECODE-CELL-INSTRUCTION hands this list straight back
  ;; as CHOICES once it has picked the matching descriptor by SUB-OPCODE, so
  ;; CHOICE-CASE (instruction.lisp) and the disassembler (disassembler.lisp)
  ;; work on a byte-encoded machine exactly as they already do on a
  ;; word-encoded one. NIL throughout for a plain (opcode n :sub s) or a
  ;; SUB-OPCODE-less descriptor alike.
  (sub-choices nil :type list)
  ;; #124/#127 (M4): byte-encoded machine only, always the same length as
  ;; OPERAND-WIDTHS when non-NIL -- one boolean per operand hole, T when that
  ;; hole's operand is a signed quantity. Precomputed at DEFINSTRUCTION time
  ;; (%BYTE-DESCRIPTOR-FORMS) rather than re-derived per decode (a
  ;; FIND-MODE-DESCRIPTOR lookup against SUB-CHOICES would work too, but the
  ;; decoder is the emulator's hot path -- see #84 for this class of
  ;; per-decode re-derivation this avoids). Always NIL on a word-encoded
  ;; descriptor, like OPERAND-WIDTHS itself -- #127's per-hole signedness
  ;; lives on WORD-FIELD-CHOICE-SIGNEDP instead, since a word-encoded
  ;; descriptor's signedness can differ by *which field-variant combo* this
  ;; descriptor is, not just by hole. A reader must treat a NIL list here the
  ;; same as an all-NIL one of the right length -- see the callers in
  ;; decoder.lisp and assembler.lisp for the shared (OR ... (MAKE-LIST ...))
  ;; guard.
  (operand-signedness nil :type list)
  ;; #130 (M4): byte-encoded machine only. NIL when no hole of this
  ;; descriptor is a PC-relative offset; else the 0-based index, into
  ;; OPERAND-WIDTHS/OPERAND-NAMES/etc, of the one hole that is. Unlike
  ;; OPERAND-SIGNEDNESS (a per-hole BOOLEAN list, since any number of holes
  ;; may independently be signed), :RELATIVE is POSITIONAL -- at most one
  ;; hole per pattern may be relative (%CHECK-RELATIVE-MODE-HOLES /
  ;; %CHECK-BYTE-ONE-OF-RELATIVE, below), so a single index suffices. Also
  ;; where a whole-mode :RELATIVE (MODE-DESCRIPTOR-RELATIVEP) folds in: a
  ;; whole-mode relative descriptor always has exactly one hole
  ;; (%CHECK-RELATIVE-MODE-HOLES), so it is always index 0 here -- every
  ;; consumer (assembler.lisp's %CHOOSE-VARIANT/%ENCODE/
  ;; %CHECK-STRICT-OPERAND-RANGE!, disassembler.lisp's
  ;; %OPERAND-RENDER-VALUES) reads this single slot instead of branching on
  ;; MODE-DESCRIPTOR-RELATIVEP separately. Precomputed at DEFINSTRUCTION time
  ;; (%BYTE-DESCRIPTOR-FORMS, %BYTE-RELATIVE-HOLE-INDEX), mirroring
  ;; OPERAND-SIGNEDNESS's own precomputation rationale. Always NIL on a
  ;; byte-encoded descriptor with no relative hole -- a word-encoded
  ;; descriptor has its OWN (per-combo) RELATIVE-HOLE-INDEX since #62
  ;; (%WORD-DESCRIPTOR-FORM, %WORD-RELATIVE-HOLE-INDEX), unrelated to this
  ;; slot's arithmetic, which assumes a cell-counted operand width.
  (relative-hole-index nil :type (or null (integer 0)))
  ;; #64: non-NIL only on a word-encoded machine declaring one or more
  ;; (layout NAME ...) alternates -- the layout this descriptor's fields were
  ;; resolved against, NIL for the machine's default layout. Stored as a
  ;; NAME, not the INSTRUCTION-WORD-LAYOUT struct itself, so it can't drift
  ;; from the machine descriptor it names -- see
  ;; INSTRUCTION-DESCRIPTOR-WORD-LAYOUT, the sole place it's resolved.
  (word-layout-name nil :type symbol)
  ;; #136 (M4): non-NIL only on a word-encoded machine, and only when one or
  ;; more (field-value FIELD-NAME n) subclauses pinned a field to a literal.
  ;; A list of WORD-CONSTANT, declaration order -- ENCODE-INSTRUCTION ORs
  ;; each straight into the instruction word, and %TRY-DECODE-WORD-CANDIDATE
  ;; (decoder.lisp) rejects the candidate outright unless every one of them
  ;; matches the fetched word's own bits. %CHECK-OPCODE-DECODABLE! treats a
  ;; pinned field exactly like a hole's raw bits for co-tenancy purposes --
  ;; see %CONSTANTS-DISJOINT-P below.
  (word-constants nil :type list))

(defun instruction-descriptor-total-operand-width (descriptor)
  "Sum of DESCRIPTOR's OPERAND-WIDTHS -- the cell count its operand encoding
occupies as a whole, regardless of how many fields it's split across. 0 for
a no-operand instruction, and always 0 for a word-encoded descriptor (#20),
whose OPERAND-WIDTHS is NIL by construction -- see INSTRUCTION-DESCRIPTOR-SIZE
for the accessor that covers both encoding schemes."
  (reduce #'+ (instruction-descriptor-operand-widths descriptor) :initial-value 0))

(defun instruction-descriptor-word-layout (descriptor)
  "DESCRIPTOR's own INSTRUCTION-WORD-LAYOUT (storage.lisp) -- the machine's
default layout, or, when DESCRIPTOR names one (#64, WORD-LAYOUT-NAME), the
alternate it was resolved against. NIL on an ordinary byte-encoded machine.
Looked up via DESCRIPTOR's own MACHINE slot rather than cached on the
descriptor, so it can't drift from the machine descriptor it names."
  (let ((default (machine-descriptor-instruction-word (find-machine-descriptor (instruction-descriptor-machine descriptor)))))
    (and default (instruction-word-layout-named default (instruction-descriptor-word-layout-name descriptor)))))

(defun instruction-descriptor-cell-width (descriptor)
  "DESCRIPTOR's machine's code cell width in bits (#53) -- looked up via
%MACHINE-CELL-WIDTH (machine.lisp) rather than cached on the descriptor, same
rationale as INSTRUCTION-DESCRIPTOR-WORD-LAYOUT."
  (%machine-cell-width (instruction-descriptor-machine descriptor)))

(defun instruction-descriptor-endian (descriptor)
  "DESCRIPTOR's machine's cell endianness (#66) -- looked up via
%MACHINE-ENDIAN (machine.lisp), same rationale as
INSTRUCTION-DESCRIPTOR-CELL-WIDTH. Only used on DESCRIPTOR's cell-encoded
path (ENCODE-INSTRUCTION below); a word-encoded descriptor's own
INSTRUCTION-WORD-LAYOUT already carries its own ENDIAN, set once at
DEFMACHINE time (%FINISH-INSTRUCTION-WORD-LAYOUT, machine.lisp)."
  (%machine-endian (instruction-descriptor-machine descriptor)))

(defun instruction-descriptor-size (descriptor)
  "Total encoded cells for one use of DESCRIPTOR -- 1 (opcode cell), plus 1
more for a sub-opcode cell when SUB-OPCODE is non-NIL (#125), plus operand
cell widths on an ordinary byte/cell-encoded machine, or
INSTRUCTION-WORD-LAYOUT-WIDTH-CELLS + EXTRA-CELLS on a word-encoded one (#20;
SUB-OPCODE is always NIL there -- #125's sub-opcode cell is a byte-machine-
only mechanism). #135: EXTRA-CELLS is a plain sum, not WIDTH-CELLS times an
extra-word count, since each :EXTRA-WORD field may now declare its own
width. Centralizes what used to be five separate \"1 + operand width\"
computations scattered across the assembler's layout/relaxation, its
relative-branch offset arithmetic, and the emulator's fetch loop, so a
word-encoded descriptor's size is computed identically everywhere rather
than each caller assuming a byte opcode."
  (let ((layout (instruction-descriptor-word-layout descriptor)))
    (if layout
        (+ (instruction-word-layout-width-cells layout) (instruction-descriptor-extra-cells descriptor))
        (+ 1 (if (instruction-descriptor-sub-opcode descriptor) 1 0)
           (instruction-descriptor-total-operand-width descriptor)))))

;;; Constant folding (the evaluated-operand slice of full expression evaluation)

;; #72: the target machine's alias name (string) -> bank index table
;; (MACHINE-DESCRIPTOR-REGISTER-ALIASES, storage.lisp), bound by
;; ASSEMBLE-STATEMENTS (assembler.lisp) around layout and encoding. A
;; special rather than an EVAL-EXPR argument -- like *STRICT-OPERAND-RANGE*
;; (diagnostic.lisp) -- so %BIND-SYMBOL! (assembler.lisp) can also see it,
;; to reject a label/.EQU name that collides with an alias, with no
;; signature change to either call chain. NIL (the default) means no
;; aliases are in scope, e.g. outside of ASSEMBLE-STATEMENTS.
(defvar *register-aliases* nil)

(defun eval-expr (ast &key symbols pc)
  "Fold the EXPR-* AST node AST (parser.lisp) to an integer. SYMBOLS, when
given, is a hash table (string -> value -- a label's address, or an .EQU's
folded value, #35) resolving EXPR-LABEL nodes -- the assembler pass
(assembler.lisp) calls this with its completed layout symbol table. PC, when
given, is the integer address EXPR-LOCATION (the \"*\" location-counter
symbol, #15) folds to. An EXPR-LABEL not found in SYMBOLS falls back to
*REGISTER-ALIASES* (#72), resolving a banked register's symbolic name (e.g.
DCPU-16's \"i\") to its bank index -- SYMBOLS is tried first so a label
always wins if a program somehow binds one anyway, though %BIND-SYMBOL!
(assembler.lisp) rejects that collision outright. Signals UNRESOLVED-LABEL
on an EXPR-LABEL matching neither, and UNRESOLVED-LOCATION on an
EXPR-LOCATION when PC is NIL. :LO/:HI (below) are fixed 8-bit byte
operators -- they split off the low/high byte of a value regardless of the
target machine's :CELL-WIDTH (#67), not an encoding-width-relative split."
  (etypecase ast
    (expr-number (expr-number-value ast))
    (expr-label
     (multiple-value-bind (value foundp)
         (and symbols (gethash (expr-label-name ast) symbols))
       (unless foundp
         (multiple-value-setq (value foundp)
           (and *register-aliases* (gethash (expr-label-name ast) *register-aliases*))))
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
         ;; Fixed 8-bit split, independent of the machine's :CELL-WIDTH (#67) --
         ;; a byte-packing convenience, not a cell-width-relative operator.
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
under that mnemonic. Every old descriptor registered under this mnemonic is
first dropped from every opcode bucket it occupied, so a redefinition that
drops a mode does not leave FIND-INSTRUCTION-DESCRIPTORS-BY-OPCODE (an
emulator's decode step) still resolving it to a now-stale descriptor; a
co-tenant *other* mnemonic sharing one of those opcodes (#105, below) is
untouched by this cleanup.

Each of MACHINE-NAME's opcode buckets holds a *list* of descriptors, not one
(#105) -- one entry per DEFINSTRUCTION-time-verified decode-distinguishable
descriptor sharing that opcode. A word-encoded machine's variant expansion
(#20, %EXPAND-WORD-COMBOS) can hand this several sibling DESCRIPTORS sharing
one opcode value with an EQUALP WORD-ALTERNATIVES menu (one DEFINSTRUCTION
mode clause, encoded differently by operand size) -- those always coexist,
since %DECODE-WORD-INSTRUCTION (decoder.lisp, via DECODE-INSTRUCTION-AT) tries
every candidate at an opcode and picks the one whose fields the fetched bits
actually match, regardless of which specific combo it's looking at. Two
descriptors that are *not* siblings -- a different mnemonic, or the same
mnemonic under a different (MODES ...) clause -- may also coexist at one
opcode on a word-encoded machine, but only once %CHECK-OPCODE-DECODABLE!
(below) confirms some operand field's raw bits tell them apart.

A byte-encoded machine has no per-field discriminator to decode by at all, so
by default any second descriptor at an opcode there -- same mnemonic or
different -- is an unconditional OPCODE-CONFLICT (#26 for the cross-mnemonic
case; #105 for the same-mnemonic-different-mode case, previously silent: it
registered with no error and then mis-decoded, since the opcode table held
exactly one descriptor, last-write-wins). #125 opens one exception: when
*every* descriptor sharing a byte-machine opcode declares its own SUB-OPCODE
(an (opcode n :sub s) subclause), and those values are pairwise distinct, the
sub-opcode cell right after the opcode gives decode (%DECODE-CELL-INSTRUCTION,
decoder.lisp) something to discriminate on, so they coexist just like a
word-encoded machine's field-distinguished co-tenants. Mixing a SUB-OPCODE
descriptor with a SUB-OPCODE-less one at the same byte-machine opcode is still
an OPCODE-CONFLICT (:SUB-OPCODE-REQUIRED) -- decode could not tell whether the
cell after the opcode is a sub-opcode or the first operand -- and so is a
collision on the same SUB-OPCODE value (:DUPLICATE-SUB-OPCODE)."
  (let* ((md (find-machine-descriptor machine-name))
         (name (instruction-descriptor-name (first descriptors)))
         (wordp (%word-machine-p machine-name)))
    (loop for opcode being the hash-keys of (machine-descriptor-opcodes md)
            using (hash-value bucket)
          do (let ((kept (remove name bucket :key #'instruction-descriptor-name :test #'string=)))
               (if kept
                   (setf (gethash opcode (machine-descriptor-opcodes md)) kept)
                   (remhash opcode (machine-descriptor-opcodes md)))))
    ;; One new descriptor at a time, so a later entry in DESCRIPTORS itself
    ;; sees an earlier one this same call already inserted -- needed for
    ;; same-mnemonic-different-mode co-tenancy, where two entries of
    ;; DESCRIPTORS (not just a pre-existing bucket) can share an opcode.
    (dolist (descriptor descriptors)
      (let* ((opcode (instruction-descriptor-opcode descriptor))
             (bucket (gethash opcode (machine-descriptor-opcodes md)))
             (sub (instruction-descriptor-sub-opcode descriptor)))
        (dolist (other bucket)
          (cond
            (wordp (%check-opcode-decodable! machine-name name descriptor other))
            ;; #125: both co-tenants declare a SUB-OPCODE -- fine as long as
            ;; they're pairwise distinct; a collision still can't be told
            ;; apart at decode time.
            ((and sub (instruction-descriptor-sub-opcode other))
             (when (= sub (instruction-descriptor-sub-opcode other))
               (error 'opcode-conflict :machine machine-name :opcode opcode :mnemonic name
                                        :other-mnemonic (instruction-descriptor-name other)
                                        :reason :duplicate-sub-opcode)))
            ;; #125: exactly one of the two declares a SUB-OPCODE -- decode
            ;; couldn't tell whether the cell after the opcode is a
            ;; sub-opcode or the first operand.
            ((or sub (instruction-descriptor-sub-opcode other))
             (error 'opcode-conflict :machine machine-name :opcode opcode :mnemonic name
                                      :other-mnemonic (instruction-descriptor-name other)
                                      :reason :sub-opcode-required))
            (t
             (error 'opcode-conflict :machine machine-name :opcode opcode :mnemonic name
                                      :other-mnemonic (instruction-descriptor-name other)
                                      :reason (when (string= name (instruction-descriptor-name other))
                                                :undecodable-byte-machine)))))
        (setf (gethash opcode (machine-descriptor-opcodes md)) (append bucket (list descriptor)))))
    (setf (gethash name (machine-descriptor-instructions md)) descriptors)
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

(defun find-instruction-descriptors-by-opcode (machine-name opcode)
  "Look up every INSTRUCTION-DESCRIPTOR registered under OPCODE on machine
MACHINE-NAME, in declaration order -- the decode direction an emulator loop
needs. More than one entry on a word-encoded machine (#105): either sibling
combos of one DEFINSTRUCTION mode clause (%EXPAND-WORD-COMBOS), which share an
EQUALP WORD-ALTERNATIVES menu, or distinct co-tenant descriptors
REGISTER-INSTRUCTION-VARIANTS!'s %CHECK-OPCODE-DECODABLE! has already
confirmed are pairwise distinguishable by some operand field's raw bits --
%DECODE-WORD-INSTRUCTION (decoder.lisp) tries each in turn against the bits
actually fetched. Also more than one on a byte-encoded machine (#125), but
only when every entry declares its own distinct SUB-OPCODE --
%DECODE-CELL-INSTRUCTION (decoder.lisp) then reads the cell after the opcode
to pick which. A byte-encoded machine's opcode table otherwise holds exactly
one entry per key, enforced at registration time. Signals UNKNOWN-INSTRUCTION
if none is registered."
  (let ((md (find-machine-descriptor machine-name)))
    (or (gethash opcode (machine-descriptor-opcodes md))
        (error 'unknown-instruction :machine machine-name :opcode opcode))))

(defun find-instruction-by-opcode (machine-name opcode)
  "Look up the first INSTRUCTION-DESCRIPTOR registered under OPCODE on
machine MACHINE-NAME -- see FIND-INSTRUCTION-DESCRIPTORS-BY-OPCODE for the
full candidate list this picks from. Correct for an ordinary byte-encoded
opcode with no SUB-OPCODE co-tenants (exactly one candidate, always) and for
a word-encoded opcode with only sibling combos at it (every sibling decodes
any one candidate's bits equivalently, per REGISTER-INSTRUCTION-VARIANTS!'s
docstring) -- not a substitute for FIND-INSTRUCTION-DESCRIPTORS-BY-OPCODE's
own decode-by-actual-bits (or, on a byte-encoded machine, decode-by-
sub-opcode-cell, #125) behavior when distinct co-tenants share an opcode."
  (first (find-instruction-descriptors-by-opcode machine-name opcode)))

;; A mode's default operand width, when neither the mode itself nor the
;; instruction gives one explicitly: the machine's sole memory element's
;; address width, rounded up to whole cells of that same element's own
;; CELL-WIDTH (#53), in that element's own endian order on encode (#66).
;; When a machine declares more
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
%OPERAND-WIDTH expects. Also used, with its own further splitting, by the
word-encoded path's %PARSE-WORD-OPERAND-SUBCLAUSE, whose SPEC instead starts
(:field field-name (variant ...)*) -- this function only tells NAME apart
from the rest, it doesn't care what shape the rest takes."
  (let ((rest (cdr subclause)))
    (if (keywordp (first rest))
        (values nil rest)
        (values (first rest) (rest rest)))))

(defun %parse-operand-register-clause (tail subclause)
  "TAIL is an (operand ...) subclause's own spec tail with :MODE/:WIDTH n (and,
on the word path, :FIELD f) already stripped off the front. Returns (VALUES
register-sym remaining-tail): an optional leading (:register ELEM . more)
(#143) is consumed and ELEM returned, else NIL and TAIL unchanged. SUBCLAUSE
is the whole original form, for the error naming it when :REGISTER appears
anywhere but this fixed position (immediately after :MODE/:WIDTH n/:FIELD f,
before any (variant ...) forms)."
  (cond
    ((eq (first tail) :register) (values (second tail) (cddr tail)))
    ((member :register tail)
     (error "Malformed operand encoding spec ~S -- :REGISTER must come right after ~
:MODE, :WIDTH n, or :FIELD f, before any (variant ...) forms" subclause))
    (t (values nil tail))))

(defun %parse-byte-operand-subclause (subclause)
  "Like %PARSE-OPERAND-SUBCLAUSE, but for the byte-encoded path: returns
(VALUES name spec variant-forms register), further splitting SPEC's own tail
off any leading :REGISTER ELEM (#143) and trailing (variant (choice m)
(sub s)) forms (#126, byte-encoded machines only) -- both NIL for the plain
(operand :mode)/(operand :width n) forms every mnemonic used before them.
:WIDTH's own numeric arg is consumed as part of SPEC, not left in
VARIANT-FORMS, so a bare :MODE (which takes no arg) and a :WIDTH N (which
does) are told apart correctly."
  (multiple-value-bind (name rest) (%parse-operand-subclause subclause)
    (destructuring-bind (spec-head &rest spec-tail) rest
      (multiple-value-bind (spec after-spec)
          (case spec-head
            (:mode (values (list :mode) spec-tail))
            (:width (values (list :width (first spec-tail)) (rest spec-tail)))
            (:register (error "Malformed operand encoding spec ~S -- :REGISTER must come after ~
:MODE or :WIDTH n, e.g. (operand NAME :mode :register ELEM), not before it" subclause))
            (t (error "Malformed operand encoding spec ~S -- expected (operand :mode) or (operand :width n)"
                      subclause)))
        (multiple-value-bind (register variant-forms) (%parse-operand-register-clause after-spec subclause)
          (values name spec variant-forms register))))))

(defun %scalar-bindable-names (machine-name)
  "The set of names WITH-MACHINE-BINDINGS (semantics.lisp) binds for
MACHINE-NAME: every register (scalar as a symbol-macro, banked (#13) as a
macrolet taking an index, plus one symbol-macro per #72 alias) plus every
flag. An operand field name colliding with one of these would be silently
shadowed inside (semantics ...) -- see %CHECK-OPERAND-NAMES. Despite the
name (kept for history), this now covers banked registers and their
aliases too -- a macrolet or alias symbol-macro binding shadows exactly as
silently as a scalar symbol-macrolet one."
  (let ((descriptor (find-machine-descriptor machine-name)))
    (loop for element in (machine-descriptor-elements descriptor)
          when (member (storage-element-kind element) '(:flag :register))
            append (cons (storage-element-name element) (storage-element-names element)))))

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
which is also a register, flag, or register alias on ~S -- (semantics ...) ~
can only see one of them" machine name mode-name n machine)))))

(defun %check-operand-registers! (registers machine name mode-name mode hole-alternatives)
  "Signal a DEFINSTRUCTION-time error naming instruction NAME (on MACHINE) and
addressing mode MODE-NAME for each non-NIL entry of REGISTERS (#143, hole-
aligned, parallel to OPERAND-WIDTHS/OPERAND-NAMES) that names an unknown
storage element, one that isn't a banked :REGISTER, or one declaring no #72
:NAMES to render as an alias -- the whole point of :REGISTER is naming an
aliased bank, so any of these would leave it rendering nothing. Also rejects
a :REGISTER hole that is RELATIVE or SIGNED for any of HOLE-ALTERNATIVES (or,
at an ungoverned hole, MODE itself): a relative hole's value is adjusted to
an absolute target at render time (disassembler.lisp's %OPERAND-RENDER-
VALUES) and a signed hole may decode negative (decoder.lisp's per-hole sign
extension) -- either would corrupt a bank index rather than merely mis-render
one, so this is checked unconditionally, not only when :REGISTER is given."
  (let ((descriptor (find-machine-descriptor machine)))
    (loop for register in registers
          for alts in hole-alternatives
          for i from 0
          when register
            do (let ((element (gethash register (machine-descriptor-table descriptor))))
                 (unless element
                   (error "DEFINSTRUCTION ~S ~S: addressing mode ~S: :REGISTER ~S at operand hole ~D ~
names no storage element on ~S" machine name mode-name register i machine))
                 (unless (eq (storage-element-kind element) :register)
                   (error "DEFINSTRUCTION ~S ~S: addressing mode ~S: :REGISTER ~S at operand hole ~D ~
is not a register on ~S" machine name mode-name register i machine))
                 (unless (storage-element-names element)
                   (error "DEFINSTRUCTION ~S ~S: addressing mode ~S: :REGISTER ~S at operand hole ~D ~
declares no #72 :NAMES -- there is no alias for the disassembler to render" machine name mode-name
                          register i))
                 (when (if alts
                           (some #'mode-descriptor-relativep (mapcar #'find-mode-descriptor alts))
                           (mode-descriptor-relativep mode))
                   (error "DEFINSTRUCTION ~S ~S: addressing mode ~S: operand hole ~D is both RELATIVE ~
and :REGISTER ~S -- a relative hole's value is adjusted to an absolute target at render time, ~
which would corrupt a register index" machine name mode-name i register))
                 (when (if alts
                           (some #'mode-descriptor-signedp (mapcar #'find-mode-descriptor alts))
                           (mode-descriptor-signedp mode))
                   (error "DEFINSTRUCTION ~S ~S: addressing mode ~S: operand hole ~D is both SIGNED ~
and :REGISTER ~S -- a signed hole may decode negative, which is not a valid register index"
                          machine name mode-name i register))))))

(defun %parse-byte-sub-variant-form (form hole-name)
  "Parse one (variant (choice m) (sub s)) form (#126) -- the byte-encoded
counterpart of a word field's (variant (choice m) ...) form
(%PARSE-WORD-VARIANT-FORM, below). HOLE-NAME identifies the carrying operand
hole in diagnostics (its own field name, or a synthetic \"hole N\" for an
unnamed one -- see %CHECK-BYTE-SUB-VARIANTS!). Returns (VALUES mode-name
sub)."
  (destructuring-bind (head selector &rest tail) form
    (unless (eq head 'variant)
      (error "DEFINSTRUCTION: operand ~A: malformed variant form ~S -- expected ~
(variant (choice m) (sub s))" hole-name form))
    (unless (and (consp selector) (eq (first selector) 'choice) (= (length selector) 2))
      (error "DEFINSTRUCTION: operand ~A: variant selector must be (choice m), got ~S"
             hole-name selector))
    (let ((choice-name (second selector)))
      (unless (and (consp (first tail)) (eq (first (first tail)) 'sub) (= (length (first tail)) 2)
                   (null (rest tail)))
        (error "DEFINSTRUCTION: operand ~A: a (choice ~S) variant must be (sub s), got ~S"
               hole-name choice-name tail))
      (values choice-name (second (first tail))))))

(defun %check-byte-sub-variants! (variant-forms hole-alternatives machine name hole-name)
  "Validate VARIANT-FORMS -- the (variant (choice m) (sub s)) forms declared
for one byte-encoded operand hole (#126), HOLE-NAME in diagnostics -- against
HOLE-ALTERNATIVES (this hole's own ONE-OF alternative names, mode.lisp's
%MODE-HOLE-ALTERNATIVES, or NIL for a hole not governed by any ONE-OF). NIL
VARIANT-FORMS (the common case: an operand hole with no sub selector at all)
returns NIL with no checking. Otherwise signals a DEFINSTRUCTION-time error,
naming MACHINE/NAME/HOLE-NAME, if: HOLE-ALTERNATIVES is NIL (a sub selector on
a plain EXPR hole has nothing to select between); any (choice m) names a mode
not among HOLE-ALTERNATIVES, or names one more than once; a sub value is
negative, doesn't fit MACHINE's code cell width, or collides with another
entry's; or some alternative of HOLE-ALTERNATIVES is claimed by no entry at
all -- unlike #118's word-machine mixed-field rule, a byte hole's sub selector
has no value-selected fallback for an unclaimed alternative to resolve into,
so partial coverage is permanently an error here, not merely a NIL CHOICES
result at runtime. On success, returns the parsed ((mode-name . sub) ...)
pairs, in VARIANT-FORMS' own declaration order."
  (when variant-forms
    (unless hole-alternatives
      (error "DEFINSTRUCTION ~S ~S: operand ~A: (variant (choice ...) ...) given for an ~
operand hole that is not a ONE-OF pattern element -- a sub-opcode selector only ~
chooses between ONE-OF alternatives" machine name hole-name))
    (let ((width (%machine-cell-width machine))
          (pairs (mapcar (lambda (f) (multiple-value-bind (m s) (%parse-byte-sub-variant-form f hole-name)
                                        (cons m s)))
                          variant-forms)))
      (dolist (p pairs)
        (unless (member (car p) hole-alternatives)
          (error "DEFINSTRUCTION ~S ~S: operand ~A: (choice ~S) is not one of this hole's ~
ONE-OF alternatives ~S" machine name hole-name (car p) hole-alternatives))
        (when (or (minusp (cdr p)) (>= (cdr p) (ash 1 width)))
          (error "DEFINSTRUCTION ~S ~S: operand ~A: sub-opcode ~D for (choice ~S) does not fit ~
machine ~S's ~D-bit code cell" machine name hole-name (cdr p) (car p) machine width)))
      (let ((dup-mode (loop for (p . later) on pairs when (member (car p) later :key #'car) return (car p))))
        (when dup-mode
          (error "DEFINSTRUCTION ~S ~S: operand ~A: (choice ~S) given more than once"
                 machine name hole-name dup-mode)))
      (let ((dup-sub (loop for (p . later) on pairs when (member (cdr p) later :key #'cdr) return (cdr p))))
        (when dup-sub
          (error "DEFINSTRUCTION ~S ~S: operand ~A: sub-opcode value ~D used by more than one ~
(choice ...) variant" machine name hole-name dup-sub)))
      (let ((missing (set-difference hole-alternatives (mapcar #'car pairs))))
        (when missing
          (error "DEFINSTRUCTION ~S ~S: operand ~A: ONE-OF alternative~P ~S ~:[has~;have~] no ~
(variant (choice ...) (sub ...)) -- every alternative of a sub-selected hole must be claimed"
                 machine name hole-name (length missing) missing (rest missing))))
      pairs)))

(defun %parse-byte-sub-table-variant-form (form)
  "Parse one (variant (choice m1 m2 ...) (sub s)) form declared inside a
(sub-opcode ...) table subclause (#128) -- the multi-hole generalization of
%PARSE-BYTE-SUB-VARIANT-FORM's single-name (choice m) form, used by the
per-hole sugar instead. Returns (VALUES name-list sub), NAME-LIST one
mode-name symbol per participating ONE-OF hole, in the table's own hole
order."
  (destructuring-bind (head selector &rest tail) form
    (unless (eq head 'variant)
      (error "DEFINSTRUCTION: (sub-opcode ...): malformed variant form ~S -- expected ~
(variant (choice m1 m2 ...) (sub s))" form))
    (unless (and (consp selector) (eq (first selector) 'choice) (rest selector))
      (error "DEFINSTRUCTION: (sub-opcode ...): variant selector must be (choice m1 m2 ...), got ~S"
             selector))
    (let ((names (rest selector)))
      (unless (and (consp (first tail)) (eq (first (first tail)) 'sub) (= (length (first tail)) 2)
                   (null (rest tail)))
        (error "DEFINSTRUCTION: (sub-opcode ...): a (choice ~S) variant must be (sub s), got ~S"
               names tail))
      (values names (second (first tail))))))

(defun %parse-byte-sub-table-holes-clause (variant-forms hole-alternatives-list machine name)
  "VARIANT-FORMS is the full body of a (sub-opcode ...) subclause (#128),
optionally led by one (holes h1 h2 ...) form naming, by 0-based
pattern-order index (the same indexing HOLE-ALTERNATIVES-LIST -- mode.lisp's
%MODE-HOLE-ALTERNATIVES -- is aligned against), which of the mode's ONE-OF
holes this table covers (#131). Returns (VALUES hole-indices
remaining-variant-forms): with a leading (holes ...) form, HOLE-INDICES is
its own names sorted into ascending pattern order regardless of how (holes
...) listed them -- so a variant's (choice m1 m2 ...) stays positional in
pattern order unambiguously either way, not in (holes ...)'s own writing
order -- and REMAINING-VARIANT-FORMS has that leading form stripped off;
with no (holes ...) form, HOLE-INDICES is every ONE-OF hole of the mode (in
pattern order, today's original meaning) and REMAINING-VARIANT-FORMS is
VARIANT-FORMS unchanged.

Signals a DEFINSTRUCTION-time error, naming MACHINE/NAME, for a (holes ...)
form that: names no hole at all; repeats an index; names an index out of
range for HOLE-ALTERNATIVES-LIST; or names an index whose hole is not
governed by any ONE-OF -- a sub-opcode table, full or subset, only chooses
between ONE-OF alternatives. A ONE-OF hole left unnamed by (holes ...) is
simply not covered by this table -- it gets no sub-opcode-selector record of
its own (%CHECK-BYTE-ONE-OF-SIGNED/-WIDTH/-RELATIVE, below, still require
such a hole's alternatives to agree with each other, exactly as an
ungoverned or fully-uncovered ONE-OF hole always has)."
  (if (and variant-forms (consp (first variant-forms)) (eq (first (first variant-forms)) 'holes))
      (let ((indices (rest (first variant-forms))))
        (unless indices
          (error "DEFINSTRUCTION ~S ~S: (sub-opcode ...): (holes) names no hole -- give at ~
least one hole index, or omit (holes ...) entirely to cover every ONE-OF hole" machine name))
        (let ((dup (loop for (i . later) on indices when (member i later) return i)))
          (when dup
            (error "DEFINSTRUCTION ~S ~S: (sub-opcode ...): (holes ...) names hole ~D more than once"
                   machine name dup)))
        (dolist (i indices)
          (unless (and (integerp i) (<= 0 i) (< i (length hole-alternatives-list)))
            (error "DEFINSTRUCTION ~S ~S: (sub-opcode ...): (holes ...) names ~S, not a valid ~
hole index for this mode (0-~D)" machine name i (1- (length hole-alternatives-list))))
          (unless (nth i hole-alternatives-list)
            (error "DEFINSTRUCTION ~S ~S: (sub-opcode ...): (holes ...) names hole ~D, which is ~
not a ONE-OF pattern element -- a sub-opcode table only chooses between ONE-OF alternatives"
                   machine name i)))
        (values (sort (copy-list indices) #'<) (rest variant-forms)))
      (values (loop for alts in hole-alternatives-list
                    for i from 0
                    when alts collect i)
              variant-forms)))

(defun %check-byte-sub-table! (variant-forms hole-alternatives-list machine name)
  "Validate VARIANT-FORMS -- an optional leading (holes ...) form (#131)
followed by the (variant (choice m1 m2 ...) (sub s)) forms -- declared by a
(sub-opcode ...) subclause (#128) -- against HOLE-ALTERNATIVES-LIST, the
whole mode's own hole-aligned alternatives (mode.lisp's
%MODE-HOLE-ALTERNATIVES, NIL at a plain EXPR hole). With no (holes ...)
form, every ONE-OF hole of the mode participates, in hole order -- this is
the multi-hole generalization of %CHECK-BYTE-SUB-VARIANTS!'s one-hole
selector, which stays the sugar for the single-hole case. With one, only
its named holes -- %PARSE-BYTE-SUB-TABLE-HOLES-CLAUSE, above -- do; a
ONE-OF hole left out is simply not covered by this table, as if #128 had
never given it a decode-time record at all. (Note: a single multi-hole
ONE-OF pattern element -- one whose alternatives themselves each span more
than one hole -- repeats its own alt-names list once per hole it
contributes, mode.lisp's %PATTERN-HOLE-ALTERNATIVES; HOLE-INDICES then
treats each of those holes as an independently participating hole here, so
the cross product is squared with combinations that can never actually
arise from one shared alternative match. (holes ...) can work around this
by naming only one such hole, but doesn't fix it -- that's #120's
territory, not this one's.)

Signals a DEFINSTRUCTION-time error, naming MACHINE/NAME, if: the mode has
no ONE-OF hole at all; some variant's (choice ...) arity doesn't match the
number of participating holes; a named alternative doesn't belong to its
own hole; a sub value is negative or doesn't fit MACHINE's code cell width;
two entries share a sub value or the same combination of names; the cross
product of every participating hole's alternatives is too large for MACHINE's
code cell width to distinguish; or the cross product isn't claimed exactly
once -- like %CHECK-BYTE-SUB-VARIANTS!, there is no value-selected fallback
for an unclaimed combination to resolve into, so partial coverage is a
permanent error here too.

On success, returns (VALUES hole-indices pairs): HOLE-INDICES the
participating holes in pattern order (every ONE-OF hole, or (holes ...)'s
own subset), PAIRS the ((name-list . sub) ...) entries in VARIANT-FORMS'
own declaration order."
  (multiple-value-bind (hole-indices variant-forms)
      (%parse-byte-sub-table-holes-clause variant-forms hole-alternatives-list machine name)
    (unless hole-indices
      (error "DEFINSTRUCTION ~S ~S: (sub-opcode ...) given but this mode has no ONE-OF ~
operand hole -- a sub-opcode table only chooses between ONE-OF alternatives" machine name))
    (let* ((width (%machine-cell-width machine))
           (n (length hole-indices))
           (alt-lists (mapcar (lambda (i) (nth i hole-alternatives-list)) hole-indices))
           (all-combos (labels ((cross (lists)
                                   (if (null lists)
                                       (list nil)
                                       (loop for a in (first lists)
                                             append (mapcar (lambda (rest) (cons a rest))
                                                             (cross (rest lists)))))))
                         (cross alt-lists)))
           (pairs (mapcar (lambda (f) (multiple-value-bind (names s)
                                          (%parse-byte-sub-table-variant-form f)
                                        (cons names s)))
                           variant-forms)))
      (when (> (length all-combos) (ash 1 width))
        (error "DEFINSTRUCTION ~S ~S: (sub-opcode ...) has ~D combinations across its ~D ~
participating ONE-OF hole~:P -- too many to fit machine ~S's ~D-bit code cell"
               machine name (length all-combos) n machine width))
      (dolist (p pairs)
        (unless (= (length (car p)) n)
          (error "DEFINSTRUCTION ~S ~S: (sub-opcode ...): (choice ~S) names ~D alternative~:P, ~
but this mode has ~D participating ONE-OF hole~:P" machine name (car p) (length (car p)) n))
        (loop for choice-name in (car p)
              for alts in alt-lists
              for i in hole-indices
              unless (member choice-name alts)
                do (error "DEFINSTRUCTION ~S ~S: (sub-opcode ...): (choice ~S) names ~S, not ~
one of operand hole ~D's ONE-OF alternatives ~S" machine name (car p) choice-name i alts))
        (when (or (minusp (cdr p)) (>= (cdr p) (ash 1 width)))
          (error "DEFINSTRUCTION ~S ~S: (sub-opcode ...): sub-opcode ~D for (choice ~S) does not ~
fit machine ~S's ~D-bit code cell" machine name (cdr p) (car p) machine width)))
      (let ((dup-combo (loop for (p . later) on pairs
                              when (member (car p) later :key #'car :test #'equal)
                                return (car p))))
        (when dup-combo
          (error "DEFINSTRUCTION ~S ~S: (sub-opcode ...): (choice ~S) given more than once"
                 machine name dup-combo)))
      (let ((dup-sub (loop for (p . later) on pairs when (member (cdr p) later :key #'cdr) return (cdr p))))
        (when dup-sub
          (error "DEFINSTRUCTION ~S ~S: (sub-opcode ...): sub-opcode value ~D used by more than ~
one (choice ...) variant" machine name dup-sub)))
      (let ((missing (set-difference all-combos (mapcar #'car pairs) :test #'equal)))
        (when missing
          (error "DEFINSTRUCTION ~S ~S: (sub-opcode ...): combination~P ~S ~:[has~;have~] no ~
(variant (choice ...) (sub ...)) -- every combination of a sub-opcode table's ONE-OF holes must ~
be claimed" machine name (length missing) missing (rest missing))))
      (values hole-indices pairs))))

(defun %parse-operand-subclauses (mode subclauses machine name mode-name machine-name
                                   &optional sub-opcode-subclause)
  "SUBCLAUSES is every (operand ...) form declared for one variant of
instruction NAME (on MACHINE) using addressing MODE (named MODE-NAME in
diagnostics), in declaration order. Returns (VALUES widths names sub-spec
mode-specified registers), one WIDTHS/NAMES/MODE-SPECIFIED/REGISTERS entry
per subclause -- their count must equal MODE's EXPR hole count exactly, since
each hole needs somewhere to put its parsed value and each operand subclause
needs a hole to size itself against; mismatch in either direction is an
error. Named fields are also checked for collisions (%CHECK-OPERAND-NAMES).
REGISTERS entries (#143, an (operand ... :register ELEM) subclause) are
validated by %CHECK-OPERAND-REGISTERS!.

MODE-SPECIFIED (#129) is T at hole I when that hole's own (operand ...)
subclause was (operand :mode) rather than an explicit (operand :width n) --
the only place %BYTE-OPERAND-WIDTHS (below) may substitute a disagreeing
ONE-OF alternative's own :WIDTH for WIDTHS' shared entry, since an explicit
:WIDTH is the author naming a width directly and must not be silently
overridden.

SUB-SPEC is NIL when neither any subclause carries a (variant (choice ...)
(sub ...)) selector nor SUB-OPTIONAL-SUBCLAUSE is given -- the common case
-- or (HOLE-INDICES . PAIRS), HOLE-INDICES the carrying hole(s) in pattern
order and PAIRS the ((name-list . sub) ...) entries naming, per hole in
HOLE-INDICES order, which alternative each descriptor was expanded for.
Exactly one hole's own selector normalizes to this shape directly
(%CHECK-BYTE-SUB-VARIANTS!'s pairs, each NAME-LIST a singleton); a
(sub-opcode ...) table (SUB-OPCODE-SUBCLAUSE, #128) produces it directly
via %CHECK-BYTE-SUB-TABLE!, one or more HOLE-INDICES at once. The two
sources are mutually exclusive -- more than one operand hole declaring its
own selector requires the table instead, and a table given together with
any per-hole selector is an error, since both would be writing the same
cell via two different mechanisms."
  (let ((holes (%mode-hole-count mode))
        (n (length subclauses)))
    (unless (= holes n)
      (error "DEFINSTRUCTION ~S ~S: addressing mode ~S has ~D EXPR hole~:P ~
but ~D (operand ...) subclause~:P ~:[were~;was~] given -- one is required ~
per hole" machine name mode-name holes n (= n 1))))
  (let* ((hole-alternatives (%mode-hole-alternatives mode))
         (parsed (loop for subclause in subclauses
                       for alts in hole-alternatives
                       for i from 0
                       collect (multiple-value-bind (op-name spec variant-forms register)
                                   (%parse-byte-operand-subclause subclause)
                                 (list op-name (%operand-width mode spec machine-name)
                                       (%check-byte-sub-variants!
                                        variant-forms alts machine name
                                        (or op-name (format nil "~D" i)))
                                       (eq (first spec) :mode)
                                       register))))
         (widths (mapcar #'second parsed))
         (names (mapcar #'first parsed))
         (mode-specified (mapcar #'fourth parsed))
         (registers (mapcar #'fifth parsed))
         (carrying (loop for p in parsed for i from 0 when (third p) collect (cons i (third p)))))
    (%check-operand-names names machine name mode-name)
    (%check-operand-registers! registers machine name mode-name mode hole-alternatives)
    (when (rest carrying)
      (error "DEFINSTRUCTION ~S ~S: more than one operand hole declares its own sub-opcode ~
selector -- combine them in a (sub-opcode ...) table instead" machine name))
    (when (and carrying sub-opcode-subclause)
      (error "DEFINSTRUCTION ~S ~S: an operand hole's own (variant (choice ...) (sub ...)) ~
selector and a (sub-opcode ...) table may not both be given -- they would write the same cell"
             machine name))
    (values widths names
            (cond
              (sub-opcode-subclause
               (multiple-value-bind (hole-indices pairs)
                   (%check-byte-sub-table! (rest sub-opcode-subclause) hole-alternatives machine name)
                 (cons hole-indices pairs)))
              (carrying
               (destructuring-bind (hole-index . pairs) (first carrying)
                 (cons (list hole-index)
                       (mapcar (lambda (p) (cons (list (car p)) (cdr p))) pairs))))
              (t nil))
            mode-specified
            registers)))

(defun %check-mode-hole-attributes (mode machine name)
  ;; A whole-mode RELATIVE mode (mode.lisp, MODE-DESCRIPTOR-RELATIVEP) marks
  ;; its *entire* pattern's operand as a single PC-relative offset -- there
  ;; is no ONE-OF here naming which hole that is, so MODE's own :RELATIVE
  ;; applies to all of its holes at once, which only has a coherent meaning
  ;; when there is exactly one. A per-hole relative marking on a ONE-OF's
  ;; alternatives (#130) is a different, narrower declaration -- it names
  ;; *its own* hole specifically, checked instead by
  ;; %CHECK-BYTE-ONE-OF-RELATIVE below -- and is unaffected by this
  ;; restriction, which is specific to a whole-mode :RELATIVE. This
  ;; restriction is also specific to RELATIVE's offset computation, not to
  ;; signedness in general -- a plain SIGNED mode (#30) may have any number
  ;; of holes; each is sign-extended independently (emulator.lisp).
  (when (and (mode-descriptor-relativep mode) (> (%mode-hole-count mode) 1))
    (error "DEFINSTRUCTION ~S ~S: addressing mode ~S is :RELATIVE and has ~
more than one EXPR hole -- a RELATIVE mode's offset applies to its whole ~
operand, so a whole-mode :RELATIVE mode may only have one hole (a ONE-OF ~
alternative's own :RELATIVE, naming just its own hole, has no such ~
restriction)"
           machine name (mode-descriptor-name mode)))
  ;; A whole-mode :RELATIVE whose single hole is itself a ONE-OF has no
  ;; coherent meaning either, even though %MODE-HOLE-COUNT is 1: %BYTE-
  ;; RELATIVE-HOLE-INDEX (and %BYTE-OPERAND-SIGNEDNESS before it, #124/#127,
  ;; the same shape) resolves a ONE-OF hole from its own matched alternative
  ;; first, never falling back to MODE's own RELATIVEP/SIGNEDP at all -- so
  ;; MODE's own :RELATIVE T would be silently dropped in favor of whichever
  ;; alternative matched (or agreed on not being relative), rather than
  ;; erroring where the contradiction is written. Rejected here rather than
  ;; left to silently encode as an absolute value.
  (when (and (mode-descriptor-relativep mode)
             (some #'identity (%mode-hole-alternatives mode)))
    (error "DEFINSTRUCTION ~S ~S: addressing mode ~S is :RELATIVE, but its ~
one hole is itself a ONE-OF -- a whole-mode :RELATIVE has no coherent ~
meaning there, since which alternative matched would silently override it; ~
give the ONE-OF's own alternative :RELATIVE T instead (per-hole :RELATIVE, ~
#130)"
           machine name (mode-descriptor-name mode)))
  ;; #132: the same hazard, one hole earlier in the pipeline, for a plain
  ;; whole-mode :SIGNED T (not :RELATIVE, which -- being itself (OR RELATIVE
  ;; SIGNED) -- is already caught by the clause above; the NOT RELATIVEP
  ;; guard here keeps that case reported under its own clearer message
  ;; rather than this one). %BYTE-OPERAND-SIGNEDNESS resolves a ONE-OF
  ;; hole's signedness from its own matched alternative first, never falling
  ;; back to MODE's own SIGNEDP -- so a mode declared (one-of a b) :signed t
  ;; would have that :SIGNED T silently dropped whenever A and B happen to
  ;; agree with each other (the only case with no selector to catch the
  ;; disagreement and error some other way). Rejected here rather than left
  ;; to silently encode as unsigned.
  (when (and (mode-descriptor-signedp mode)
             (not (mode-descriptor-relativep mode))
             (some #'identity (%mode-hole-alternatives mode)))
    (error "DEFINSTRUCTION ~S ~S: addressing mode ~S is :SIGNED, but its ~
one hole is itself a ONE-OF -- a whole-mode :SIGNED has no coherent meaning ~
there, since which alternative matched would silently override it; give the ~
ONE-OF's own alternative :SIGNED T instead (per-hole :SIGNED, #124/#127)"
           machine name (mode-descriptor-name mode))))

(defun %check-no-varying-one-of! (mode machine name)
  "Signal a DEFINSTRUCTION-time error if MODE has a ONE-OF element whose
alternatives disagree on hole count (#120) -- varying hole counts are
supported only on a word-encoded machine's operand path so far
(%WORD-MODE-DESCRIPTOR-FORMS' per-tuple expansion); a byte-encoded machine
reaching here has no such expansion to fall back to, so it rejects a
varying mode outright rather than silently using %MODE-HOLE-COUNT's minimum
and dropping every over-count alternative's extra hole on the floor.
Byte-encoded varying hole counts are #151."
  (when (mode-descriptor-varyingp mode)
    (error "DEFINSTRUCTION ~S ~S: addressing mode ~S has a ONE-OF whose alternatives disagree ~
on hole count -- not yet supported on a byte-encoded machine (#120's initial slice is ~
word-encoded-machine-only; byte-encoded support is #151)"
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

;;; CHOICE-CASE (#73): dispatching (semantics ...) on the ONE-OF alternative
;;; an operand hole actually matched -- #104's (choice MODE) selector steers
;;; a word-encoded field's own code, but every sibling descriptor its
;;; combinations expand into still shares one semantics-fn. The information
;;; needed is already computed at decode time (DECODE-INSTRUCTION-AT's fourth
;;; CHOICES value, decoder.lisp) and at assemble time (MATCH-OPERAND-MODE's
;;; own CHOICES, mode.lisp) -- CHOICE-CASE just gives a semantics body
;;; somewhere to read it.

(defun %choice-case-operand-index (name operand-names &optional mode-operand-names)
  "NAME is a CHOICE-CASE operand argument (unevaluated). Returns (VALUES
index T) when NAME resolves against OPERAND-NAMES -- this specific
descriptor's own hole-aligned operand names -- the symbol OPERAND always
means hole 0 (mirroring DEFINSTRUCTION's own OPERAND alias for the first
field, whether or not that field has a name of its own), same as any
declared field name.

MODE-OPERAND-NAMES (#120) is the union of every sibling alternative-tuple's
own OPERAND-NAMES for this DEFINSTRUCTION mode -- wider than OPERAND-NAMES
only when the mode has a varying ONE-OF element whose over-count
alternatives declare their own extra operand names via (for-choice ...).
When NAME is absent from OPERAND-NAMES but present in MODE-OPERAND-NAMES,
returns (VALUES NIL NIL) instead of erroring -- some sibling tuple has this
operand, this descriptor doesn't, so a CHOICE-CASE clause reading it can
never actually run for this descriptor (its own hole simply isn't there);
%CHOICE-CASE-FORM below then expands that clause to dispatch on NIL, which
can never match a clause key, rather than binding an unbound variable.

Signals a DEFINSTRUCTION-time error when NAME names no operand anywhere on
this mode at all."
  (unless (or operand-names mode-operand-names)
    (error "DEFINSTRUCTION: CHOICE-CASE ~S: this variant has no operand fields" name))
  (let ((index (position name operand-names)))
    (cond
      (index (values index t))
      ((and (eq name 'operand) operand-names) (values 0 t))
      ((or (member name mode-operand-names) (and (eq name 'operand) mode-operand-names))
       (values nil nil))
      (t (error "DEFINSTRUCTION: CHOICE-CASE: no operand field named ~S -- ~
declared fields are ~S" name (or (remove nil mode-operand-names) '(operand)))))))

(defun %check-choice-case-keys! (name keys hole-alternatives)
  "Signal a DEFINSTRUCTION-time error if any of KEYS (one CHOICE-CASE clause's
key -- a single mode-name symbol, or a list of them) names a mode not among
HOLE-ALTERNATIVES -- this operand's own ONE-OF alternatives, in hole order
(mode.lisp's %MODE-HOLE-ALTERNATIVES). HOLE-ALTERNATIVES NIL means this
NAME's hole isn't a ONE-OF at all -- e.g. a shared top-level (semantics ...)
whose other modes never route this field through a ONE-OF -- so the check is
skipped silently rather than erroring: %MATCHED-CHOICE-NAME returns NIL
there, and CHOICE-CASE's own OTHERWISE/NO-MATCHING-CHOICE fallback already
covers what happens at runtime. CL:CASE's own OTHERWISE/T fallback keys are
exempted unconditionally, same rationale."
  (when hole-alternatives
    (dolist (key (if (listp keys) keys (list keys)))
      (unless (member key '(otherwise t))
        (unless (member key hole-alternatives)
          (error "DEFINSTRUCTION: CHOICE-CASE ~S: ~S is not one of this operand's ~
ONE-OF alternatives ~S" name key hole-alternatives))))))

(defun %choice-case-form (name clauses machine-name instruction-name operand-names hole-alternatives-list
                           &optional mode-operand-names)
  "Expansion of one (CHOICE-CASE NAME CLAUSE...) form (see the DEFINSTRUCTION
docstring) inside a (semantics ...) body -- a plain CL:CASE on
%MATCHED-CHOICE-NAME's result, with a NO-MATCHING-CHOICE fallback spliced in
unless CLAUSES already supplies its own OTHERWISE/T clause.

#120: when %CHOICE-CASE-OPERAND-INDEX reports NAME absent from this
descriptor's own OPERAND-NAMES (some sibling tuple has it, this one
doesn't), CHOICE-VAR is bound to a literal NIL rather than reading CHOICES
at some index -- no clause key can ever be NIL (%CHECK-CHOICE-CASE-KEYS! is
skipped too, since there is no HOLE-ALTERNATIVES entry to validate against),
so this always falls to OTHERWISE/NO-MATCHING-CHOICE, unreachable in
practice since the shared (semantics ...) body's own CHOICE-CASE on NAME's
governing hole is what selects which descriptor ran in the first place."
  (multiple-value-bind (index foundp) (%choice-case-operand-index name operand-names mode-operand-names)
    (when foundp
      (let ((hole-alternatives (nth index hole-alternatives-list)))
        (dolist (clause clauses)
          (%check-choice-case-keys! name (first clause) hole-alternatives))))
    (let ((has-fallback (some (lambda (c) (member (first c) '(otherwise t))) clauses))
          (choice-var (gensym "CHOICE")))
      `(let ((,choice-var ,(if foundp `(%matched-choice-name choices ,index) nil)))
         (case ,choice-var
           ,@clauses
           ,@(unless has-fallback
               `((otherwise (error 'no-matching-choice
                                    :machine ',machine-name
                                    :instruction ',instruction-name
                                    :operand ',name
                                    :choice ,choice-var)))))))))

(defun %validate-choice-case-forms! (form machine name operand-names hole-alternatives-list
                                      &optional mode-operand-names)
  "Walk FORM (one top-level element of a (semantics ...) body, or any
sub-form of one) for every literal (CHOICE-CASE ...) sub-form, eagerly
re-running its own validation (%CHOICE-CASE-FORM, discarding the expansion
it builds) right here, at DEFINSTRUCTION's own outer macroexpansion.
CHOICE-CASE's validation also runs again, redundantly, inside its MACROLET
expander when the generated semantics lambda is actually compiled -- but an
error signalled *there* is a nested macroexpansion inside a to-be-compiled
sub-form, which SBCL's compiler absorbs as a diagnostic rather than
propagating as a normal condition, invisible to a caller's HANDLER-CASE or
FIVEAM:SIGNALS. Running the same check here, synchronously in this
function's own call stack, is what makes a bad operand name or clause key an
error EVAL/COMPILE's caller actually sees. A generic car/cdr tree walk (not
just the top level) since CHOICE-CASE may appear nested inside LET/IF/PROGN/
etc., not only as a form's own head -- except a (QUOTE ...) sub-form, which
this does not descend into: quoted data merely containing the symbols
CHOICE-CASE is not a use of the macro and has nothing to validate."
  (when (and (consp form) (not (eq (first form) 'quote)))
    (if (eq (first form) 'choice-case)
        (destructuring-bind (op-name &rest clauses) (rest form)
          (%choice-case-form op-name clauses machine name operand-names hole-alternatives-list mode-operand-names))
        (progn
          (%validate-choice-case-forms! (car form) machine name operand-names hole-alternatives-list
                                         mode-operand-names)
          (%validate-choice-case-forms! (cdr form) machine name operand-names hole-alternatives-list
                                         mode-operand-names)))))

;; Both declarations are load-bearing for the build, not style: without them
;; SBCL narrows this function's return type and re-triggers the fatal
;; WARNING its docstring below describes.
(declaim (ftype (function () t) %absent-choice-operand) (notinline %absent-choice-operand))
(defun %absent-choice-operand ()
  "Always NIL -- used (#120's %SEMANTICS-FN-FORM) to bind an operand name a
sibling alternative-tuple declares but this descriptor doesn't, instead of a
literal NIL: an ordinary function call's return type is opaque to the
compiler, where a literal NIL would let SBCL narrow the binding to type
NULL and then flag a spurious type-conflict WARNING at the dead CHOICE-CASE
branch that reads it in arithmetic (e.g. (+ ... absent-name)) -- a WARNING
ASDF's COMPILE-FILE-ERROR treats as fatal, even though the branch can never
actually run for this descriptor."
  nil)

(defun %semantics-fn-form (semantics-forms machine name operand-names hole-alternatives-list
                            &optional mode-operand-names)
  "MODE-OPERAND-NAMES (#120), when given, is the union of every sibling
alternative-tuple's own OPERAND-NAMES for this DEFINSTRUCTION mode -- wider
than OPERAND-NAMES only when a varying ONE-OF element's over-count
alternatives declare their own extra names. A name in the union but absent
from this descriptor's own OPERAND-NAMES is bound to NIL and declared
IGNORABLE, not left unbound -- a shared (semantics ...) body naming it is
only ever read inside the CHOICE-CASE branch that selects the sibling tuple
which actually has it, unreachable for this descriptor (see
%CHOICE-CASE-FORM). A name in neither list stays an unbound-variable
compile error, preserving typo protection."
  (dolist (form semantics-forms)
    (%validate-choice-case-forms! form machine name operand-names hole-alternatives-list mode-operand-names))
  (let* ((own-names (remove nil operand-names))
         (absent-names (set-difference (remove nil mode-operand-names) own-names))
         (named-bindings (loop for op-name in operand-names
                                for i from 0
                                when op-name
                                  collect `(,op-name (nth ,i operands))))
         (absent-bindings (mapcar (lambda (n) `(,n (%absent-choice-operand))) absent-names)))
    `(lambda (machine operands choices)
       (declare (ignorable operands choices))
       (with-machine-bindings (machine ,machine)
         (let ((operand (first operands))
               ,@named-bindings
               ,@absent-bindings)
           (declare (ignorable operand ,@own-names ,@absent-names))
           (macrolet ((choice-case (choice-name &body clauses)
                        (%choice-case-form choice-name clauses ',machine ',name
                                            ',operand-names ',hole-alternatives-list ',mode-operand-names)))
             ,@semantics-forms))))))

(defun %descriptor-form (machine name mode-form opcode operand-widths operand-names cycles semantics-fn-form
                          &optional sub-opcode sub-choices operand-signedness
                            relative-hole-index word-layout-name word-constants-form operand-registers)
  "SEMANTICS-FN-FORM is an already-built %SEMANTICS-FN-FORM lambda form, or a
gensym bound to one by the caller's own LET* (#150) -- built once and shared
across every sibling descriptor whose SEMANTICS-FN-FORM inputs (SEMANTICS-
FORMS/OPERAND-NAMES/HOLE-ALTERNATIVES-LIST) are the same, rather than
rebuilt (and so re-emitted as compiled code) once per sibling; see
%BYTE-DESCRIPTOR-FORMS. WORD-LAYOUT-NAME/WORD-CONSTANTS-FORM (#136) are only
ever non-NIL from the no-mode (encoding ...) DEFINSTRUCTION path -- a
no-operand, word-encoded instruction (CLS/RET-shaped) that pins one or more
fields via (field-value ...); every other caller of this function is
byte-encoded and leaves both at their NIL default. WORD-CONSTANTS-FORM is an
already-quoted %WORD-CONSTANTS-FORM builder form, not a bare list, mirroring
how %WORD-DESCRIPTOR-FORM splices its own WORD-FIELDS form in unquoted.
OPERAND-REGISTERS (#143) is shared across every sibling descriptor exactly
like OPERAND-NAMES -- which hole indexes which register doesn't vary by
SUB-CHOICES or field-variant combo."
  `(make-instruction-descriptor
    :name ,(string-upcase (symbol-name name))
    :machine ',machine
    :mode ,mode-form
    :opcode ,opcode
    :sub-opcode ,sub-opcode
    :sub-choices ',sub-choices
    :operand-widths ',operand-widths
    :operand-names ',operand-names
    :operand-registers ',operand-registers
    :operand-signedness ',operand-signedness
    :relative-hole-index ',relative-hole-index
    :word-layout-name ',word-layout-name
    :word-constants ,word-constants-form
    :cycles ,cycles
    :semantics-fn ,semantics-fn-form))

(defun %byte-operand-signedness (mode hole-alternatives-list sub-choices n)
  "Hole-aligned list of N booleans -- this descriptor's own per-hole
signedness (#124/#127's byte half), computed once per expanded descriptor
since SUB-CHOICES (and so which ONE-OF alternative a hole selected) can
differ between sibling descriptors sharing one carrying hole. Entry I is T
when hole I's operand is a signed quantity: the alternative SUB-CHOICES
names for that hole, when non-NIL (%CHECK-ONE-OF-SIGNED, below, guarantees a
descriptor's own SUB-CHOICES entry is populated at the one hole, if any,
whose alternatives disagree on signedness); else -- an ungoverned hole, or a
ONE-OF hole whose alternatives all agree, %CHECK-ONE-OF-SIGNED's other
branch -- MODE's own SIGNEDP for an ungoverned hole, or, for an agreeing
ONE-OF hole, MODE's own SIGNEDP together with the alternatives' shared one
(a ONE-OF pattern element contributes no operand syntax of MODE's own, so
MODE itself never declares :SIGNED at a ONE-OF hole; the alternatives'
common value is what matters there)."
  (loop for i below n
        for alts = (nth i hole-alternatives-list)
        for chosen = (nth i sub-choices)
        collect (cond
                  (chosen (mode-descriptor-signedp (find-mode-descriptor chosen)))
                  (alts (mode-descriptor-signedp (find-mode-descriptor (first alts))))
                  (t (mode-descriptor-signedp mode)))))

(defun %byte-relative-hole-index (mode hole-alternatives-list sub-choices n)
  "This descriptor's own RELATIVE-HOLE-INDEX (#130) -- the 0-based index of
the one hole (of N) whose operand is a PC-relative offset, or NIL if none
is. Built on %BYTE-RELATIVE-FLAGS (below), the same per-hole resolution
%BYTE-OPERAND-SIGNEDNESS uses, projected to a single index since :RELATIVE
is positional rather than a per-hole flag. %CHECK-RELATIVE-MODE-HOLES and
%CHECK-BYTE-ONE-OF-RELATIVE (below) together guarantee at most one flag is
ever T here -- across both a whole-mode :RELATIVE (always hole 0, since a
relative MODE may only have one hole) and a per-hole ONE-OF :RELATIVE
alternative -- so POSITION's first match is the only one there could be."
  (position t (%byte-relative-flags mode hole-alternatives-list sub-choices n)))

(defun %byte-relative-flags (mode hole-alternatives-list sub-choices n)
  "Hole-aligned list of N booleans -- entry I is T when hole I resolves to a
:RELATIVE operand for this specific SUB-CHOICES combination (one expanded
sibling descriptor). Same per-hole CHOSEN/ALTS/ungoverned resolution as
%BYTE-OPERAND-SIGNEDNESS and %BYTE-RELATIVE-HOLE-INDEX, kept as its own
function (rather than folding straight into a POSITION call) so
%CHECK-BYTE-ONE-OF-RELATIVE can COUNT how many holes of *this one sibling*
resolve T -- %BYTE-RELATIVE-HOLE-INDEX itself only reports the first, which
would silently swallow a second one instead of erroring."
  (loop for i below n
        for alts = (nth i hole-alternatives-list)
        for chosen = (nth i sub-choices)
        collect (cond
                  (chosen (mode-descriptor-relativep (find-mode-descriptor chosen)))
                  (alts (mode-descriptor-relativep (find-mode-descriptor (first alts))))
                  (t (mode-descriptor-relativep mode)))))

(defun %byte-operand-widths (hole-alternatives-list sub-choices declared-widths mode-specified)
  "Hole-aligned list, one entry per DECLARED-WIDTHS -- this descriptor's own
per-hole width (#129), computed once per expanded descriptor since
SUB-CHOICES can differ between sibling descriptors sharing one carrying
hole, mirroring %BYTE-OPERAND-SIGNEDNESS. Substitution only happens at a
hole whose MODE-SPECIFIED entry is T, i.e. whose own (operand ...) subclause
was (operand :mode) rather than an explicit (operand :width n)
(%PARSE-OPERAND-SUBCLAUSES) -- an explicit :WIDTH is the author naming a
width directly, and always wins over a matched alternative's own :WIDTH, so
a hole given one needs no per-hole record at all regardless of whether its
alternatives agree (%CHECK-BYTE-ONE-OF-WIDTH). At a MODE-SPECIFIED hole,
entry I is the SUB-CHOICES-named alternative's own MODE-DESCRIPTOR-WIDTH
when that alternative declares one, else -- an ungoverned hole, a ONE-OF
hole whose alternatives all agree, or a chosen alternative that itself
declares no :WIDTH -- DECLARED-WIDTHS' own entry, whatever %OPERAND-WIDTH
already resolved (the mode's default width, or -- for an agreeing ONE-OF
hole sharing a common :WIDTH -- that shared value)."
  (loop for i below (length declared-widths)
        for alts = (nth i hole-alternatives-list)
        for chosen = (nth i sub-choices)
        for declared = (nth i declared-widths)
        for specified = (nth i mode-specified)
        collect (if specified
                    (or (and chosen (mode-descriptor-width (find-mode-descriptor chosen)))
                        (and alts (mode-descriptor-width (find-mode-descriptor (first alts))))
                        declared)
                    declared)))

(defun %resolve-operand-fields (mode operand-subclauses machine name mode-name machine-name
                                 &optional sub-opcode-subclause)
  "Resolve the (operand ...) subclauses (zero or more whole forms, in
declaration order) given for one addressing-mode use into (VALUES widths
names sub-spec mode-specified registers), one WIDTHS/NAMES/MODE-SPECIFIED/
REGISTERS entry per MODE hole. With no subclauses at all, MODE must have
exactly one hole (a bare width can't be inferred for more) -- its default
width (%MODE-OPERAND-WIDTH) is used, unnamed, SUB-SPEC is NIL, MODE-SPECIFIED
is (T) (the default width traces back to :MODE, not an explicit :WIDTH), and
REGISTERS is (NIL) (#143, no subclause means no :REGISTER either), unless
SUB-OPCODE-SUBCLAUSE was given, which is an error -- a defaulted single-hole
operand has no (operand ...) subclause to attach a per-hole selector to, and
a (sub-opcode ...) table has nothing to name without explicit per-hole
subclauses either; the same absence of a subclause means a width-disagreeing
hole can never reach this branch (#129) -- one can only exist under an
explicit (operand ...) subclause carrying a selector. With one or more
subclauses, their count must match MODE's hole count exactly, and SUB-SPEC
(#126/#128), MODE-SPECIFIED (#129), and REGISTERS (#143) are whatever
%PARSE-OPERAND-SUBCLAUSES resolved."
  (if operand-subclauses
      (%parse-operand-subclauses mode operand-subclauses machine name mode-name machine-name
                                  sub-opcode-subclause)
      (if (= (%mode-hole-count mode) 1)
          (progn
            (when sub-opcode-subclause
              (error "DEFINSTRUCTION ~S ~S: (sub-opcode ...) given but addressing mode ~S has no ~
(operand ...) subclauses -- a defaulted single-hole operand has no room to declare one"
                     machine name mode-name))
            (values (list (%mode-operand-width mode machine-name)) (list nil) nil (list t) (list nil)))
          (error "DEFINSTRUCTION ~S ~S: addressing mode ~S has ~D EXPR holes ~
-- an (operand ...) subclause is required per hole" machine name mode-name
                 (%mode-hole-count mode)))))

(defun %check-byte-sub-conflict! (machine name explicit-sub sub-spec)
  "Signal a DEFINSTRUCTION-time error if EXPLICIT-SUB (an (opcode n :sub s)
subclause's own SUB, #125) and SUB-SPEC (a hole-selected sub-opcode
selector, #126, from %RESOLVE-OPERAND-FIELDS/%PARSE-OPERAND-SUBCLAUSES) are
both non-NIL -- the sub-opcode cell is one cell, so an explicit value and a
hole-selected one would both be trying to write it."
  (when (and explicit-sub sub-spec)
    (error "DEFINSTRUCTION ~S ~S: an explicit (opcode n :sub s) and a hole-selected ~
(variant (choice ...) (sub ...)) may not both be given -- they would write the same cell"
           machine name)))

(defun %byte-descriptor-forms (machine name mode-form opcode explicit-sub operand-widths operand-names cycles
                                semantics-forms hole-alternatives-list sub-spec mode mode-specified
                                &optional operand-registers)
  "One INSTRUCTION-DESCRIPTOR form per byte-encoded addressing-mode use for
one (MODES ...) variant or single-mode (ENCODING ...) clause -- a single one
when SUB-SPEC is NIL (the ordinary case, sharing EXPLICIT-SUB, #125's plain
(opcode n :sub s) or NIL), or one per claimed combination SUB-SPEC's own
HOLE-INDICES/PAIRS name (#126's single carrying hole, or #128's several)
-- mirroring %WORD-MODE-DESCRIPTOR-FORMS' one-descriptor-per-combo shape
for the word-encoded path, generalized here from \"one per field-variant
combination\" to \"one per matched-alternative-tuple pair\". Every expanded
descriptor shares OPCODE, OPERAND-NAMES, and SEMANTICS-FORMS -- SUB-OPCODE,
SUB-CHOICES, OPERAND-SIGNEDNESS (#124/#127, %BYTE-OPERAND-SIGNEDNESS),
OPERAND-WIDTHS (#129, %BYTE-OPERAND-WIDTHS), and RELATIVE-HOLE-INDEX (#130,
%BYTE-RELATIVE-HOLE-INDEX) all differ, computed fresh per expanded
descriptor since SUB-CHOICES itself does. MODE (the MODE-DESCRIPTOR
MODE-FORM names, already resolved by both call sites) is needed only for
OPERAND-SIGNEDNESS's and RELATIVE-HOLE-INDEX's own MODE-DESCRIPTOR-SIGNEDP/
-RELATIVEP reads; OPERAND-WIDTHS itself is the shared, declared widths list
(whatever %OPERAND-WIDTH resolved per hole from its own (operand ...)
subclause) that %BYTE-OPERAND-WIDTHS falls back to at a hole whose
alternatives don't override it. MODE-SPECIFIED (#129,
%PARSE-OPERAND-SUBCLAUSES/%RESOLVE-OPERAND-FIELDS) is the hole-aligned gate
%BYTE-OPERAND-WIDTHS needs to know where such an override is allowed.

Returns (VALUES BINDINGS FORMS) (#150) -- BINDINGS is a LET* binding list
the caller must wrap FORMS in. SEMANTICS-FORMS' own %SEMANTICS-FN-FORM
expansion is identical for every sibling here (its inputs -- SEMANTICS-
FORMS/OPERAND-NAMES/HOLE-ALTERNATIVES-LIST -- don't vary by SUB-SPEC pair),
so it is built once, bound to one gensym in BINDINGS, and referenced from
every sibling descriptor's :SEMANTICS-FN slot instead of re-emitted as
compiled code once per sibling."
  (%check-byte-sub-conflict! machine name explicit-sub sub-spec)
  (let ((n (length operand-widths))
        (semantics-fn-gensym (gensym "SEMANTICS-FN")))
    (values
     (list (list semantics-fn-gensym
                 (%semantics-fn-form semantics-forms machine name operand-names hole-alternatives-list)))
     (if (null sub-spec)
         (list (%descriptor-form machine name mode-form opcode
                                  (%byte-operand-widths hole-alternatives-list nil operand-widths mode-specified)
                                  operand-names cycles
                                  semantics-fn-gensym explicit-sub nil
                                  (%byte-operand-signedness mode hole-alternatives-list nil n)
                                  (%byte-relative-hole-index mode hole-alternatives-list nil n)
                                  nil nil operand-registers))
         (destructuring-bind (hole-indices . pairs) sub-spec
           (mapcar (lambda (pair)
                     (let ((sub-choices (make-list n :initial-element nil)))
                       (loop for idx in hole-indices
                             for chosen-name in (car pair)
                             do (setf (nth idx sub-choices) chosen-name))
                       (%descriptor-form machine name mode-form opcode
                                          (%byte-operand-widths hole-alternatives-list sub-choices operand-widths
                                                                 mode-specified)
                                          operand-names cycles
                                          semantics-fn-gensym (cdr pair) sub-choices
                                          (%byte-operand-signedness mode hole-alternatives-list sub-choices n)
                                          (%byte-relative-hole-index mode hole-alternatives-list sub-choices n)
                                          nil nil operand-registers)))
                   pairs))))))

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
  ;; #120: :TRAILING-WORD is a fieldless hole's own single, always-matching
  ;; variant -- see WORD-OPERAND-SPEC below. Distinct from :EXTRA-WORD, whose
  ;; own field bits carry an escape marker that must match a fetched value
  ;; before its trailing word is read; a :TRAILING-WORD hole has no field bits
  ;; of its own to match at all, so it is unconditional wherever it appears.
  (kind nil :type (member :inline :extra-word :trailing-word))
  (bias 0 :type integer)                 ; :inline only
  (range nil :type (or null cons))       ; :inline only, pre-bias (lo . hi)
  (escape nil :type (or null integer))   ; :extra-word only
  ;; #135: :extra-word only -- the trailing word's own width in cells. Parsed
  ;; raw (NIL when the (extra-word ...) form gave no :CELLS) and defaulted to
  ;; the layout's own WIDTH-CELLS once %PARSE-WORD-OPERAND-SUBCLAUSE has a
  ;; LAYOUT to default against, so every downstream reader (word-field-choice,
  ;; below) always sees a concrete positive integer. #120: :TRAILING-WORD's
  ;; own width in cells, same defaulting.
  (extra-cells nil :type (or null (integer 1)))
  ;; #104: non-NIL for a (CHOICE M) selector -- the ONE-OF alternative
  ;; mode-name symbol M that must be this hole's matched alternative
  ;; (mode.lisp's hole-aligned CHOICES) for this variant to apply, rather
  ;; than the operand's own folded VALUE choosing between a (RANGE LO HI)
  ;; variant and an :ELSE one. #118: a field may mix CHOICE-selected
  ;; variants with value-selected (RANGE/:ELSE) ones -- when it does,
  ;; %CHECK-WORD-VARIANT-CHOICES! stamps this slot on every value-selected
  ;; variant too, with the one ONE-OF alternative no (CHOICE ...) variant
  ;; already claims, so a value-selected variant on a mixed field is no
  ;; longer NIL here by the time %EXPAND-WORD-COMBOS/%WORD-FIELD-CHOICE-FORM
  ;; (below) see it. A field with no CHOICE variant at all is left alone --
  ;; every variant there stays NIL, exactly as before #118.
  (choice nil :type (or null symbol))
  ;; #187: T on a CHOICE-selected :extra-word variant that is a second
  ;; spelling of another variant's escape. Never matched at decode.
  (alias nil :type boolean))

(defstruct word-operand-spec
  (name nil)                  ; operand field name, or NIL for unnamed
  ;; #120: FIELD/WIDTH/SHIFT are all NIL for a :TRAILING-WORD spec -- a
  ;; fieldless hole with no bits of its own in the instruction word, only a
  ;; single :TRAILING-WORD VARIANTS entry. Every other spec is field-bearing,
  ;; as before.
  (field nil :type (or null symbol))    ; instruction-word field name
  (width nil :type (or null (integer 1)))
  (shift nil :type (or null (integer 0)))
  (variants nil :type list)   ; list of WORD-VARIANT, declaration order
  ;; #143: this hole's (operand ... :register ELEM) storage-element name, or
  ;; NIL -- carried through to INSTRUCTION-DESCRIPTOR-OPERAND-REGISTERS the
  ;; same way NAME above becomes OPERAND-NAMES.
  (register nil :type (or null symbol)))

;; #136 (M4): a (field-value FIELD-NAME n) encoding subclause -- a field
;; pinned to a literal value with no operand hole at all, discriminating
;; opcode families that share their opcode field (CHIP8's 8XY0-8XYE,
;; 5XY0/9XY0, EX9E/EXA1, FX__, 00E0/00EE). WIDTH/SHIFT locate its bits the
;; same as a WORD-OPERAND-SPEC's; VALUE is the raw (unsigned, already
;; range-checked against WIDTH) bit pattern ENCODE-INSTRUCTION ORs in and
;; DECODE-INSTRUCTION-AT requires an exact match on. Kept as its own list on
;; INSTRUCTION-DESCRIPTOR rather than folded into WORD-ALTERNATIVES/
;; WORD-FIELDS -- those are hole-aligned with OPERAND-NAMES and the decoded
;; VALUES list, and a constant consumes no operand value at all.
(defstruct word-constant
  (name nil :type symbol)     ; instruction-word field name
  (width nil :type (integer 1))
  (shift nil :type (integer 0))
  (value nil :type (integer 0)))

;; One operand's *chosen* (or, in an INSTRUCTION-DESCRIPTOR's WORD-ALTERNATIVES,
;; one *candidate*) field encoding -- WIDTH/SHIFT locate its bits in the
;; instruction word; KIND says whether VALUE packs in biased by BIAS or is
;; replaced by ESCAPE with VALUE following in its own word. RANGE (pre-bias)
;; is kept alongside BIAS so decode (emulator.lisp) can test a fetched raw
;; field value for membership without redoing DEFINSTRUCTION-time arithmetic.
(defstruct word-field-choice
  ;; #120: WIDTH/SHIFT are NIL for a :TRAILING-WORD choice -- a fieldless
  ;; hole has no bits of its own to locate.
  (width nil :type (or null (integer 1)))
  (shift nil :type (or null (integer 0)))
  (kind nil :type (member :inline :extra-word :trailing-word))
  (bias 0 :type integer)
  (range nil :type (or null cons))
  (escape nil :type (or null integer))
  ;; #135: :EXTRA-WORD only -- mirrors WORD-VARIANT-EXTRA-CELLS, already
  ;; resolved to a concrete positive integer by DEFINSTRUCTION time. The
  ;; trailing word's own width in cells, read by ENCODE-INSTRUCTION,
  ;; %TRY-DECODE-WORD-CANDIDATE (decoder.lisp), and the assembler's own fit
  ;; checks (%WORD-VARIANT-FITS-P, %WORD-RELATIVE-OFFSET-FITS-P) instead of
  ;; always the instruction word's own WIDTH-CELLS.
  (extra-cells nil :type (or null (integer 1)))
  ;; #104: mirrors WORD-VARIANT-CHOICE -- non-NIL only for a variant
  ;; selected by matched ONE-OF alternative rather than by value. Carried
  ;; through to every descriptor's WORD-FIELDS/WORD-ALTERNATIVES so
  ;; %CHOOSE-VARIANT (assembler.lisp) can filter combos by the operand's
  ;; actually-matched alternative, and so DECODE-INSTRUCTION-AT's matched
  ;; choice (decoder.lisp) gives the disassembler (disassembler.lisp, #117)
  ;; a record of which alternative was really encoded, instead of always
  ;; rendering a ONE-OF's first alternative.
  (choice nil :type (or null symbol))
  ;; #127 (M4): T when this field's operand is a signed quantity --
  ;; stamped, at DEFINSTRUCTION time (%WORD-FIELD-CHOICE-FORM), from CHOICE's
  ;; own MODE-DESCRIPTOR-SIGNEDP when CHOICE is non-NIL, else NIL. Scoped to
  ;; CHOICE-selected fields only, matching #127's own design: a per-hole
  ;; :SIGNED needs the same decode-time discriminator per-hole :SIGNED needs
  ;; on the byte path (SUB-CHOICES, #124) -- a value-selected field (CHOICE
  ;; NIL) has no ONE-OF alternative of its own to read :SIGNED off in the
  ;; first place. %WORD-CHOICE-MATCHES-P, %TRY-DECODE-WORD-CANDIDATE
  ;; (decoder.lisp), and %WORD-FIELD-CHOICE-VALUES all reinterpret a signed
  ;; field's raw bits as two's-complement before comparing against its
  ;; (biased) RANGE.
  (signedp nil :type boolean)
  ;; #187: mirrors WORD-VARIANT-ALIAS.
  (alias nil :type boolean))

(defun %word-choice-matches-p (raw-value choice)
  "T if RAW-VALUE -- a field's bits as actually fetched or, at
DEFINSTRUCTION time (#105's %CHECK-OPCODE-DECODABLE!), enumerated -- is what
CHOICE (a WORD-FIELD-CHOICE) would encode: its exact ESCAPE for an
:EXTRA-WORD choice, or a value in its (biased) RANGE for an :INLINE one --
reinterpreted as two's-complement over CHOICE's own WIDTH first when
CHOICE-SIGNEDP (#127), the exact inverse of how a signed field's value is
encoded (WRAP-VALUE of a biased, possibly negative value -- ENCODE-
INSTRUCTION, below): without this, a negative-range signed field could never
match its own encoding, since a wrapped negative value's raw bits, read
unsigned, fall outside its biased RANGE entirely. Only the :INLINE branch
reinterprets -- an :EXTRA-WORD choice's ESCAPE is a fixed marker bit pattern
in the same small field, compared as unsigned regardless of SIGNEDP; the
signed reinterpretation of the *value itself* on that path happens once the
following word is fetched (%TRY-DECODE-WORD-CANDIDATE, decoder.lisp), not
here. Lives here, not in decoder.lisp (which loads after this file), so
REGISTER-INSTRUCTION-VARIANTS! can call it too; DECODE-INSTRUCTION-AT
(decoder.lisp) still uses it for its own, original purpose."
  (ecase (word-field-choice-kind choice)
    (:extra-word (and (not (word-field-choice-alias choice))
                      (= raw-value (word-field-choice-escape choice))))
    ;; #120: a :TRAILING-WORD choice has no field bits of its own to test --
    ;; it always matches wherever it appears. Only reachable defensively;
    ;; decoder.lisp's %TRY-DECODE-WORD-CANDIDATE never calls this for a
    ;; :TRAILING-WORD hole at all, since there is no RAW-VALUE to compute.
    (:trailing-word t)
    (:inline (let ((raw-value (if (word-field-choice-signedp choice)
                                   (signed-value raw-value (word-field-choice-width choice))
                                   raw-value)))
               (destructuring-bind (lo . hi) (word-field-choice-range choice)
                 (<= (+ lo (word-field-choice-bias choice)) raw-value (+ hi (word-field-choice-bias choice))))))))

(defun %matched-choice-name (choices index)
  "The mode-name symbol INDEX's hole actually matched, from CHOICES (a
positional, hole-aligned list) -- or NIL if CHOICES is too short, INDEX's
entry is NIL, or it names a value-selected field (no CHOICE of its own).
Normalizes the two shapes CHOICES arrives in: a WORD-FIELD-CHOICE
(DECODE-INSTRUCTION-AT/EXECUTE-INSTRUCTION, decoder.lisp) or a MODE-DESCRIPTOR
(MATCH-OPERAND-MODE, mode.lisp -- the same shape %WORD-CHOICES-ELIGIBLE-P
already reads at assemble time, assembler.lisp); a bare symbol or NIL passes
through unchanged, for a caller that already extracted a mode name itself."
  (let ((entry (nth index choices)))
    (etypecase entry
      (null nil)
      (symbol entry)
      (word-field-choice (word-field-choice-choice entry))
      (mode-descriptor (mode-descriptor-name entry)))))

(defun %word-machine-p (machine-name)
  "T if MACHINE-NAME's DEFMACHINE declared an (instruction-word ...) clause
(machine.lisp, #20) -- DEFINSTRUCTION branches on this to pick the
word-field/variant encoding path below instead of the byte-encoded
(operand :mode)/(operand :width n) one."
  (and (machine-descriptor-instruction-word (find-machine-descriptor machine-name)) t))

(defun %sibling-combos-p (a b)
  "T if descriptors A and B are sibling combos %EXPAND-WORD-COMBOS (below)
expanded from *one* DEFINSTRUCTION mode clause: the same mnemonic (siblings
are always produced together, for one mnemonic, by one call) whose
WORD-ALTERNATIVES are EQUALP. Both conditions matter -- same mnemonic alone
doesn't imply compatibility (two distinct (MODES ...) clauses of one
mnemonic could, in principle, land on byte-identical field encodings without
being the combo expansion's own siblings), and same WORD-ALTERNATIVES alone
doesn't either: two *different* mnemonics can coincidentally declare
identical field ranges, and unlike true siblings they carry different
SEMANTICS-FN, so decode picking whichever one happens to come first would
silently run the wrong effect -- exactly the ambiguity #105's
%CHECK-OPCODE-DECODABLE! exists to catch, not wave through. True siblings
always decode compatibly (REGISTER-INSTRUCTION-VARIANTS!'s long-standing
guarantee, predating #105) -- %CHECK-OPCODE-DECODABLE! skips checking them
against each other, since there is nothing to check. An all-NIL
WORD-ALTERNATIVES (a no-operand mode) EQUALP-compares equal to itself, which
is correct when the mnemonic also matches: two sibling no-operand combos
(possible only via a value-selected field with no operand at all, which does
not occur today, but nothing rules it out) are exactly as decode-compatible
as any other sibling pair."
  (and (string= (instruction-descriptor-name a) (instruction-descriptor-name b))
       (equalp (instruction-descriptor-word-alternatives a) (instruction-descriptor-word-alternatives b))))

(defun %word-field-choice-values (choice)
  "Every raw field value CHOICE (a WORD-FIELD-CHOICE) accepts: its ESCAPE
alone for an :EXTRA-WORD choice, or the whole (biased) RANGE, inclusive, for
an :INLINE one -- each already WRAP-VALUEd to CHOICE-SIGNEDP's own field
width when SIGNEDP (#127), i.e. the actual raw bit pattern decode would fetch
for that value, not the value itself; %WORD-CHOICE-MATCHES-P (the only
caller of these, via %HOLE-DISJOINT-P below) expects raw values and does its
own signed reinterpretation from there, so a mismatch here would silently
compare the wrong value set. Used at DEFINSTRUCTION time by
%CHECK-OPCODE-DECODABLE! to test two co-tenant descriptors' field menus for
disjointness -- field widths in practice are small (a handful of bits), so
enumerating is simpler than range algebra over RANGE/BIAS/ESCAPE together,
and cheap: called only when two descriptors are about to share an opcode,
not on any hot path."
  (ecase (word-field-choice-kind choice)
    (:extra-word (list (word-field-choice-escape choice)))
    ;; #120: never actually called -- %WORD-BIT-CONSTRAINTS excludes every
    ;; :TRAILING-WORD hole before mapping this, since it has no bits of its
    ;; own to constrain (the governing field it's paired with already
    ;; disambiguates its descriptor). Kept for ECASE completeness only.
    (:trailing-word nil)
    (:inline (destructuring-bind (lo . hi) (word-field-choice-range choice)
               (loop for v from (+ lo (word-field-choice-bias choice))
                       to (+ hi (word-field-choice-bias choice))
                     collect (if (word-field-choice-signedp choice)
                                 (wrap-value v (word-field-choice-width choice))
                                 v))))))

(defun %word-bit-constraint (choices)
  "One hole's (WIDTH SHIFT VALUES) constraint -- CHOICES is a descriptor's
single hole (a WORD-FIELD-CHOICE list, every entry sharing that hole's own
WIDTH/SHIFT), VALUES the raw bit patterns any of its variants match. Used
only to build %WORD-BIT-CONSTRAINTS below."
  (list (word-field-choice-width (first choices))
        (word-field-choice-shift (first choices))
        (mapcan #'%word-field-choice-values choices)))

(defun %word-bit-constraints (descriptor)
  "Every bit-level constraint DESCRIPTOR's encoding imposes -- one
(WIDTH SHIFT VALUES) per operand hole (WORD-ALTERNATIVES) plus one per
(field-value ...) pin (WORD-CONSTANTS, #136, VALUES a singleton) -- as one
uniform list. #140: a pin and a hole are the same kind of thing to
%DESCRIPTORS-DISTINGUISHABLE-P below -- both simply constrain some range of
the instruction word's bits to a set of raw values -- so co-tenancy no
longer needs a separate hole-vs-hole and pin-vs-pin check. #120: a
:TRAILING-WORD hole (a fieldless extra hole a varying ONE-OF alternative
contributes) is excluded -- it has no bits of its own in the instruction
word to constrain; the governing field it's paired with is what
%CHECK-OPCODE-DECODABLE! actually needs disjoint, and per-tuple variant
filtering (%WORD-MODE-DESCRIPTOR-FORMS) is what makes that field's own
values disjoint across tuples."
  (append (mapcar #'%word-bit-constraint
                   (remove-if (lambda (choices) (eq (word-field-choice-kind (first choices)) :trailing-word))
                              (instruction-descriptor-word-alternatives descriptor)))
          (mapcar (lambda (c) (list (word-constant-width c) (word-constant-shift c)
                                     (list (word-constant-value c))))
                  (instruction-descriptor-word-constants descriptor))))

(defun %bit-constraints-disjoint-p (constraint-a constraint-b)
  "T if CONSTRAINT-A and CONSTRAINT-B (two %WORD-BIT-CONSTRAINTS entries, each
a possibly *different* WIDTH/SHIFT) share some bits of the instruction word
and, projected onto only that shared range, accept disjoint raw value sets --
NIL if they don't overlap in bits at all, the same non-distinguishing
treatment a field only one side mentions has always had (#137).

#140: this projection is the generalization that lets two fields of
*different* width/shift still tell their descriptors apart, not just an
exact width/shift match (the only case #137's positional-pairing fix
handled) -- e.g. a 4-bit field at shift 8 and two 2-bit fields at shifts 10
and 8 share bits [10, 12), and are distinguishable exactly when their values,
narrowed to those two bits, are disjoint. An exact-match pair (today's only
case before this ticket) projects onto its own whole width on both sides,
so this is a strict generalization of the old %HOLE-DISJOINT-P/
%CONSTANT-DISTINGUISHES-P answer, not a different one.

Each raw VALUE is already the field's own bit pattern, right-aligned at 0
regardless of the field's SHIFT in the word (%WORD-FIELD-CHOICE-VALUES,
WORD-CONSTANT-VALUE) -- LDB against (- OVERLAP-SHIFT SHIFT) re-aligns it to
the shared range's own low bit before comparing.

TODO: O(|VALUES-A|*|VALUES-B|) via MEMBER over two enumerated lists -- fine
at the field widths every current machine uses (a handful of bits), same
ceiling %WORD-FIELD-CHOICE-VALUES has always had (#105), but a wide field on
a wide word would want range algebra over the projected intervals instead of
enumeration."
  (destructuring-bind (width-a shift-a values-a) constraint-a
    (destructuring-bind (width-b shift-b values-b) constraint-b
      (let ((lo (max shift-a shift-b))
            (hi (min (+ shift-a width-a) (+ shift-b width-b))))
        (and (< lo hi)
             (let ((width (- hi lo)))
               (flet ((project (value shift) (ldb (byte width (- lo shift)) value)))
                 (let ((projected-a (mapcar (lambda (v) (project v shift-a)) values-a)))
                   (notany (lambda (v) (member (project v shift-b) projected-a)) values-b)))))))))

(defun %descriptors-distinguishable-p (a b)
  "T if some bit-level constraint of A (%WORD-BIT-CONSTRAINTS, #140) and some
constraint of B share bits and disagree there (%BIT-CONSTRAINTS-DISJOINT-P)
-- the sole decodability proof %CHECK-OPCODE-DECODABLE! needs. Checking every
constraint of A against every constraint of B covers each unordered pair
regardless of which descriptor it came from, so one nested loop suffices
where the old hole/pin split needed two separate checks."
  (let ((constraints-a (%word-bit-constraints a))
        (constraints-b (%word-bit-constraints b)))
    (loop for ca in constraints-a
            thereis (some (lambda (cb) (%bit-constraints-disjoint-p ca cb)) constraints-b))))

(defun %check-opcode-decodable! (machine-name name a b)
  "Signal OPCODE-CONFLICT unless A and B -- two INSTRUCTION-DESCRIPTORs about
to share one opcode on word-encoded MACHINE-NAME (#105), neither a sibling
combo of the other (%SIBLING-COMBOS-P) -- can be told apart at decode time.

#140: A and B need not name the same instruction-word layout (#64) --
%DESCRIPTORS-DISTINGUISHABLE-P pairs their bit-level constraints (operand
holes and (field-value ...) pins alike) by the bits they actually occupy,
fully or partially overlapping, so decode never needs to know which layout
matched before it can tell two candidates apart: every WORD-FIELD-CHOICE and
WORD-CONSTANT already carries its own absolute WIDTH/SHIFT, resolved against
its own descriptor's layout at DEFINSTRUCTION time. :REASON :INDISTINGUISHABLE
when no such disagreement exists.

Requires *at least one* disagreement, not that every constraint disagrees --
decode only needs one field to tell the two apart. A no-operand descriptor
with no WORD-CONSTANTS has no constraints at all, so
%DESCRIPTORS-DISTINGUISHABLE-P finds nothing to check on its own --
%TRY-DECODE-WORD-CANDIDATE (decoder.lisp) matches a no-operand descriptor
vacuously, so it would collide with *any* co-tenant unless its own
WORD-CONSTANTS (#136, a CLS/RET-shaped no-operand instruction pinning every
field) supply the disagreement instead; two co-tenant no-operand descriptors
with no constants are truly indistinguishable unless they are siblings
(caught by %SIBLING-COMBOS-P above)."
  (unless (%sibling-combos-p a b)
    (unless (%descriptors-distinguishable-p a b)
      (error 'opcode-conflict :machine machine-name
                               :opcode (instruction-descriptor-opcode a)
                               :mnemonic name
                               :other-mnemonic (instruction-descriptor-name b)
                               :reason :indistinguishable))))

(defun %parse-word-variant-form (form field-name)
  "Parse one (variant selector kind...) form (DEFINSTRUCTION's docstring)
into a WORD-VARIANT. SELECTOR is (range LO HI) for a value-selected :INLINE
variant (optionally :BIAS N, default 0), :ELSE for the value-selected
:EXTRA-WORD fallback (kind form (extra-word :escape n [:cells k])), or
(choice M) (#104) for a variant selected by hole M matching mode.lisp's
hole-aligned CHOICES instead of by the operand's folded value -- kind form
INLINE (requiring its own :RANGE (lo hi), since unlike (range lo hi) a
CHOICE selector carries no range to double as one; optionally :BIAS N,
default 0) or (extra-word :escape n [:cells k]), the latter an
*unconditional* trailing word once M is the matched alternative, not a
value-triggered fallback.

#135: :CELLS K gives the trailing word its own width in cells, rather than
always the instruction word's own WIDTH-CELLS -- left NIL here (parsed raw)
when omitted; %PARSE-WORD-OPERAND-SUBCLAUSE defaults it to the layout's
WIDTH-CELLS once it has a LAYOUT to default against."
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
       (destructuring-bind (extra-word-kw &key escape cells) (first tail)
         (declare (ignore extra-word-kw))
         (unless escape
           (error "DEFINSTRUCTION: field ~S: (extra-word ...) requires :escape n" field-name))
         (make-word-variant :kind :extra-word :escape escape :extra-cells cells)))
      ((and (consp selector) (eq (first selector) 'choice))
       (destructuring-bind (choice-kw choice-name) selector
         (declare (ignore choice-kw))
         (cond
           ((and (consp (first tail)) (eq (first (first tail)) 'extra-word))
            (destructuring-bind (extra-word-kw &key escape cells alias) (first tail)
              (declare (ignore extra-word-kw))
              (unless escape
                (error "DEFINSTRUCTION: field ~S: (extra-word ...) requires :escape n" field-name))
              (make-word-variant :kind :extra-word :escape escape :choice choice-name
                                 :extra-cells cells :alias (and alias t))))
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

(defun %word-variant-signedp-at-parse (v hole-signedp)
  "V's own signedness (#127/#63), as far as it is knowable at the point
%CHECK-WORD-VARIANTS runs -- before %CHECK-WORD-VARIANT-CHOICES! (below) has
backfilled a mixed field's value-selected variants with the one leftover
ONE-OF alternative. A variant already carrying an explicit
(variant (choice m) ...) form -- WORD-VARIANT-CHOICE already non-NIL at
parse time -- reads M's own MODE-DESCRIPTOR-SIGNEDP (which already folds in
RELATIVEP, mode.lisp) directly. A value-selected (RANGE/:ELSE) variant with
no CHOICE of its own has no ONE-OF alternative to read :SIGNED off before it
is claimed, so it falls back to HOLE-SIGNEDP (#63, %WORD-HOLE-SIGNEDP-LIST)
-- this hole's own resolved signedness when ungoverned by a disagreeing
ONE-OF -- rather than always NIL as it did before #62/#63: a signed or
relative hole's plain (range lo hi) variant needs signed raw-chunk splitting
below the same as any other signed variant would."
  (if (word-variant-choice v)
      (mode-descriptor-signedp (find-mode-descriptor (word-variant-choice v)))
      hole-signedp))

(define-condition signed-range-out-of-field (error)
  ((lo :initarg :lo) (hi :initarg :hi) (low-bound :initarg :low-bound) (high-bound :initarg :high-bound))
  (:documentation "Internal to %WORD-VARIANT-RAW-CHUNKS/%CHECK-WORD-VARIANTS
(#127) -- a signed :INLINE variant's declared (biased) range doesn't fit its
field's signed bound. Always caught and re-signalled with FIELD-NAME context
by %CHECK-WORD-VARIANTS; never escapes to a DEFINSTRUCTION caller directly."))

(defun %word-variant-raw-chunks (lo hi signedp field-width)
  "The RAW (wrapped, unsigned) bit-pattern interval(s) an :INLINE variant's
already-biased value-space range [LO, HI] occupies in a FIELD-WIDTH-bit
field (#127) -- one contiguous (raw-lo . raw-hi) chunk for an unsigned
variant (raw is just value, so LO and HI must already be within [0, MAX]),
or, for a signed one, up to two chunks: two's-complement wraps a negative
value up by 2^FIELD-WIDTH, so a range spanning zero splits into a
non-negative chunk (0..HI) and a negative-turned-high chunk
(LO+2^FIELD-WIDTH..MAX) that are not adjacent in raw space, while a range
entirely on one side of zero wraps to one contiguous chunk same as the
unsigned case (a non-negative range is already its own raw chunk; an
all-negative one just shifts up by 2^FIELD-WIDTH, preserving order). Also
validates LO/HI themselves fit FIELD-WIDTH bits -- the signed bound
[-2^(FIELD-WIDTH-1), 2^(FIELD-WIDTH-1)-1] rather than the unsigned
[0, 2^FIELD-WIDTH-1] %CHECK-WORD-VARIANTS used unconditionally before #127 --
signalling FIELD-NAME-less callers must catch and re-signal with context, or
just calling this from within %CHECK-WORD-VARIANTS' own error-reporting
scope."
  (let ((max (1- (ash 1 field-width))))
    (if (not signedp)
        (list (cons lo hi))
        (let ((low-bound (- (ash 1 (1- field-width)))) (high-bound (1- (ash 1 (1- field-width)))))
          (unless (and (<= low-bound lo) (<= hi high-bound))
            (error 'signed-range-out-of-field :lo lo :hi hi :low-bound low-bound :high-bound high-bound))
          (cond
            ((>= lo 0) (list (cons lo hi)))
            ((< hi 0) (list (cons (+ lo (ash 1 field-width)) (+ hi (ash 1 field-width)))))
            (t (list (cons 0 hi) (cons (+ lo (ash 1 field-width)) max))))))))

(defun %check-word-variants (variants field-width field-name hole-signedp)
  "Signal an error if any of VARIANTS (one FIELD-NAME operand's declared
variant list, already parsed) doesn't fit FIELD-WIDTH bits; if an
:EXTRA-WORD variant's escape value falls inside another variant's biased
inline range; if two :INLINE variants' raw bit-pattern footprints overlap;
or if two :EXTRA-WORD variants share one escape value -- every one of these
is an ambiguity a decoder reading a raw field value could never resolve
(the RANGE/:ELSE-only versions of the first two checks predate #104; a
field with only one value-selected :INLINE and one :ELSE, the only shape
possible before #104, could never trigger the overlap/duplicate-escape
cases, so this doesn't change any existing DEFINSTRUCTION's validity).
#118: CHOICE-selected and value-selected variants may now share one field --
see %CHECK-WORD-VARIANT-CHOICES!, which resolves which ONE-OF alternative
the value-selected ones belong to, and runs after this function, so every
range/escape here (CHOICE-selected or not) is still checked against every
other regardless of kind.

#127: a CHOICE-selected :INLINE variant's fit/overlap checks operate in RAW
bit-pattern space via %WORD-VARIANT-RAW-CHUNKS, not directly on its
(possibly negative) value-space RANGE/BIAS the way an unsigned variant's do
-- a signed variant's declared range is validated against the field's
*signed* bound, and, since two's complement can split a range spanning zero
into two non-adjacent raw chunks, overlap is checked chunk-against-chunk,
not variant-against-variant, so a signed variant correctly collides with an
unsigned one that shares its high (negative-wrapped) raw values even though
their value-space ranges never numerically overlap.

HOLE-SIGNEDP (#62/#63) is this hole's own resolved signedness when
ungoverned by a disagreeing ONE-OF (%WORD-HOLE-SIGNEDP-LIST, which already
folds in per-hole relativeness) -- passed through to
%WORD-VARIANT-SIGNEDP-AT-PARSE so a signed or relative hole's value-selected
variant is treated as signed here too."
  (let ((max (1- (ash 1 field-width))) inline-chunks escapes)
    (dolist (v variants)
      (ecase (word-variant-kind v)
        (:inline
         (let* ((lo (+ (car (word-variant-range v)) (word-variant-bias v)))
                (hi (+ (cdr (word-variant-range v)) (word-variant-bias v)))
                (signedp (%word-variant-signedp-at-parse v hole-signedp)))
           (handler-case
               (dolist (chunk (%word-variant-raw-chunks lo hi signedp field-width))
                 (cl:push chunk inline-chunks))
             (signed-range-out-of-field (c)
               (error "DEFINSTRUCTION: field ~S: signed inline range ~D..~D does not fit its ~
~D-bit field (must be between ~D and ~D)"
                      field-name lo hi field-width (slot-value c 'low-bound) (slot-value c 'high-bound))))
           (unless signedp
             (when (or (< lo 0) (> hi max))
               (error "DEFINSTRUCTION: field ~S: biased inline range ~D..~D does ~
not fit its ~D-bit field" field-name lo hi field-width)))))
        (:extra-word
         (let ((e (word-variant-escape v)))
           (when (or (< e 0) (> e max))
             (error "DEFINSTRUCTION: field ~S: escape ~D does not fit its ~D-bit field"
                    field-name e field-width))
           ;; #135: EXTRA-CELLS is defaulted by the time this runs
           ;; (%PARSE-WORD-OPERAND-SUBCLAUSE), so any non-positive-integer
           ;; value here is an explicit, invalid :CELLS.
           (unless (typep (word-variant-extra-cells v) '(integer 1))
             (error "DEFINSTRUCTION: field ~S: (extra-word ...) :cells ~S must be a ~
positive integer" field-name (word-variant-extra-cells v)))
           (cl:push e escapes)))))
    (dolist (e escapes)
      (dolist (r inline-chunks)
        (when (<= (car r) e (cdr r))
          (error "DEFINSTRUCTION: field ~S: escape value ~D is inside inline ~
range ~D..~D -- an encoded field value of ~D can never be told apart from a ~
genuine inline value" field-name e (car r) (cdr r) e))))
    ;; #104/#127: reachable now that several CHOICE-selected :INLINE
    ;; variants -- signed or not -- can share one field -- unreachable
    ;; before, when a field had at most one value-selected :INLINE variant.
    (loop for (r . later) on inline-chunks
          do (dolist (r2 later)
               (when (<= (max (car r) (car r2)) (min (cdr r) (cdr r2)))
                 (error "DEFINSTRUCTION: field ~S: inline ranges overlap in raw field value ~D..~D -- ~
an encoded field value in the overlap could never be told apart"
                        field-name (max (car r) (car r2)) (min (cdr r) (cdr r2))))))
    ;; #104/#187: a shared escape is legal only as (extra-word ... :alias t)
    ;; -- a second spelling of one canonical variant's encoding.
    (let ((extras (remove-if-not (lambda (v) (eq (word-variant-kind v) :extra-word)) variants)))
      (dolist (e (remove-duplicates (mapcar #'word-variant-escape extras)))
        (let* ((group (remove e extras :key #'word-variant-escape :test #'/=))
               (canonical (remove-if #'word-variant-alias group))
               (aliases (remove-if-not #'word-variant-alias group)))
          (cond
            ((rest canonical)
             (error "DEFINSTRUCTION: field ~S: escape value ~D is used by more than one variant ~
-- mark a second spelling of the same encoding with (extra-word :escape ~D :alias t)"
                    field-name e e))
            ((and aliases (null canonical))
             (error "DEFINSTRUCTION: field ~S: (extra-word :escape ~D :alias t) has no ~
non-alias variant on that escape to be an alias of" field-name e))
            (aliases
             (dolist (a aliases)
               (%check-alias-encodes-like-canonical! a (first canonical) field-name e)))))))))

(defun %check-alias-encodes-like-canonical! (alias canonical field-name escape)
  "Signal an error unless ALIAS (#187) -- an :ALIAS T :EXTRA-WORD variant --
encodes exactly as CANONICAL, the non-alias variant on the same ESCAPE: same
extra-cell width, and ONE-OF alternatives agreeing on hole count, signedness,
width and relativeness."
  (unless (and (word-variant-choice alias) (word-variant-choice canonical))
    (error "DEFINSTRUCTION: field ~S: :alias t on escape ~D requires both variants to be ~
(choice ...) selected" field-name escape))
  (let ((am (find-mode-descriptor (word-variant-choice alias)))
        (cm (find-mode-descriptor (word-variant-choice canonical))))
    (unless (and (eql (word-variant-extra-cells alias) (word-variant-extra-cells canonical))
                 (= (%mode-hole-count am) (%mode-hole-count cm))
                 (eq (mode-descriptor-signedp am) (mode-descriptor-signedp cm))
                 (eql (mode-descriptor-width am) (mode-descriptor-width cm))
                 (eq (mode-descriptor-relativep am) (mode-descriptor-relativep cm)))
      (error "DEFINSTRUCTION: field ~S: alias ~S on escape ~D does not encode like ~S -- ~
extra-word cells, hole count, signedness, width and relativeness must all agree"
             field-name (word-variant-choice alias) escape (word-variant-choice canonical)))))

(defun %check-word-variant-choices! (variants field-name hole-alternatives)
  "Signal an error if any of VARIANTS' non-NIL WORD-VARIANT-CHOICE (#104)
names a mode not registered (FIND-MODE-DESCRIPTOR signals), or one not among
HOLE-ALTERNATIVES -- this operand hole's actual ONE-OF alternatives, per
mode.lisp's %MODE-HOLE-ALTERNATIVES (NIL when the hole isn't a ONE-OF at
all, which makes any (CHOICE M) on it an error unconditionally).

#118: also resolves a *mixed* field -- one with both CHOICE-selected
variants and value-selected (RANGE/:ELSE) ones. VARIANTS is wholly
CHOICE-selected, wholly value-selected, or mixed; only the mixed case does
anything below. When mixed, every value-selected variant is selected, at
decode/assemble time, by whichever ONE-OF alternative no CHOICE-selected
variant here already claims -- there must be exactly one such
UNCLAIMED alternative, since a decoder reading a raw field value has
nothing else to disambiguate the value-selected rows by: zero unclaimed
alternatives means the value-selected variants could never be selected at
all (every alternative already routes to a CHOICE-selected variant
instead); two or more means nothing tells decode which of them the
value-selected rows belong to. Once resolved, every value-selected
variant's own WORD-VARIANT-CHOICE is SETF to that one alternative -- so
%WORD-CHOICES-ELIGIBLE-P (assembler.lisp), %MATCHED-CHOICE-NAME
(CHOICE-CASE dispatch, above) and %RENDER-OPERAND-TEXT (disassembler.lisp)
all see a real, non-NIL choice for a value-selected row on a mixed field
and need no separate mixed-field logic of their own; a field with no
CHOICE variant at all is untouched, exactly as before #118.

#127: this backfill is also why a mixed field may not resolve to a SIGNED
unclaimed alternative -- %WORD-FIELD-CHOICE-FORM stamps a variant's own
SIGNEDP from whatever mode its (now-backfilled) CHOICE names, but
VALUE-SELECTED's own (RANGE lo hi) was validated as unsigned by
%CHECK-WORD-VARIANTS, which runs before this backfill and so cannot know
what the eventual unclaimed alternative's signedness will turn out to be.
Signalled here, once the unclaimed alternative is actually known, rather
than left to silently skew encode and decode apart."
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
alternatives ~S" field-name choice hole-alternatives)))))
  (let* ((choice-selected (remove-if-not #'word-variant-choice variants))
         (value-selected (remove-if #'word-variant-choice variants)))
    (when (and choice-selected value-selected)
      (let ((unclaimed (set-difference hole-alternatives (mapcar #'word-variant-choice choice-selected))))
        (cond
          ((null unclaimed)
           (error "DEFINSTRUCTION: field ~S: every ONE-OF alternative ~S is already claimed ~
by a (choice ...) variant, so this field's value-selected variant~P could never be selected"
                  field-name hole-alternatives (length value-selected)))
          ((rest unclaimed)
           (error "DEFINSTRUCTION: field ~S: value-selected (RANGE/:ELSE) variants would be ~
selected by more than one unclaimed ONE-OF alternative ~S -- nothing could tell them apart ~
at decode" field-name unclaimed))
          ;; #127: the unclaimed alternative backfilled onto VALUE-SELECTED
          ;; below has no (CHOICE ...) of its own -- %WORD-FIELD-CHOICE-FORM
          ;; (below) would otherwise read this backfilled CHOICE's own
          ;; MODE-DESCRIPTOR-SIGNEDP and stamp a value-selected variant
          ;; SIGNEDP T purely because the one alternative left over happens
          ;; to be signed, even though nothing about VALUE-SELECTED's own
          ;; (RANGE lo hi) declares a signed range or was validated as one
          ;; (%CHECK-WORD-VARIANTS, above, runs before this backfill and so
          ;; validates it as unsigned) -- raw field bits would then be
          ;; sign-extended on decode against an unsigned-declared range,
          ;; and %WORD-FIELD-CHOICE-VALUES/%HOLE-DISJOINT-P would silently
          ;; compare the wrong raw value set. Rejected here rather than
          ;; left to skew encode/decode apart.
          ;;
          ;; #63: this rationale weakens once a HOLE's own signedness
          ;; (%WORD-HOLE-SIGNEDP-LIST, reading (FIRST HOLE-ALTERNATIVES),
          ;; not necessarily the alternative that ends up UNCLAIMED here) can
          ;; itself be non-NIL -- %CHECK-WORD-VARIANTS may then have already
          ;; validated VALUE-SELECTED as signed, against a *different*
          ;; alternative's signedness than the one being backfilled onto it
          ;; below. Left unchanged rather than narrowed or reworked: this
          ;; guard's error still fires whenever the unclaimed alternative
          ;; itself declares :SIGNED T, which remains a correct (if now
          ;; occasionally redundant with an already-signed HOLE-SIGNEDP)
          ;; rejection -- narrowing it to fire only when HOLE-SIGNEDP and the
          ;; unclaimed alternative's own signedness actually disagree would
          ;; legalize a mixed-field shape docs/instructions.md and
          ;; docs/modes.md both currently document as rejected outright, and
          ;; is out of #63's scope (see #63's closing comment).
          ((mode-descriptor-signedp (find-mode-descriptor (first unclaimed)))
           (error "DEFINSTRUCTION: field ~S: the unclaimed ONE-OF alternative ~S left for this ~
field's value-selected variant~P declares :SIGNED T -- a value-selected variant has no (CHOICE ~
...) of its own to read :SIGNED from, so a mixed field cannot carry a signed fallback; give ~S ~
its own (CHOICE ...) variant instead"
                  field-name (first unclaimed) (length value-selected) (first unclaimed)))
          ;; #120: same shape as the :SIGNED rejection just above -- a
          ;; value-selected variant has no (CHOICE ...) of its own to declare
          ;; extra holes on, so the unclaimed alternative's own hole count
          ;; must match every other alternative sharing this element's base
          ;; (minimum) count; an unclaimed alternative contributing MORE
          ;; holes would imply a hole with no encoding anywhere.
          ((/= (%mode-hole-count (find-mode-descriptor (first unclaimed)))
               (%pattern-one-of-min-hole-count hole-alternatives))
           (error "DEFINSTRUCTION: field ~S: the unclaimed ONE-OF alternative ~S left for this ~
field's value-selected variant~P has ~D hole~:P, not this element's base hole count ~D -- a ~
value-selected variant has no (CHOICE ...) of its own to declare extra holes on; give ~S its own ~
(CHOICE ...) variant instead"
                  field-name (first unclaimed) (length value-selected)
                  (%mode-hole-count (find-mode-descriptor (first unclaimed)))
                  (%pattern-one-of-min-hole-count hole-alternatives) (first unclaimed)))
          (t (dolist (v value-selected)
               (setf (word-variant-choice v) (first unclaimed)))))))))

(defun %word-hole-relativep-list (mode hole-alternatives-list n)
  "Hole-aligned list of N booleans -- this MODE's per-hole relativeness (#62)
as it applies to a value-selected word-field variant (one with no (CHOICE m)
selector of its own): the hole's own ONE-OF alternatives' shared RELATIVEP
when the hole has alternatives (they either agree, or %CHECK-WORD-ONE-OF-
RELATIVE requires every variant at a disagreeing hole to be CHOICE-selected,
so this fallback is never actually consulted for a disagreeing hole), else
MODE's own MODE-DESCRIPTOR-RELATIVEP for an ungoverned hole. Mirrors
%BYTE-RELATIVE-FLAGS' same CHOSEN/ALTS/ungoverned resolution, minus CHOSEN --
a word-field variant's own choice, when it has one, already carries its own
alternative's RELATIVEP via %WORD-FIELD-CHOICE-FORM (MODE-DESCRIPTOR-SIGNEDP
already folds RELATIVEP in, mode.lisp), so this list is only ever read for
the no-CHOICE case -- both at DEFINSTRUCTION-time parsing (below, so a
signed inline range is accepted for a relative hole's value-selected
variant) and at %WORD-FIELD-CHOICE-FORM (further down). MODE's own RELATIVEP
contributes at exactly one hole in practice -- a whole-mode :RELATIVE mode
always has exactly one hole (%CHECK-RELATIVE-MODE-HOLES) -- same as
%BYTE-RELATIVE-FLAGS' identical fallback.

Kept as its own list, distinct from %WORD-HOLE-SIGNEDP-LIST below, because
RELATIVE-HOLE-INDEX (%WORD-RELATIVE-HOLE-INDEX) is positional -- at most one
hole may ever be relative -- while signedness (like a byte-encoded operand's
OPERAND-SIGNEDNESS) is an independent per-hole boolean any number of holes
may set."
  (loop for i below n
        for alts = (nth i hole-alternatives-list)
        collect (if alts
                    (mode-descriptor-relativep (find-mode-descriptor (first alts)))
                    (mode-descriptor-relativep mode))))

(defun %word-hole-signedp-list (mode hole-alternatives-list n)
  "Hole-aligned list of N booleans -- this MODE's per-hole signedness (#63) as
it applies to a value-selected word-field variant (one with no (CHOICE m)
selector of its own). Exactly %WORD-HOLE-RELATIVEP-LIST's shape, reading
MODE-DESCRIPTOR-SIGNEDP instead of -RELATIVEP -- MODE-DESCRIPTOR-SIGNEDP
already folds RELATIVEP in (mode.lisp, :SIGNEDP (OR RELATIVE SIGNED)), so a
relative hole is signed here too, with no separate case. A CHOICE-selected
variant never consults this list -- it reads its own alternative's
MODE-DESCRIPTOR-SIGNEDP directly (%WORD-FIELD-CHOICE-FORM) -- so this is
only read for the no-CHOICE case, same restriction as the relativep list.

Before #63, a value-selected word field was only ever signed via this
fallback when its hole was RELATIVE; a plain :SIGNED T mode had no effect at
all on a word-encoded field, and its declared negative range was rejected
outright by %CHECK-WORD-VARIANTS as failing the field's unsigned bound."
  (loop for i below n
        for alts = (nth i hole-alternatives-list)
        collect (if alts
                    (mode-descriptor-signedp (find-mode-descriptor (first alts)))
                    (mode-descriptor-signedp mode))))

(defun %parse-word-operand-subclause (subclause layout layout-name machine-name hole-alternatives hole-signedp)
  "SUBCLAUSE is one whole (operand [NAME] :field FIELD-NAME (variant ...)*)
form on a word-encoded machine. Returns a WORD-OPERAND-SPEC. With no
(variant ...) forms at all, the operand is plain inline over the field's
full range, bias 0 -- the word-encoded equivalent of a byte-encoded
(operand :mode)'s implicit default. #62/#63: that full range is the field's
*signed* bound, [-2^(FWIDTH-1), 2^(FWIDTH-1)-1], when HOLE-SIGNEDP, rather
than its unsigned one, [0, 2^FWIDTH-1] -- a relative hole's offset (signed
via MODE-DESCRIPTOR-SIGNEDP folding in RELATIVEP) or a plain :SIGNED T
hole's value can be negative, and there is no bias here to carry a negative
value the way a plain value-selected signed variant's own declared :BIAS
could, so the implicit default must already be signed or a negative value
could never encode at all. HOLE-ALTERNATIVES (#104) is this hole's own
ONE-OF alternative mode-name list (mode.lisp's %MODE-HOLE-ALTERNATIVES), or
NIL for a plain EXPR hole -- validated against any (CHOICE M) variant here
(%CHECK-WORD-VARIANT-CHOICES!). HOLE-SIGNEDP (%WORD-HOLE-SIGNEDP-LIST) is
this hole's own resolved signedness when ungoverned by a disagreeing ONE-OF
-- also threaded into %CHECK-WORD-VARIANTS so an explicit value-selected
variant's signed (negative-LO) range is validated against the field's
*signed* bound rather than rejected as an unsigned range with a negative LO;
without this, a :SIGNED T (or whole-mode :RELATIVE) mode's plain
(variant (range -128 127) inline) would fail at DEFINSTRUCTION time before
ever reaching the machinery it's meant to feed.

LAYOUT (#64) is the machine's default INSTRUCTION-WORD-LAYOUT, or the
alternate this instruction named via its own (layout NAME) subclause --
:FIELD is resolved *within* that one layout, so a field name that only
exists in a different layout is reported as unknown here rather than
silently resolving against the wrong bits. LAYOUT-NAME (the plain symbol, or
NIL for the default) is only for the error message below.

#120: SUBCLAUSE may instead be (operand [NAME] :trailing-word [:cells k]) --
a fieldless hole with no bits of its own in the instruction word, only a
single, unconditionally-matching :TRAILING-WORD variant. Used for the extra
hole a varying ONE-OF alternative contributes beyond its element's base hole
count (a (for-choice ALT (operand ...)...) subclause, %WORD-MODE-DESCRIPTOR-
FORMS below); that caller stamps the returned spec's own variant CHOICE with
ALT afterwards, since there is no (choice m) syntax on a fieldless hole to
carry it."
  (multiple-value-bind (name spec) (%parse-operand-subclause subclause)
    (if (eq (first spec) :trailing-word)
        (destructuring-bind (trailing-kw &key cells register) spec
          (declare (ignore trailing-kw))
          (when (and cells (not (and (integerp cells) (plusp cells))))
            (error "DEFINSTRUCTION: (operand ~@[~S ~]:trailing-word :cells ~S): :CELLS must be ~
a positive integer" name cells))
          (make-word-operand-spec
           :name name :field nil :width nil :shift nil :register register
           :variants (list (make-word-variant
                             :kind :trailing-word
                             :extra-cells (or cells (instruction-word-layout-width-cells layout))))))
        (%parse-word-field-operand-subclause subclause name spec layout layout-name machine-name
                                              hole-alternatives hole-signedp))))

(defun %parse-word-field-operand-subclause (subclause name spec layout layout-name machine-name
                                             hole-alternatives hole-signedp)
  "The :FIELD-bearing half of %PARSE-WORD-OPERAND-SUBCLAUSE, split out so the
#120 :TRAILING-WORD case above doesn't have to thread LAYOUT/FWIDTH through
a branch that never uses them. NAME/SPEC are %PARSE-OPERAND-SUBCLAUSE's own
split of SUBCLAUSE."
  (destructuring-bind (field-kw field-name &rest after-field) spec
      (unless (eq field-kw :field)
        (error "DEFINSTRUCTION: malformed word operand spec ~S -- expected ~
(operand [name] :field f ...) or (operand [name] :trailing-word [:cells k])" subclause))
      (let ((field (instruction-word-field layout field-name)))
        (unless field
          (error "DEFINSTRUCTION: no field named ~S in ~:[the default instruction-word ~
layout~;instruction-word layout ~:*~S~] on machine ~S" field-name layout-name machine-name))
        (destructuring-bind (fname fwidth fshift) field
          (declare (ignore fname))
          (multiple-value-bind (register variant-forms) (%parse-operand-register-clause after-field subclause)
            (let ((variants (if variant-forms
                                 (mapcar (lambda (f) (%parse-word-variant-form f field-name)) variant-forms)
                                 (list (make-word-variant
                                        :kind :inline :bias 0
                                        :range (if hole-signedp
                                                   (cons (- (ash 1 (1- fwidth))) (1- (ash 1 (1- fwidth))))
                                                   (cons 0 (1- (ash 1 fwidth)))))))))
              ;; #135: an :EXTRA-WORD variant with no explicit :CELLS defaults
              ;; to the layout's own WIDTH-CELLS -- today's assumption, now
              ;; just the default rather than the only option. Defaulted here,
              ;; not at parse time, since %PARSE-WORD-VARIANT-FORM has no
              ;; LAYOUT to default against; %CHECK-WORD-VARIANTS below then
              ;; validates every variant's EXTRA-CELLS -- explicit or
              ;; defaulted -- as one concrete positive integer.
              (dolist (v variants)
                (when (and (%word-variant-extra-p v) (null (word-variant-extra-cells v)))
                  (setf (word-variant-extra-cells v) (instruction-word-layout-width-cells layout))))
              (%check-word-variants variants fwidth field-name hole-signedp)
              (%check-word-variant-choices! variants field-name hole-alternatives)
              (make-word-operand-spec :name name :field field-name :width fwidth :shift fshift
                                       :register register :variants variants)))))))

(defun %parse-word-operand-subclauses (mode subclauses machine name mode-name machine-name layout layout-name
                                        &optional hole-alternatives-list)
  "Like %PARSE-OPERAND-SUBCLAUSES but for a word-encoded machine -- one
WORD-OPERAND-SPEC per hole, in hole order. Threads hole-by-hole ONE-OF
alternatives (mode.lisp's %MODE-HOLE-ALTERNATIVES, #104) and, for #62/#63,
MODE's own per-hole signedness (%WORD-HOLE-SIGNEDP-LIST, which already folds
in per-hole relativeness -- MODE-DESCRIPTOR-SIGNEDP is (OR RELATIVEP
SIGNEDP)) through to each subclause so a (CHOICE M) variant can be checked
against what its hole can actually match, and a signed or relative hole's
value-selected variant is parsed as signed. LAYOUT/LAYOUT-NAME (#64) are
this instruction's own selected instruction-word layout (the machine's
default, or a (layout NAME) alternate) and its name, threaded to each
subclause so :FIELD resolves within that one layout.

HOLE-ALTERNATIVES-LIST (#120), when given, is a specific alternative-tuple's
own hole-aligned alternatives (mode.lisp's MODE-HOLE-TUPLE-HOLE-ALTERNATIVES)
-- SUBCLAUSES must then have exactly that many entries, not necessarily
MODE's own (minimum) %MODE-HOLE-COUNT. Defaults to
MODE's own base %MODE-HOLE-ALTERNATIVES/%MODE-HOLE-COUNT, unchanged from
before #120, when omitted.

#138: rejects a :FIELD naming OPCODE outright -- (opcode n) already owns it,
and letting an operand also write it would OR the operand's bits into the
already-placed opcode field at encode time (%ENCODE-WORD-INSTRUCTION),
silently corrupting it. Also rejects two subclauses naming the same
non-NIL field -- same hazard, since both would OR into the same bits; a
#120 :TRAILING-WORD spec's NIL field is exempt, since several fieldless
holes write no bits at all. Mirrors the two checks %PARSE-FIELD-VALUE-
SUBCLAUSES (#136) already makes for (field-value ...)."
  (let* ((hole-alternatives (or hole-alternatives-list (%mode-hole-alternatives mode)))
         (holes (length hole-alternatives))
         (n (length subclauses)))
    (unless (= holes n)
      (error "DEFINSTRUCTION ~S ~S: addressing mode ~S has ~D EXPR hole~:P ~
but ~D (operand ...) subclause~:P ~:[were~;was~] given -- one is required ~
per hole" machine name mode-name holes n (= n 1)))
    (let* ((hole-signedp-list (%word-hole-signedp-list mode hole-alternatives holes))
           (specs (mapcar (lambda (s alts hole-signedp)
                             (%parse-word-operand-subclause s layout layout-name machine-name alts hole-signedp))
                           subclauses hole-alternatives hole-signedp-list)))
      (when (find 'opcode specs :key #'word-operand-spec-field)
        (error "DEFINSTRUCTION ~S ~S~@[ ~S~]: (operand ... :field opcode) is not allowed -- the ~
opcode field is already given by this instruction's own (opcode n)" machine name mode-name))
      (let ((dup (loop for (s . later) on specs
                        when (and (word-operand-spec-field s)
                                  (find (word-operand-spec-field s) later :key #'word-operand-spec-field))
                          return (word-operand-spec-field s))))
        (when dup
          (error "DEFINSTRUCTION ~S ~S~@[ ~S~]: more than one (operand ... :field ~S) subclause -- a ~
field may carry at most one operand" machine name mode-name dup)))
      (%check-operand-names (mapcar #'word-operand-spec-name specs) machine name mode-name)
      (%check-operand-registers! (mapcar #'word-operand-spec-register specs) machine name mode-name mode
                                  hole-alternatives)
      specs)))

(defun %word-variant-extra-p (v)
  "T if V spends a trailing word -- :EXTRA-WORD (a field's own escape) or
:TRAILING-WORD (#120, a fieldless extra hole's unconditional one) alike;
both contribute EXTRA-CELLS the same way to a combo's total size and
narrow-before-wide ordering (%EXPAND-WORD-COMBOS)."
  (member (word-variant-kind v) '(:extra-word :trailing-word)))

(defun %expand-word-combos (specs)
  "Cartesian product of SPECS' (WORD-OPERAND-SPEC) variant lists -- one combo
per element, each a list of (SPEC . VARIANT) pairs parallel to SPECS. Ordered
by ascending total :EXTRA-WORD cells (#135; formerly a plain :EXTRA-WORD
count, back when every extra word was implicitly one instruction-word wide),
ties in declaration order -- matching %CHOOSE-VARIANT's documented
\"narrower before wider\" convention (assembler.lisp) so an all-inline combo
is always tried before one needing an extra word, and a combo needing a
narrower extra word before one needing a wider one."
  (let ((combos (list nil)))
    (dolist (spec specs)
      (setf combos
            (loop for combo in combos
                  append (loop for variant in (word-operand-spec-variants spec)
                               collect (append combo (list (cons spec variant)))))))
    (stable-sort combos #'<
                 :key (lambda (combo)
                        (reduce #'+ combo :key (lambda (p) (if (%word-variant-extra-p (cdr p))
                                                                (word-variant-extra-cells (cdr p))
                                                                0)))))))

(defun %word-field-choice-form (spec variant hole-signedp)
  "#127/#62/#63: VARIANT's own SIGNEDP is stamped from its CHOICE
alternative's MODE-DESCRIPTOR-SIGNEDP when CHOICE is non-NIL --
MODE-DESCRIPTOR-SIGNEDP is itself (OR RELATIVEP SIGNEDP) (mode.lisp), so a
CHOICE-selected :RELATIVE alternative is already signed here with no extra
work. A value-selected variant (CHOICE NIL) has no ONE-OF alternative of
its own to read :SIGNED off, so it falls back to HOLE-SIGNEDP -- this
hole's own resolved signedness when ungoverned by a disagreeing ONE-OF
(%WORD-HOLE-SIGNEDP-LIST, below, itself folding in per-hole relativeness)
-- rather than always NIL as it did before #62/#63: an ungoverned or
agreeing-ONE-OF signed or :RELATIVE hole's value is signed regardless of
which value-selected variant a given combo happens to pick."
  `(make-word-field-choice
    :width ,(word-operand-spec-width spec)
    :shift ,(word-operand-spec-shift spec)
    :kind ,(word-variant-kind variant)
    :bias ,(word-variant-bias variant)
    :range ',(word-variant-range variant)
    :escape ,(word-variant-escape variant)
    :extra-cells ,(word-variant-extra-cells variant)
    :choice ',(word-variant-choice variant)
    :alias ,(word-variant-alias variant)
    :signedp ,(if (word-variant-choice variant)
                  (mode-descriptor-signedp (find-mode-descriptor (word-variant-choice variant)))
                  hole-signedp)))

(defun %word-alternatives-form (specs hole-signedp-list)
  "One (quoted) form building SPECS' full per-operand variant menu -- shared
by every sibling combo of one word-field operand list, since decode
(decoder.lisp's %TRY-DECODE-WORD-CANDIDATE) needs every alternative, not
just whichever combo happens to occupy the opcode table, to tell an inline
value from an escaped extra-word marker apart by comparing against the
actually fetched bits. HOLE-SIGNEDP-LIST (#62/#63, %WORD-HOLE-SIGNEDP-LIST)
is hole-aligned with SPECS -- %TRY-DECODE-WORD-CANDIDATE reads SIGNEDP off
*this* menu, not a chosen descriptor's own WORD-FIELDS, so a signed or
relative hole's signedness must be stamped here too, identically to
%WORD-DESCRIPTOR-FORM's own WORD-FIELDS below -- else encode would sign a
value that decode then reads back unsigned."
  `(list ,@(mapcar (lambda (spec hole-signedp)
                      `(list ,@(mapcar (lambda (variant) (%word-field-choice-form spec variant hole-signedp))
                                       (word-operand-spec-variants spec))))
                    specs hole-signedp-list)))

(defun %word-constants-form (constants)
  "One (quoted) form building CONSTANTS (a list of WORD-CONSTANT, #136) --
shared by every sibling combo of one word-encoded DEFINSTRUCTION variant,
same as %WORD-ALTERNATIVES-FORM shares its own menu across siblings: a
(field-value ...) pin doesn't vary by which value-range combo a mnemonic's
operand happened to expand into, so there is exactly one CONSTANTS list per
variant, not one per combo."
  `(list ,@(mapcar (lambda (c) `(make-word-constant :name ',(word-constant-name c)
                                                      :width ,(word-constant-width c)
                                                      :shift ,(word-constant-shift c)
                                                      :value ,(word-constant-value c)))
                    constants)))

(defun %word-descriptor-form (machine name mode-form opcode alternatives-form combo cycles semantics-fn-form
                               hole-signedp-list hole-relativep-list layout-name constants-form)
  "One INSTRUCTION-DESCRIPTOR form for word-field COMBO (a list of (SPEC
. VARIANT) pairs from %EXPAND-WORD-COMBOS, in hole order). RELATIVE-HOLE-
INDEX (#62) is COMBO's own %WORD-RELATIVE-HOLE-INDEX -- computed per combo,
not once per mode, since sibling combos may pick different CHOICE-selected
variants at the same hole and so disagree on which hole (if any) is
relative, same reason WORD-FIELDS itself is computed per combo rather than
shared. HOLE-SIGNEDP-LIST and HOLE-RELATIVEP-LIST (#63) are kept separate --
signedness is an independent per-hole boolean any number of holes may set,
while RELATIVE-HOLE-INDEX is positional, at most one hole ever. LAYOUT-NAME
(#64) is this mode's own selected instruction-word layout name, stamped
straight onto every combo -- see INSTRUCTION-DESCRIPTOR-WORD-LAYOUT-NAME.

ALTERNATIVES-FORM, SEMANTICS-FN-FORM, and CONSTANTS-FORM (#150) are each
either an already-built form or a gensym the caller's own LET* binds to
one, shared across every combo of one tuple the same way ALTERNATIVES-FORM
already was documented to be -- %WORD-MODE-DESCRIPTOR-FORMS builds each at
most once per tuple (ALTERNATIVES-FORM, SEMANTICS-FN-FORM) or once per mode
(CONSTANTS-FORM, which doesn't vary by which value-range combo a mnemonic's
operand happened to expand into) and passes the gensym through here, rather
than each being rebuilt -- and so re-emitted as compiled code -- once per
combo."
  (let* ((operand-names (mapcar (lambda (p) (word-operand-spec-name (car p))) combo))
         (operand-registers (mapcar (lambda (p) (word-operand-spec-register (car p))) combo))
         (word-fields-form `(list ,@(mapcar (lambda (p hole-signedp)
                                               (%word-field-choice-form (car p) (cdr p) hole-signedp))
                                             combo hole-signedp-list)))
         ;; #135: sum of each :EXTRA-WORD field's own cell width, not a
         ;; plain count -- a combo mixing a narrow and a wide extra word no
         ;; longer overstates or understates the total.
         (extra-cells (reduce #'+ combo :key (lambda (p) (if (%word-variant-extra-p (cdr p))
                                                               (word-variant-extra-cells (cdr p))
                                                               0))))
         (relative-index (%word-relative-hole-index combo hole-relativep-list)))
    `(make-instruction-descriptor
      :name ,(string-upcase (symbol-name name))
      :machine ',machine
      :mode ,mode-form
      :opcode ,opcode
      :operand-widths nil
      :operand-names ',operand-names
      :operand-registers ',operand-registers
      :word-fields ,word-fields-form
      :word-alternatives ,alternatives-form
      :extra-cells ,extra-cells
      :relative-hole-index ',relative-index
      :word-layout-name ',layout-name
      :word-constants ,constants-form
      :cycles ,cycles
      :semantics-fn ,semantics-fn-form)))

(defun %parse-for-choice-subclause (subclause)
  "SUBCLAUSE is one whole (for-choice ALT (operand ...)*) form (#120) -- the
extra (operand ...) subclauses filling the holes ALT's own alternative
contributes beyond its ONE-OF element's base (minimum) hole count. Returns
(VALUES alt-name operand-subclauses)."
  (destructuring-bind (for-choice-kw alt-name &rest operand-subclauses) subclause
    (declare (ignore for-choice-kw))
    (dolist (s operand-subclauses)
      (unless (and (consp s) (eq (first s) 'operand))
        (error "DEFINSTRUCTION: (for-choice ~S ...) may only contain (operand ...) subclauses, got ~S"
               alt-name s)))
    (values alt-name operand-subclauses)))

(defun %check-for-choice-subclauses! (mode for-choice-alist machine name)
  "Validate FOR-CHOICE-ALIST (an (ALT-NAME . OPERAND-SUBCLAUSES) alist,
#120) against MODE's own varying ONE-OF element, if it has one: every
ALT-NAME given must be one of that element's own alternatives whose hole
count exceeds the element's base (minimum) count, every such over-count
alternative must have exactly one (FOR-CHOICE ...) group, and each group
must supply exactly as many (operand ...) subclauses as its own alternative
needs extra holes."
  (let* ((pattern (mode-descriptor-pattern mode))
         (varying-element (%pattern-varying-one-of-element pattern)))
    (unless varying-element
      (when for-choice-alist
        (error "DEFINSTRUCTION ~S ~S: (for-choice ...) given but addressing mode ~S has no ONE-OF ~
with varying hole counts" machine name (mode-descriptor-name mode)))
      (return-from %check-for-choice-subclauses!))
    (let* ((alt-names (rest varying-element))
           (base-count (%pattern-one-of-min-hole-count alt-names))
           (over-alts (remove-if (lambda (n) (= (%mode-hole-count (find-mode-descriptor n)) base-count))
                                  alt-names)))
      (dolist (entry for-choice-alist)
        (unless (member (car entry) alt-names)
          (error "DEFINSTRUCTION ~S ~S: (for-choice ~S ...) does not name one of addressing mode ~
~S's own ONE-OF alternatives ~S" machine name (car entry) (mode-descriptor-name mode) alt-names))
        (unless (member (car entry) over-alts)
          (error "DEFINSTRUCTION ~S ~S: (for-choice ~S ...) given, but ~S has the same hole count ~
as this ONE-OF element's other alternatives -- nothing extra to fill"
                 machine name (car entry) (car entry))))
      (let ((given (mapcar #'car for-choice-alist)))
        (dolist (alt over-alts)
          (unless (member alt given)
            (error "DEFINSTRUCTION ~S ~S: addressing mode ~S's ONE-OF alternative ~S has ~D extra ~
hole~:P beyond its element's base hole count but no (for-choice ~S ...) subclause supplies them"
                   machine name (mode-descriptor-name mode) alt
                   (- (%mode-hole-count (find-mode-descriptor alt)) base-count) alt))))
      (dolist (entry for-choice-alist)
        (let* ((alt (car entry)) (subclauses (cdr entry))
               (needed (- (%mode-hole-count (find-mode-descriptor alt)) base-count)))
          (unless (= (length subclauses) needed)
            (error "DEFINSTRUCTION ~S ~S: (for-choice ~S ...) supplies ~D (operand ...) ~
subclause~:P but ~S needs ~D extra hole~:P beyond its element's base hole count"
                   machine name alt (length subclauses) alt needed)))))))

(defun %tuple-operand-subclauses (mode operand-subclauses tuple for-choice-alist)
  "The (operand ...) subclause list for TUPLE (a MODE-HOLE-TUPLE, #120):
OPERAND-SUBCLAUSES (the base, minimum-hole-count subclauses given at
DEFINSTRUCTION) unchanged for the base tuple (TUPLE's own ALT-NAME NIL), or
with FOR-CHOICE-ALIST's entry for TUPLE's own alternative spliced in right
after the varying element's own base holes, for an over-count tuple."
  (let ((alt-name (mode-hole-tuple-alt-name tuple)))
    (if (null alt-name)
        operand-subclauses
        (multiple-value-bind (start base-count) (%varying-element-hole-range mode)
          (let ((end (+ start base-count))
                (extras (cdr (assoc alt-name for-choice-alist))))
            (append (subseq operand-subclauses 0 end) extras (subseq operand-subclauses end)))))))

(defun %filter-spec-variants-for-tuple (spec own-alt-names)
  "A copy of SPEC (a WORD-OPERAND-SPEC) whose own VARIANTS are restricted to
those whose WORD-VARIANT-CHOICE is a member of OWN-ALT-NAMES (#120): each
alternative-tuple's governing field must see only the variants belonging to
its own alternative(s) -- left unfiltered, %EXPAND-WORD-COMBOS would build
cross-tuple-incoherent combos (e.g. a 1-hole tuple's descriptor whose field
code actually means \"2 holes follow\"), and %WORD-ALTERNATIVES-FORM would
give decode identical menus for descriptors that are not actually siblings,
producing a spurious opcode conflict or a misdecode depending on which
sibling %TRY-DECODE-WORD-CANDIDATE (decoder.lisp) tries first."
  (make-word-operand-spec :name (word-operand-spec-name spec)
                           :field (word-operand-spec-field spec)
                           :width (word-operand-spec-width spec)
                           :shift (word-operand-spec-shift spec)
                           :register (word-operand-spec-register spec)
                           :variants (remove-if-not (lambda (v) (member (word-variant-choice v) own-alt-names))
                                                     (word-operand-spec-variants spec))))

(defun %filter-tuple-governing-specs (mode specs tuple machine name)
  "SPECS (this TUPLE's own %PARSE-WORD-OPERAND-SUBCLAUSES result) with the
varying element's own governing hole positions (#120,
%VARYING-ELEMENT-HOLE-RANGE) filtered to TUPLE's own alternative(s) via
%FILTER-SPEC-VARIANTS-FOR-TUPLE -- every other position (a mode with no
varying element, or a hole outside the varying element's own range) is
returned unchanged. Signals a DEFINSTRUCTION-time error if filtering leaves
a governing spec with no variants at all -- TUPLE's own alternative has no
(choice ...) variant claiming it on that field."
  (multiple-value-bind (start base-count) (%varying-element-hole-range mode)
    (if (null start)
        specs
        (let* ((element (%pattern-varying-one-of-element (mode-descriptor-pattern mode)))
               (element-alt-names (rest element))
               (alt-name (mode-hole-tuple-alt-name tuple))
               (own-alt-names (if alt-name
                                  (list alt-name)
                                  (set-difference
                                   element-alt-names
                                   (remove-if (lambda (n) (= (%mode-hole-count (find-mode-descriptor n)) base-count))
                                              element-alt-names)))))
          (loop for spec in specs
                for i from 0
                collect (if (and (>= i start) (< i (+ start base-count)) (word-operand-spec-field spec))
                            (let ((filtered (%filter-spec-variants-for-tuple spec own-alt-names)))
                              (unless (word-operand-spec-variants filtered)
                                (error "DEFINSTRUCTION ~S ~S: field ~S has no (choice ~S) variant to ~
claim ~:[the base tuple's~;~:*~S's~] own ONE-OF alternative~P (~{~S~^, ~}) -- give it one on this field"
                                       machine name (word-operand-spec-field spec) (first own-alt-names)
                                       alt-name (length own-alt-names) own-alt-names))
                              filtered)
                            spec))))))

(defun %stamp-tuple-trailing-choices! (mode specs tuple)
  "Destructively stamp WORD-VARIANT-CHOICE on each :TRAILING-WORD spec's
sole variant, within TUPLE's own extra hole positions (#120's
%VARYING-ELEMENT-HOLE-RANGE), to TUPLE's own ALT-NAME -- there is no
(choice m) syntax on a fieldless hole to carry this; the (for-choice ALT
...) grouping that declared it already says which alternative it belongs
to. A no-op for the base tuple (ALT-NAME NIL, no extra holes at all).
Returns SPECS."
  (let ((alt-name (mode-hole-tuple-alt-name tuple)))
    (when alt-name
      (multiple-value-bind (start base-count) (%varying-element-hole-range mode)
        (loop for spec in specs
              for i from 0
              when (>= i (+ start base-count))
                do (dolist (v (word-operand-spec-variants spec))
                     (setf (word-variant-choice v) alt-name))))))
  specs)

(defun %word-mode-descriptor-forms (machine name mode-form opcode operand-subclauses mode mode-name machine-name
                                     cycles semantics-forms &optional layout-name field-value-subclauses
                                       for-choice-subclauses)
  "Every INSTRUCTION-DESCRIPTOR form for one word-encoded addressing-mode
use -- one per %EXPAND-WORD-COMBOS combo within one alternative-tuple
(mode.lisp's %MODE-HOLE-TUPLES, #120), across every tuple MODE's pattern
expands into, or a single no-operand descriptor if OPERAND-SUBCLAUSES is
empty and MODE has no holes. A mode with no varying ONE-OF element has
exactly one tuple, so this is byte-identical to the pre-#120 single-shape
expansion in that (overwhelmingly common) case. Unlike the byte-encoded
multi-mode form, a single-hole MODE may NOT omit (operand ...) here even
though the mode itself has only one hole to fill -- there is no
\"default field\" a word-encoded operand could fall back to the way a
byte-encoded one falls back to %MODE-OPERAND-WIDTH, so silently accepting
zero subclauses against a mode with holes would drop that hole's value on
the floor instead of encoding it anywhere. LAYOUT-NAME (#64) is this
instruction's own selected instruction-word layout (NIL for the machine's
default) -- resolved by the caller's (layout NAME) subclause parsing.
FIELD-VALUE-SUBCLAUSES (#136) is every (field-value FIELD-NAME n) subclause
given alongside OPERAND-SUBCLAUSES -- resolved into WORD-CONSTANTs and
stamped onto every combo of every tuple the same CONSTANTS-FORM, since a
pinned field doesn't vary by which value-range combo the mode's own operand
expanded into. FOR-CHOICE-SUBCLAUSES (#120) is every (for-choice ALT
(operand ...)...) subclause given alongside OPERAND-SUBCLAUSES, supplying
the extra holes an over-count alternative of MODE's varying ONE-OF element
(if it has one) contributes beyond OPERAND-SUBCLAUSES' own base count.

Returns (VALUES BINDINGS FORMS) (#150) -- BINDINGS is a LET* binding list
the caller must wrap FORMS in. CONSTANTS-FORM doesn't vary across the whole
mode (see above), so it is bound once, at mode scope, ahead of every
tuple's own bindings; ALTERNATIVES-FORM and %SEMANTICS-FN-FORM's own
expansion are each identical across every combo of one tuple (a combo
varies only which VARIANT half of each SPEC pair is chosen, never the
SPECS/OPERAND-NAMES/HOLE-ALTERNATIVES themselves), so each is built once
per tuple and bound to its own gensym, referenced from every combo's
descriptor instead of rebuilt -- and so re-emitted as compiled code -- once
per combo."
  (when (and (null operand-subclauses) (plusp (%mode-hole-count mode)))
    (error "DEFINSTRUCTION ~S ~S: addressing mode ~S has ~D EXPR hole~:P but no ~
(operand ...) subclause was given -- a word-encoded operand has no default ~
field to fall back to" machine name mode-name (%mode-hole-count mode)))
  (let* ((layout (instruction-word-layout-named
                   (machine-descriptor-instruction-word (find-machine-descriptor machine-name))
                   layout-name))
         (for-choice-alist (mapcar (lambda (fc)
                                      (multiple-value-bind (alt subs) (%parse-for-choice-subclause fc)
                                        (cons alt subs)))
                                    for-choice-subclauses))
         (operand-field-names (%operand-subclause-field-names
                                (append operand-subclauses (mapcan #'cdr for-choice-alist))))
         (constants (%parse-field-value-subclauses machine name mode-name layout layout-name
                                                     field-value-subclauses operand-field-names))
         (constants-form (%word-constants-form constants)))
    (%check-for-choice-subclauses! mode for-choice-alist machine name)
    (if (null operand-subclauses)
        (values nil
                (list `(make-instruction-descriptor
                        :name ,(string-upcase (symbol-name name))
                        :machine ',machine
                        :mode ,mode-form
                        :opcode ,opcode
                        :operand-widths nil
                        :operand-names nil
                        :word-fields nil
                        :word-alternatives nil
                        :extra-cells 0
                        :word-layout-name ',layout-name
                        :word-constants ,constants-form
                        :cycles ,cycles
                        :semantics-fn ,(%semantics-fn-form semantics-forms machine name nil nil))))
        ;; #120: two passes -- SPECS must be known for every tuple before
        ;; MODE-OPERAND-NAMES (the union of every tuple's own operand names)
        ;; can be computed, and every tuple's %SEMANTICS-FN-FORM needs that
        ;; union to bind an absent sibling-tuple name to NIL rather than
        ;; leaving a shared (semantics ...) body's reference to it unbound.
        (let* ((tuple-specs (mapcar
                              (lambda (tuple)
                                (let* ((tuple-hole-alternatives (mode-hole-tuple-hole-alternatives tuple))
                                       (tuple-subclauses (%tuple-operand-subclauses mode operand-subclauses tuple
                                                                                     for-choice-alist)))
                                  (cons tuple
                                        (%filter-tuple-governing-specs
                                         mode
                                         (%stamp-tuple-trailing-choices!
                                          mode
                                          (%parse-word-operand-subclauses mode tuple-subclauses machine name
                                                                           mode-name machine-name layout layout-name
                                                                           tuple-hole-alternatives)
                                          tuple)
                                         tuple machine name))))
                              (%mode-hole-tuples mode)))
               (mode-operand-names (remove-duplicates
                                     (mapcan (lambda (pair) (remove nil (mapcar #'word-operand-spec-name (cdr pair))))
                                             tuple-specs)
                                     :from-end t))
               (constants-gensym (gensym "WORD-CONSTANTS"))
               (bindings (list (list constants-gensym constants-form)))
               (forms nil))
          (loop for (tuple . specs) in tuple-specs
                for tuple-hole-alternatives = (mode-hole-tuple-hole-alternatives tuple)
                for hole-signedp-list = (%word-hole-signedp-list mode tuple-hole-alternatives
                                                                  (length tuple-hole-alternatives))
                for hole-relativep-list = (%word-hole-relativep-list mode tuple-hole-alternatives
                                                                      (length tuple-hole-alternatives))
                for operand-names = (mapcar #'word-operand-spec-name specs)
                do (%check-word-one-of-signed specs tuple-hole-alternatives machine name)
                   (%check-word-one-of-width tuple-hole-alternatives machine name)
                   (let* ((alternatives-form (%word-alternatives-form specs hole-signedp-list))
                          (alternatives-gensym (gensym "WORD-ALTERNATIVES"))
                          (semantics-fn-gensym (gensym "SEMANTICS-FN"))
                          (combos (%expand-word-combos specs)))
                     (%check-word-one-of-relative specs combos tuple-hole-alternatives hole-relativep-list
                                                   machine name)
                     (setf bindings (nconc bindings
                                            (list (list alternatives-gensym alternatives-form)
                                                  (list semantics-fn-gensym
                                                        (%semantics-fn-form semantics-forms machine name
                                                                             operand-names tuple-hole-alternatives
                                                                             mode-operand-names)))))
                     (setf forms (nconc forms
                                         (mapcar (lambda (combo)
                                                   (%word-descriptor-form machine name mode-form opcode
                                                                           alternatives-gensym combo cycles
                                                                           semantics-fn-gensym hole-signedp-list
                                                                           hole-relativep-list layout-name
                                                                           constants-gensym))
                                                 combos)))))
          (values bindings forms)))))

(defun %check-word-opcode (machine name opcode)
  "Signal an error if OPCODE doesn't fit MACHINE's instruction-word OPCODE
field. Registration keys the opcode table by this *declared* value
(REGISTER-INSTRUCTION-VARIANTS!), while %ENCODE-WORD-INSTRUCTION writes it
through WRAP-VALUE against the field's own width -- without this check, an
opcode too wide for its field would register under one value but encode (and
so decode) as a different, silently wrapped one, an ambiguity of exactly the
kind %CHECK-WORD-VARIANTS already guards against for operand fields.

Reads OPCODE's width off MACHINE's *default* layout alone -- correct
regardless of which (layout NAME) the instruction being checked will
eventually select, since machine.lisp validates every alternate's OPCODE
field identical in width and shift to the default's (#64)."
  (when (%word-machine-p machine)
    (let ((width (second (instruction-word-field (machine-descriptor-instruction-word
                                                    (find-machine-descriptor machine))
                                                  'opcode))))
      (when (or (minusp opcode) (>= opcode (ash 1 width)))
        (error "DEFINSTRUCTION ~S ~S: opcode ~D does not fit the ~D-bit OPCODE field"
               machine name opcode width)))))

(defun %one-of-signed-disagreement (hole-alternatives-list)
  "Hole-aligned list, one entry per HOLE-ALTERNATIVES-LIST -- NIL for a hole
not governed by any ONE-OF, or for a ONE-OF hole whose alternatives all
declare the same MODE-DESCRIPTOR-SIGNEDP; the hole's own alternative
mode-name symbols when they disagree, i.e. exactly the holes #124/#127's
per-hole :SIGNED needs a decode-time discriminator for. Alternatives that
agree need no discriminator at all -- the hole's signedness is static
regardless of which one matched, mode.lisp's %CHECK-ONE-OF-ELEMENTS! having
already ensured none of them declares a whole-mode :SUFFIX to disagree about
instead (:WIDTH, #129, and :RELATIVE, #130, may each disagree here just as
freely as :SIGNED does -- their own decode-time gates are
%CHECK-BYTE-ONE-OF-WIDTH and %CHECK-BYTE-ONE-OF-RELATIVE, below, each
entirely independent of this one; note MODE-DESCRIPTOR-SIGNEDP is itself
(OR RELATIVE SIGNED), so a hole whose alternatives disagree only on
:RELATIVE, not on a plain :SIGNED, already shows up as a disagreement
here too -- %CHECK-BYTE-ONE-OF-SIGNED and %CHECK-BYTE-ONE-OF-RELATIVE both
then require the same selector for it, which is harmless: satisfying one
satisfies both)."
  (mapcar (lambda (alts)
            (and alts
                 (rest (remove-duplicates (mapcar (lambda (m) (mode-descriptor-signedp (find-mode-descriptor m)))
                                                   alts)))
                 alts))
          hole-alternatives-list))

(defun %check-byte-one-of-signed (hole-alternatives-list sub-spec machine name)
  "Signal a DEFINSTRUCTION-time error unless every hole whose ONE-OF
alternatives disagree on signedness (#124's byte half) is one of the holes
SUB-SPEC (#126/#128, %RESOLVE-OPERAND-FIELDS) names as carrying a
sub-opcode selector -- a hole's own (variant (choice m) (sub s)), or its
membership in a (sub-opcode ...) table's participating holes -- that
selector is this scheme's only per-hole decode record, so it is the only
thing that can tell apart which alternative's signedness applies once bits
are on the wire. Any number of holes may carry one under #128's table, so
this is a set-membership test, not the single-index comparison #126's
one-hole restriction used to allow."
  (let ((carrying-indices (and sub-spec (car sub-spec))))
    (loop for alts in (%one-of-signed-disagreement hole-alternatives-list)
          for i from 0
          when (and alts (not (member i carrying-indices)))
            do (error "DEFINSTRUCTION ~S ~S: operand hole ~D's ONE-OF alternatives ~S ~
disagree on :SIGNED, but this hole carries no sub-opcode selector -- per-hole :SIGNED needs a ~
(variant (choice ...) (sub ...)) selector, alone or inside a (sub-opcode ...) table, as its ~
decode-time record of which alternative matched"
                      machine name i alts))))

(defun %one-of-width-disagreement (hole-alternatives-list)
  "Hole-aligned list, one entry per HOLE-ALTERNATIVES-LIST -- NIL for a hole
not governed by any ONE-OF, or for a ONE-OF hole whose alternatives all
declare the same MODE-DESCRIPTOR-WIDTH (including all-NIL, i.e. none of them
declares :WIDTH at all); the hole's own alternative mode-name symbols when
they disagree, i.e. exactly the holes #129's per-hole :WIDTH needs a
decode-time discriminator for. Alternatives that agree need no discriminator
at all -- the hole's width is static regardless of which one matched."
  (mapcar (lambda (alts)
            (and alts
                 (rest (remove-duplicates (mapcar (lambda (m) (mode-descriptor-width (find-mode-descriptor m)))
                                                   alts)))
                 alts))
          hole-alternatives-list))

(defun %check-byte-one-of-width (hole-alternatives-list sub-spec mode-specified machine name)
  "Byte-encoded analogue of %CHECK-BYTE-ONE-OF-SIGNED, for #129's per-hole
:WIDTH. Signal a DEFINSTRUCTION-time error for a hole whose ONE-OF
alternatives disagree on :WIDTH, is MODE-SPECIFIED (hole-aligned, T when
that hole's (operand ...) subclause was (operand :mode) rather than an
explicit (operand :width n), %PARSE-OPERAND-SUBCLAUSES), and carries no
sub-opcode selector -- the same decode-time record :SIGNED needs, since
which alternative was written is otherwise unrecoverable once bits are on
the wire. A hole given an explicit (operand :width n) is exempt regardless
of whether its alternatives disagree: an explicit :WIDTH is the author
naming a width directly, always wins over a matched alternative's own
:WIDTH (%BYTE-OPERAND-WIDTHS), and so needs no per-hole record at all."
  (let ((carrying-indices (and sub-spec (car sub-spec))))
    (loop for alts in (%one-of-width-disagreement hole-alternatives-list)
          for specified in mode-specified
          for i from 0
          when (and alts specified (not (member i carrying-indices)))
            do (error "DEFINSTRUCTION ~S ~S: operand hole ~D's ONE-OF alternatives ~S ~
disagree on :WIDTH, but this hole carries no sub-opcode selector -- per-hole :WIDTH needs a ~
(variant (choice ...) (sub ...)) selector, alone or inside a (sub-opcode ...) table, as its ~
decode-time record of which alternative matched"
                      machine name i alts))))

(defun %one-of-relative-disagreement (hole-alternatives-list)
  "Hole-aligned list, one entry per HOLE-ALTERNATIVES-LIST -- NIL for a hole
not governed by any ONE-OF, or for a ONE-OF hole whose alternatives all
declare the same MODE-DESCRIPTOR-RELATIVEP; the hole's own alternative
mode-name symbols when they disagree, i.e. exactly the holes #130's per-hole
:RELATIVE needs a decode-time discriminator for. Tests RELATIVEP directly,
not SIGNEDP -- two alternatives can both be (plain, non-relative) :SIGNED,
agreeing on SIGNEDP and so invisible to %ONE-OF-SIGNED-DISAGREEMENT, while
still disagreeing on RELATIVEP, which is what actually governs whether
%RELATIVE-OFFSET's PC-relative arithmetic applies to this hole's value.
Alternatives that agree need no discriminator at all -- the hole's
relativeness is static regardless of which one matched."
  (mapcar (lambda (alts)
            (and alts
                 (rest (remove-duplicates (mapcar (lambda (m) (mode-descriptor-relativep (find-mode-descriptor m)))
                                                   alts)))
                 alts))
          hole-alternatives-list))

(defun %check-byte-one-of-relative (mode hole-alternatives-list sub-spec machine name)
  "Byte-encoded analogue of %CHECK-BYTE-ONE-OF-SIGNED, for #130's per-hole
:RELATIVE, plus the one rule :SIGNED/:WIDTH have no analogue for: :RELATIVE
is positional, not a per-hole boolean, so beyond the selector requirement
every disagreeing hole shares with :SIGNED/:WIDTH, at most one hole of any
one expanded sibling descriptor may ever resolve relative. Selector rule
first (mirrors %CHECK-BYTE-ONE-OF-SIGNED exactly): a hole whose ONE-OF
alternatives disagree on :RELATIVE must carry a sub-opcode selector, since
that is the only decode-time record of which alternative matched. Positional
rule second: walks the same SUB-SPEC pairs %BYTE-DESCRIPTOR-FORMS will
expand into descriptors (or the single no-SUB-SPEC case), and for each one
counts %BYTE-RELATIVE-FLAGS' T entries -- more than one means this sibling's
own operand would need its PC-relative offset applied to two different
holes at once, which %RELATIVE-OFFSET has no way to do."
  (let ((carrying-indices (and sub-spec (car sub-spec)))
        (n (length hole-alternatives-list)))
    (loop for alts in (%one-of-relative-disagreement hole-alternatives-list)
          for i from 0
          when (and alts (not (member i carrying-indices)))
            do (error "DEFINSTRUCTION ~S ~S: operand hole ~D's ONE-OF alternatives ~S ~
disagree on :RELATIVE, but this hole carries no sub-opcode selector -- per-hole :RELATIVE needs a ~
(variant (choice ...) (sub ...)) selector, alone or inside a (sub-opcode ...) table, as its ~
decode-time record of which alternative matched"
                      machine name i alts))
    (flet ((check-sibling (sub-choices)
             (when (> (count t (%byte-relative-flags mode hole-alternatives-list sub-choices n)) 1)
               (error "DEFINSTRUCTION ~S ~S: more than one operand hole resolves to a :RELATIVE ~
alternative for the same addressing-mode use -- a RELATIVE operand's offset applies to one hole ~
only, so at most one hole may ever be the relative one"
                      machine name))))
      (if (null sub-spec)
          (check-sibling nil)
          (destructuring-bind (hole-indices . pairs) sub-spec
            (dolist (pair pairs)
              (let ((sub-choices (make-list n :initial-element nil)))
                (loop for idx in hole-indices
                      for chosen-name in (car pair)
                      do (setf (nth idx sub-choices) chosen-name))
                (check-sibling sub-choices))))))))

(defun %check-word-one-of-signed (specs hole-alternatives-list machine name)
  "Word-encoded analogue of %CHECK-BYTE-ONE-OF-SIGNED (#127): signal a
DEFINSTRUCTION-time error unless every hole whose ONE-OF alternatives
disagree on signedness has every one of its field variants CHOICE-selected
(WORD-VARIANT-CHOICE non-NIL) by the time %PARSE-WORD-OPERAND-SUBCLAUSES
returns SPECS -- %CHECK-WORD-VARIANT-CHOICES! (above) has already run by
then, so a mixed field's value-selected variants carry the one alternative
left unclaimed by its CHOICE-selected siblings; a variant with no CHOICE at
all (a field with no CHOICE variant whatsoever) leaves decode with no record
of which alternative a raw value came from, so that case is what this
rejects."
  (loop for alts in (%one-of-signed-disagreement hole-alternatives-list)
        for spec in specs
        for i from 0
        when (and alts (notevery #'word-variant-choice (word-operand-spec-variants spec)))
          do (error "DEFINSTRUCTION ~S ~S: operand hole ~D's ONE-OF alternatives ~S ~
disagree on :SIGNED, but not every field variant at that hole is CHOICE-selected -- ~
per-hole :SIGNED needs a (choice m) selector on every variant as its decode-time record ~
of which alternative matched" machine name i alts)))

(defun %check-word-one-of-width (hole-alternatives-list machine name)
  "Per-hole :WIDTH (#129) is permanently, intentionally out of scope on a
word-encoded machine -- OPERAND-WIDTHS is always NIL there since operand
sizes come from word fields, so a per-hole :WIDTH has nothing to mean.
Signal a DEFINSTRUCTION-time error, naming MACHINE/NAME, if any ONE-OF
hole's alternatives declare :WIDTH at all -- agreeing or not, since even an
agreeing declaration is meaningless here (unlike the byte-encoded path, where
it constrains OPERAND-WIDTHS whether or not the alternatives disagree) --
keeping docs/modes.md's \"permanently out of scope\" true by erroring loudly
rather than silently ignoring an inert declaration."
  (loop for alts in hole-alternatives-list
        for i from 0
        when (and alts (some (lambda (m) (mode-descriptor-width (find-mode-descriptor m))) alts))
          do (error "DEFINSTRUCTION ~S ~S: operand hole ~D's ONE-OF alternatives ~S declare ~
:WIDTH, but per-hole :WIDTH is permanently out of scope on word-encoded machine ~S -- operand ~
sizes come from word fields, not OPERAND-WIDTHS, which is always NIL there"
                      machine name i alts machine)))

(defun %word-relative-flags (combo hole-relativep-list)
  "Hole-aligned list of booleans, one per COMBO pair (a %EXPAND-WORD-COMBOS
combo, (SPEC . VARIANT) pairs in hole order) -- entry I is T when hole I
resolves to a :RELATIVE operand for this specific combo. A variant with its
own (CHOICE m) selector reads M's own MODE-DESCRIPTOR-RELATIVEP directly
(word-machine analogue of %BYTE-RELATIVE-FLAGS' CHOSEN branch); a
value-selected variant (no CHOICE) falls back to HOLE-RELATIVEP-LIST's own
entry (%WORD-HOLE-RELATIVEP-LIST, above) -- the ALTS/ungoverned branches
%BYTE-RELATIVE-FLAGS also has, precomputed once per hole rather than
per-combo since they don't depend on which variant this combo picked."
  (loop for (nil . variant) in combo
        for hole-relativep in hole-relativep-list
        for choice = (word-variant-choice variant)
        collect (if choice
                    (mode-descriptor-relativep (find-mode-descriptor choice))
                    hole-relativep)))

(defun %word-relative-hole-index (combo hole-relativep-list)
  "This COMBO's own RELATIVE-HOLE-INDEX (#62) -- the 0-based index of the
one hole whose operand is a PC-relative offset, or NIL if none is. Built on
%WORD-RELATIVE-FLAGS, mirroring %BYTE-RELATIVE-HOLE-INDEX exactly;
%CHECK-WORD-ONE-OF-RELATIVE's positional rule (below) guarantees at most one
flag is ever T here, so POSITION's first match is the only one there could
be."
  (position t (%word-relative-flags combo hole-relativep-list)))

(defun %check-word-one-of-relative (specs combos hole-alternatives-list hole-relativep-list machine name)
  "Word-encoded analogue of %CHECK-BYTE-ONE-OF-RELATIVE (#62, replacing the
old unconditional word-machine :RELATIVE ban): two rules, mirroring
%CHECK-WORD-ONE-OF-SIGNED and %CHECK-BYTE-ONE-OF-RELATIVE respectively.

Selector rule first (identical in shape to %CHECK-WORD-ONE-OF-SIGNED): a
hole whose ONE-OF alternatives disagree on :RELATIVE must have every one of
its field variants CHOICE-selected -- that is the only decode-time record of
which alternative matched, without which a raw fetched value could never be
told apart from an ordinary (non-relative) sibling.

Positional rule second (mirrors %CHECK-BYTE-ONE-OF-RELATIVE): walks every
COMBO %EXPAND-WORD-COMBOS built and counts %WORD-RELATIVE-FLAGS' T entries --
more than one means this sibling descriptor's own operand would need its
PC-relative offset applied to two different holes at once, which
%RELATIVE-OFFSET has no way to do."
  (loop for alts in (%one-of-relative-disagreement hole-alternatives-list)
        for spec in specs
        for i from 0
        when (and alts (notevery #'word-variant-choice (word-operand-spec-variants spec)))
          do (error "DEFINSTRUCTION ~S ~S: operand hole ~D's ONE-OF alternatives ~S ~
disagree on :RELATIVE, but not every field variant at that hole is CHOICE-selected -- ~
per-hole :RELATIVE needs a (choice m) selector on every variant as its decode-time record ~
of which alternative matched" machine name i alts))
  (dolist (combo combos)
    (when (> (count t (%word-relative-flags combo hole-relativep-list)) 1)
      (error "DEFINSTRUCTION ~S ~S: more than one operand hole resolves to a :RELATIVE ~
alternative for the same addressing-mode use -- a RELATIVE operand's offset applies to one hole ~
only, so at most one hole may ever be the relative one"
             machine name))))

(defun %parse-layout-subclause (machine name context layout-subclause)
  "Parse an optional (layout NAME) subclause (#64) -- the same shape at all
three DEFINSTRUCTION sites that accept one (the multi-mode (modes ...)
form's per-variant body, and the single-mode sugar's (encoding ...) form).
Returns the layout NAME symbol, or NIL for the machine's default layout when
LAYOUT-SUBCLAUSE is NIL (absent). CONTEXT is the enclosing mode name, or NIL
outside a multi-mode variant, folded into error messages via ~~@[~~S ~~]a
skips it when NIL. Signals an error when a (layout ...) is given on a
byte-encoded machine -- there is only ever one, unnamed encoding there -- or
when it names a layout the machine's instruction-word clause does not
declare (INSTRUCTION-WORD-LAYOUT-NAMED)."
  (when layout-subclause
    (unless (%word-machine-p machine)
      (error "DEFINSTRUCTION ~S ~S~@[ ~S~]: (layout ...) is only meaningful on a ~
word-encoded machine (#64) -- ~S declares no instruction-word clause"
             machine name context machine))
    (destructuring-bind (layout-name) (rest layout-subclause)
      (unless (instruction-word-layout-named
               (machine-descriptor-instruction-word (find-machine-descriptor machine))
               layout-name)
        (error "DEFINSTRUCTION ~S ~S~@[ ~S~]: no instruction-word layout named ~S on machine ~S"
               machine name context layout-name machine))
      layout-name)))

(defun %operand-subclause-field-names (operand-subclauses)
  "The instruction-word field name each of OPERAND-SUBCLAUSES' (operand
[name] :field F ...) forms targets, in declaration order -- used only so
%PARSE-FIELD-VALUE-SUBCLAUSES (#136) can reject a (field-value ...) that
collides with a field an ordinary operand hole here already claims. NIL
entries (a subclause not shaped like a word operand at all) are dropped
rather than erroring here -- %PARSE-WORD-OPERAND-SUBCLAUSE is the actual
validator for OPERAND-SUBCLAUSES' own shape."
  (remove nil (mapcar (lambda (s) (multiple-value-bind (op-name spec) (%parse-operand-subclause s)
                                     (declare (ignore op-name))
                                     (and (eq (first spec) :field) (second spec))))
                       operand-subclauses)))

(defun %parse-field-value-subclause (machine name context layout layout-name subclause)
  "Parse one (field-value FIELD-NAME n) encoding subclause (#136) -- a field
pinned to a literal with no operand hole at all, discriminating opcode
families that share their OPCODE field (CHIP8's 8XY0-8XYE, 5XY0/9XY0,
EX9E/EXA1, FX__, 00E0/00EE). FIELD-NAME resolves within LAYOUT -- the
instruction's own selected instruction-word layout (the machine's default,
or a (layout NAME) alternate) -- exactly as an (operand ... :field F) hole
resolves it (%PARSE-WORD-OPERAND-SUBCLAUSE), so an unknown field name is
reported identically either way.

Rejects OPCODE as the target field outright: (opcode n) already owns it, and
letting (field-value ...) also write it would silently corrupt the
already-placed opcode -- the same hazard #138 tracks for a stray
(operand ... :field opcode). Rejects a literal outside FIELD-NAME's own
[0, 2^width-1] range: registration and %CHECK-OPCODE-DECODABLE! both key off
this declared VALUE while %ENCODE-WORD-INSTRUCTION writes it through
WRAP-VALUE, so an over-wide constant would register under one value and
encode a different, silently wrapped one -- the same rationale as
%CHECK-WORD-OPCODE for the OPCODE field itself."
  (destructuring-bind (field-value-kw field-name value) subclause
    (declare (ignore field-value-kw))
    (when (eq field-name 'opcode)
      (error "DEFINSTRUCTION ~S ~S~@[ ~S~]: (field-value opcode ...) is not allowed -- the ~
opcode field is already given by this instruction's own (opcode n)" machine name context))
    (let ((field (instruction-word-field layout field-name)))
      (unless field
        (error "DEFINSTRUCTION ~S ~S~@[ ~S~]: no field named ~S in ~:[the default instruction-word ~
layout~;instruction-word layout ~:*~S~] on machine ~S"
               machine name context field-name layout-name machine))
      (destructuring-bind (fname fwidth fshift) field
        (declare (ignore fname))
        (when (or (minusp value) (> value (1- (ash 1 fwidth))))
          (error "DEFINSTRUCTION ~S ~S~@[ ~S~]: (field-value ~S ~D) does not fit its ~D-bit field"
                 machine name context field-name value fwidth))
        (make-word-constant :name field-name :width fwidth :shift fshift :value value)))))

(defun %parse-field-value-subclauses (machine name context layout layout-name field-value-subclauses
                                       operand-field-names)
  "Parse every (field-value ...) subclause (#136) on one word-encoded
instruction variant into a list of WORD-CONSTANT. Signals an error when
FIELD-VALUE-SUBCLAUSES is non-empty on a byte-encoded machine -- #136's
constant-discriminator field is a word-machine-only mechanism, with
(opcode n :sub s) (#125) as its byte-machine analogue -- before touching
LAYOUT at all, which is meaningless there. Also signals an error for two
field-value subclauses naming the same field, or a field-value naming a
field OPERAND-FIELD-NAMES says an (operand ... :field F) hole here already
claims -- both would OR two different values into the same bits, silently
corrupting whichever one loses."
  (when (and field-value-subclauses (not (%word-machine-p machine)))
    (error "DEFINSTRUCTION ~S ~S~@[ ~S~]: (field-value ...) is a word-encoded-machine-only ~
mechanism (#136) -- ~S declares no instruction-word clause; a byte-encoded machine's analogue ~
is (opcode n :sub s) (#125)" machine name context machine))
  (let ((constants (mapcar (lambda (s) (%parse-field-value-subclause machine name context layout layout-name s))
                            field-value-subclauses)))
    (let ((dup (loop for (c . later) on constants
                      when (find (word-constant-name c) later :key #'word-constant-name)
                        return (word-constant-name c))))
      (when dup
        (error "DEFINSTRUCTION ~S ~S~@[ ~S~]: (field-value ~S ...) is given more than once"
               machine name context dup)))
    (dolist (c constants)
      (when (member (word-constant-name c) operand-field-names)
        (error "DEFINSTRUCTION ~S ~S~@[ ~S~]: (field-value ~S ...) names the same field an ~
(operand ... :field ~S) subclause already uses -- a field may be pinned to a constant or ~
carry an operand, not both" machine name context (word-constant-name c) (word-constant-name c))))
    constants))

(defun %parse-opcode-subclause (machine name opcode-subclause)
  "Parse one (opcode n [:sub s]) subclause -- the same shape at all three
DEFINSTRUCTION sites that accept one (the multi-mode (modes ...) form, the
no-mode (encoding ...) form, and the single-mode sugar's (encoding ...) form)
-- into (VALUES opcode sub), SUB NIL when no :SUB was given. #125's byte-machine
sub-opcode cell: SUB reserves the cell right after OPCODE as a second,
purely discriminating value, letting several DESCRIPTORs coexist at one
byte-machine OPCODE (REGISTER-INSTRUCTION-VARIANTS!) the way a word-encoded
machine's operand fields already let them. Signals an error for any plist key
other than :SUB, or for a :SUB value that's negative or doesn't fit MACHINE's
code cell width (%MACHINE-CELL-WIDTH) -- an out-of-range sub-opcode would
register under one value but wrap to a different one when %ENCODE-VALUE-CELLS
writes it, same rationale as %CHECK-WORD-OPCODE for the word-encoded OPCODE
field itself. Also signals an error when :SUB is given on a word-encoded
machine (%WORD-MACHINE-P) -- #125's sub-opcode cell is a byte-machine-only
mechanism; a word-encoded machine already has %CHECK-OPCODE-DECODABLE!'s
per-field discrimination and has no use for a second, separate cell."
  (destructuring-bind (opcode &rest plist) (rest opcode-subclause)
    (loop for key in plist by #'cddr
          unless (eq key :sub)
            do (error "DEFINSTRUCTION ~S ~S: unknown (opcode ...) option ~S" machine name key))
    (let ((sub (getf plist :sub)))
      (when sub
        (when (%word-machine-p machine)
          (error "DEFINSTRUCTION ~S ~S: (opcode ~S :sub ~S) -- a sub-opcode is a ~
byte-machine-only mechanism (#125), not supported on word-encoded machine ~S"
                 machine name opcode sub machine))
        (let ((width (%machine-cell-width machine)))
          (when (or (minusp sub) (>= sub (ash 1 width)))
            (error "DEFINSTRUCTION ~S ~S: sub-opcode ~D does not fit machine ~S's ~D-bit code cell"
                   machine name sub machine width))))
      (values opcode sub))))

(defun %parse-mode-variant-clause-forms (variant-form machine name default-semantics-forms cycles-form)
  "VARIANT-FORM is one element of a multi-mode (modes ...) clause:
(MODE-NAME (opcode n [:sub s]) (operand ...)* [(semantics form...)] [(cycles n)]).
Returns (VALUES BINDINGS FORMS) (#150) for this variant, forwarded unchanged
from whichever of %WORD-MODE-DESCRIPTOR-FORMS/%BYTE-DESCRIPTOR-FORMS below
produced it -- FORMS has more than one INSTRUCTION-DESCRIPTOR form on a
word-encoded machine (#20), where a variant-bearing operand field expands
into several descriptors sharing this one mode/opcode, and likewise on a
byte-encoded machine (#126/#128) when an (operand ...) subclause here
carries a hole-selected (variant (choice m) (sub s)) sub-opcode selector, or
a (sub-opcode ...) table subclause names several -- one descriptor per
combination claimed (%BYTE-DESCRIPTOR-FORMS). :SUB (#125, byte-machine-only)
is this variant's own explicit sub-opcode, letting it share its OPCODE with
another mode's own :SUB-bearing variant -- see %PARSE-OPCODE-SUBCLAUSE; it
may not be combined with a hole-selected selector on the same variant
(%CHECK-BYTE-SUB-CONFLICT!).

#75: a variant's own (cycles n) subclause overrides the shared top-level
CYCLES-FORM for this mode alone -- e.g. a zero-page mode costing less than
its absolute-mode sibling."
  (destructuring-bind (mode-sym &rest body) variant-form
    (let* ((mode (find-mode-descriptor mode-sym))
           (opcode-subclause (find 'opcode body :key #'first))
           (operand-subclauses (remove-if-not (lambda (c) (eq (first c) 'operand)) body))
           ;; #136: repeatable, like OPERAND-SUBCLAUSES above -- REMOVE-IF-NOT,
           ;; not FIND, or every field-value but the first would silently vanish.
           (field-value-subclauses (remove-if-not (lambda (c) (eq (first c) 'field-value)) body))
           ;; #120: repeatable, same reason -- more than one alternative of a
           ;; varying ONE-OF may each need its own (for-choice ...) group.
           (for-choice-subclauses (remove-if-not (lambda (c) (eq (first c) 'for-choice)) body))
           (sub-opcode-subclause (find 'sub-opcode body :key #'first))
           (layout-subclause (find 'layout body :key #'first))
           (semantics-subclause (find 'semantics body :key #'first))
           ;; NOTE (#92): like OPCODE-SUBCLAUSE/OPERAND-SUBCLAUSES/SUB-OPCODE-
           ;; SUBCLAUSE/SEMANTICS-SUBCLAUSE above, this FINDs known subclause
           ;; heads out of BODY and silently drops anything unrecognized -- a
           ;; typo'd (cycle 2) vanishes with no error. Pre-existing, not
           ;; specific to CYCLES; #92 tracks rejecting unknown subclauses
           ;; here instead.
           (cycles-subclause (find 'cycles body :key #'first)))
      (%check-mode-hole-attributes mode machine name)
      (unless opcode-subclause
        (error "DEFINSTRUCTION ~S ~S: mode ~S requires an (opcode n) subclause"
               machine name mode-sym))
      (when (and sub-opcode-subclause (%word-machine-p machine))
        (error "DEFINSTRUCTION ~S ~S: mode ~S: (sub-opcode ...) is a byte-machine-only ~
mechanism (#128), not supported on word-encoded machine ~S" machine name mode-sym machine))
      (when (and field-value-subclauses (not (%word-machine-p machine)))
        (error "DEFINSTRUCTION ~S ~S: mode ~S: (field-value ...) is a word-encoded-machine-only ~
mechanism (#136), not supported on byte-encoded machine ~S -- see (opcode n :sub s) (#125)"
               machine name mode-sym machine))
      (let ((layout-name (%parse-layout-subclause machine name mode-sym layout-subclause)))
        (multiple-value-bind (opcode sub) (%parse-opcode-subclause machine name opcode-subclause)
        (let ((cycles-form (if cycles-subclause (second cycles-subclause) cycles-form))
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
                                            cycles-form semantics-forms layout-name
                                            field-value-subclauses for-choice-subclauses)
              ;; #124/#127: the word path's own gate runs inside
              ;; %WORD-MODE-DESCRIPTOR-FORMS itself (unlike the byte path's,
              ;; called here) -- it needs SPECS, which only that function
              ;; computes, and there is exactly one call site for it, unlike
              ;; %BYTE-DESCRIPTOR-FORMS' two.
              (multiple-value-bind (operand-widths operand-names sub-spec mode-specified operand-registers)
                  (progn
                    (%check-no-varying-one-of! mode machine name)
                    (%resolve-operand-fields mode operand-subclauses machine name mode-sym machine
                                              sub-opcode-subclause))
                (%check-byte-one-of-signed (%mode-hole-alternatives mode) sub-spec machine name)
                (%check-byte-one-of-width (%mode-hole-alternatives mode) sub-spec mode-specified machine name)
                (%check-byte-one-of-relative mode (%mode-hole-alternatives mode) sub-spec machine name)
                (%byte-descriptor-forms machine name `(find-mode-descriptor ',mode-sym)
                                         opcode sub operand-widths operand-names cycles-form semantics-forms
                                         (%mode-hole-alternatives mode) sub-spec mode mode-specified
                                         operand-registers)))))))))

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

On a byte-encoded machine, an (operand ...) subclause whose hole came from a
ONE-OF pattern element (mode.lisp) may append one or more
  (variant (choice MODE) (sub S))
forms (#126) -- the byte-machine analogue of the word-encoded (choice MODE)
selector below: MODE must be one of that hole's own ONE-OF alternatives, and
S becomes the instruction's sub-opcode cell (#125's (opcode n :sub s), but
chosen by which alternative the hole actually matched rather than written
once for the whole mode). Every alternative of the carrying hole must be
claimed by exactly one such variant -- unlike the word-encoded (choice MODE)
below, there is no value-selected fallback for one left unclaimed, so partial
coverage is a DEFINSTRUCTION-time error, not a runtime gap. At most one
operand hole per mode may carry these selectors, and an explicit
(opcode n :sub s) may not be combined with one on the same mode -- both would
be trying to write the same cell. DEFINSTRUCTION then registers one
INSTRUCTION-DESCRIPTOR per claimed alternative, sharing the mnemonic, mode,
and opcode, each with its own SUB-OPCODE and a hole-aligned SUB-CHOICES
record naming that alternative -- the same mechanism #125's plain :SUB uses
to let several descriptors coexist at one byte-machine opcode, but selected
per operand hole rather than per whole (MODES ...) clause. Once assembled,
decode (%DECODE-CELL-INSTRUCTION, decoder.lisp) reads the sub-opcode cell
back and reports the matched descriptor's SUB-CHOICES as DECODE-INSTRUCTION-
AT's own CHOICES -- so CHOICE-CASE (below) and disassembly both see which
alternative was actually written, exactly as they already do on a
word-encoded machine.

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
:RELATIVE addressing mode is not supported on a word-encoded machine.

CHOICE-CASE (#73), usable inside any (semantics ...) body alongside SET!/
PUSH/POP/SET-FLAGS!/TRAP, dispatches on which ONE-OF alternative an operand
hole actually matched -- the piece the CHOICE-selected word fields above
deliberately leave open, since every sibling descriptor one (choice ...)
combination expands into still shares one semantics body:

  (choice-case NAME
    (MODE-OR-MODES form...)
    ...
    [(otherwise form...)])

NAME is an operand field name from this variant's (operand ...) subclauses,
or the symbol OPERAND for the first field (mirroring the existing OPERAND
binding), even when that field also has its own name. Each clause's key is
one mode-name symbol or a list of them (as CL:CASE); every key must be one
of NAME's hole's own ONE-OF alternatives, checked here at DEFINSTRUCTION
time -- unless NAME's hole isn't governed by any ONE-OF at all (e.g. this
(semantics ...) is a multi-mode instruction's shared default and some other
mode routes the same field name through a plain EXPR hole instead), in which
case no key can ever be validated against anything and the check is skipped.
At runtime, CHOICE-CASE reads EXECUTE-INSTRUCTION's CHOICES argument (see
below) for NAME's hole and dispatches like CL:CASE; with no OTHERWISE clause,
a hole matching none of the given keys -- including a hole with no recorded
choice at all, e.g. a cell-encoded machine's hole with no hole-selected
(variant (choice ...) (sub ...)) selector of its own (#126) -- signals
NO-MATCHING-CHOICE rather than silently falling through."
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
                (operand-subclause (find 'operand (rest encoding-clause) :key #'first))
                (layout-subclause (find 'layout (rest encoding-clause) :key #'first))
                ;; #136: repeatable, like OPERAND-SUBCLAUSES elsewhere --
                ;; REMOVE-IF-NOT, not FIND.
                (field-value-subclauses (remove-if-not (lambda (c) (eq (first c) 'field-value))
                                                        (rest encoding-clause))))
           (unless opcode-subclause
             (error "DEFINSTRUCTION ~S ~S: (encoding ...) requires an (opcode n) subclause"
                    machine name))
           (when operand-subclause
             (error "DEFINSTRUCTION ~S ~S: (encoding ...) has an (operand ...) subclause ~
but no (modes ...) clause declares an addressing mode" machine name))
           ;; #64/#136: a no-operand instruction has no field an ordinary
           ;; operand hole could resolve, and every layout shares one OPCODE
           ;; field, so naming a non-default layout here says nothing *unless*
           ;; a (field-value ...) also pins one of that layout's other
           ;; fields -- reject only the bare case, since the other genuinely
           ;; needs the layout to know which fields exist to pin (CLS/RET-
           ;; shaped CHIP8 opcodes, e.g. 00E0/00EE, need exactly this).
           (when (and layout-subclause (null field-value-subclauses))
             (error "DEFINSTRUCTION ~S ~S: (encoding ...) has a (layout ...) subclause ~
but no (modes ...) clause declares an addressing mode and no (field-value ...) pins a field ~
in it -- a no-operand instruction with nothing to pin has no field to resolve, so naming a ~
layout has no effect" machine name))
           (let ((layout-name (%parse-layout-subclause machine name nil layout-subclause)))
             (multiple-value-bind (opcode sub) (%parse-opcode-subclause machine name opcode-subclause)
               (%check-word-opcode machine name opcode)
               (let* ((layout (and (%word-machine-p machine)
                                    (instruction-word-layout-named
                                     (machine-descriptor-instruction-word (find-machine-descriptor machine))
                                     layout-name)))
                      (constants (%parse-field-value-subclauses machine name nil layout layout-name
                                                                  field-value-subclauses nil))
                      (constants-form (%word-constants-form constants)))
                 `(eval-when (:compile-toplevel :load-toplevel :execute)
                    (register-instruction-variants!
                     ',machine
                     (list ,(%descriptor-form machine name nil opcode nil nil
                                               cycles-form
                                               (%semantics-fn-form (rest semantics-clause) machine name nil nil)
                                               sub nil nil nil layout-name constants-form)))
                    ',name))))))
        ;; Multi-mode form: (modes (MODE ...) (MODE ...) ...).
        ((consp (first mode-forms))
         (when encoding-clause
           (error "DEFINSTRUCTION ~S ~S: a multi-mode (modes ...) clause gives ~
each mode its own (opcode n) -- a top-level (encoding ...) clause is not allowed"
                  machine name))
         (unless (rest mode-forms)
           (error "DEFINSTRUCTION ~S ~S: a multi-mode (modes ...) clause needs ~
at least two modes -- use (modes MODE) with (encoding ...) for just one" machine name))
         (let ((default-semantics-forms (and semantics-clause (rest semantics-clause)))
               (all-bindings nil)
               (all-forms nil))
           ;; #150: each mode's own BINDINGS/FORMS accumulate separately --
           ;; one shared LET* below wraps every mode's descriptors, so a
           ;; sibling descriptor's semantics/alternatives/constants form is
           ;; compiled once per tuple/mode rather than once per descriptor.
           (dolist (variant-form mode-forms)
             (multiple-value-bind (bindings forms)
                 (%parse-mode-variant-clause-forms variant-form machine name
                                                    default-semantics-forms cycles-form)
               (setf all-bindings (nconc all-bindings bindings))
               (setf all-forms (nconc all-forms forms))))
           `(eval-when (:compile-toplevel :load-toplevel :execute)
              (register-instruction-variants!
               ',machine
               (let* (,@all-bindings) (list ,@all-forms)))
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
                                                    (rest encoding-clause)))
                ;; #136: repeatable, like OPERAND-SUBCLAUSES above --
                ;; REMOVE-IF-NOT, not FIND.
                (field-value-subclauses (remove-if-not (lambda (c) (eq (first c) 'field-value))
                                                        (rest encoding-clause)))
                ;; #120: repeatable, same reason as FIELD-VALUE-SUBCLAUSES.
                (for-choice-subclauses (remove-if-not (lambda (c) (eq (first c) 'for-choice))
                                                       (rest encoding-clause)))
                (sub-opcode-subclause (find 'sub-opcode (rest encoding-clause) :key #'first))
                (layout-subclause (find 'layout (rest encoding-clause) :key #'first)))
           (%check-mode-hole-attributes mode machine name)
           (unless opcode-subclause
             (error "DEFINSTRUCTION ~S ~S: (encoding ...) requires an (opcode n) subclause"
                    machine name))
           (unless operand-subclauses
             (error "DEFINSTRUCTION ~S ~S: (modes ~A) declares an addressing mode but ~
(encoding ...) has no (operand ...) subclause" machine name mode-sym))
           (when (and sub-opcode-subclause (%word-machine-p machine))
             (error "DEFINSTRUCTION ~S ~S: (sub-opcode ...) is a byte-machine-only mechanism ~
(#128), not supported on word-encoded machine ~S" machine name machine))
           (when (and field-value-subclauses (not (%word-machine-p machine)))
             (error "DEFINSTRUCTION ~S ~S: (field-value ...) is a word-encoded-machine-only ~
mechanism (#136), not supported on byte-encoded machine ~S -- see (opcode n :sub s) (#125)"
                    machine name machine))
           (let ((layout-name (%parse-layout-subclause machine name nil layout-subclause)))
           (multiple-value-bind (opcode sub) (%parse-opcode-subclause machine name opcode-subclause)
             (%check-word-opcode machine name opcode)
             (if (%word-machine-p machine)
                 (multiple-value-bind (bindings forms)
                     (%word-mode-descriptor-forms machine name `(find-mode-descriptor ',mode-sym)
                                                   opcode operand-subclauses
                                                   mode mode-sym machine
                                                   cycles-form (rest semantics-clause) layout-name
                                                   field-value-subclauses for-choice-subclauses)
                   `(eval-when (:compile-toplevel :load-toplevel :execute)
                      (register-instruction-variants!
                       ',machine
                       (let* (,@bindings) (list ,@forms)))
                      ',name))
                 (multiple-value-bind (operand-widths operand-names sub-spec mode-specified operand-registers)
                     (progn
                       (%check-no-varying-one-of! mode machine name)
                       (%parse-operand-subclauses mode operand-subclauses machine name mode-sym machine
                                                   sub-opcode-subclause))
                   (%check-byte-one-of-signed (%mode-hole-alternatives mode) sub-spec machine name)
                   (%check-byte-one-of-width (%mode-hole-alternatives mode) sub-spec mode-specified machine name)
                   (%check-byte-one-of-relative mode (%mode-hole-alternatives mode) sub-spec machine name)
                   (multiple-value-bind (bindings forms)
                       (%byte-descriptor-forms machine name `(find-mode-descriptor ',mode-sym)
                                                opcode sub operand-widths operand-names
                                                cycles-form (rest semantics-clause)
                                                (%mode-hole-alternatives mode) sub-spec mode
                                                mode-specified operand-registers)
                     `(eval-when (:compile-toplevel :load-toplevel :execute)
                        (register-instruction-variants!
                         ',machine
                         (let* (,@bindings) (list ,@forms)))
                        ',name))))))))))))

;;; Encoding / execution

(defun %encode-value-cells (value width cell-width &optional (endian :little))
  "Split (already-evaluated integer) VALUE into WIDTH (unsigned-byte
CELL-WIDTH) cells in ENDIAN order (#66: :LITTLE, the default, or :BIG),
wrapping each with WRAP-VALUE (storage.lisp) like every other encoded
quantity in this codebase. Shared by ENCODE-INSTRUCTION below and the
assembler's .BYTE/.WORD directive encoding (assembler.lisp, #14), so
instruction operands and directive data can't drift apart in how they lay
cells down. The returned list is always in ascending address order --
ENDIAN only chooses which cell is the low-order one, never reorders which
cell goes at which address."
  (loop for i below width
        for shift = (if (eq endian :big) (- width 1 i) i)
        collect (wrap-value (ash value (* (- cell-width) shift)) cell-width)))

(defun %word-emit-order (descriptor choices)
  "Hole indices of CHOICES (one WORD-FIELD-CHOICE per operand hole) in the
order their trailing words follow the instruction word (#191). Holes whose
field is named by the machine's (extra-word-order ...) come first, in that
order; every other hole, and every fieldless :TRAILING-WORD one, keeps hole
order after them. A choice's field is found in DESCRIPTOR's own layout by its
width and shift. Hole order outright when the machine declares no order."
  (let* ((indices (loop for i below (length choices) collect i))
         (machine (find-machine-descriptor (instruction-descriptor-machine descriptor)))
         (default (machine-descriptor-instruction-word machine))
         (order (and default (instruction-word-layout-extra-word-order default))))
    (if (null order)
        indices
        (let ((layout (instruction-descriptor-word-layout descriptor)))
          (flet ((rank (choice)
                   (or (let ((field (and (word-field-choice-shift choice)
                                         (find-if (lambda (f)
                                                    (and (= (second f) (word-field-choice-width choice))
                                                         (= (third f) (word-field-choice-shift choice))))
                                                  (instruction-word-layout-fields layout)))))
                         (and field (position (first field) order)))
                       (length order))))
            (stable-sort indices #'< :key (lambda (i) (rank (nth i choices)))))))))

(defun %encode-word-instruction (descriptor layout values)
  "ENCODE-INSTRUCTION's word-encoded path (#20): OR DESCRIPTOR's opcode,
each of its (field-value ...) WORD-CONSTANTS (#136), and each operand's
chosen WORD-FIELD-CHOICE (WORD-FIELDS, parallel to VALUES) into one
LAYOUT-WIDTH-bit word by shift, then emit that word in LAYOUT's own ENDIAN
order (%ENCODE-VALUE-CELLS, at LAYOUT's own CELL-WIDTH, #66) followed by
each :EXTRA-WORD operand's own value, also in LAYOUT's endian order, at its
own WORD-FIELD-CHOICE-EXTRA-CELLS width (#135; formerly always LAYOUT's own
WIDTH-CELLS). The extra words follow %WORD-EMIT-ORDER: operand declaration
order unless the machine declares (extra-word-order ...) (#191). The
instruction word always precedes its extra words regardless of ENDIAN --
endianness only governs cell order *within* one multi-cell value, never field
or word order."
  (let ((word 0) extra-word-values
        (cell-width (instruction-word-layout-cell-width layout))
        (endian (instruction-word-layout-endian layout))
        (choices (instruction-descriptor-word-fields descriptor)))
    (destructuring-bind (opcode-width opcode-shift)
        (rest (instruction-word-field layout 'opcode))
      (setf word (ash (wrap-value (instruction-descriptor-opcode descriptor) opcode-width) opcode-shift)))
    (dolist (constant (instruction-descriptor-word-constants descriptor))
      (setf word (logior word (ash (wrap-value (word-constant-value constant) (word-constant-width constant))
                                    (word-constant-shift constant)))))
    (loop for choice in choices
          for value in values
          for index from 0
          do (ecase (word-field-choice-kind choice)
               (:inline
                (setf word (logior word (ash (wrap-value (+ value (word-field-choice-bias choice))
                                                          (word-field-choice-width choice))
                                              (word-field-choice-shift choice)))))
               (:extra-word
                (setf word (logior word (ash (word-field-choice-escape choice)
                                              (word-field-choice-shift choice))))
                (cl:push (list index value (word-field-choice-extra-cells choice)) extra-word-values))
               ;; #120: a :TRAILING-WORD choice ORs no bits into WORD at all --
               ;; it has no field of its own -- and spends its own trailing
               ;; cells unconditionally.
               (:trailing-word
                (cl:push (list index value (word-field-choice-extra-cells choice)) extra-word-values))))
    (append (%encode-value-cells word (instruction-word-layout-width-cells layout) cell-width endian)
            (loop for index in (%word-emit-order descriptor choices)
                  for extra = (find index extra-word-values :key #'first)
                  when extra
                    append (%encode-value-cells (second extra) (third extra) cell-width endian)))))

(defun encode-instruction (descriptor values)
  "Encode one use of instruction DESCRIPTOR with operand VALUES (a list of
already-evaluated integers, one per operand encoding field, in the same
order -- NIL for a no-operand instruction) into a list of
(unsigned-byte cell-width) cells, CELL-WIDTH being DESCRIPTOR's machine's own
code cell width (#53, INSTRUCTION-DESCRIPTOR-CELL-WIDTH). On an ordinary
cell-encoded machine: the opcode, then, when DESCRIPTOR declares a SUB-OPCODE
(#125), that sub-opcode as its own cell, then each value's cells in the
machine's own endian order (#66, INSTRUCTION-DESCRIPTOR-ENDIAN) in turn, per
DESCRIPTOR's OPERAND-WIDTHS. On a word-encoded machine (#20,
INSTRUCTION-DESCRIPTOR-WORD-LAYOUT non-NIL): one instruction word packing the
opcode and every inline operand's biased value or extra-word escape by bit
field, in the layout's own endian order, followed by each extra-word
operand's own value, also in that order, in declaration order
(%ENCODE-WORD-INSTRUCTION; SUB-OPCODE is always NIL here -- #125's sub-opcode
cell is byte-machine-only). VALUES shorter than DESCRIPTOR declares silently
encodes fewer fields, rather than erroring -- every caller in this codebase
(%ENCODE, assembler.lisp) always supplies exactly one value per field, so
this is unreachable internally, but a caller of this exported function on
its own should supply the same."
  (let ((layout (instruction-descriptor-word-layout descriptor)))
    (if layout
        (%encode-word-instruction descriptor layout values)
        (let ((cell-width (instruction-descriptor-cell-width descriptor))
              (endian (instruction-descriptor-endian descriptor))
              (sub (instruction-descriptor-sub-opcode descriptor)))
          (cons (wrap-value (instruction-descriptor-opcode descriptor) cell-width)
                (append (when sub (list (wrap-value sub cell-width)))
                        (loop for value in values
                              for width in (instruction-descriptor-operand-widths descriptor)
                              append (%encode-value-cells value width cell-width endian))))))))

(defun execute-instruction (descriptor machine values &optional choices)
  "Execute instruction DESCRIPTOR against a live MACHINE instance, passing
VALUES (a list of already-evaluated integers, one per operand encoding
field, or NIL for a no-operand instruction) to its semantics -- OPERAND is
bound to the first (or only) value, and any named field to its own value
(see %SEMANTICS-FN-FORM).

CHOICES (#73), when given, is DECODE-INSTRUCTION-AT's fourth return value (or
the assemble-time equivalent, MATCH-OPERAND-MODE's own CHOICES) -- the ONE-OF
alternative each operand hole actually matched, positionally hole-aligned.
STEP-MACHINE (emulator.lisp) always supplies it; a caller with no CHOICES to
give (or on a cell-encoded machine with no hole-selected sub-opcode selector
anywhere in this descriptor -- #126, see %DECODE-CELL-INSTRUCTION,
decoder.lisp) can omit it, in which case a (semantics ...) body's CHOICE-CASE
(if it has one) sees every hole as unmatched, same as an operand not governed
by any ONE-OF at all."
  (funcall (instruction-descriptor-semantics-fn descriptor) machine values choices))
