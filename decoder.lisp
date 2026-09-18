;;;; decoder.lisp
;;;; DECODE-INSTRUCTION-AT: a pure fetch/decode step -- the inverse of
;;;; ENCODE-INSTRUCTION (instruction.lisp) -- shared by the emulator
;;;; (emulator.lisp's STEP-MACHINE) and the disassembler (disassembler.lisp)
;;;; so the two cannot drift on the same subtle decode logic (word-encoded
;;;; alternatives, biases, escapes, sign-extension) the way
;;;; %MACHINE-CELL-WIDTH (machine.lisp) already keeps DEFINSTRUCTION, ASSEMBLE
;;;; and the emulator from drifting on cell-width resolution.
;;;;
;;;; This file knows nothing about a live MACHINE, a PC register, or
;;;; execution -- it reads cells through a READ-CELL closure of one argument
;;;; (an address, returning that cell's unsigned integer value) and returns
;;;; what it decoded. MACHINE-CELL-READER and VECTOR-CELL-READER below build
;;;; that closure over the two sources callers actually have: a live
;;;; machine's memory, or a bare sequence of cells (e.g. an ASSEMBLY).
;;;;
;;;; Lifted from what were %STEP-BYTE-MACHINE/%STEP-WORD-MACHINE in
;;;; emulator.lisp before this ticket (#21) -- STEP-MACHINE now calls
;;;; DECODE-INSTRUCTION-AT and only handles the PC write and EXECUTE-INSTRUCTION
;;;; itself.

(in-package #:lasm)

;;; Cell readers

(defun machine-cell-reader (machine memory)
  "A READ-CELL closure (DECODE-INSTRUCTION-AT's ADDRESS -> cell contract)
reading MACHINE's MEMORY element via MREF (storage.lisp) -- the source used
by STEP-MACHINE (emulator.lisp)."
  (lambda (address) (mref machine memory address)))

(defun machine-peek-reader (machine memory)
  "A READ-CELL closure reading MACHINE's MEMORY element via MPEEK
(storage.lisp) rather than MREF -- the source used by DISASSEMBLE-MEMORY
(disassembler.lisp), which inspects memory rather than executing it and so
must not trigger a #107 :DEVICE region's :READ side effects just by
disassembling across one."
  (lambda (address) (mpeek machine memory address)))

(defun vector-cell-reader (cells &key (origin 0) end)
  "A READ-CELL closure reading a bare sequence CELLS (e.g. an ASSEMBLY-CELLS
vector) as if it were mapped into address space starting at ORIGIN --
address A reads (ELT CELLS (- A ORIGIN)). Signals ADDRESS-OUT-OF-RANGE for
an address outside [ORIGIN, END) (END defaults to ORIGIN + (LENGTH CELLS)),
the same condition MREF signals off the end of a live machine's memory, so a
caller handling decode failure at the edges of a buffer (disassembler.lisp)
can use one HANDLER-CASE for both cell sources."
  (let ((end (or end (+ origin (length cells)))))
    (lambda (address)
      (unless (<= origin address (1- end))
        (error 'address-out-of-range :machine nil :name 'cells :address address))
      (elt cells (- address origin)))))

;;; Decode

(defun %fetch-cells (read-cell address width-cells cell-width &optional (endian :little))
  "Read WIDTH-CELLS cells starting at ADDRESS through READ-CELL as one
unsigned integer, each cell CELL-WIDTH bits wide, in ENDIAN order (#66:
:LITTLE, the default, or :BIG) -- the exact inverse of %ENCODE-VALUE-CELLS
(instruction.lisp). Shared by an instruction word itself, every extra word
following it, and %DECODE-CELL-INSTRUCTION's ordinary operand fetch.
Formerly %FETCH-WORD (emulator.lisp), generalized to read through any
READ-CELL closure rather than always MREF.

Always walks ADDRESS ascending, regardless of ENDIAN -- only the shift each
cell contributes flips, never the order cells are read in. This matters:
VECTOR-CELL-READER's range check and %TRY-DECODE-WORD-CANDIDATE's
check-constants-before-fetching-an-extra-word ordering both assume reads
never run backward or past a rejected candidate's own bounds."
  (loop with v = 0
        for i below width-cells
        for shift = (if (eq endian :big) (- width-cells 1 i) i)
        do (setf v (logior v (ash (funcall read-cell (+ address i)) (* cell-width shift))))
        finally (return v)))

;; %WORD-CHOICE-MATCHES-P now lives in instruction.lisp (#105) -- registration
;; (REGISTER-INSTRUCTION-VARIANTS!'s %CHECK-OPCODE-DECODABLE!) needs it too,
;; and decoder.lisp loads after instruction.lisp in the :SERIAL T system.

(defun %try-decode-word-candidate (read-cell address width-cells cell-width descriptor word endian)
  "Try decoding WORD (already fetched at ADDRESS) against one candidate
DESCRIPTOR sharing this opcode (#105) -- decode each operand field against
DESCRIPTOR's WORD-ALTERNATIVES: a fetched raw field value matching some
alternative's escape means the real value follows in its own word (fetched
and consumed in turn); matching an inline alternative's biased range instead
means the value *is* the field, debiased. Returns (VALUES values offset
matches okp) on success (OKP T; VALUES is NIL, not a failure signal, for a
legitimately no-operand DESCRIPTOR -- see OKP), or (VALUES NIL NIL NIL NIL)
if some field's raw bits match none of this candidate's own alternatives --
the caller (%DECODE-WORD-INSTRUCTION) tries the next candidate at this
opcode rather than failing outright, since #105's %CHECK-OPCODE-DECODABLE!
(instruction.lisp) only guarantees candidates are pairwise distinguishable,
not that every raw bit pattern at the opcode names exactly one of them --
this trial-and-reject is what actually does the telling-apart.

Layout-agnostic by construction (#140): every WORD-FIELD-CHOICE and
WORD-CONSTANT already carries its own absolute WIDTH/SHIFT, resolved
against DESCRIPTOR's own instruction-word layout at DEFINSTRUCTION time
(instruction.lisp), so this function never needs to know which layout
DESCRIPTOR named -- co-tenant candidates at one opcode may name different
layouts.

#127: a MATCH whose own WORD-FIELD-CHOICE-SIGNEDP is T reinterprets its raw
bits as two's-complement before debiasing (:INLINE, over its own field
WIDTH) or the fetched extra word (:EXTRA-WORD, over the match's own
EXTRA-CELLS*CELL-WIDTH bits, #135 -- not always WIDTH-CELLS*CELL-WIDTH,
since an extra word may now be narrower or wider than the instruction word
itself) -- the exact inverse of how ENCODE-INSTRUCTION's %ENCODE-WORD-
INSTRUCTION writes a signed value (WRAP-VALUE of a possibly negative,
already-biased quantity). SIGNEDP is only ever T on a CHOICE-selected MATCH
(instruction.lisp's %WORD-FIELD-CHOICE-FORM), so an ungoverned or
value-selected field is unaffected -- unlike the byte path
(%DECODE-CELL-INSTRUCTION below), word decode was previously never signed at
all; this is the first case where it is.

#136: every one of DESCRIPTOR's own WORD-CONSTANTS (a (field-value ...)
pin) must match WORD's already-fetched bits exactly, checked *before* the
operand-hole loop below and before OFFSET is ever advanced past the
instruction word itself. This ordering is load-bearing, not cosmetic:
DECODE-INSTRUCTION-AT documents that a condition READ-CELL signals (e.g.
ADDRESS-OUT-OF-RANGE past the end of a buffer) propagates rather than being
caught here, so a rejected candidate that also has an :EXTRA-WORD operand
hole could otherwise call %FETCH-CELLS past a buffer's end while still
failing to match its own constants -- killing the disassembler on a
would-be :DECODE-FAILURE instead of quietly trying the next candidate at
this opcode. Checking constants first means a mismatched candidate is
rejected before any extra word is ever fetched."
  (dolist (constant (instruction-descriptor-word-constants descriptor))
    (unless (= (ldb (byte (word-constant-width constant) (word-constant-shift constant)) word)
               (word-constant-value constant))
      (return-from %try-decode-word-candidate (values nil nil nil nil))))
  (loop with offset = width-cells
        for alternatives in (instruction-descriptor-word-alternatives descriptor)
        for choice0 = (first alternatives)
        ;; #120: a :TRAILING-WORD hole has no field bits of its own --
        ;; ALTERNATIVES is always its own single, unconditionally-matching
        ;; entry, so RAW stays NIL (WORD-FIELD-CHOICE-WIDTH/-SHIFT are both
        ;; NIL there, nothing to LDB) and MATCH is CHOICE0 directly, with no
        ;; %WORD-CHOICE-MATCHES-P call at all. RAW is only ever read below
        ;; from the :INLINE collect clause, unreachable for a :TRAILING-WORD
        ;; MATCH (its own KIND is always :TRAILING-WORD, never :INLINE).
        for trailingp = (eq (word-field-choice-kind choice0) :trailing-word)
        for raw = (unless trailingp
                    (ldb (byte (word-field-choice-width choice0) (word-field-choice-shift choice0)) word))
        for match = (if trailingp choice0 (find-if (lambda (c) (%word-choice-matches-p raw c)) alternatives))
        do (unless match (return-from %try-decode-word-candidate (values nil nil nil nil)))
        collect (ecase (word-field-choice-kind match)
                  (:inline (- (if (word-field-choice-signedp match)
                                   (signed-value raw (word-field-choice-width match))
                                   raw)
                              (word-field-choice-bias match)))
                  ;; #120: :TRAILING-WORD fetches exactly like :EXTRA-WORD -- an
                  ;; unconditional trailing value at OFFSET, of its own EXTRA-CELLS
                  ;; width -- it just never had field bits to escape-match first.
                  ((:extra-word :trailing-word)
                   (let ((extra-cells (word-field-choice-extra-cells match)))
                     (prog1 (let ((v (%fetch-cells read-cell (+ address offset) extra-cells cell-width endian)))
                              (if (word-field-choice-signedp match)
                                  (signed-value v (* extra-cells cell-width))
                                  v))
                       (incf offset extra-cells)))))
          into values
        collect match into matches
        finally (return (values values offset matches t))))

(defun %decode-word-instruction (read-cell address machine-name layout)
  "DECODE-INSTRUCTION-AT's word-encoded (#20) path: fetch one
INSTRUCTION-WORD-LAYOUT-WIDTH-CELLS-wide word at ADDRESS, extract its OPCODE
field, and try each candidate DESCRIPTOR registered under that opcode in
turn (#105: more than one only when several mode-distinguished variants of
one or more mnemonics share the opcode) via %TRY-DECODE-WORD-CANDIDATE,
returning the first that fully matches. A raw value matching no candidate's
alternatives at all is :DECODE-FAILURE, same as an unregistered opcode -- an
encoding no DEFINSTRUCTION on this machine ever declared.

LAYOUT is always the machine's *default* instruction-word layout, never a
candidate's own named one (#64) -- every WORD-FIELD/OPCODE field is fetched
off it alone, sound for any candidate regardless of which layout it names
because every layout is held (PARSE-INSTRUCTION-WORD-CLAUSE, machine.lisp)
to the default's own :WIDTH and an identical OPCODE field. %TRY-DECODE-WORD-
CANDIDATE itself never touches LAYOUT at all (#140) -- see its own
docstring.

Candidate order only matters for determinism, not correctness:
%CHECK-OPCODE-DECODABLE! (instruction.lisp) requires every pair of
co-tenant candidates to disagree at some field range they share bits with
(#140: candidates may name different layouts and even different field
names, as long as some shared bit range disagrees), so at most one
candidate can ever match a given fetched word -- the first-match loop below
never has to arbitrate a genuine tie, it just stops as soon as it finds the
one candidate that was always going to match.

SIZE (the third return value on success) is accumulated as cells are
consumed, never read off INSTRUCTION-DESCRIPTOR-SIZE -- %EXPAND-WORD-COMBOS
(instruction.lisp) sorts a mnemonic's sibling combos ascending by total
extra-word cells (#135), so a mnemonic's own combo actually matched here
need not be the one INSTRUCTION-DESCRIPTOR-SIZE would compute for whichever
combo happens to sit first in the candidate list. INSTRUCTION-DESCRIPTOR-SIZE
would overstate the size of any narrower encoding genuinely present in the
stream; it is only trustworthy in the encode direction (assembler.lisp) and
on the byte-encoded path below.

CHOICES (the fourth return value on success, #104) is the matched
WORD-FIELD-CHOICE per operand hole, in hole order -- exactly the alternative
%WORD-CHOICE-MATCHES-P found for each field, kept (not just its debiased
value) so a CHOICE-selected field's own WORD-FIELD-CHOICE-CHOICE (the ONE-OF
alternative mode-name that was actually encoded, instruction.lisp) survives
to the disassembler (disassembler.lisp, #117). NIL entries mix in freely for
a value-selected field (WORD-FIELD-CHOICE-CHOICE NIL there).

A word machine never needs a separate :RELATIVE sign-extension step the way
the byte path below does -- a word-encoded RELATIVE hole's WORD-FIELD-CHOICE
is already stamped SIGNEDP (#62, instruction.lisp's %WORD-FIELD-CHOICE-FORM,
MODE-DESCRIPTOR-SIGNEDP folding in RELATIVEP), so %TRY-DECODE-WORD-CANDIDATE
sign-extends it the same way it sign-extends any other per-hole :SIGNED
field (#127), via WORD-FIELD-CHOICE-SIGNEDP -- see that function. The
resulting signed offset is folded back to an absolute target the same way
on both encodings: %OPERAND-RENDER-VALUES (disassembler.lisp) for display,
and a branch's own (set! pc (+ pc operand)) semantics at run time."
  (let* ((width-cells (instruction-word-layout-width-cells layout))
         (cell-width (instruction-word-layout-cell-width layout))
         (endian (instruction-word-layout-endian layout))
         (word (%fetch-cells read-cell address width-cells cell-width endian))
         (opcode-field (instruction-word-field layout 'opcode)))
    (destructuring-bind (opcode-width opcode-shift) (rest opcode-field)
      (let ((opcode (ldb (byte opcode-width opcode-shift) word)))
        (handler-case
            (let ((candidates (find-instruction-descriptors-by-opcode machine-name opcode)))
              (dolist (descriptor candidates (values :decode-failure nil nil))
                (multiple-value-bind (values offset matches okp)
                    (%try-decode-word-candidate read-cell address width-cells cell-width descriptor word endian)
                  (when okp
                    (return-from %decode-word-instruction (values descriptor values offset matches))))))
          (unknown-instruction () (values :decode-failure nil nil)))))))

(defun %decode-cell-instruction (read-cell address machine-name cell-width endian)
  "DECODE-INSTRUCTION-AT's ordinary cell-encoded path -- unchanged in shape
from before #20/#21, only reading through READ-CELL rather than always MREF,
plus #125's sub-opcode cell and #126's hole-selected CHOICES below. Each
operand's cells are reassembled via %FETCH-CELLS (the same routine the
word-encoded path below uses), which is what makes ENDIAN (#66) apply here
too -- previously a second, hand-rolled little-endian-only copy of that
loop lived here.

A SIGNED operand (mode.lisp, #30 -- RELATIVE, #23, implies SIGNEDP) was
assembled as a signed quantity (a RELATIVE operand specifically as an
offset, assembler.lisp's %RELATIVE-OFFSET) but is fetched here as an
unsigned WIDTH-cell quantity, like every other operand -- reinterpret each
hole by its own width so callers see a plain signed integer. Per hole, not
per whole mode (#124/#127): DESCRIPTOR's own OPERAND-SIGNEDNESS
(instruction.lisp, precomputed at DEFINSTRUCTION time) says which holes are
signed -- for an ungoverned hole this is just MODE's own SIGNEDP (unchanged
from before #124), but a ONE-OF hole whose alternatives disagree can differ
by which alternative a hole-selected (variant (choice m) (sub s)) selector
resolved to, which is exactly what OPERAND-SIGNEDNESS bakes in per
descriptor. A NIL OPERAND-SIGNEDNESS (a word-encoded descriptor never
reaches this function at all, so in practice always non-NIL here, but
guarded the same way assembler.lisp's readers are) is treated as
all-unsigned, not an error.

#125: OPCODE's bucket (FIND-INSTRUCTION-DESCRIPTORS-BY-OPCODE) holds more than
one candidate only when every one of them declares its own SUB-OPCODE
(REGISTER-INSTRUCTION-VARIANTS! guarantees they're pairwise distinct when it
does) -- in that case the cell right after OPCODE is read and matched against
each candidate's SUB-OPCODE to pick the one to decode, and operands start one
cell later than usual. A bucket with no SUB-OPCODE at all (the ordinary case)
has exactly one candidate, unaffected by any of this.

#126: the matched descriptor's own SUB-CHOICES (non-NIL only when its
SUB-OPCODE was selected by a hole-selected (variant (choice m) (sub s))
rather than a plain (opcode n :sub s)) is returned as this function's own
fourth CHOICES value -- already hole-aligned and already a list of bare
mode-name symbols/NILs, exactly the shape %MATCHED-CHOICE-NAME expects, so
no further conversion is needed here. NIL throughout for a descriptor with no
hole-selected sub-opcode selector at all -- the same NIL a caller saw
unconditionally before #126."
  (let* ((opcode (funcall read-cell address))
         (candidates (handler-case (find-instruction-descriptors-by-opcode machine-name opcode)
                       (unknown-instruction () nil))))
    (if (null candidates)
        (values :decode-failure nil nil)
        (let* ((subbed (some #'instruction-descriptor-sub-opcode candidates))
               (sub-offset (if subbed 1 0))
               (descriptor (if subbed
                               (let ((sub (funcall read-cell (+ address 1))))
                                 (find sub candidates :key #'instruction-descriptor-sub-opcode))
                               (first candidates))))
          (if (null descriptor)
              (values :decode-failure nil nil)
              (let* ((widths (instruction-descriptor-operand-widths descriptor))
                     (signedness (or (instruction-descriptor-operand-signedness descriptor)
                                      (make-list (length widths))))
                     (values (loop with offset = (+ 1 sub-offset)
                                   for width in widths
                                   collect (%fetch-cells read-cell (+ address offset) width cell-width endian)
                                   do (incf offset width))))
                (setf values (mapcar (lambda (v w signedp) (if signedp (signed-value v (* cell-width w)) v))
                                      values widths signedness))
                (values descriptor values (instruction-descriptor-size descriptor)
                        (instruction-descriptor-sub-choices descriptor))))))))

(defun decode-instruction-at (read-cell address machine-name &key memory)
  "Decode one instruction at ADDRESS by reading cells through READ-CELL, a
closure of one argument (an address) returning that cell's unsigned integer
value -- see MACHINE-CELL-READER/VECTOR-CELL-READER for the two ready-made
sources. Pure: reads nothing but READ-CELL, writes nothing, executes
nothing -- the inverse of ENCODE-INSTRUCTION (instruction.lisp), shared by
STEP-MACHINE (emulator.lisp) and the disassembler (disassembler.lisp) so
they cannot decode the same encoding two different ways.

Returns (VALUES descriptor values size choices) on success: the matched
INSTRUCTION-DESCRIPTOR, its decoded operand VALUES in hole order (already
sign-extended per hole where applicable -- see %DECODE-CELL-INSTRUCTION's
OPERAND-SIGNEDNESS on a byte-encoded machine, #124/#127, and
%TRY-DECODE-WORD-CANDIDATE's WORD-FIELD-CHOICE-SIGNEDP on a word-encoded
one), and
SIZE, the instruction's width in cells, accumulated during decode rather than
taken from INSTRUCTION-DESCRIPTOR-SIZE (see %DECODE-WORD-INSTRUCTION's
docstring for why that matters on a word-encoded machine). CHOICES is the
matched WORD-FIELD-CHOICE per hole on a word-encoded machine (#104, see
%DECODE-WORD-INSTRUCTION), or the matched descriptor's own SUB-CHOICES on a
byte-encoded machine (#126, see %DECODE-CELL-INSTRUCTION) -- NIL throughout
when the descriptor declares no hole-selected sub-opcode selector, which
includes every byte-encoded descriptor before #126. Existing callers
(STEP-MACHINE, emulator.lisp) that only bind the first three values are
unaffected; DISASSEMBLE-CELLS (disassembler.lisp, #117) and a (semantics ...)
body's CHOICE-CASE (instruction.lisp, #73/#122) both read it to see which
ONE-OF alternative was actually encoded, on either encoding scheme alike.

Returns (VALUES :DECODE-FAILURE NIL NIL) on an unregistered opcode, on a
byte-encoded machine's opcode whose candidates all declare a SUB-OPCODE
(#125) when the fetched sub-opcode cell matches none of them, or, on a
word-encoded machine, a raw operand field matching none of the descriptor's
WORD-ALTERNATIVES -- an encoding no DEFINSTRUCTION on this machine declared.

A condition signalled by READ-CELL itself (e.g. ADDRESS-OUT-OF-RANGE past
the end of a buffer) propagates rather than being caught here -- whether a
truncated trailing instruction is a stop condition or a decodable-as-data
byte is the caller's policy, not this function's.

MEMORY, when given, is only used to resolve MACHINE-NAME's INSTRUCTION-WORD
layout, cell width and endianness (#66) when the machine declares more than
one memory element (see %RESOLVE-MEMORY, machine.lisp); READ-CELL itself
already knows which memory it reads. A word-encoded machine instead reads
ENDIAN off LAYOUT's own slot, set once at DEFMACHINE time -- see
%DECODE-WORD-INSTRUCTION."
  (let* ((memory (%resolve-memory machine-name memory))
         (layout (machine-descriptor-instruction-word (find-machine-descriptor machine-name))))
    (if layout
        (%decode-word-instruction read-cell address machine-name layout)
        (%decode-cell-instruction read-cell address machine-name
                                   (%machine-cell-width machine-name memory)
                                   (%machine-endian machine-name memory)))))
