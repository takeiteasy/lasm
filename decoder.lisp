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
reading MACHINE's MEMORY element via MREF (storage.lisp) as instruction
fetches (#303) -- the source used by STEP-MACHINE (emulator.lisp)."
  (lambda (address) (%mref machine memory address :execute)))

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
:LITTLE, the default, :BIG, or an (OUTER INNER GROUP) list) -- the exact inverse of %ENCODE-VALUE-CELLS
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
        with order = (and (consp endian) (%cell-significance-order endian width-cells))
        for i below width-cells
        for shift = (cond (order (cl:pop order)) ((eq endian :big) (- width-cells 1 i)) (t i))
        do (setf v (logior v (ash (funcall read-cell (+ address i)) (* cell-width shift))))
        finally (return v)))

;; %WORD-CHOICE-MATCHES-P now lives in instruction.lisp (#105) -- registration
;; (REGISTER-INSTRUCTION-VARIANTS!'s %CHECK-OPCODE-DECODABLE!) needs it too,
;; and decoder.lisp loads after instruction.lisp in the :SERIAL T system.

(defun %word-field-match (alternatives word)
  (let ((first (first alternatives)))
    (if (eq (word-field-choice-kind first) :trailing-word)
        first
        (let ((raw (ldb (byte (word-field-choice-width first)
                             (word-field-choice-shift first)) word)))
          (find-if (lambda (choice) (%word-choice-matches-p raw choice)) alternatives)))))

(defun %word-candidate-matches-p (descriptor word)
  "Match only the instruction bits, without fetching trailing cells."
  (and (every (lambda (constant)
                (= (ldb (byte (word-constant-width constant)
                              (word-constant-shift constant)) word)
                   (word-constant-value constant)))
              (instruction-descriptor-word-constants descriptor))
       (every (lambda (alternatives) (%word-field-match alternatives word))
              (instruction-descriptor-word-alternatives descriptor))))

(defun %find-word-candidate (descriptor layout word)
  (destructuring-bind (name width shift) (instruction-word-field layout 'opcode)
    (declare (ignore name))
    (find-if (lambda (candidate) (%word-candidate-matches-p candidate word))
             (gethash (ldb (byte width shift) word)
                      (machine-descriptor-opcodes descriptor)))))

(defun %find-word-decode-candidate (descriptor layout word)
  "Like %FIND-WORD-CANDIDATE, but ranks removed instructions alongside live
ones, so a removed specific encoding is not decoded by a (fallback) it
shadowed. Returns the candidate and whether it is a removed one."
  (let ((disabled (machine-descriptor-disabled-opcodes descriptor)))
    (if (zerop (hash-table-count disabled))
        (values (%find-word-candidate descriptor layout word) nil)
        (destructuring-bind (name width shift) (instruction-word-field layout 'opcode)
          (declare (ignore name))
          (let* ((opcode (ldb (byte width shift) word))
                 (dead (gethash opcode disabled))
                 (found (find-if (lambda (candidate) (%word-candidate-matches-p candidate word))
                                 (%insert-by-specificity
                                  dead (gethash opcode (machine-descriptor-opcodes descriptor))))))
            (values found (and found (member found dead) t)))))))

(defun %shadowing-descriptor (descriptor cells)
  "For a (fallback) DESCRIPTOR encoded as CELLS, the other descriptor that
decodes those cells instead, or NIL. A second value is true when that
descriptor was removed from the machine, so the cells decode as nothing."
  (when (instruction-descriptor-fallback descriptor)
    (let* ((layout (instruction-descriptor-word-layout descriptor))
           (word (%fetch-cells (lambda (i) (nth i cells)) 0
                               (instruction-word-layout-width-cells layout)
                               (instruction-word-layout-cell-width layout)
                               (instruction-word-layout-endian layout))))
      (multiple-value-bind (found disabledp)
          (%find-word-decode-candidate
           (find-machine-descriptor (instruction-descriptor-machine descriptor))
           layout word)
        (and found
             (not (eq found descriptor))
             (not (%sibling-combos-p found descriptor))
             (values found disabledp))))))

(defun %word-decode-table (descriptor layout)
  "Publish the dispatch table for words up to 16 bits, every entry unfilled
until first looked up (%WORD-DECODE-ENTRY)."
  (when (<= (instruction-word-layout-width layout) 16)
    (or (machine-descriptor-word-decode-table descriptor)
        (setf (machine-descriptor-word-decode-table descriptor)
              (make-array (ash 1 (instruction-word-layout-width layout))
                          :initial-element '%unfilled)))))

(defun %word-decode-entry (descriptor layout table word)
  "TABLE's entry for WORD, computed and stored on first use. A removed
instruction's word maps to :DISABLED. Racing fills store the same value."
  (let ((entry (aref table word)))
    (if (eq entry '%unfilled)
        (setf (aref table word)
              (multiple-value-bind (found disabledp) (%find-word-decode-candidate descriptor layout word)
                (if disabledp :disabled found)))
        entry)))

(defun %try-decode-word-candidate (read-cell address width-cells cell-width descriptor word endian)
  "Decode one candidate, returning values, size, choices and a success flag.
Reject constants and operand fields before reading any trailing cells. Signed
fields are sign-extended before debiasing; trailing cells follow the declared
emission order. All operand lists belong to this call."
  (dolist (constant (instruction-descriptor-word-constants descriptor))
    (unless (= (ldb (byte (word-constant-width constant) (word-constant-shift constant)) word)
               (word-constant-value constant))
      (return-from %try-decode-word-candidate (values nil nil nil nil))))
  (let* ((matches (loop for alternatives in (instruction-descriptor-word-alternatives descriptor)
                        for match = (%word-field-match alternatives word)
                        do (unless match (return-from %try-decode-word-candidate (values nil nil nil nil)))
                        collect match))
         (values (make-list (length matches)))
         (order (instruction-descriptor-word-decode-order descriptor))
         (offset width-cells))
    ;; Resolve all matches before reading any trailing cells.
    (dolist (index (if (eq order :dynamic) (%word-emit-order descriptor matches) order))
      (let ((match (nth index matches)))
        (setf (nth index values)
              (ecase (word-field-choice-kind match)
                (:inline (let ((raw (ldb (byte (word-field-choice-width match)
                                               (word-field-choice-shift match)) word)))
                           (- (if (word-field-choice-signedp match)
                                  (signed-value raw (word-field-choice-width match))
                                  raw)
                              (word-field-choice-bias match))))
                ((:extra-word :trailing-word)
                 (let ((extra-cells (word-field-choice-extra-cells match)))
                   (prog1 (let ((v (%fetch-cells read-cell (+ address offset) extra-cells cell-width
                                              (or (word-field-choice-endian match) endian))))
                            (if (word-field-choice-signedp match)
                                (signed-value v (* extra-cells cell-width))
                                v))
                     (incf offset extra-cells))))))))
    (values values offset matches t)))

(defun %resolved-word-sibling (descriptor matches)
  (or (and (instruction-descriptor-word-siblings descriptor)
           (gethash matches (instruction-descriptor-word-siblings descriptor)))
      descriptor))

(defun %decode-word-instruction (read-cell address machine-name layout)
  "Fetch a word and select its first matching descriptor. A bucket is in
specificity order, so a more specific encoding wins over a (fallback).
Words up to 16 bits use a shared dispatch table; wider words scan their opcode
bucket. Trailing cells are always fetched anew, and size follows the selected
field choices rather than the descriptor's encoding-size variant."
  (let* ((width-cells (instruction-word-layout-width-cells layout))
         (cell-width (instruction-word-layout-cell-width layout))
         (endian (instruction-word-layout-endian layout))
         (word (%fetch-cells read-cell address width-cells cell-width endian))
         (machine (find-machine-descriptor machine-name))
         (table (%word-decode-table machine layout))
         (descriptor (if table
                         (%word-decode-entry machine layout table
                                             (ldb (byte (instruction-word-layout-width layout) 0) word))
                         (multiple-value-bind (found disabledp)
                             (%find-word-decode-candidate machine layout word)
                           (if disabledp :disabled found)))))
    (if (and descriptor (not (eq descriptor :disabled)))
        (multiple-value-bind (values offset matches okp)
            (%try-decode-word-candidate read-cell address width-cells cell-width descriptor word endian)
          (declare (ignore okp))
          (let ((selected (%resolved-word-sibling descriptor matches)))
            (values selected values offset matches
                    (instruction-descriptor-choice-selections selected))))
        (values :decode-failure nil nil))))

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
                         (instruction-descriptor-sub-choices descriptor)
                         (instruction-descriptor-choice-selections descriptor))))))))

(defun %decode-instruction-at-resolved (read-cell address machine-name layout cell-width endian)
  (if layout
      (%decode-word-instruction read-cell address machine-name layout)
      (%decode-cell-instruction read-cell address machine-name cell-width endian)))

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
    (%decode-instruction-at-resolved read-cell address machine-name layout
                                     (unless layout (%machine-cell-width machine-name memory))
                                     (unless layout (%machine-endian machine-name memory)))))

(defun %undefined-opcode-extent (read-cell address machine-name layout)
  "For the undecodable instruction at ADDRESS, values: the cells to skip past
it, its opcode (the whole word on a word-encoded machine), and whether it is
a removed instruction (whose full size is known) rather than an unassigned
opcode (one cell, or one instruction word). Reads only the opcode, sub-opcode
and instruction word, never an operand."
  (let ((md (find-machine-descriptor machine-name)))
    (if layout
        (let* ((width-cells (instruction-word-layout-width-cells layout))
               (word (%fetch-cells read-cell address width-cells
                                   (instruction-word-layout-cell-width layout)
                                   (instruction-word-layout-endian layout))))
          (multiple-value-bind (found disabledp) (%find-word-decode-candidate md layout word)
            (if disabledp
                (values (+ width-cells
                           (loop for alternatives in (instruction-descriptor-word-alternatives found)
                                 for match = (%word-field-match alternatives word)
                                 when (and match (member (word-field-choice-kind match)
                                                         '(:extra-word :trailing-word)))
                                   sum (word-field-choice-extra-cells match)))
                        word t)
                (values width-cells word nil))))
        (let* ((opcode (funcall read-cell address))
               (candidates (gethash opcode (machine-descriptor-disabled-opcodes md)))
               (found (if (some #'instruction-descriptor-sub-opcode candidates)
                          (find (funcall read-cell (1+ address)) candidates
                                :key #'instruction-descriptor-sub-opcode)
                          (first candidates))))
          (if found
              (values (instruction-descriptor-size found) opcode t)
              (values 1 opcode nil))))))
