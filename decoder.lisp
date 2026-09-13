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

(defun %fetch-cells (read-cell address width-cells cell-width)
  "Read WIDTH-CELLS cells starting at ADDRESS through READ-CELL as one
little-endian unsigned integer, each cell CELL-WIDTH bits wide -- shared by
an instruction word itself and every extra word following it. Formerly
%FETCH-WORD (emulator.lisp), generalized to read through any READ-CELL
closure rather than always MREF."
  (loop with v = 0
        for i below width-cells
        do (setf v (logior v (ash (funcall read-cell (+ address i)) (* cell-width i))))
        finally (return v)))

(defun %word-choice-matches-p (raw-value choice)
  "T if RAW-VALUE -- a field's bits as actually fetched -- is what CHOICE (a
WORD-FIELD-CHOICE, instruction.lisp) would encode: its exact ESCAPE for an
:EXTRA-WORD choice, or a value in its (biased) RANGE for an :INLINE one."
  (ecase (word-field-choice-kind choice)
    (:extra-word (= raw-value (word-field-choice-escape choice)))
    (:inline (destructuring-bind (lo . hi) (word-field-choice-range choice)
               (<= (+ lo (word-field-choice-bias choice)) raw-value (+ hi (word-field-choice-bias choice)))))))

(defun %decode-word-instruction (read-cell address machine-name layout)
  "DECODE-INSTRUCTION-AT's word-encoded (#20) path: fetch one
INSTRUCTION-WORD-LAYOUT-WIDTH-CELLS-wide word at ADDRESS, extract its OPCODE
field to find DESCRIPTOR (any sibling combo registered under that opcode
works equally well here -- see REGISTER-INSTRUCTION-VARIANTS!'s docstring,
instruction.lisp), then decode each operand field against DESCRIPTOR's
WORD-ALTERNATIVES: a fetched raw field value matching some alternative's
escape means the real value follows in its own word (fetched and consumed in
turn); matching an inline alternative's biased range instead means the value
*is* the field, debiased. A raw value matching no alternative at all is
:DECODE-FAILURE, same as an unregistered opcode -- an encoding this
DEFINSTRUCTION never declared.

SIZE (the third return value on success) is accumulated as cells are
consumed, never read off INSTRUCTION-DESCRIPTOR-SIZE -- %EXPAND-WORD-COMBOS
(instruction.lisp) sorts a mnemonic's sibling combos ascending by extra-word
count and REGISTER-INSTRUCTION-VARIANTS! is last-write-wins, so the
descriptor actually sitting in the opcode table is the combo with the *most*
extra words. INSTRUCTION-DESCRIPTOR-SIZE would overstate the size of any
narrower encoding genuinely present in the stream; it is only trustworthy in
the encode direction (assembler.lisp) and on the byte-encoded path below.

Word machines never sign-extend a decoded value, unlike the byte path below
-- %CHECK-WORD-RELATIVE (instruction.lisp) forbids a :RELATIVE mode on a
word-encoded machine outright, so there is no signed word-machine operand to
extend. This mirrors that asymmetry rather than unifying it."
  (let* ((width-cells (instruction-word-layout-width-cells layout))
         (cell-width (instruction-word-layout-cell-width layout))
         (word (%fetch-cells read-cell address width-cells cell-width))
         (opcode-field (instruction-word-field layout 'opcode)))
    (destructuring-bind (opcode-width opcode-shift) (rest opcode-field)
      (let ((opcode (ldb (byte opcode-width opcode-shift) word)))
        (handler-case
            (let ((descriptor (find-instruction-by-opcode machine-name opcode)))
              (loop with offset = width-cells
                    for alternatives in (instruction-descriptor-word-alternatives descriptor)
                    for choice0 = (first alternatives)
                    for raw = (ldb (byte (word-field-choice-width choice0) (word-field-choice-shift choice0)) word)
                    for match = (find-if (lambda (c) (%word-choice-matches-p raw c)) alternatives)
                    do (unless match (return-from %decode-word-instruction (values :decode-failure nil nil)))
                    collect (ecase (word-field-choice-kind match)
                              (:inline (- raw (word-field-choice-bias match)))
                              (:extra-word
                               (prog1 (%fetch-cells read-cell (+ address offset) width-cells cell-width)
                                 (incf offset width-cells))))
                      into values
                    finally (return (values descriptor values offset))))
          (unknown-instruction () (values :decode-failure nil nil)))))))

(defun %decode-cell-instruction (read-cell address machine-name cell-width)
  "DECODE-INSTRUCTION-AT's ordinary cell-encoded path -- unchanged in shape
from before #20/#21, only reading through READ-CELL rather than always MREF.

A SIGNED operand (mode.lisp, #30 -- RELATIVE, #23, implies SIGNEDP) was
assembled as a signed quantity (a RELATIVE operand specifically as an
offset, assembler.lisp's %RELATIVE-OFFSET) but is fetched here as an
unsigned WIDTH-cell quantity, like every other operand -- reinterpret each
hole by its own width so callers see a plain signed integer. Unlike RELATIVE
(%CHECK-RELATIVE-MODE-HOLES, instruction.lisp), a SIGNED mode may have more
than one hole, so this maps over every VALUE/WIDTH pair rather than assuming
a single element."
  (let ((opcode (funcall read-cell address)))
    (handler-case
        (let* ((descriptor (find-instruction-by-opcode machine-name opcode))
               (mode (instruction-descriptor-mode descriptor))
               (widths (instruction-descriptor-operand-widths descriptor))
               (values (loop with offset = 1
                             for width in widths
                             collect (loop with v = 0
                                           for i below width
                                           do (setf v (logior v (ash (funcall read-cell (+ address offset i))
                                                                      (* cell-width i))))
                                           finally (return v))
                             do (incf offset width))))
          (when (and mode (mode-descriptor-signedp mode))
            (setf values (mapcar (lambda (v w) (signed-value v (* cell-width w))) values widths)))
          (values descriptor values (instruction-descriptor-size descriptor)))
      (unknown-instruction () (values :decode-failure nil nil)))))

(defun decode-instruction-at (read-cell address machine-name &key memory)
  "Decode one instruction at ADDRESS by reading cells through READ-CELL, a
closure of one argument (an address) returning that cell's unsigned integer
value -- see MACHINE-CELL-READER/VECTOR-CELL-READER for the two ready-made
sources. Pure: reads nothing but READ-CELL, writes nothing, executes
nothing -- the inverse of ENCODE-INSTRUCTION (instruction.lisp), shared by
STEP-MACHINE (emulator.lisp) and the disassembler (disassembler.lisp) so
they cannot decode the same encoding two different ways.

Returns (VALUES descriptor values size) on success: the matched
INSTRUCTION-DESCRIPTOR, its decoded operand VALUES in hole order (already
sign-extended per mode where applicable -- see %DECODE-CELL-INSTRUCTION), and
SIZE, the instruction's width in cells, accumulated during decode rather than
taken from INSTRUCTION-DESCRIPTOR-SIZE (see %DECODE-WORD-INSTRUCTION's
docstring for why that matters on a word-encoded machine).

Returns (VALUES :DECODE-FAILURE NIL NIL) on an unregistered opcode, or, on a
word-encoded machine, a raw operand field matching none of the descriptor's
WORD-ALTERNATIVES -- an encoding no DEFINSTRUCTION on this machine declared.

A condition signalled by READ-CELL itself (e.g. ADDRESS-OUT-OF-RANGE past
the end of a buffer) propagates rather than being caught here -- whether a
truncated trailing instruction is a stop condition or a decodable-as-data
byte is the caller's policy, not this function's.

MEMORY, when given, is only used to resolve MACHINE-NAME's INSTRUCTION-WORD
layout and cell width when the machine declares more than one memory
element (see %RESOLVE-MEMORY, machine.lisp); READ-CELL itself already knows
which memory it reads."
  (let* ((memory (%resolve-memory machine-name memory))
         (layout (machine-descriptor-instruction-word (find-machine-descriptor machine-name))))
    (if layout
        (%decode-word-instruction read-cell address machine-name layout)
        (%decode-cell-instruction read-cell address machine-name (%machine-cell-width machine-name memory)))))
