;;;; emulator.lisp
;;;; The M1 emulator loop: fetch/decode/execute over encoded bytes
;;;; (instruction.lisp) against a live MACHINE (storage.lisp).
;;;;
;;;; PC and program memory are found by convention -- a register named PC
;;;; and the machine's sole :MEMORY storage element -- per the "PC is a
;;;; plain register" convention already relied on throughout
;;;; tests/instruction.lisp. :PC and :MEMORY keyword arguments override the
;;;; convention for a machine that names either differently or declares more
;;;; than one memory element; this mirrors DEFINSTRUCTION's own
;;;; %DEFAULT-ABSOLUTE-WIDTH, which resolves a machine's sole memory element
;;;; the same way for ABSOLUTE mode's default operand width.
;;;;
;;;; Halting has no dedicated mechanism in M1 -- the existing TRAP semantics
;;;; primitive (semantics.lisp) already is one: an instruction whose
;;;; semantics are (trap :halt) signals LASM-TRAP, which RUN catches and
;;;; reports as a stop reason. A generalized interrupt/exception model
;;;; replacing this is M6.

(in-package #:lasm)

;;; PC resolution

(defun %resolve-pc (machine-name pc)
  (or pc
      (let* ((descriptor (find-machine-descriptor machine-name))
             (element (gethash 'pc (machine-descriptor-table descriptor))))
        (if (and element (eq (storage-element-kind element) :register))
            'pc
            (error "RUN/STEP-MACHINE on machine ~S: no register named PC -- ~
pass :PC explicitly" machine-name)))))

;;; Loading

(defun load-program (machine cells &key memory origin)
  "Write CELLS (an ASSEMBLY, or any sequence of (unsigned-byte n)) into
MACHINE's MEMORY element starting at ORIGIN, and set MACHINE's PC register
to ORIGIN. MEMORY defaults per %RESOLVE-MEMORY. ORIGIN defaults to CELLS'
own ASSEMBLY-ORIGIN when CELLS is an ASSEMBLY (so a program assembled with
:ORIGIN #x200 always loads where its labels were computed against),
otherwise 0. Signals if CELLS is an ASSEMBLY whose own ASSEMBLY-CELL-WIDTH
does not match MEMORY's declared :CELL-WIDTH (#53) -- e.g. a program
assembled against a byte-addressed memory element loaded into a
word-addressed one would otherwise place every assembled cell one address
too far apart with no other symptom."
  (let* ((machine-name (machine-descriptor-name (machine-descriptor machine)))
         (memory (%resolve-memory machine-name memory))
         (assembly-p (assembly-p cells))
         (origin (or origin (if assembly-p (assembly-origin cells) 0)))
         (data (if assembly-p (assembly-cells cells) cells)))
    (when assembly-p
      (let ((target-width (%machine-cell-width machine-name memory))
            (source-width (assembly-cell-width cells)))
        (unless (= target-width source-width)
          (error "LOAD-PROGRAM on machine ~S: assembly's cell width (~D) does not ~
match memory ~S's cell width (~D)" machine-name source-width memory target-width))))
    (let ((address origin))
      (map nil (lambda (cell)
                 (setf (mref machine memory address) cell)
                 (incf address))
           data))
    (setf (sref machine 'pc) origin)
    machine))

;;; Step

(defun %fetch-word (machine memory address width-cells cell-width)
  "Read WIDTH-CELLS cells of MACHINE's MEMORY starting at ADDRESS as one
little-endian unsigned integer, each cell CELL-WIDTH bits wide -- the
word-encoded (#20) counterpart of STEP-MACHINE's cell-encoded operand fetch
loop, shared by the instruction word itself and every extra word following
it."
  (loop with v = 0
        for i below width-cells
        do (setf v (logior v (ash (mref machine memory (+ address i)) (* cell-width i))))
        finally (return v)))

(defun %word-choice-matches-p (raw-value choice)
  "T if RAW-VALUE -- a field's bits as actually fetched -- is what CHOICE (a
WORD-FIELD-CHOICE, instruction.lisp) would encode: its exact ESCAPE for an
:EXTRA-WORD choice, or a value in its (biased) RANGE for an :INLINE one."
  (ecase (word-field-choice-kind choice)
    (:extra-word (= raw-value (word-field-choice-escape choice)))
    (:inline (destructuring-bind (lo . hi) (word-field-choice-range choice)
               (<= (+ lo (word-field-choice-bias choice)) raw-value (+ hi (word-field-choice-bias choice)))))))

(defun %step-word-machine (machine machine-name pc memory address layout)
  "STEP-MACHINE's word-encoded (#20) path: fetch one INSTRUCTION-WORD-LAYOUT-
WIDTH-CELLS-wide word at ADDRESS, extract its OPCODE field to find DESCRIPTOR
(any sibling combo registered under that opcode works equally well here --
see REGISTER-INSTRUCTION-VARIANTS!'s docstring, instruction.lisp), then
decode each operand field against DESCRIPTOR's WORD-ALTERNATIVES: a fetched
raw field value matching some alternative's escape means the real value
follows in its own word (fetched and consumed in turn); matching an inline
alternative's biased range instead means the value *is* the field, debiased.
A raw value matching no alternative at all is :DECODE-FAILURE, same as an
unregistered opcode -- an encoding this DEFINSTRUCTION never declared."
  (let* ((width-cells (instruction-word-layout-width-cells layout))
         (cell-width (instruction-word-layout-cell-width layout))
         (word (%fetch-word machine memory address width-cells cell-width))
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
                    do (unless match (return-from %step-word-machine :decode-failure))
                    collect (ecase (word-field-choice-kind match)
                              (:inline (- raw (word-field-choice-bias match)))
                              (:extra-word
                               (prog1 (%fetch-word machine memory (+ address offset) width-cells cell-width)
                                 (incf offset width-cells))))
                      into values
                    finally
                       (setf (sref machine pc) (+ address offset))
                       (execute-instruction descriptor machine values)
                       (return descriptor)))
          (unknown-instruction () :decode-failure))))))

(defun %step-byte-machine (machine machine-name pc memory address)
  "STEP-MACHINE's ordinary cell-encoded path, unchanged in shape since before
#20 -- only the (* 8 i)/(* 8 w) shifts are now the machine's own cell width
(#53), never hardcoded to 8 bits.

PERFORMANCE: %MACHINE-CELL-WIDTH below re-resolves MACHINE-NAME's cell width
(two hash lookups) on every single step, where before #53 this path did no
such lookup at all. INSTRUCTION-DESCRIPTOR-CELL-WIDTH (instruction.lisp,
ENCODE-INSTRUCTION's cell-encoded path) has the same shape. Both are cheap
relative to a full fetch/decode/execute step, but this is the emulator's
innermost loop -- caching the resolved width on the MACHINE-DESCRIPTOR (or
threading it down from STEP-MACHINE/RUN, which already resolve MEMORY once
per call) instead of re-deriving it every step is a real speedup on a tight
loop. Left uncached for now (this ticket's #55/#21 M4-M7 targets aren't
performance-sensitive); see the follow-up ticket filed for this."
  (let* ((opcode (mref machine memory address))
         (cell-width (%machine-cell-width machine-name memory)))
    (handler-case
        (let* ((descriptor (find-instruction-by-opcode machine-name opcode))
               (mode (instruction-descriptor-mode descriptor))
               (widths (instruction-descriptor-operand-widths descriptor))
               (values (loop with offset = 1
                             for width in widths
                             collect (loop with v = 0
                                           for i below width
                                           do (setf v (logior v (ash (mref machine memory (+ address offset i))
                                                                      (* cell-width i))))
                                           finally (return v))
                             do (incf offset width))))
          ;; A SIGNED operand (mode.lisp, #30 -- RELATIVE, #23, implies
          ;; SIGNED) was assembled as a signed quantity (a RELATIVE operand
          ;; specifically as an offset, assembler.lisp's %RELATIVE-OFFSET)
          ;; but is fetched above as an unsigned WIDTH-cell quantity, like
          ;; every other operand -- reinterpret each hole here, by its own
          ;; width, so semantics sees a plain signed integer (a RELATIVE
          ;; instruction body can then write (set! pc (+ pc operand)) with no
          ;; width of its own to track). Unlike RELATIVE (%CHECK-RELATIVE-
          ;; MODE-HOLES, instruction.lisp), a SIGNED mode may have more than
          ;; one hole, so this maps over every VALUE/WIDTH pair rather than
          ;; assuming a single element.
          (when (and mode (mode-descriptor-signedp mode))
            (setf values (mapcar (lambda (v w) (signed-value v (* cell-width w))) values widths)))
          (setf (sref machine pc) (+ address (instruction-descriptor-size descriptor)))
          (execute-instruction descriptor machine values)
          descriptor)
      (unknown-instruction () :decode-failure))))

(defun step-machine (machine &key pc memory)
  "Fetch one instruction from MACHINE's MEMORY at its PC register, advance
PC past it, then execute it against MACHINE. Returns the executed
INSTRUCTION-DESCRIPTOR, or :DECODE-FAILURE (without advancing PC or
executing anything) when the byte(s) at PC do not decode to a registered
instruction -- distinguishable from an UNKNOWN-INSTRUCTION signal so RUN can
treat it as an ordinary stop reason rather than a crash.

PC is advanced past the whole instruction *before* executing its
semantics, not after -- so a branch instruction's own (set! pc operand)
in its semantics overrides the increment, rather than being clobbered by
it.

On a word-encoded machine (#20, MACHINE's INSTRUCTION-WORD DEFMACHINE
clause), fetch/decode goes through %STEP-WORD-MACHINE instead of the
ordinary byte-per-operand path below."
  (let* ((machine-name (machine-descriptor-name (machine-descriptor machine)))
         (pc (%resolve-pc machine-name pc))
         (memory (%resolve-memory machine-name memory))
         (address (sref machine pc))
         (layout (machine-descriptor-instruction-word (find-machine-descriptor machine-name))))
    (if layout
        (%step-word-machine machine machine-name pc memory address layout)
        (%step-byte-machine machine machine-name pc memory address))))

;;; Run

(defun run (machine &key pc memory (max-steps 10000))
  "Repeatedly STEP-MACHINE against MACHINE until one of three stop
conditions, returning (VALUES reason steps [condition]):
  :TRAP           -- an instruction's semantics signalled LASM-TRAP; the
                      condition itself is returned as a third value.
  :DECODE-FAILURE -- STEP-MACHINE hit a byte that is not a registered
                      opcode.
  :MAX-STEPS      -- MAX-STEPS instructions executed without stopping
                      otherwise (a runaway-program guard, not a real timer)."
  (loop for steps from 0 below max-steps
        do (handler-case
               (when (eq (step-machine machine :pc pc :memory memory) :decode-failure)
                 (return-from run (values :decode-failure steps)))
             (lasm-trap (c)
               ;; The trapping instruction's semantics ran to completion (the
               ;; trap fires from inside them) before signalling, so it
               ;; counts as an executed step -- unlike a decode failure,
               ;; where nothing was executed this iteration.
               (return-from run (values :trap (1+ steps) c))))
        finally (return (values :max-steps steps))))
