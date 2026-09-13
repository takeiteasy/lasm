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
;;;;
;;;; #21: the actual fetch/decode step (byte-encoded and word-encoded alike)
;;;; now lives in decoder.lisp's DECODE-INSTRUCTION-AT, shared with the
;;;; disassembler -- STEP-MACHINE below only resolves PC/MEMORY, decodes,
;;;; advances PC, and executes.

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

The fetch/decode step itself -- byte-encoded and word-encoded (#20) alike --
is DECODE-INSTRUCTION-AT (decoder.lisp), shared with the disassembler
(disassembler.lisp, #21); this function only resolves PC/MEMORY, advances
PC by the decoded SIZE, and executes."
  (let* ((machine-name (machine-descriptor-name (machine-descriptor machine)))
         (pc (%resolve-pc machine-name pc))
         (memory (%resolve-memory machine-name memory))
         (address (sref machine pc)))
    (multiple-value-bind (descriptor values size)
        (decode-instruction-at (machine-cell-reader machine memory) address machine-name :memory memory)
      (if (eq descriptor :decode-failure)
          :decode-failure
          (progn
            (setf (sref machine pc) (+ address size))
            (execute-instruction descriptor machine values)
            descriptor)))))

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
