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

;;; PC / memory resolution

(defun %resolve-pc (machine-name pc)
  (or pc
      (let* ((descriptor (find-machine-descriptor machine-name))
             (element (gethash 'pc (machine-descriptor-table descriptor))))
        (if (and element (eq (storage-element-kind element) :register))
            'pc
            (error "RUN/STEP-MACHINE on machine ~S: no register named PC -- ~
pass :PC explicitly" machine-name)))))

(defun %resolve-memory (machine-name memory)
  (or memory
      (let* ((descriptor (find-machine-descriptor machine-name))
             (mem-elements (remove-if-not (lambda (e) (eq (storage-element-kind e) :memory))
                                           (machine-descriptor-elements descriptor))))
        (cond
          ((null mem-elements)
           (error "RUN/STEP-MACHINE on machine ~S: no memory element declared" machine-name))
          ((> (length mem-elements) 1)
           (error "RUN/STEP-MACHINE on machine ~S: more than one memory element ~
declared (~S) -- pass :MEMORY explicitly" machine-name
                  (mapcar #'storage-element-name mem-elements)))
          (t (storage-element-name (first mem-elements)))))))

;;; Loading

(defun load-program (machine bytes &key memory origin)
  "Write BYTES (an ASSEMBLY, or any sequence of (unsigned-byte 8)) into
MACHINE's MEMORY element starting at ORIGIN, and set MACHINE's PC register
to ORIGIN. MEMORY defaults per %RESOLVE-MEMORY. ORIGIN defaults to BYTES'
own ASSEMBLY-ORIGIN when BYTES is an ASSEMBLY (so a program assembled with
:ORIGIN #x200 always loads where its labels were computed against),
otherwise 0."
  (let* ((machine-name (machine-descriptor-name (machine-descriptor machine)))
         (memory (%resolve-memory machine-name memory))
         (assembly-p (assembly-p bytes))
         (origin (or origin (if assembly-p (assembly-origin bytes) 0)))
         (data (if assembly-p (assembly-bytes bytes) bytes)))
    (let ((address origin))
      (map nil (lambda (byte)
                 (setf (mref machine memory address) byte)
                 (incf address))
           data))
    (setf (sref machine 'pc) origin)
    machine))

;;; Step

(defun step-machine (machine &key pc memory)
  "Fetch one instruction from MACHINE's MEMORY at its PC register, advance
PC past it, then execute it against MACHINE. Returns the executed
INSTRUCTION-DESCRIPTOR, or :DECODE-FAILURE (without advancing PC or
executing anything) when the byte at PC is not a registered opcode --
distinguishable from an UNKNOWN-INSTRUCTION signal so RUN can treat it as
an ordinary stop reason rather than a crash.

PC is advanced past the whole instruction *before* executing its
semantics, not after -- so a branch instruction's own (set! pc operand)
in its semantics overrides the increment, rather than being clobbered by
it."
  (let* ((machine-name (machine-descriptor-name (machine-descriptor machine)))
         (pc (%resolve-pc machine-name pc))
         (memory (%resolve-memory machine-name memory))
         (address (sref machine pc))
         (opcode (mref machine memory address)))
    (handler-case
        (let* ((descriptor (find-instruction-by-opcode machine-name opcode))
               (mode (instruction-descriptor-mode descriptor))
               (width (or (instruction-descriptor-operand-width descriptor) 0))
               (value (when (plusp width)
                        (loop with v = 0
                              for i below width
                              do (setf v (logior v (ash (mref machine memory (+ address 1 i)) (* 8 i))))
                              finally (return v)))))
          ;; A RELATIVE operand (mode.lisp, #23) was assembled as a signed
          ;; offset (assembler.lisp's %RELATIVE-OFFSET) but is fetched above
          ;; as an unsigned WIDTH-byte quantity, like every other operand --
          ;; reinterpret it here so semantics can write a plain
          ;; (set! pc (+ pc operand)) with no width of its own to track.
          (when (and mode (mode-descriptor-relativep mode))
            (setf value (signed-value value (* 8 width))))
          (setf (sref machine pc) (+ address 1 width))
          (execute-instruction descriptor machine value)
          descriptor)
      (unknown-instruction () :decode-failure))))

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
