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
;;;;
;;;; #75: cycle-cost model, clock speed, and cycle-accurate execution.
;;;; STEP-MACHINE accumulates each executed instruction's (cycles n) cost
;;;; (instruction.lisp) -- defaulting to 1 when undeclared -- onto MACHINE-
;;;; CYCLES (storage.lisp) regardless of whether the machine declares a
;;;; CLOCK-SPEED; only converting that count to wall-time-equivalent seconds
;;;; (MACHINE-ELAPSED-SECONDS, RUN-FOR-DURATION) needs one. RUN, RUN-FOR-
;;;; CYCLES, and RUN-FOR-DURATION share one stop-condition loop, %RUN-LOOP,
;;;; differing only in what additional budget (if any) they check after each
;;;; step -- a budget check happens *after* the step executes, so it may be
;;;; overshot by at most one instruction's own cost (its cost isn't known
;;;; until the instruction has already been decoded and run).

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
too far apart with no other symptom.

#107: writes via %POKE, not MREF -- a ROM image is burned in here, not
stored by the CPU, so this ignores any :ROM region's write protection at
ORIGIN. A :DEVICE region's :WRITE is likewise never called."
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
                 (%poke machine memory address cell)
                 (incf address))
           data))
    (setf (sref machine 'pc) origin)
    machine))

;;; Cycle cost

(defun %descriptor-cycle-cost (descriptor)
  "DESCRIPTOR's cycle cost (#75): its own (cycles n), or 1 when undeclared.
The one place this default lives, so STEP-MACHINE's accumulation and any
future listing annotation (a follow-up ticket) can't disagree on it."
  (or (instruction-descriptor-cycles descriptor) 1))

;;; Step

(defun step-machine (machine &key pc memory)
  "Fetch one instruction from MACHINE's MEMORY at its PC register, advance
PC past it, then execute it against MACHINE. Returns (VALUES result cost):
RESULT is the executed INSTRUCTION-DESCRIPTOR, or :DECODE-FAILURE (without
advancing PC or executing anything, COST 0) when the byte(s) at PC do not
decode to a registered instruction -- distinguishable from an UNKNOWN-
INSTRUCTION signal so RUN can treat it as an ordinary stop reason rather
than a crash. COST is the executed instruction's cycle cost (#75,
%DESCRIPTOR-CYCLE-COST), already added to MACHINE-CYCLES by the time this
returns.

PC is advanced past the whole instruction *before* executing its
semantics, not after -- so a branch instruction's own (set! pc operand)
in its semantics overrides the increment, rather than being clobbered by
it. MACHINE-CYCLES is likewise incremented before executing semantics, not
after -- so an instruction whose semantics signal LASM-TRAP still counts
its own cost, the same way RUN still counts a trapping step (its semantics
ran to completion before signalling).

The fetch/decode step itself -- byte-encoded and word-encoded (#20) alike --
is DECODE-INSTRUCTION-AT (decoder.lisp), shared with the disassembler
(disassembler.lisp, #21); this function only resolves PC/MEMORY, advances
PC by the decoded SIZE, accounts cycles, and executes.

DECODE-INSTRUCTION-AT's fourth value, CHOICES (#73) -- the matched ONE-OF
alternative per operand hole -- is forwarded straight to EXECUTE-INSTRUCTION,
so a (semantics ...) body's CHOICE-CASE sees exactly what was actually
decoded, not just the values."
  (let* ((machine-name (machine-descriptor-name (machine-descriptor machine)))
         (pc (%resolve-pc machine-name pc))
         (memory (%resolve-memory machine-name memory))
         (address (sref machine pc)))
    (multiple-value-bind (descriptor values size choices)
        (decode-instruction-at (machine-cell-reader machine memory) address machine-name :memory memory)
      (if (eq descriptor :decode-failure)
          (values :decode-failure 0)
          (let ((cost (%descriptor-cycle-cost descriptor)))
            (setf (sref machine pc) (+ address size))
            (incf (machine-cycles machine) cost)
            (execute-instruction descriptor machine values choices)
            (values descriptor cost))))))

;;; Run

(defun %run-loop (machine &key pc memory (max-steps 10000) stop-reason stop-p on-step)
  "Shared stop-condition loop behind RUN, RUN-FOR-CYCLES, and RUN-FOR-
DURATION. Repeatedly STEP-MACHINE against MACHINE until one of:
  :TRAP           -- an instruction's semantics signalled LASM-TRAP; the
                      condition itself is returned as a third value.
  :DECODE-FAILURE -- STEP-MACHINE hit a byte that is not a registered
                      opcode.
  STOP-REASON     -- STOP-P (a no-argument predicate, checked after each
                      step successfully executes, once its cost is already
                      on MACHINE-CYCLES) returned true. NIL/NIL is RUN's own
                      \"no extra budget\" case, where this never trips.
  :MAX-STEPS      -- MAX-STEPS instructions executed without stopping
                      otherwise (a runaway-program guard, not a real timer).
Returns (VALUES reason steps [condition]).

STOP-P is checked *after* the step executes -- a cycle/duration budget may
be overshot by at most one instruction's own cost, since that cost isn't
known until the instruction has already been decoded and run. ON-STEP, when
given, is called with the step's cost after it executes but before STOP-P
is checked (RUN-FOR-DURATION's :THROTTLE hook)."
  (loop for steps from 0 below max-steps
        do (handler-case
               (multiple-value-bind (result cost) (step-machine machine :pc pc :memory memory)
                 (when (eq result :decode-failure)
                   (return-from %run-loop (values :decode-failure steps)))
                 (when on-step (funcall on-step cost))
                 (when (and stop-p (funcall stop-p))
                   (return-from %run-loop (values stop-reason (1+ steps)))))
             (lasm-trap (c)
               ;; The trapping instruction's semantics ran to completion (the
               ;; trap fires from inside them) before signalling, so it
               ;; counts as an executed step -- unlike a decode failure,
               ;; where nothing was executed this iteration.
               (return-from %run-loop (values :trap (1+ steps) c))))
        finally (return (values :max-steps steps))))

(defun run (machine &key pc memory (max-steps 10000))
  "Repeatedly STEP-MACHINE against MACHINE until :TRAP, :DECODE-FAILURE, or
:MAX-STEPS -- see %RUN-LOOP. Returns (VALUES reason steps [condition])."
  (%run-loop machine :pc pc :memory memory :max-steps max-steps))

;;; Cycle-accurate execution (#75)

(defun run-for-cycles (machine cycles &key pc memory (max-steps 10000))
  "Like RUN, but also stops with :MAX-CYCLES once MACHINE-CYCLES has
advanced by at least CYCLES since this call started (independent of any
CLOCK-SPEED -- a plain cycle budget). May overshoot CYCLES by at most one
instruction's own cost; see %RUN-LOOP."
  (let ((start (machine-cycles machine)))
    (%run-loop machine :pc pc :memory memory :max-steps max-steps
                        :stop-reason :max-cycles
                        :stop-p (lambda () (>= (- (machine-cycles machine) start) cycles)))))

(defun machine-elapsed-seconds (machine)
  "MACHINE-CYCLES converted to wall-time-equivalent seconds using MACHINE's
declared CLOCK-SPEED (defmachine's (clock-speed n) clause, machine.lisp).
Pure arithmetic -- no timer involved, and available regardless of whether
any run has ever throttled. Signals if MACHINE's descriptor declares no
CLOCK-SPEED, since there is then no rate to convert against."
  (let* ((descriptor (machine-descriptor machine))
         (clock-speed (machine-descriptor-clock-speed descriptor)))
    (unless clock-speed
      (error "MACHINE-ELAPSED-SECONDS on machine ~S: no (clock-speed n) clause ~
declared -- cycles cannot be converted to seconds without one"
             (machine-descriptor-name descriptor)))
    (/ (machine-cycles machine) (float clock-speed 1.0d0))))

(defun run-for-duration (machine seconds &key pc memory (max-steps 10000) throttle)
  "Like RUN, but also stops with :DURATION once the wall-time-equivalent of
the cycles consumed since this call started (elapsed-cycles / CLOCK-SPEED)
reaches SECONDS. Requires MACHINE's descriptor to declare a (clock-speed n)
clause (machine.lisp) -- signals otherwise, same as MACHINE-ELAPSED-SECONDS.

THROTTLE (default NIL) additionally paces real wall-clock time to match:
after each step, it measures real elapsed time against simulated elapsed
time (via trivial-high-precision-timer) and SLEEPs off any surplus once it
exceeds ~1ms -- SBCL's SLEEP floors near that resolution, so sleeping on
every single step (each perhaps a few hundred nanoseconds of simulated time)
would slow execution by orders of magnitude rather than pace it. Recomputed
from scratch every step (not accumulated), so it self-corrects rather than
drifting. With THROTTLE NIL (the default), :DURATION is purely a cycle
budget expressed in simulated seconds -- no timer is touched at all."
  (let* ((descriptor (machine-descriptor machine))
         (clock-speed (machine-descriptor-clock-speed descriptor)))
    (unless clock-speed
      (error "RUN-FOR-DURATION on machine ~S: no (clock-speed n) clause ~
declared -- cycles cannot be converted to seconds without one"
             (machine-descriptor-name descriptor)))
    (let* ((start (machine-cycles machine))
           (timer (and throttle (trivial-high-precision-timer:make-precision-timer)))
           (clock-speed-f (float clock-speed 1.0d0)))
      (%run-loop machine :pc pc :memory memory :max-steps max-steps
                          :stop-reason :duration
                          :stop-p (lambda ()
                                    (>= (/ (- (machine-cycles machine) start) clock-speed-f) seconds))
                          :on-step (and throttle
                                        (lambda (cost)
                                          (declare (ignore cost))
                                          (let* ((simulated (/ (- (machine-cycles machine) start) clock-speed-f))
                                                 (real (trivial-high-precision-timer:sec
                                                        timer (trivial-high-precision-timer:now timer)))
                                                 (deficit (- simulated real)))
                                            (when (> deficit 0.001d0)
                                              (sleep deficit)))))))))
