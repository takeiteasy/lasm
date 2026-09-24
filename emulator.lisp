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
;;;;
;;;; #108: STEP-MACHINE also ticks every live device on the machine's bus
;;;; (TICK-DEVICES, device.lisp) with each step's own cost, once per step,
;;;; regardless of which of the five entry points into STEP-MACHINE ran it
;;;; (RUN/RUN-FOR-CYCLES/RUN-FOR-DURATION via %RUN-LOOP, or the debugger's
;;;; DEBUG-STEP calling STEP-MACHINE directly).

(in-package #:lasm)

;;; Idle/sleep (#110)

(defun machine-idle-p (machine)
  "T when MACHINE is currently idle (the IDLE semantics primitive,
semantics.lisp, has run and no interrupt has been delivered since)."
  (machine-idle machine))

(defun wake-machine (machine)
  "Clear MACHINE's idle state directly, without an interrupt. For a host
driving a machine that declares no (interrupts ...) clause -- IDLE's own
macroexpansion never signals on such a machine (#110), so this is the only
way such a machine wakes back up."
  (setf (machine-idle machine) nil)
  machine)

;;; PC resolution

(defun %resolve-pc (machine-name pc)
  (or pc
      (let* ((descriptor (find-machine-descriptor machine-name))
             (element (gethash 'pc (machine-descriptor-table descriptor))))
        (if (and element (eq (storage-element-kind element) :register))
            'pc
            (%emulator-usage-error "RUN/STEP-MACHINE on machine ~S: no register named PC -- ~
pass :PC explicitly" machine-name)))))

;;; Loading

(defun load-program (machine cells &key memory origin bank)
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
ORIGIN. A :DEVICE region's :WRITE is likewise never called.

BANK loads CELLS into that bank of the banked region containing ORIGIN,
whether or not it is mapped in, leaving the mapping and PC untouched.
Signals if ORIGIN is not in a banked region or CELLS run past its end.

An ASSEMBLY loaded without BANK is retained as MACHINE-PROGRAM, with its
memory and load offset, so runtime conditions can name the source line; a
load of raw cells clears it, and RESET does too.

An ASSEMBLY that placed output in banks with .BANK also has each of those
banks filled, without changing the mapping, when BANK is not given. Signals
if main-image output lies in a banked window whose mapped bank the assembly
also has an image for, since that image would replace it."
  (let* ((machine-name (machine-descriptor-name (machine-descriptor machine)))
         (memory (%resolve-memory machine-name memory))
         (assembly-p (assembly-p cells))
         (origin (or origin (if assembly-p (assembly-origin cells) 0)))
         (data (if assembly-p (assembly-cells cells) cells)))
    (when assembly-p
      (let ((target-width (%machine-cell-width machine-name memory))
            (source-width (assembly-cell-width cells)))
        (unless (= target-width source-width)
          (%emulator-usage-error "LOAD-PROGRAM on machine ~S: assembly's cell width (~D) does not ~
match memory ~S's cell width (~D)" machine-name source-width memory target-width))))
    (when (and assembly-p (not bank))
      (%check-main-image-banks machine memory cells))
    (unless bank
      (setf (machine-program machine) (and assembly-p cells)
            (machine-program-memory machine) memory
            (machine-program-offset machine) (if assembly-p (- origin (assembly-origin cells)) 0)))
    (if bank
        (%load-into-bank machine memory origin data bank)
        (let ((address origin))
          (map nil (lambda (cell)
                     (%poke machine memory address cell)
                     (incf address))
               data)
          (setf (sref machine 'pc) origin)
          (%record-loaded-banks machine memory origin (length data))
          (when assembly-p
            (dolist (image (assembly-banks cells))
              (%load-into-bank machine memory (bank-image-origin image)
                               (bank-image-cells image) (bank-image-bank image))))))
    machine))

(defun %check-main-image-banks (machine memory assembly)
  "Signal if ASSEMBLY's main-image output lies in a banked window of MEMORY
whose mapped bank ASSEMBLY also has an image for."
  (let ((element (descriptor-element (machine-descriptor machine) memory)))
    (loop for (owner . region) in (%banked-regions (machine-descriptor machine))
          for name = (memory-region-name region)
          for bank = (current-bank machine name)
          when (and (eq owner element) (assembly-bank-image assembly name bank))
            do (dolist (l (assembly-listing assembly))
                 (when (and (null (listing-line-region l))
                            (plusp (listing-line-size l))
                            (<= (listing-line-address l) (memory-region-end region))
                            (< (memory-region-start region)
                               (+ (listing-line-address l) (listing-line-size l))))
                   (%emulator-usage-error "LOAD-PROGRAM: main-image output at ~D lands in bank ~D of ~(~A~), ~
which the assembly's bank image replaces"
                          (listing-line-address l) bank name))))))

(defun %record-loaded-banks (machine memory origin length)
  "Note the mapped bank of each banked region of MEMORY that LENGTH cells
loaded at ORIGIN overlap."
  (let ((element (descriptor-element (machine-descriptor machine) memory)))
    (loop for (owner . region) in (%banked-regions (machine-descriptor machine))
          when (and (eq owner element)
                    (<= origin (memory-region-end region))
                    (< (memory-region-start region) (+ origin length)))
            do (setf (gethash (memory-region-name region) (machine-loaded-banks machine))
                     (current-bank machine (memory-region-name region))))))

(defun %load-into-bank (machine memory origin data bank)
  (let* ((element (descriptor-element (machine-descriptor machine) memory))
         (region (find-if (lambda (r)
                            (and (memory-region-banks r)
                                 (<= (memory-region-start r) origin (memory-region-end r))))
                          (storage-element-regions element))))
    (unless region
      (%emulator-usage-error "LOAD-PROGRAM :BANK ~S: address ~S of memory ~S is not in a banked region"
             bank origin memory))
    (when (> (+ origin (length data) -1) (memory-region-end region))
      (%emulator-usage-error "LOAD-PROGRAM :BANK ~S: ~D cells at ~S run past the end of region ~S"
             bank (length data) origin (memory-region-name region)))
    (loop for cell across (coerce data 'vector)
          for address from origin
          do (setf (bank-peek machine (memory-region-name region) bank address) cell))))

;;; Cycle cost

(defun %descriptor-cycle-cost (descriptor)
  "DESCRIPTOR's cycle cost (#75): its own (cycles n), or 1 when undeclared.
The one place this default lives, so STEP-MACHINE's accumulation and any
future listing annotation (a follow-up ticket) can't disagree on it."
  (or (instruction-descriptor-cycles descriptor) 1))

;;; Step

(defun %undefined-opcode-step (machine pc address memory machine-name layout)
  "The undecodable instruction at ADDRESS, handled per the machine's
undefined-opcode policy: :FAULT returns :DECODE-FAILURE, :TRAP signals
LASM-TRAP, :NOP steps over it, evaluating no operand. Returns (VALUES result
cost)."
  (let ((policy (machine-descriptor-undefined-opcode (machine-descriptor machine))))
    (if (eq policy :fault)
        (values :decode-failure 0)
        (multiple-value-bind (size opcode removedp)
            (%undefined-opcode-extent (machine-peek-reader machine memory) address machine-name layout)
          (ecase policy
            (:trap (error 'lasm-trap :tag :undefined-opcode
                                     :data (list :pc address :opcode opcode)))
            (:nop
             (let ((cost (if removedp size 1)))
               (setf (%sref machine pc) (+ address size))
               (incf (machine-cycles machine) cost)
               (tick-devices machine cost)
               (values :nop cost))))))))

(defun %locate-runtime-condition (condition machine address memory)
  "Record on CONDITION the instruction at ADDRESS that raised it, and its
source line when MACHINE retained its program."
  (unless (runtime-location-pc condition)
    (let* ((line (machine-listing-line machine address :memory memory))
           (assembly (machine-program machine)))
      (setf (runtime-location-pc condition) address
            (runtime-location-listing-line condition) line
            (runtime-location-source-text condition)
            (and line (listing-line-source-text line assembly))))))

(defun %step-machine-resolved (machine pc memory machine-name layout cell-width endian)
  "Fetch one instruction from MACHINE's MEMORY at its PC register, advance
PC past it, then execute it against MACHINE. Returns (VALUES result cost):
RESULT is the executed INSTRUCTION-DESCRIPTOR, or :DECODE-FAILURE (without
advancing PC or executing anything, COST 0) when the byte(s) at PC do not
decode to a registered instruction -- distinguishable from an UNKNOWN-
INSTRUCTION signal so RUN can treat it as an ordinary stop reason rather
than a crash. COST is the executed instruction's cycle cost (#75,
%DESCRIPTOR-CYCLE-COST), already added to MACHINE-CYCLES by the time this
returns.

#90: an instruction's semantics may add cycles beyond its declared cost
with (extra-cycles n) (semantics.lisp) -- a page-crossing or branch-taken
penalty, say. They are added to MACHINE-CYCLES even if the instruction
traps, ticked to devices in a second TICK-DEVICES call once the semantics
return (never during a trap's unwind, where a signalling device would
replace the LASM-TRAP), and included in the returned COST. A machine that
never calls EXTRA-CYCLES sees exactly one tick per step, as before.

PC is advanced past the whole instruction *before* executing its
semantics, not after -- so a branch instruction's own (set! pc operand)
in its semantics overrides the increment, rather than being clobbered by
it. MACHINE-CYCLES is likewise incremented before executing semantics, not
after -- so an instruction whose semantics signal LASM-TRAP still counts
its own cost, the same way RUN still counts a trapping step (its semantics
ran to completion before signalling).

#108: TICK-DEVICES (device.lisp) runs for the same reason, in the same
place -- before EXECUTE-INSTRUCTION, not after, so a trapping instruction's
devices still tick instead of that step silently going missing from device
time (EXECUTE-INSTRUCTION's LASM-TRAP unwinds straight past anything placed
after it). Called with this step's own COST, not on the :DECODE-FAILURE
early return below, where nothing executed and no time elapsed.

#109: DELIVER-PENDING-INTERRUPT (interrupt.lisp) runs first, before PC is
even read -- a pending, unmasked signal is delivered by pushing state,
writing the vector into PC, and (if the machine's (interrupts ...) clause
gives delivery a non-zero cost) ticking devices for it, all before this
step's own fetch -- so the very same step both delivers the interrupt and
executes the handler's first instruction. This is the only point where PC
is unambiguously the next instruction to run -- delivering any later
(e.g. after EXECUTE-INSTRUCTION) would push a return address that skips
whatever this step was about to execute. Placed in STEP-MACHINE itself,
not %RUN-LOOP (emulator.lisp), so single-stepping (DEBUG-STEP,
debugger.lisp) sees delivery too, not just RUN. A machine declaring no
(interrupts ...) clause pays nothing here -- DELIVER-PENDING-INTERRUPT is a
single NULL test on MACHINE-DESCRIPTOR-INTERRUPTS.

#110: if MACHINE is still idle after that delivery attempt (the IDLE
semantics primitive, semantics.lisp, ran on some earlier step and nothing
has woken it since), this step ticks devices and accounts one cycle but
does not fetch, decode, execute, or advance PC -- returning (VALUES :IDLE
1) instead. Checked after DELIVER-PENDING-INTERRUPT, not before, so a
signal delivered this same step both wakes the machine and executes the
handler's first instruction, exactly the same one-step coincidence #109's
own delivery gets against an ordinary fetch. TODO: the idle cost is fixed
at 1 cycle -- a declarable idle cost is a follow-up ticket (#164).

The fetch/decode step itself -- byte-encoded and word-encoded (#20) alike --
is shared with the disassembler through the decoder. This function advances
PC by the decoded SIZE, accounts cycles, and executes.

DECODE-INSTRUCTION-AT's fourth value, CHOICES (#73) -- the matched ONE-OF
alternative per operand hole -- is forwarded straight to EXECUTE-INSTRUCTION,
so a (semantics ...) body's CHOICE-CASE sees exactly what was actually
decoded, not just the values."
  (deliver-pending-interrupt machine pc)
  (when (machine-idle machine)
    (incf (machine-cycles machine) 1)
    (tick-devices machine 1)
    (return-from %step-machine-resolved (values :idle 1)))
  (let ((address (%sref machine pc)))
    (handler-bind ((runtime-location
                    (lambda (c) (%locate-runtime-condition c machine address memory))))
      (multiple-value-bind (descriptor values size choices)
          (%decode-instruction-at-resolved (machine-cell-reader machine memory) address
                                           machine-name layout cell-width endian)
        (if (eq descriptor :decode-failure)
            (%undefined-opcode-step machine pc address memory machine-name layout)
            (let ((cost (%descriptor-cycle-cost descriptor)))
              (setf (%sref machine pc) (+ address size))
              (incf (machine-cycles machine) cost)
              ;; TODO: devices tick once per instruction with its declared
              ;; cost, plus a second tick for any EXTRA-CYCLES (#90) -- a
              ;; device needing intra-instruction resolution can't express
              ;; either; sub-instruction tick granularity is a follow-up
              ;; (#108, #159).
              (tick-devices machine cost)
              (setf (machine-extra-cycles machine) 0)
              (unwind-protect (execute-instruction descriptor machine values choices)
                (incf (machine-cycles machine) (machine-extra-cycles machine)))
              (let ((extra (machine-extra-cycles machine)))
                (when (plusp extra)
                  (tick-devices machine extra))
                (values descriptor (+ cost extra)))))))))

(defun step-machine (machine &key pc memory)
  "Execute one instruction, returning its descriptor and cycle cost, or
:DECODE-FAILURE and zero cost. A machine whose undefined-opcode policy is :NOP
returns :NOP and the skipped cost instead. MEMORY and PC select the fetch
location."
  (let* ((descriptor (machine-descriptor machine))
         (machine-name (machine-descriptor-name descriptor))
         (pc (%resolve-pc machine-name pc))
         (memory (%resolve-memory machine-name memory))
         (layout (machine-descriptor-instruction-word descriptor)))
    (%step-machine-resolved machine pc memory machine-name layout
                            (unless layout (%machine-cell-width machine-name memory))
                            (unless layout (%machine-endian machine-name memory)))))

;;; Run

(defun %run-loop (machine &key pc memory (max-steps 10000) stop-reason stop-p on-step idle-stop)
  "Shared stop-condition loop behind RUN, RUN-FOR-CYCLES, and RUN-FOR-
DURATION. Repeatedly STEP-MACHINE against MACHINE until one of:
  :TRAP           -- an instruction's semantics signalled LASM-TRAP; the
                      condition itself is returned as a third value.
  :FAULT          -- a storage access signalled STORAGE-ERROR; the condition
                      itself is returned as a third value.
  :DECODE-FAILURE -- STEP-MACHINE hit a byte that is not a registered
                      opcode.
  STOP-REASON     -- STOP-P (a no-argument predicate, checked after each
                      step successfully executes, once its cost is already
                      on MACHINE-CYCLES) returned true. NIL/NIL is RUN's own
                      \"no extra budget\" case, where this never trips.
  :IDLE           -- #110: see below. Only when IDLE-STOP is true.
  :MAX-STEPS      -- MAX-STEPS instructions executed without stopping
                      otherwise (a runaway-program guard, not a real timer).
Returns (VALUES reason steps [condition]).

STOP-P is checked *after* the step executes -- a cycle/duration budget may
be overshot by at most one instruction's own cost, since that cost isn't
known until the instruction has already been decoded and run. ON-STEP, when
given, is called with the step's cost after it executes but before STOP-P
is checked (RUN-FOR-DURATION's :THROTTLE hook).

#110: an :IDLE step from STEP-MACHINE is otherwise an ordinary executed
step -- ON-STEP/STOP-P still run, so a cycle/duration budget still
terminates normally (as its own STOP-REASON, e.g. :MAX-CYCLES) on a
sleeping machine, checked before and entirely unaffected by the idle-
exhausted check below. IDLE-STOP (true for RUN and the debugger's
DEBUG-CONTINUE/DEBUG-CONTINUE-TO, false for RUN-FOR-CYCLES/RUN-FOR-
DURATION -- a plain STOP-P being present or absent doesn't by itself say
which case this is, since DEBUG-CONTINUE always passes one, its breakpoint
predicate) gates whether that idle-exhausted check runs at all: when the
machine is *still* idle after a step, its pending interrupt queue is
empty, and no live device remains on its bus, nothing left running this
loop could ever wake it -- so this returns :IDLE itself rather than
spinning to :MAX-STEPS, the same way a decode failure short-circuits
rather than running the budget dry. A host can SIGNAL-INTERRUPT
(interrupt.lisp) or WAKE-MACHINE and call RUN/DEBUG-CONTINUE again,
exactly as it already can after :TRAP."
  (let* ((descriptor (machine-descriptor machine))
         (machine-name (machine-descriptor-name descriptor))
         (layout (machine-descriptor-instruction-word descriptor))
         (selected-pc nil)
         (selected-memory nil)
         (cell-width nil)
         (endian nil)
         (resolved nil))
    (loop for steps from 0 below max-steps
          do (unless resolved
               (setf selected-pc (%resolve-pc machine-name pc)
                     selected-memory (%resolve-memory machine-name memory))
               (unless layout
                 (setf cell-width (%machine-cell-width machine-name selected-memory)
                       endian (%machine-endian machine-name selected-memory)))
               (setf resolved t))
             (handler-case
               (multiple-value-bind (result cost)
                   (handler-case
                       (%step-machine-resolved machine selected-pc selected-memory machine-name
                                               layout cell-width endian)
                     (storage-error (c)
                       (return-from %run-loop (values :fault (1+ steps) c))))
                 (when (eq result :decode-failure)
                   (return-from %run-loop (values :decode-failure steps)))
                 (when on-step (funcall on-step cost))
                 (when (and stop-p (funcall stop-p))
                   (return-from %run-loop (values stop-reason (1+ steps))))
                 (when (and idle-stop
                            (eq result :idle)
                            (machine-idle machine)
                            (null (machine-interrupt-queue machine))
                            (notany #'identity (machine-devices machine)))
                   (return-from %run-loop (values :idle (1+ steps)))))
             (lasm-trap (c)
               ;; The trapping instruction's semantics ran to completion (the
               ;; trap fires from inside them) before signalling, so it
               ;; counts as an executed step -- unlike a decode failure,
               ;; where nothing was executed this iteration.
               (return-from %run-loop (values :trap (1+ steps) c))))
          finally (return (values :max-steps steps)))))

(defun run (machine &key pc memory (max-steps 10000))
  "Repeatedly STEP-MACHINE against MACHINE until :TRAP, :FAULT, :DECODE-FAILURE,
:IDLE (#110 -- the machine went idle with nothing left that could wake it,
see %RUN-LOOP), or :MAX-STEPS. Returns (VALUES reason steps [condition])."
  (%run-loop machine :pc pc :memory memory :max-steps max-steps :idle-stop t))

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
      (%emulator-usage-error "MACHINE-ELAPSED-SECONDS on machine ~S: no (clock-speed n) clause ~
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
      (%emulator-usage-error "RUN-FOR-DURATION on machine ~S: no (clock-speed n) clause ~
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
