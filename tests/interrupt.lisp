;;;; tests/interrupt.lisp
;;;; #109: the interrupt-delivery subsystem -- the (interrupts ...) clause,
;;;; SIGNAL-INTERRUPT/DEVICE-SIGNAL enqueueing, overflow policy, masking,
;;;; delivery under STEP-MACHINE/RUN/DEBUG-STEP, RESET, and INTERRUPT-RETURN.
;;;;
;;;; INTERRUPT-TEST-MACHINE is its own fixture, same reason tests/device.lisp
;;;; keeps its own rather than extending SUITES.LISP's shared TEST-MACHINE.

(in-package #:lasm)

(fiveam:def-suite interrupt :in lasm)
(fiveam:in-suite interrupt)

;;; Fixture

;; A signalling device: TICK raises an interrupt once ARMED is true, then
;; disarms itself so a test controls exactly when one signal fires.
(defvar *armed* nil)

(defun %signaller-init (machine device) (declare (ignore machine device)) nil)

(defun %signaller-tick (machine device cycles)
  (declare (ignore cycles))
  (when *armed*
    (setf *armed* nil)
    ;; #xDE, not a keyword -- the delivered signal's DATA is written into
    ;; the :MESSAGE register via SETF SREF, which wraps it as an unsigned
    ;; integer (WRAP-VALUE, storage.lisp); a device's payload must be one.
    (device-signal machine device #xde)))

(defmachine interrupt-test-machine
  (register pc :width 16)
  (register ia :width 16)
  (register a :width 16)
  (register b :width 16)
  (stack sp :width 16 :depth 8)
  (flags iaq)
  (memory ram :width 8 :addr-width 16)
  (device signaller :init %signaller-init :tick %signaller-tick)
  (interrupts :vector ia :message a :save (pc b) :mask-flag iaq :cycles 0))

(definstruction interrupt-test-machine nop (encoding (opcode #x00)) (semantics nil) (cycles 1))
(definstruction interrupt-test-machine hlt (encoding (opcode #xff)) (semantics (trap :halt)))
(definstruction interrupt-test-machine rfi (encoding (opcode #x01)) (semantics (interrupt-return)))
(definstruction interrupt-test-machine int
  (encoding (opcode #x02))
  (semantics (signal-interrupt machine (sref machine 'b))))
(definstruction interrupt-test-machine slp (encoding (opcode #x03)) (semantics (idle)) (cycles 1))

;;; DEFMACHINE-time clause parsing / validation

(fiveam:test defmachine-accepts-a-well-formed-interrupts-clause
  (fiveam:finishes (make-machine 'interrupt-test-machine)))

(fiveam:test defmachine-rejects-more-than-one-interrupts-clause
  (fiveam:signals machine-definition-error
    (eval '(defmachine interrupt-dup-clause-test
             (register pc :width 8) (register ia :width 8) (register a :width 8)
             (stack sp :width 8 :depth 4)
             (memory ram :width 8 :addr-width 8)
             (interrupts :vector ia :message a :save (pc))
             (interrupts :vector ia :message a :save (pc))))))

(fiveam:test defmachine-rejects-interrupts-vector-naming-nothing
  (fiveam:signals machine-definition-error
    (eval '(defmachine interrupt-bad-vector-test
             (register pc :width 8) (register a :width 8)
             (stack sp :width 8 :depth 4)
             (memory ram :width 8 :addr-width 8)
             (interrupts :vector nosuch :message a :save (pc))))))

(fiveam:test defmachine-rejects-interrupts-save-naming-nothing
  (fiveam:signals machine-definition-error
    (eval '(defmachine interrupt-bad-save-test
             (register pc :width 8) (register ia :width 8) (register a :width 8)
             (stack sp :width 8 :depth 4)
             (memory ram :width 8 :addr-width 8)
             (interrupts :vector ia :message a :save (nosuch))))))

(fiveam:test defmachine-rejects-interrupts-mask-flag-naming-a-non-flag
  (fiveam:signals machine-definition-error
    (eval '(defmachine interrupt-bad-mask-flag-test
             (register pc :width 8) (register ia :width 8) (register a :width 8)
             (stack sp :width 8 :depth 4)
             (memory ram :width 8 :addr-width 8)
             (interrupts :vector ia :message a :save (pc) :mask-flag a)))))

(fiveam:test defmachine-rejects-a-banked-register-as-vector
  (fiveam:signals machine-definition-error
    (eval '(defmachine interrupt-banked-vector-test
             (register pc :width 8) (register ia :width 8 :count 4) (register a :width 8)
             (stack sp :width 8 :depth 4)
             (memory ram :width 8 :addr-width 8)
             (interrupts :vector ia :message a :save (pc))))))

(fiveam:test defmachine-rejects-both-mask-when-and-mask-flag
  (fiveam:signals machine-definition-error
    (eval '(defmachine interrupt-both-masks-test
             (register pc :width 8) (register ia :width 8) (register a :width 8)
             (stack sp :width 8 :depth 4) (flags z)
             (memory ram :width 8 :addr-width 8)
             (interrupts :vector ia :message a :save (pc) :mask-when zerop :mask-flag z)))))

(fiveam:test defmachine-rejects-interrupts-with-no-stack-and-no-explicit-one
  (fiveam:signals machine-definition-error
    (eval '(defmachine interrupt-no-stack-test
             (register pc :width 8) (register ia :width 8) (register a :width 8)
             (memory ram :width 8 :addr-width 8)
             (interrupts :vector ia :message a :save (pc))))))

(fiveam:test defmachine-rejects-interrupts-with-ambiguous-stack
  (fiveam:signals machine-definition-error
    (eval '(defmachine interrupt-ambiguous-stack-test
             (register pc :width 8) (register ia :width 8) (register a :width 8)
             (stack sp1 :width 8 :depth 4) (stack sp2 :width 8 :depth 4)
             (memory ram :width 8 :addr-width 8)
             (interrupts :vector ia :message a :save (pc))))))

;;; Enqueueing: DEVICE-SIGNAL and SIGNAL-INTERRUPT

(fiveam:test device-signal-reaches-the-auto-installed-hook-and-enqueues
  (let ((m (make-machine 'interrupt-test-machine)))
    (setf (sref m 'ia) #x0100)
    (device-signal m (device-at m 0) :hello)
    (fiveam:is (equal (list (list (device-at m 0) :hello 0 nil)) (%pending m)))))

(fiveam:test signal-interrupt-enqueues-with-no-device
  (let ((m (make-machine 'interrupt-test-machine)))
    (setf (sref m 'ia) #x0100)
    (signal-interrupt m 42)
    (fiveam:is (equal (list (list nil 42 0 nil)) (%pending m)))))

(fiveam:test signal-interrupt-with-zero-vector-drops-the-signal-by-default
  (let ((m (make-machine 'interrupt-test-machine)))
    (setf (sref m 'ia) 0)
    (signal-interrupt m 42)
    (fiveam:is (null (%pending m)))))

;; A machine opting out of the zero-vector drop rule, for the negative case.
(defmachine interrupt-zero-vector-ok-test-machine
  (register pc :width 8) (register ia :width 8) (register a :width 8)
  (stack sp :width 8 :depth 4)
  (memory ram :width 8 :addr-width 8)
  (interrupts :vector ia :message a :save (pc) :drop-on-zero-vector nil))

(fiveam:test signal-interrupt-with-zero-vector-queues-when-opted-out
  (let ((m (make-machine 'interrupt-zero-vector-ok-test-machine)))
    (setf (sref m 'ia) 0)
    (signal-interrupt m 42)
    (fiveam:is (= 1 (machine-interrupt-pending-count m)))))

;;; Overflow policy

(defmacro %make-overflow-test-machine (name policy)
  `(defmachine ,name
     (register pc :width 8) (register ia :width 8) (register a :width 8)
     (stack sp :width 8 :depth 4)
     (memory ram :width 8 :addr-width 8)
     (interrupts :vector ia :message a :save (pc) :queue 1 :on-overflow ,policy)))

(%make-overflow-test-machine interrupt-overflow-error-test-machine :error)
(%make-overflow-test-machine interrupt-overflow-trap-test-machine :trap)
(%make-overflow-test-machine interrupt-overflow-drop-test-machine :drop)
(%make-overflow-test-machine interrupt-overflow-drop-oldest-test-machine :drop-oldest)

(fiveam:test overflow-error-policy-signals-interrupt-queue-full
  (let ((m (make-machine 'interrupt-overflow-error-test-machine)))
    (setf (sref m 'ia) 1)
    (signal-interrupt m 1)
    (fiveam:signals interrupt-queue-full (signal-interrupt m 2))))

(fiveam:test overflow-trap-policy-signals-lasm-trap
  (let ((m (make-machine 'interrupt-overflow-trap-test-machine)))
    (setf (sref m 'ia) 1)
    (signal-interrupt m 1)
    (fiveam:signals lasm-trap (signal-interrupt m 2))))

(fiveam:test overflow-drop-policy-discards-the-incoming-signal
  (let ((m (make-machine 'interrupt-overflow-drop-test-machine)))
    (setf (sref m 'ia) 1)
    (signal-interrupt m 1)
    (fiveam:finishes (signal-interrupt m 2))
    (fiveam:is (equal (list (list nil 1 0 nil)) (%pending m)))))

(fiveam:test overflow-drop-oldest-policy-evicts-the-head
  (let ((m (make-machine 'interrupt-overflow-drop-oldest-test-machine)))
    (setf (sref m 'ia) 1)
    (signal-interrupt m 1)
    (fiveam:finishes (signal-interrupt m 2))
    (fiveam:is (equal (list (list nil 2 0 nil)) (%pending m)))))

;;; Delivery under STEP-MACHINE

(fiveam:test pending-signal-delivers-at-the-top-of-the-next-step-machine
  (let ((m (make-machine 'interrupt-test-machine)))
    (setf (sref m 'ia) #x0010 (sref m 'b) 99)
    (load-program m (list #x00) :origin 0) ; nop, pc = 0
    (signal-interrupt m 7)
    ;; Deliver-then-fetch: this one STEP-MACHINE call both delivers (push,
    ;; write MESSAGE, set PC <- VECTOR) and fetches/executes whatever's at
    ;; the vector -- nothing is loaded at #x10, so it decodes as the
    ;; machine's own opcode-0 NOP, advancing PC by that instruction's size.
    (step-machine m)
    ;; :SAVE is (pc b), pushed in that order -- STACK-POP is LIFO, so B (the
    ;; last one pushed) comes back first, then PC.
    (fiveam:is (= 99 (stack-pop m 'sp))) ; b, as it read before delivery
    (fiveam:is (= 0 (stack-pop m 'sp))) ; pc, as it read before delivery
    (fiveam:is (= 7 (sref m 'a))) ; message written
    (fiveam:is (= (1+ #x0010) (sref m 'pc))))) ; vector, plus the implicit nop's size

(fiveam:test with-no-pending-signal-step-machine-behaves-as-before
  (let ((m (make-machine 'interrupt-test-machine)))
    (load-program m (list #x00) :origin 0) ; nop
    (step-machine m)
    (fiveam:is (= 1 (sref m 'pc)))
    (fiveam:is (zerop (stack-depth m 'sp)))))

(fiveam:test deliver-pending-interrupt-ticks-devices-only-when-cycles-is-non-zero
  (let ((m (make-machine 'interrupt-test-machine)))
    (setf (sref m 'ia) #x0010)
    (signal-interrupt m 1)
    ;; INTERRUPT-TEST-MACHINE declares :cycles 0 -- the delivering step must
    ;; not tick devices a second time for a zero-cost delivery. SIGNALLER's
    ;; own TICK only fires when *ARMED*, which is NIL here, so a spurious
    ;; second tick would be silently harmless to observe directly; instead
    ;; assert MACHINE-CYCLES gains nothing beyond the instruction's own cost.
    (load-program m (list #x00) :origin 0) ; nop, cost 1
    (step-machine m)
    (fiveam:is (= 1 (machine-cycles m)))))

;;; Masking

(fiveam:test masked-signal-stays-queued-until-unmasked
  (let ((m (make-machine 'interrupt-test-machine)))
    (setf (sref m 'ia) #x0010 (flag m 'iaq) t)
    (load-program m (list #x00 #x00) :origin 0) ; nop, nop
    (signal-interrupt m 7)
    (step-machine m)
    (fiveam:is (= 1 (machine-interrupt-pending-count m))) ; still queued
    (fiveam:is (/= 7 (sref m 'a)))
    (setf (flag m 'iaq) nil)
    (step-machine m)
    (fiveam:is (null (%pending m)))
    (fiveam:is (= 7 (sref m 'a)))))

;;; RUN / DEBUG-STEP

(fiveam:test run-delivers-a-pending-interrupt-without-disturbing-step-count
  (let ((m (make-machine 'interrupt-test-machine)))
    (setf (sref m 'ia) #x0010)
    (load-program m (list #x00 #xff) :origin 0) ; nop, hlt
    (setf (sref m 'pc) #x0010)
    (load-program m (list #xff) :origin #x0010) ; hlt (the handler)
    (setf (sref m 'pc) 0)
    (signal-interrupt m 7)
    (multiple-value-bind (reason steps) (run m)
      (fiveam:is (eq :trap reason))
      (fiveam:is (= 1 steps))))) ; delivery + the handler's hlt count as one step

(fiveam:test debug-step-also-sees-delivery
  (let* ((m (make-machine 'interrupt-test-machine))
         (session (progn (setf (sref m 'ia) #x0010)
                          (load-program m (list #x00) :origin 0)
                          (signal-interrupt m 7)
                          (make-debug-session m))))
    (debug-step session)
    ;; Deliver-then-fetch: this one step delivers (PC <- vector) and then
    ;; also executes whatever's at the vector -- nothing is loaded at
    ;; #x10, so it decodes as the implicit opcode-0 NOP, advancing PC once
    ;; more past it.
    (fiveam:is (= (1+ #x0010) (sref m 'pc)))
    (fiveam:is (= 7 (sref m 'a)))))

;;; RESET

(fiveam:test reset-clears-the-pending-queue
  (let ((m (make-machine 'interrupt-test-machine)))
    (setf (sref m 'ia) #x0010)
    (signal-interrupt m 7)
    (fiveam:is (= 1 (machine-interrupt-pending-count m)))
    (reset m)
    (fiveam:is (null (%pending m)))))

(fiveam:test reset-leaves-a-host-replaced-hook-alone
  (let ((m (make-machine 'interrupt-test-machine))
        (custom (lambda (machine device data) (declare (ignore machine device data)))))
    (setf (machine-interrupt-hook m) custom)
    (reset m)
    (fiveam:is (eq custom (machine-interrupt-hook m)))))

(fiveam:test reset-leaves-the-auto-installed-hook-in-place
  (let ((m (make-machine 'interrupt-test-machine)))
    (reset m)
    (fiveam:is (eq #'%default-interrupt-hook (machine-interrupt-hook m)))))

;;; INTERRUPT-RETURN

(fiveam:test interrupt-return-restores-save-places-in-reverse-order
  (let ((m (make-machine 'interrupt-test-machine)))
    (setf (sref m 'ia) #x0010 (sref m 'b) 99)
    (load-program m (list #x00) :origin 0) ; nop
    (signal-interrupt m 7)
    (step-machine m) ; delivers: pushes pc(=0), b(=99); a<-7; pc<-#x0010
    (load-program m (list #x01) :origin #x0010) ; rfi
    (step-machine m)
    (fiveam:is (= 99 (sref m 'b))) ; restored
    (fiveam:is (= 0 (sref m 'pc))) ; restored to the pre-interrupt pc
    (fiveam:is (zerop (stack-depth m 'sp)))))

(defmachine interrupt-return-no-clause-test
  (register pc :width 8)
  (memory ram :width 8 :addr-width 8))

(fiveam:test interrupt-return-on-a-machine-with-no-interrupts-clause-errors-at-macroexpansion-time
  ;; Mirrors tests/semantics.lisp's own PUSH/POP-with-no-stack coverage --
  ;; (WITH-MACHINE ...), not DEFINSTRUCTION, since SBCL's EVAL of a DEFUN-
  ;; shaped form downgrades a macroexpansion-time error to a compiler
  ;; warning plus a runtime-erroring stub rather than propagating it.
  (fiveam:signals error
    (eval '(with-machine (m interrupt-return-no-clause-test) (interrupt-return)))))

;;; :STACK given explicitly, and :CYCLES > 0 actually ticking devices

;; A separate fixture declaring two stacks -- INTERRUPT-TEST-MACHINE above
;; has exactly one, so it never exercises the :STACK keyword itself, only
;; %RESOLVE-INTERRUPT-STACK's no-:STACK-given default path.
(defvar *cost-log* nil)

(defun %cost-logger-init (machine device) (declare (ignore machine device)) nil)
(defun %cost-logger-tick (machine device cycles)
  (declare (ignore machine device))
  (cl:push cycles *cost-log*))

(defmachine interrupt-explicit-stack-test-machine
  (register pc :width 8) (register ia :width 8) (register a :width 8)
  (stack sp1 :width 8 :depth 4) (stack sp2 :width 8 :depth 4)
  (memory ram :width 8 :addr-width 8)
  (device cost-logger :init %cost-logger-init :tick %cost-logger-tick)
  (interrupts :vector ia :message a :save (pc) :stack sp2 :cycles 5))

(definstruction interrupt-explicit-stack-test-machine nop
  (encoding (opcode #x00)) (semantics nil) (cycles 1))

(fiveam:test interrupts-stack-keyword-selects-the-named-stack-on-a-multi-stack-machine
  (let ((m (make-machine 'interrupt-explicit-stack-test-machine)))
    (setf (sref m 'ia) 1)
    (load-program m (list #x00) :origin 0) ; nop
    (signal-interrupt m 9)
    (step-machine m)
    (fiveam:is (= 1 (stack-depth m 'sp2))) ; pushed onto the named stack
    (fiveam:is (zerop (stack-depth m 'sp1))))) ; the other stack is untouched

(fiveam:test delivery-with-non-zero-cycles-ticks-devices-with-that-cost
  (let ((m (make-machine 'interrupt-explicit-stack-test-machine))
        (*cost-log* nil))
    (setf (sref m 'ia) 1)
    (load-program m (list #x00) :origin 0) ; nop, own cost 1
    (signal-interrupt m 9)
    (step-machine m) ; delivers (cost 5), then executes the implicit nop at the vector (cost 1)
    (fiveam:is (equal '(1 5) *cost-log*)))) ; delivery's own tick, then the fetched instruction's

;;; INTERRUPT-RETURN restoring a saved flag

;; INTERRUPT-TEST-MACHINE saves only registers; this fixture saves a flag.
(defmachine interrupt-flag-save-test-machine
  (register pc :width 8) (register ia :width 8) (register a :width 8)
  (stack sp :width 8 :depth 4)
  (flags z)
  (memory ram :width 8 :addr-width 8)
  (interrupts :vector ia :message a :save (z)))

(definstruction interrupt-flag-save-test-machine rfi
  (encoding (opcode #x01)) (semantics (interrupt-return)))
(definstruction interrupt-flag-save-test-machine nop
  (encoding (opcode #x00)) (semantics nil))

(fiveam:test interrupt-return-restores-a-saved-flag-from-stack
  (let ((m (make-machine 'interrupt-flag-save-test-machine)))
    (setf (sref m 'ia) #x10 (flag m 'z) nil) ; Z starts false -- pushes as 0
    (load-program m (list #x00) :origin 0) ; nop
    (signal-interrupt m 0)
    (step-machine m) ; delivers: pushes z(=0); a<-0; pc<-#x10
    (load-program m (list #x01) :origin #x10) ; rfi
    (step-machine m)
    (fiveam:is (zerop (flag m 'z)))
    (fiveam:is (zerop (stack-depth m 'sp)))))

;;; INT-style software interrupt end-to-end

(fiveam:test device-raised-signal-delivers-one-step-after-its-own-tick
  (let ((m (make-machine 'interrupt-test-machine)))
    (setf (sref m 'ia) #x0010)
    (setf *armed* t)
    (load-program m (list #x00) :origin 0) ; nop -- its own TICK raises the signaller's signal
    (step-machine m) ; ticks the signaller mid-step; too late for this step's own delivery check
    (fiveam:is (= 1 (sref m 'pc)))
    (fiveam:is (= 1 (machine-interrupt-pending-count m)))
    (fiveam:is (/= #xde (sref m 'a)))
    (step-machine m) ; now delivers, then executes the implicit nop at the vector
    (fiveam:is (null (%pending m)))
    (fiveam:is (= #xde (sref m 'a)))
    (fiveam:is (= (1+ #x0010) (sref m 'pc)))))

;;; IDLE / wake (#110)

(fiveam:test idle-signal-raised-mid-idle-step-delivers-on-the-next-step
  (let ((m (make-machine 'interrupt-test-machine)))
    (setf (sref m 'ia) #x0010)
    (load-program m (list #x03) :origin 0) ; slp
    (step-machine m) ; executes slp -- idle set, pc now 1
    (fiveam:is (machine-idle-p m))
    (fiveam:is (eq :idle (step-machine m))) ; idle step, queue empty, signaller not armed yet
    (setf *armed* t)
    (fiveam:is (eq :idle (step-machine m))) ; idle step -- its own tick raises the signal,
    ;; too late for this same step's delivery check (which already ran at the top)
    (fiveam:is (= 1 (machine-interrupt-pending-count m)))
    (fiveam:is (machine-idle-p m)) ; still idle -- not delivered yet
    (step-machine m) ; now delivers: clears idle, pc <- vector, then fetches there
    (fiveam:is (not (machine-idle-p m)))
    (fiveam:is (= #xde (sref m 'a)))
    (fiveam:is (= (1+ #x0010) (sref m 'pc)))))

(fiveam:test masked-machine-stays-idle-while-signals-queue-and-wakes-on-unmask
  (let ((m (make-machine 'interrupt-test-machine)))
    (setf (sref m 'ia) #x0010 (flag m 'iaq) t)
    (load-program m (list #x03) :origin 0) ; slp
    (step-machine m) ; idle set
    (signal-interrupt m 7)
    (step-machine m) ; delivery is masked -- stays idle, signal stays queued
    (fiveam:is (machine-idle-p m))
    (fiveam:is (= 1 (machine-interrupt-pending-count m)))
    (setf (flag m 'iaq) nil)
    (step-machine m) ; now delivers
    (fiveam:is (not (machine-idle-p m)))
    (fiveam:is (= 7 (sref m 'a)))))

(fiveam:test wake-resumes-after-slp-not-back-at-it
  (let ((m (make-machine 'interrupt-test-machine)))
    (setf (sref m 'ia) #x0010 (sref m 'b) 5)
    (load-program m (list #x03) :origin 0) ; slp
    (step-machine m) ; executes slp -- idle; pc now 1 (past slp)
    (signal-interrupt m 42)
    (load-program m (list #x01) :origin #x0010) ; rfi at the handler (load-program resets pc)
    (setf (sref m 'pc) 1) ; put pc back where slp left it, before delivery
    (step-machine m) ; delivers: pushes pc(1), b(5); a<-42; pc<-#x10; then runs rfi found there
    (fiveam:is (= 1 (sref m 'pc))) ; resumes at the instruction after slp, not slp itself
    (fiveam:is (= 5 (sref m 'b)))))

(fiveam:test int-then-rfi-round-trips-through-a-software-interrupt
  (let ((m (make-machine 'interrupt-test-machine)))
    (setf (sref m 'ia) #x0010 (sref m 'b) 55)
    (load-program m (list #x02) :origin 0) ; int -- signals b's current value (55)
    (step-machine m) ; executes int -- enqueues; too late for this step's own delivery
    (fiveam:is (= 1 (machine-interrupt-pending-count m)))
    (fiveam:is (= 1 (sref m 'pc)))
    (setf (sref m 'b) 77) ; change b after enqueueing, before delivery snapshots it
    (load-program m (list #x01) :origin #x0010) ; rfi at the handler
    (setf (sref m 'pc) 1) ; load-program above reset pc to #x10 -- put it back where int left it
    (step-machine m) ; delivers: pushes pc(1), b(77); a<-55 (int's own signalled data); pc<-#x10; runs rfi
    (fiveam:is (= 55 (sref m 'a))) ; int's own message, untouched by rfi
    (fiveam:is (= 77 (sref m 'b))) ; restored to what it was just before delivery
    (fiveam:is (= 1 (sref m 'pc))) ; restored to where int had left off
    (fiveam:is (zerop (stack-depth m 'sp)))))

;;; #166: register-indexed (stack-pointer ...) stacks

;; ANIMA-16-shaped: SP is a plain register indexed into RAM, not a lasm
;; :stack element -- this fixture declares no (stack ...) at all, so it
;; exercises %RESOLVE-INTERRUPT-STACK's :POINTER path exclusively.
(defmachine interrupt-pointer-stack-test-machine
  (register pc :width 16) (register ia :width 16)
  (register a :width 16) (register b :width 16) (register sp :width 16)
  (flags iaq)
  (memory ram :width 16 :addr-width 16 :cell-width 16)
  (stack-pointer sp :memory ram :grows :down)
  (interrupts :vector ia :message a :save (pc b) :mask-flag iaq))

(definstruction interrupt-pointer-stack-test-machine nop
  (encoding (opcode #x00)) (semantics nil) (cycles 1))
(definstruction interrupt-pointer-stack-test-machine rfi
  (encoding (opcode #x01)) (semantics (interrupt-return)))
(definstruction interrupt-pointer-stack-test-machine int
  (encoding (opcode #x02))
  (semantics (signal-interrupt machine (sref machine 'b))))
(definstruction interrupt-pointer-stack-test-machine jsr
  (encoding (opcode #x03))
  (semantics (push (sref machine 'pc) sp)))
(definstruction interrupt-pointer-stack-test-machine ret
  (encoding (opcode #x04))
  (semantics (setf (sref machine 'pc) (pop sp))))

(fiveam:test delivery-on-a-pointer-stack-writes-descending-ram-cells-and-decrements-sp
  (let ((m (make-machine 'interrupt-pointer-stack-test-machine)))
    (setf (sref m 'ia) #x0010 (sref m 'pc) 10 (sref m 'b) 42 (sref m 'sp) 0)
    (signal-interrupt m 99)
    (step-machine m) ; delivers: pushes pc(10) then b(42) -- SP pre-decrements each time
    (fiveam:is (= #xfffe (sref m 'sp))) ; two words pushed, :DOWN growth
    (fiveam:is (= 10 (mref m 'ram #xffff))) ; pc pushed first, at the higher address
    (fiveam:is (= 42 (mref m 'ram #xfffe))) ; b pushed second
    (fiveam:is (= 99 (sref m 'a)))
    ;; delivery sets pc to the vector, then this same step-machine call
    ;; fetches/executes the implicit nop found there (zeroed ram, opcode 0)
    (fiveam:is (= (1+ #x0010) (sref m 'pc)))))

(fiveam:test interrupt-return-on-a-pointer-stack-restores-in-reverse-and-returns-sp
  (let ((m (make-machine 'interrupt-pointer-stack-test-machine)))
    (setf (sref m 'ia) #x0010 (sref m 'pc) 10 (sref m 'b) 42 (sref m 'sp) 0)
    (signal-interrupt m 99)
    (step-machine m) ; delivers
    (load-program m (list #x01) :origin #x0010) ; rfi at the handler
    (step-machine m)
    (fiveam:is (= 0 (sref m 'sp))) ; back to its pre-delivery value
    (fiveam:is (= 10 (sref m 'pc)))
    (fiveam:is (= 42 (sref m 'b)))))

;; :GROWS :UP mirrors :DOWN -- SP points one PAST the top item, so push
;; stores then increments and the frame lands at ascending addresses.
(defmachine interrupt-pointer-stack-up-test-machine
  (register pc :width 16) (register ia :width 16)
  (register a :width 16) (register b :width 16) (register sp :width 16)
  (memory ram :width 16 :addr-width 16 :cell-width 16)
  (stack-pointer sp :memory ram :grows :up)
  (interrupts :vector ia :message a :save (pc b)))

(definstruction interrupt-pointer-stack-up-test-machine rfi
  (encoding (opcode #x01)) (semantics (interrupt-return)))

(fiveam:test delivery-on-a-grows-up-pointer-stack-writes-ascending-ram-cells-and-increments-sp
  (let ((m (make-machine 'interrupt-pointer-stack-up-test-machine)))
    (setf (sref m 'ia) #x0010 (sref m 'pc) 10 (sref m 'b) 42 (sref m 'sp) 0)
    (signal-interrupt m 99)
    (step-machine m)
    (fiveam:is (= 2 (sref m 'sp)))
    (fiveam:is (= 10 (mref m 'ram 0))) ; pc pushed first, at the lower address
    (fiveam:is (= 42 (mref m 'ram 1))) ; b pushed second
    (load-program m (list #x01) :origin #x0010)
    (step-machine m)
    (fiveam:is (= 0 (sref m 'sp)))
    (fiveam:is (= 10 (sref m 'pc)))
    (fiveam:is (= 42 (sref m 'b)))))

(fiveam:test pointer-stack-sp-wraps-at-zero-on-push-and-back-on-pop
  (let ((m (make-machine 'interrupt-pointer-stack-test-machine)))
    (setf (sref m 'sp) 0)
    (with-machine-bindings (m interrupt-pointer-stack-test-machine)
      (push 7 sp))
    (fiveam:is (= #xffff (sref m 'sp))) ; :DOWN wraps to the top of the address space
    (fiveam:is (= 7 (mref m 'ram #xffff)))
    (let ((popped (with-machine-bindings (m interrupt-pointer-stack-test-machine) (pop sp))))
      (fiveam:is (= 7 popped))
      (fiveam:is (zerop (sref m 'sp)))))) ; back to 0

;; A flag in :SAVE on a pointer stack exercises restoration through SP-POP.
(defmachine interrupt-pointer-stack-flag-save-test-machine
  (register pc :width 8) (register ia :width 8) (register a :width 8) (register sp :width 8)
  (flags z)
  (memory ram :width 8 :addr-width 8)
  (stack-pointer sp :memory ram)
  (interrupts :vector ia :message a :save (z)))

(definstruction interrupt-pointer-stack-flag-save-test-machine rfi
  (encoding (opcode #x01)) (semantics (interrupt-return)))
(definstruction interrupt-pointer-stack-flag-save-test-machine nop
  (encoding (opcode #x00)) (semantics nil))

(fiveam:test interrupt-return-restores-a-saved-flag-from-pointer-stack
  (let ((m (make-machine 'interrupt-pointer-stack-flag-save-test-machine)))
    (setf (sref m 'ia) #x10 (flag m 'z) nil (sref m 'sp) 0)
    (signal-interrupt m 0)
    (step-machine m) ; delivers: pushes z(=0)
    (load-program m (list #x01) :origin #x10) ; rfi
    (step-machine m)
    (fiveam:is (zerop (flag m 'z)))
    (fiveam:is (zerop (sref m 'sp)))))

;; Masking, :CYCLES > 0 device-tick cost, and queue overflow all compose with
;; a pointer stack the same as a native one -- one assertion each proving
;; the shared delivery/overflow/masking machinery is untouched by #166.
(fiveam:test masking-composes-with-a-pointer-stack
  (let ((m (make-machine 'interrupt-pointer-stack-test-machine)))
    (setf (sref m 'ia) #x0010 (flag m 'iaq) t (sref m 'sp) 0)
    (signal-interrupt m 7)
    (step-machine m) ; masked -- stays queued, sp untouched
    (fiveam:is (= 1 (machine-interrupt-pending-count m)))
    (fiveam:is (zerop (sref m 'sp)))
    (setf (flag m 'iaq) nil)
    (step-machine m) ; now delivers
    (fiveam:is (/= 0 (sref m 'sp)))
    (fiveam:is (= 7 (sref m 'a)))))

(defvar *pointer-cost-log* nil)
(defun %pointer-cost-logger-init (machine device) (declare (ignore machine device)) nil)
(defun %pointer-cost-logger-tick (machine device cycles)
  (declare (ignore machine device))
  (cl:push cycles *pointer-cost-log*))

(defmachine interrupt-pointer-stack-cycles-test-machine
  (register pc :width 8) (register ia :width 8) (register a :width 8) (register sp :width 8)
  (memory ram :width 8 :addr-width 8)
  (stack-pointer sp :memory ram)
  (device cost-logger :init %pointer-cost-logger-init :tick %pointer-cost-logger-tick)
  (interrupts :vector ia :message a :save (pc) :cycles 5))

(definstruction interrupt-pointer-stack-cycles-test-machine nop
  (encoding (opcode #x00)) (semantics nil) (cycles 1))

(fiveam:test delivery-cycles-cost-composes-with-a-pointer-stack
  (let ((m (make-machine 'interrupt-pointer-stack-cycles-test-machine))
        (*pointer-cost-log* nil))
    (setf (sref m 'ia) 1)
    (load-program m (list #x00) :origin 0) ; nop
    (signal-interrupt m 9)
    (step-machine m) ; delivers (cost 5), then executes the nop at the vector (cost 1)
    (fiveam:is (equal '(1 5) *pointer-cost-log*))))

(defmachine interrupt-pointer-stack-overflow-test-machine
  (register pc :width 8) (register ia :width 8) (register a :width 8) (register sp :width 8)
  (memory ram :width 8 :addr-width 8)
  (stack-pointer sp :memory ram)
  (interrupts :vector ia :message a :save (pc) :queue 1))

(fiveam:test queue-overflow-composes-with-a-pointer-stack
  (let ((m (make-machine 'interrupt-pointer-stack-overflow-test-machine)))
    (setf (sref m 'ia) 1)
    (signal-interrupt m 1)
    (fiveam:signals interrupt-queue-full (signal-interrupt m 2))))

;;; DEFMACHINE-time clause parsing / validation for (stack-pointer ...)

(fiveam:test defmachine-accepts-a-well-formed-stack-pointer-clause
  (fiveam:finishes (make-machine 'interrupt-pointer-stack-test-machine)))

(fiveam:test defmachine-rejects-interrupts-stack-naming-a-register-with-no-stack-pointer-clause
  (fiveam:signals machine-definition-error
    (eval '(defmachine interrupt-sp-no-clause-test
             (register pc :width 8) (register ia :width 8) (register a :width 8) (register sp :width 8)
             (memory ram :width 8 :addr-width 8)
             (interrupts :vector ia :message a :save (pc) :stack sp)))))

(fiveam:test defmachine-rejects-a-banked-register-as-a-stack-pointer
  (fiveam:signals machine-definition-error
    (eval '(defmachine interrupt-sp-banked-test
             (register sp :width 8 :count 4)
             (memory ram :width 8 :addr-width 8)
             (stack-pointer sp :memory ram)))))

(fiveam:test defmachine-rejects-a-stack-pointer-on-a-non-register-name
  (fiveam:signals machine-definition-error
    (eval '(defmachine interrupt-sp-non-register-test
             (memory ram :width 8 :addr-width 8)
             (stack-pointer ram)))))

(fiveam:test defmachine-rejects-stack-pointer-memory-naming-a-non-memory-element
  (fiveam:signals machine-definition-error
    (eval '(defmachine interrupt-sp-bad-memory-test
             (register sp :width 8) (register other :width 8)
             (memory ram :width 8 :addr-width 8)
             (stack-pointer sp :memory other)))))

(fiveam:test defmachine-rejects-stack-pointer-with-no-memory-and-two-declared
  (fiveam:signals machine-definition-error
    (eval '(defmachine interrupt-sp-ambiguous-memory-test
             (register sp :width 8)
             (memory ram1 :width 8 :addr-width 8) (memory ram2 :width 8 :addr-width 8)
             (stack-pointer sp)))))

(fiveam:test defmachine-rejects-two-stack-pointer-clauses-for-one-register
  (fiveam:signals machine-definition-error
    (eval '(defmachine interrupt-sp-dup-test
             (register sp :width 8)
             (memory ram :width 8 :addr-width 8)
             (stack-pointer sp :memory ram)
             (stack-pointer sp :memory ram)))))

;;; #167: a :save place wider than the memory's cell splits across cells in
;;; the memory's own endianness, each place at its own width. 6502-shaped: an
;;; 8-bit P and a 16-bit PC on an 8-bit stack.

(defmacro %def-wide-frame-machine (name &key (endian :little) (grows :down))
  `(progn
     (defmachine ,name
       (register pc :width 16) (register ia :width 16) (register a :width 8)
       (register p :width 8) (register sp :width 16)
       (memory ram :width 8 :addr-width 16 :endian ,endian)
       (stack-pointer sp :memory ram :grows ,grows)
       (interrupts :vector ia :message a :save (pc p) :stack sp))
     (definstruction ,name nop (encoding (opcode #x00)) (semantics nil))
     (definstruction ,name rfi (encoding (opcode #x01)) (semantics (interrupt-return)))))

(%def-wide-frame-machine wide-frame-le)
(%def-wide-frame-machine wide-frame-be :endian :big)
(%def-wide-frame-machine wide-frame-up :grows :up)

(defun %wide-frame-delivered (name)
  (let ((m (make-machine name)))
    (setf (sref m 'ia) #x0100 (sref m 'pc) #x1234 (sref m 'p) #xAB (sref m 'sp) #x0200)
    (signal-interrupt m 7)
    (deliver-pending-interrupt m 'pc)
    m))

(fiveam:test wide-save-place-splits-little-endian-onto-a-down-stack
  (let ((m (%wide-frame-delivered 'wide-frame-le)))
    (fiveam:is (= #x01FD (sref m 'sp)))
    (fiveam:is (equal '(#xAB #x34 #x12) (list (mref m 'ram #x01FD) (mref m 'ram #x01FE) (mref m 'ram #x01FF))))))

(fiveam:test wide-save-place-splits-big-endian-with-the-memorys-order
  (let ((m (%wide-frame-delivered 'wide-frame-be)))
    (fiveam:is (equal '(#xAB #x12 #x34) (list (mref m 'ram #x01FD) (mref m 'ram #x01FE) (mref m 'ram #x01FF))))))

(fiveam:test wide-save-place-on-an-up-stack-ascends-and-post-increments
  (let ((m (%wide-frame-delivered 'wide-frame-up)))
    (fiveam:is (= #x0203 (sref m 'sp)))
    (fiveam:is (equal '(#x34 #x12 #xAB) (list (mref m 'ram #x0200) (mref m 'ram #x0201) (mref m 'ram #x0202))))))

(fiveam:test interrupt-return-restores-each-wide-place-at-its-own-width
  (dolist (name '(wide-frame-le wide-frame-be wide-frame-up))
    (let ((m (%wide-frame-delivered name)))
      (setf (sref m 'pc) #x0100)
      (load-program m (list #x01) :origin #x0100)
      (setf (sref m 'pc) #x0100)
      (step-machine m)
      (fiveam:is (= #x1234 (sref m 'pc)))
      (fiveam:is (= #xAB (sref m 'p)))
      (fiveam:is (= #x0200 (sref m 'sp))))))

;;; Banked-register places (:message/:save/:vector as (NAME INDEX)) and
;;; :mask-on-deliver

(defmachine interrupt-banked-place-test-machine
  (register pc :width 16)
  (register ia :width 16)
  (register reg :width 16 :count 4)
  (register sp :width 16)
  (flags iaq)
  (memory ram :width 16 :addr-width 16 :cell-width 16)
  (stack-pointer sp :memory ram :grows :down)
  (interrupts :vector ia :message (reg 0) :save (pc (reg 0)) :stack sp
              :mask-flag iaq :mask-on-deliver t))

(definstruction interrupt-banked-place-test-machine nop
  (encoding (opcode #x00)) (semantics nil) (cycles 1))
(definstruction interrupt-banked-place-test-machine rfi
  (encoding (opcode #x01)) (semantics (interrupt-return)))

(fiveam:test banked-place-delivery-pushes-pc-then-bank-cell-and-writes-message
  (let ((m (make-machine 'interrupt-banked-place-test-machine)))
    (setf (sref m 'ia) 10
          (regref m 'reg 0) 77
          (sref m 'pc) 5)
    (signal-interrupt m 42)
    (deliver-pending-interrupt m 'pc)
    (fiveam:is (= 42 (regref m 'reg 0)))
    (fiveam:is (= 10 (sref m 'pc)))
    ;; PC pushed first, then the bank cell: the cell is on top.
    (fiveam:is (= 77 (mref m 'ram (sref m 'sp))))
    (fiveam:is (= 5 (mref m 'ram (1+ (sref m 'sp)))))))

(fiveam:test banked-place-frame-round-trips-through-interrupt-return
  (let ((m (make-machine 'interrupt-banked-place-test-machine)))
    (setf (sref m 'ia) 10 (regref m 'reg 0) 77)
    (load-program m (list #x01) :origin 10) ; rfi at the vector
    (setf (sref m 'pc) 5) ; LOAD-PROGRAM points PC at the origin
    (signal-interrupt m 42)
    (step-machine m) ; delivers, then runs the rfi
    (fiveam:is (= 77 (regref m 'reg 0)))
    (fiveam:is (= 5 (sref m 'pc)))
    (fiveam:is (zerop (sref m 'sp)))))

(fiveam:test mask-on-deliver-sets-the-flag-before-the-handler-runs
  (let ((m (make-machine 'interrupt-banked-place-test-machine)))
    (setf (sref m 'ia) 10)
    (signal-interrupt m 1)
    (fiveam:is (zerop (flag m 'iaq)))
    (deliver-pending-interrupt m 'pc)
    (fiveam:is (= 1 (flag m 'iaq)))))

(fiveam:test mask-on-deliver-holds-a-second-signal-in-the-queue
  (let ((m (make-machine 'interrupt-banked-place-test-machine)))
    (setf (sref m 'ia) 10)
    (load-program m (list #x00) :origin 10)
    (signal-interrupt m 1)
    (signal-interrupt m 2)
    (step-machine m)
    (step-machine m)
    (fiveam:is (= 1 (machine-interrupt-pending-count m)))
    (fiveam:is (= 1 (regref m 'reg 0)))))

(fiveam:test defmachine-rejects-an-out-of-range-banked-place
  (fiveam:signals machine-definition-error
    (eval '(defmachine interrupt-banked-range-test
             (register pc :width 8) (register ia :width 8)
             (register reg :width 8 :count 2)
             (stack sp :width 8 :depth 4)
             (memory ram :width 8 :addr-width 8)
             (interrupts :vector ia :message (reg 2) :save (pc))))))

(fiveam:test defmachine-rejects-mask-on-deliver-without-a-mask-flag
  (fiveam:signals machine-definition-error
    (eval '(defmachine interrupt-mask-on-deliver-no-flag-test
             (register pc :width 8) (register ia :width 8) (register a :width 8)
             (stack sp :width 8 :depth 4)
             (memory ram :width 8 :addr-width 8)
             (interrupts :vector ia :message a :save (pc) :mask-on-deliver t)))))

;;; Priority and nesting (#161)

(defmacro %define-nesting-machine (name &rest interrupts-keys)
  `(progn
     (defmachine ,name
       (register pc :width 16) (register ia :width 16) (register a :width 16)
       (stack sp :width 16 :depth 8)
       (memory ram :width 8 :addr-width 16)
       (device urgent :priority 5)
       (device routine)
       (interrupts :vector ia :message a :save (pc) ,@interrupts-keys))
     (definstruction ,name nop (encoding (opcode #x00)) (semantics nil) (cycles 1))
     (definstruction ,name rfi (encoding (opcode #x01)) (semantics (interrupt-return)))))

(%define-nesting-machine interrupt-plain-nesting-test-machine :queue 2 :on-overflow :drop-oldest)
(%define-nesting-machine interrupt-depth-test-machine :max-depth 1)
(%define-nesting-machine interrupt-priority-test-machine :nesting :priority)

(defun %nesting-machine (name)
  "A NAME machine with a nop-filled handler at #x10 ending in RFI at #x12."
  (let ((m (make-machine name)))
    (load-program m (list #x00 #x00 #x01) :origin #x10)
    (load-program m (list #x00 #x00 #x00) :origin 0)
    (setf (sref m 'ia) #x10)
    m))

(defun %queued-data (m) (mapcar #'second (%pending m)))

(fiveam:test higher-priority-signals-queue-ahead-fifo-within-a-level
  (let ((m (make-machine 'interrupt-test-machine)))
    (setf (sref m 'ia) #x10)
    (signal-interrupt m 10 :priority 1)
    (signal-interrupt m 20 :priority 5)
    (signal-interrupt m 30 :priority 3)
    (signal-interrupt m 40 :priority 5)
    (fiveam:is (equal '(20 40 30 10) (%queued-data m)))))

(fiveam:test device-priority-is-the-default-signal-priority
  (let ((m (make-machine 'interrupt-priority-test-machine)))
    (setf (sref m 'ia) #x10)
    (device-signal m (device-at m 1) 1)
    (device-signal m (device-at m 0) 2)
    (signal-interrupt m 3)
    (fiveam:is (equal '(2 1 3) (%queued-data m)))
    (fiveam:is (equal '(5 0 0) (mapcar #'third (%pending m))))))

(fiveam:test delivery-takes-the-highest-priority-signal-first
  (let ((m (%nesting-machine 'interrupt-plain-nesting-test-machine)))
    (signal-interrupt m 1 :priority 1)
    (signal-interrupt m 2 :priority 4)
    (step-machine m)
    (fiveam:is (= 2 (sref m 'a)))))

(fiveam:test drop-oldest-evicts-the-oldest-lowest-priority-signal
  (let ((m (%nesting-machine 'interrupt-plain-nesting-test-machine)))
    (signal-interrupt m 1 :priority 1)
    (signal-interrupt m 2 :priority 3)
    (signal-interrupt m 3 :priority 2)
    (fiveam:is (equal '(2 3) (%queued-data m)))
    (signal-interrupt m 4 :priority 0)
    (fiveam:is (equal '(2 3) (%queued-data m)) "an incoming signal below everything queued is dropped")
    (signal-interrupt m 5 :priority 2)
    (fiveam:is (equal '(2 5) (%queued-data m)) "an equal-priority incoming signal evicts the older one")))

(fiveam:test nesting-allow-does-not-track-depth
  (let ((m (%nesting-machine 'interrupt-plain-nesting-test-machine)))
    (signal-interrupt m 1)
    (step-machine m)
    (fiveam:is (zerop (machine-interrupt-depth m)))
    (fiveam:is (null (machine-interrupt-active m)))))

(fiveam:test max-depth-holds-a-signal-until-the-handler-returns
  (let ((m (%nesting-machine 'interrupt-depth-test-machine)))
    (signal-interrupt m 1)
    (step-machine m)                    ; delivers, runs nop at #x10
    (fiveam:is (= 1 (machine-interrupt-depth m)))
    (signal-interrupt m 2)
    (step-machine m)                    ; blocked: nop at #x11
    (fiveam:is (equal '(2) (%queued-data m)))
    (step-machine m)                    ; rfi at #x12, still blocked while it runs
    (fiveam:is (zerop (machine-interrupt-depth m)))
    (fiveam:is (equal '(2) (%queued-data m)))
    (step-machine m)                    ; delivers the held signal
    (fiveam:is (= 2 (sref m 'a)))
    (fiveam:is (= 1 (machine-interrupt-depth m)))
    (fiveam:is (null (%pending m)))))

(fiveam:test nesting-priority-preempts-only-a-strictly-higher-priority-signal
  (let ((m (%nesting-machine 'interrupt-priority-test-machine)))
    (signal-interrupt m 1 :priority 2)
    (step-machine m)
    (signal-interrupt m 2 :priority 2)
    (step-machine m)
    (fiveam:is (= 1 (machine-interrupt-depth m)) "an equal priority waits")
    (fiveam:is (equal '(2) (%queued-data m)))
    (signal-interrupt m 3 :priority 3)
    (step-machine m)
    (fiveam:is (= 2 (machine-interrupt-depth m)))
    (fiveam:is (= 3 (sref m 'a)))
    (fiveam:is (equal '(3 2) (machine-interrupt-active m)) "innermost first")))

(fiveam:test reset-clears-the-handler-depth
  (let ((m (%nesting-machine 'interrupt-depth-test-machine)))
    (signal-interrupt m 1)
    (step-machine m)
    (reset m)
    (fiveam:is (zerop (machine-interrupt-depth m)))))

;; The parent's RFI is compiled without nesting; a child that adds :MAX-DEPTH
;; through :EXTENDS must still unwind it.
(defmachine interrupt-ext-parent
  (register pc :width 16) (register ia :width 16) (register a :width 16)
  (stack sp :width 16 :depth 8)
  (memory ram :width 8 :addr-width 16)
  (interrupts :vector ia :message a :save (pc)))
(definstruction interrupt-ext-parent nop (encoding (opcode #x00)) (semantics nil) (cycles 1))
(definstruction interrupt-ext-parent rfi (encoding (opcode #x01)) (semantics (interrupt-return)))
(defmachine (interrupt-ext-child (:extends interrupt-ext-parent))
  (interrupts :max-depth 1))

(fiveam:test inherited-rfi-unwinds-depth-a-child-adds
  (let ((m (%nesting-machine 'interrupt-ext-child)))
    (signal-interrupt m 1)
    (step-machine m)
    (fiveam:is (= 1 (machine-interrupt-depth m)))
    (signal-interrupt m 2)
    (step-machine m)
    (step-machine m)                    ; the inherited rfi
    (fiveam:is (zerop (machine-interrupt-depth m)))
    (step-machine m)
    (fiveam:is (= 2 (sref m 'a)))))

(fiveam:test defmachine-rejects-bad-nesting-and-priority-options
  (dolist (form '((interrupts :vector ia :message a :save (pc) :nesting :bogus)
                  (interrupts :vector ia :message a :save (pc) :max-depth 0)
                  (device dev :priority :high)))
    (fiveam:signals machine-definition-error
      (eval `(defmachine interrupt-bad-option-test
               (register pc :width 8) (register ia :width 8) (register a :width 8)
               (stack sp :width 8 :depth 4)
               (memory ram :width 8 :addr-width 8)
               ,form)))))

;;; Level masking and non-maskable signals (#305)

(defun %mask-level-of (machine) (sref machine 'lvl))

(defmacro %define-mask-machine (name &rest interrupts-keys)
  (unless (getf interrupts-keys :save)
    (setf interrupts-keys (list* :save '(pc) interrupts-keys)))
  `(progn
     (defmachine ,name
       (register pc :width 16) (register ia :width 16) (register a :width 16)
       (register lvl :width 8)
       (stack sp :width 16 :depth 8)
       (memory ram :width 8 :addr-width 16)
       (flags iaq)
       (device nmi :non-maskable t)
       (interrupts :vector ia :message a ,@interrupts-keys))
     (definstruction ,name nop (encoding (opcode #x00)) (semantics nil) (cycles 1))
     (definstruction ,name rfi (encoding (opcode #x01)) (semantics (interrupt-return)))))

(%define-mask-machine interrupt-level-test-machine :mask-level lvl)
(%define-mask-machine interrupt-level-fn-test-machine :mask-level-when %mask-level-of)
(%define-mask-machine interrupt-level-flag-test-machine :mask-level lvl :mask-flag iaq)
(%define-mask-machine interrupt-level-depth-test-machine :mask-level lvl :max-depth 1)
(%define-mask-machine interrupt-level-raise-test-machine
  :mask-level lvl :mask-level-on-deliver t :save (pc lvl))
(%define-mask-machine interrupt-level-drop-test-machine
  :mask-level lvl :queue 2 :on-overflow :drop-oldest)

(defun %mask-machine (name &key (level 0))
  "A NAME machine with a nop-filled handler at #x10 ending in RFI at #x12."
  (let ((m (%nesting-machine name)))
    (setf (sref m 'lvl) level)
    m))

(fiveam:test mask-level-holds-back-priorities-at-or-below-it
  (let ((m (%mask-machine 'interrupt-level-test-machine :level 3)))
    (signal-interrupt m 1 :priority 3)
    (step-machine m)
    (fiveam:is (equal '(1) (%queued-data m)))
    (signal-interrupt m 2 :priority 4)
    (step-machine m)
    (fiveam:is (= 2 (sref m 'a)))
    (fiveam:is (equal '(1) (%queued-data m)))
    (setf (sref m 'lvl) 2)
    (step-machine m)
    (fiveam:is (= 1 (sref m 'a)))
    (fiveam:is (null (%pending m)))))

(fiveam:test mask-level-when-reads-its-function
  (let ((m (%mask-machine 'interrupt-level-fn-test-machine :level 5)))
    (signal-interrupt m 1 :priority 5)
    (step-machine m)
    (fiveam:is (equal '(1) (%queued-data m)))
    (setf (sref m 'lvl) 4)
    (step-machine m)
    (fiveam:is (null (%pending m)))))

(fiveam:test mask-level-combines-with-mask-flag
  (let ((m (%mask-machine 'interrupt-level-flag-test-machine :level 0)))
    (setf (flag m 'iaq) 1)
    (signal-interrupt m 1 :priority 5)
    (step-machine m)
    (fiveam:is (equal '(1) (%queued-data m)))
    (setf (flag m 'iaq) 0 (sref m 'lvl) 5)
    (step-machine m)
    (fiveam:is (equal '(1) (%queued-data m)))
    (setf (sref m 'lvl) 4)
    (step-machine m)
    (fiveam:is (null (%pending m)))))

(fiveam:test non-maskable-signal-ignores-flag-and-level-masks
  (let ((m (%mask-machine 'interrupt-level-flag-test-machine :level 9)))
    (setf (flag m 'iaq) 1)
    (signal-interrupt m 7 :priority 1 :non-maskable t)
    (step-machine m)
    (fiveam:is (= 7 (sref m 'a)))
    (fiveam:is (null (%pending m)))))

(fiveam:test non-maskable-signal-delivers-past-a-masked-higher-priority-head
  (let ((m (%mask-machine 'interrupt-level-test-machine :level 9)))
    (signal-interrupt m 1 :priority 7)
    (signal-interrupt m 2 :non-maskable t)
    (fiveam:is (equal '(1 2) (%queued-data m)))
    (step-machine m)
    (fiveam:is (= 2 (sref m 'a)))
    (fiveam:is (equal '(1) (%queued-data m)))))

(fiveam:test non-maskable-device-signals-ignore-masks
  (let ((m (%mask-machine 'interrupt-level-test-machine :level 9)))
    (device-signal m (device-at m 0) 5)
    (step-machine m)
    (fiveam:is (= 5 (sref m 'a)))
    (device-signal m (device-at m 0) 6)
    (fiveam:is (fourth (first (%pending m))))))

(fiveam:test signal-interrupt-can-make-a-non-maskable-device-signal-maskable
  (let ((m (%mask-machine 'interrupt-level-test-machine :level 9)))
    (signal-interrupt m 5 :device (device-at m 0) :non-maskable nil)
    (step-machine m)
    (fiveam:is (equal '(5) (%queued-data m)))))

(fiveam:test non-maskable-signals-still-obey-max-depth
  (let ((m (%mask-machine 'interrupt-level-depth-test-machine)))
    (signal-interrupt m 1 :priority 1)
    (step-machine m)
    (fiveam:is (= 1 (machine-interrupt-depth m)))
    (signal-interrupt m 2 :non-maskable t)
    (step-machine m)
    (fiveam:is (equal '(2) (%queued-data m)))
    (step-machine m)                    ; rfi
    (step-machine m)
    (fiveam:is (= 2 (sref m 'a)))))

(fiveam:test mask-level-on-deliver-raises-the-level-and-rfi-restores-it
  (let ((m (%mask-machine 'interrupt-level-raise-test-machine :level 2)))
    (signal-interrupt m 1 :priority 5)
    (step-machine m)
    (fiveam:is (= 5 (sref m 'lvl)))
    (signal-interrupt m 2 :priority 5)
    (step-machine m)
    (fiveam:is (equal '(2) (%queued-data m)))
    (step-machine m)                    ; rfi
    (fiveam:is (= 2 (sref m 'lvl)))))

(fiveam:test drop-oldest-evicts-maskable-signals-first
  (let ((m (%mask-machine 'interrupt-level-drop-test-machine :level 9)))
    (signal-interrupt m 1 :non-maskable t)
    (signal-interrupt m 2 :priority 5)
    (signal-interrupt m 3 :priority 6)
    (fiveam:is (equal '(3 1) (%queued-data m)))
    (signal-interrupt m 4 :non-maskable t)
    (fiveam:is (equal '(1 4) (%queued-data m)))
    (signal-interrupt m 5 :priority 9)
    (fiveam:is (equal '(5 4) (%queued-data m)) "all non-maskable: lowest priority goes")))

(fiveam:test drop-oldest-drops-a-maskable-signal-below-every-maskable-entry
  (let ((m (%mask-machine 'interrupt-level-drop-test-machine :level 9)))
    (signal-interrupt m 1 :priority 5)
    (signal-interrupt m 2 :priority 6)
    (signal-interrupt m 3 :priority 1)
    (fiveam:is (equal '(2 1) (%queued-data m)))))

(defmachine (interrupt-level-child-test-machine (:extends interrupt-level-test-machine))
  (interrupts :vector ia :message a :save (pc) :mask-level lvl :mask-level-on-deliver t))

(fiveam:test child-machine-can-change-the-mask-level-options
  (let ((m (%mask-machine 'interrupt-level-child-test-machine :level 1)))
    (signal-interrupt m 1 :priority 4)
    (step-machine m)
    (fiveam:is (= 4 (sref m 'lvl)))))

(fiveam:test defmachine-rejects-bad-mask-level-and-non-maskable-options
  (dolist (form '((interrupts :vector ia :message a :save (pc) :mask-level lvl :mask-level-when foo)
                  (interrupts :vector ia :message a :save (pc) :mask-level-on-deliver t)
                  (interrupts :vector ia :message a :save (pc) :mask-level-when foo :mask-level-on-deliver t)
                  (interrupts :vector ia :message a :save (pc) :mask-level iaq)
                  (interrupts :vector ia :message a :save (pc) :mask-level nope)
                  (interrupts :vector ia :message a :save (pc) :mask-level 3)
                  (device dev :non-maskable 1)))
    (fiveam:signals machine-definition-error
      (eval `(defmachine interrupt-bad-mask-test
               (register pc :width 8) (register ia :width 8) (register a :width 8)
               (register lvl :width 8)
               (flags iaq)
               (stack sp :width 8 :depth 4)
               (memory ram :width 8 :addr-width 8)
               ,form)))))

;;; Per-priority queue (#304)

(fiveam:test pending-count-tracks-enqueue-delivery-and-reset
  (let ((m (make-machine 'interrupt-test-machine)))
    (setf (sref m 'ia) #x10)
    (fiveam:is (zerop (machine-interrupt-pending-count m)))
    (signal-interrupt m 1 :priority 2)
    (signal-interrupt m 2)
    (fiveam:is (= 2 (machine-interrupt-pending-count m)))
    (step-machine m)
    (fiveam:is (= 1 (machine-interrupt-pending-count m)))
    (reset m)
    (fiveam:is (zerop (machine-interrupt-pending-count m)))
    (fiveam:is (null (%pending m)))))

(fiveam:test map-pending-interrupts-visits-in-delivery-order
  (let ((m (make-machine 'interrupt-test-machine)) seen)
    (setf (sref m 'ia) #x10)
    (signal-interrupt m 1 :priority 1)
    (signal-interrupt m 2 :priority 3)
    (signal-interrupt m 3 :priority 1)
    (map-pending-interrupts (lambda (device data priority nmi)
                              (declare (ignore device nmi))
                              (cl:push (cons data priority) seen))
                            m)
    (fiveam:is (equal '((2 . 3) (1 . 1) (3 . 1)) (nreverse seen)))))

(fiveam:test one-priority-keeps-arrival-order-across-maskability
  (let ((m (make-machine 'interrupt-test-machine)))
    (setf (sref m 'ia) #x10)
    (signal-interrupt m 1 :priority 2)
    (signal-interrupt m 2 :priority 2 :non-maskable t)
    (signal-interrupt m 3 :priority 2)
    (signal-interrupt m 4 :priority 2 :non-maskable t)
    (fiveam:is (equal '(1 2 3 4) (%queued-data m)))))

(fiveam:test masked-machine-still-delivers-the-non-maskable-signal-of-a-mixed-bucket
  (let ((m (make-machine 'interrupt-test-machine)))
    (setf (sref m 'ia) #x10
          (flag m 'iaq) t)
    (signal-interrupt m 1 :priority 2)
    (signal-interrupt m 2 :priority 2 :non-maskable t)
    (signal-interrupt m 3 :priority 2 :non-maskable t)
    (step-machine m)
    (fiveam:is (= 2 (sref m 'a)))
    (fiveam:is (equal '(1 3) (%queued-data m)))))

(fiveam:test drop-oldest-evicts-the-lowest-priority-maskable-head-first
  (let ((m (make-machine 'interrupt-drop-oldest-test-machine)))
    (setf (sref m 'ia) #x10)
    (signal-interrupt m 1 :priority 1 :non-maskable t)
    (signal-interrupt m 2 :priority 1)
    (signal-interrupt m 3 :priority 5)
    (fiveam:is (equal '(3 1 2) (%queued-data m)))
    (signal-interrupt m 4 :priority 5)
    (fiveam:is (equal '(3 4 1) (%queued-data m)))
    (fiveam:is (= 3 (machine-interrupt-pending-count m)))))

(fiveam:test drop-oldest-drops-a-lower-priority-incoming-signal
  (let ((m (make-machine 'interrupt-drop-oldest-test-machine)))
    (setf (sref m 'ia) #x10)
    (signal-interrupt m 1 :priority 5)
    (signal-interrupt m 2 :priority 5)
    (signal-interrupt m 3 :priority 5)
    (signal-interrupt m 4 :priority 1)
    (fiveam:is (equal '(1 2 3) (%queued-data m)))))

(fiveam:test drop-oldest-non-maskable-incoming-displaces-a-maskable-entry
  (let ((m (make-machine 'interrupt-drop-oldest-test-machine)))
    (setf (sref m 'ia) #x10)
    (signal-interrupt m 1 :priority 5)
    (signal-interrupt m 2 :priority 5)
    (signal-interrupt m 3 :priority 5)
    (signal-interrupt m 4 :priority 1 :non-maskable t)
    (fiveam:is (equal '(2 3 4) (%queued-data m)))))

(fiveam:test snapshot-round-trips-a-mixed-priority-and-maskability-queue
  (let ((source (make-machine 'interrupt-test-machine))
        (target (make-machine 'interrupt-test-machine)))
    (setf (sref source 'ia) #x10)
    (signal-interrupt source 1 :priority 2)
    (signal-interrupt source 2 :priority 2 :non-maskable t)
    (signal-interrupt source 3 :priority 4)
    (signal-interrupt source 4 :priority 2)
    (restore-snapshot target (machine-snapshot source))
    (fiveam:is (equal (%pending source) (%pending target)))
    (fiveam:is (= 4 (machine-interrupt-pending-count target)))
    (signal-interrupt target 5 :priority 2)
    (fiveam:is (equal '(3 1 2 4 5) (%queued-data target)))))

;;; Non-maskable vector (#311)
;; A step delivers, then executes the handler's first instruction (a nop), so
;; PC ends one past the vector.

(defmachine interrupt-drop-oldest-test-machine
  (register pc :width 16) (register ia :width 16) (register a :width 16)
  (register b :width 16)
  (stack sp :width 16 :depth 8)
  (memory ram :width 8 :addr-width 16)
  (interrupts :vector ia :message a :save (pc b) :queue 3 :on-overflow :drop-oldest))

(defmachine interrupt-nmi-vector-test-machine
  (register pc :width 16) (register ia :width 16) (register nmi :width 16) (register a :width 16)
  (register b :width 16)
  (stack sp :width 16 :depth 8)
  (memory ram :width 8 :addr-width 16)
  (interrupts :vector ia :nmi-vector nmi :message a :save (pc b)))

(defmachine (interrupt-nmi-vector-child-machine (:extends interrupt-drop-oldest-test-machine))
  (register nmi :width 16)
  (interrupts :nmi-vector nmi))

(definstruction interrupt-nmi-vector-test-machine nop (encoding (opcode #x00)) (semantics nil) (cycles 1))
(definstruction interrupt-drop-oldest-test-machine nop (encoding (opcode #x00)) (semantics nil) (cycles 1))

(fiveam:test non-maskable-signal-delivers-through-the-nmi-vector
  (let ((m (make-machine 'interrupt-nmi-vector-test-machine)))
    (setf (sref m 'ia) #x10 (sref m 'nmi) #x20)
    (signal-interrupt m 1 :non-maskable t)
    (step-machine m)
    (fiveam:is (= #x21 (sref m 'pc)))
    (signal-interrupt m 2)
    (step-machine m)
    (fiveam:is (= #x11 (sref m 'pc)))))

(fiveam:test non-maskable-signal-shares-the-vector-without-nmi-vector
  (let ((m (make-machine 'interrupt-test-machine)))
    (setf (sref m 'ia) #x10)
    (signal-interrupt m 1 :non-maskable t)
    (step-machine m)
    (fiveam:is (= #x11 (sref m 'pc)))))

(fiveam:test zero-vector-drop-checks-the-vector-the-signal-uses
  (let ((m (make-machine 'interrupt-nmi-vector-test-machine)))
    (setf (sref m 'nmi) #x20)
    (signal-interrupt m 1)
    (fiveam:is (zerop (machine-interrupt-pending-count m)))
    (signal-interrupt m 2 :non-maskable t)
    (fiveam:is (= 1 (machine-interrupt-pending-count m))))
  (let ((m (make-machine 'interrupt-nmi-vector-test-machine)))
    (setf (sref m 'ia) #x10)
    (signal-interrupt m 1 :non-maskable t)
    (fiveam:is (zerop (machine-interrupt-pending-count m)))
    (signal-interrupt m 2)
    (fiveam:is (= 1 (machine-interrupt-pending-count m)))))

(fiveam:test nmi-vector-can-be-inherited-by-a-child-machine
  (let ((m (make-machine 'interrupt-nmi-vector-child-machine)))
    (setf (sref m 'ia) #x10 (sref m 'nmi) #x30)
    (signal-interrupt m 1 :non-maskable t)
    (step-machine m)
    (fiveam:is (= #x31 (sref m 'pc)))))

(fiveam:test defmachine-rejects-a-bad-nmi-vector
  (dolist (form '((interrupts :vector ia :message a :save (pc) :nmi-vector nope)
                  (interrupts :vector ia :message a :save (pc) :nmi-vector iaq)
                  (interrupts :vector ia :message a :save (pc) :nmi-vector 3)))
    (fiveam:signals machine-definition-error
      (eval `(defmachine interrupt-bad-nmi-test
               (register pc :width 8) (register ia :width 8) (register a :width 8)
               (flags iaq)
               (stack sp :width 8 :depth 4)
               (memory ram :width 8 :addr-width 8)
               ,form)))))
