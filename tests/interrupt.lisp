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
  (fiveam:signals error
    (eval '(defmachine interrupt-dup-clause-test
             (register pc :width 8) (register ia :width 8) (register a :width 8)
             (stack sp :width 8 :depth 4)
             (memory ram :width 8 :addr-width 8)
             (interrupts :vector ia :message a :save (pc))
             (interrupts :vector ia :message a :save (pc))))))

(fiveam:test defmachine-rejects-interrupts-vector-naming-nothing
  (fiveam:signals error
    (eval '(defmachine interrupt-bad-vector-test
             (register pc :width 8) (register a :width 8)
             (stack sp :width 8 :depth 4)
             (memory ram :width 8 :addr-width 8)
             (interrupts :vector nosuch :message a :save (pc))))))

(fiveam:test defmachine-rejects-interrupts-save-naming-nothing
  (fiveam:signals error
    (eval '(defmachine interrupt-bad-save-test
             (register pc :width 8) (register ia :width 8) (register a :width 8)
             (stack sp :width 8 :depth 4)
             (memory ram :width 8 :addr-width 8)
             (interrupts :vector ia :message a :save (nosuch))))))

(fiveam:test defmachine-rejects-interrupts-mask-flag-naming-a-non-flag
  (fiveam:signals error
    (eval '(defmachine interrupt-bad-mask-flag-test
             (register pc :width 8) (register ia :width 8) (register a :width 8)
             (stack sp :width 8 :depth 4)
             (memory ram :width 8 :addr-width 8)
             (interrupts :vector ia :message a :save (pc) :mask-flag a)))))

(fiveam:test defmachine-rejects-a-banked-register-as-vector
  (fiveam:signals error
    (eval '(defmachine interrupt-banked-vector-test
             (register pc :width 8) (register ia :width 8 :count 4) (register a :width 8)
             (stack sp :width 8 :depth 4)
             (memory ram :width 8 :addr-width 8)
             (interrupts :vector ia :message a :save (pc))))))

(fiveam:test defmachine-rejects-both-mask-when-and-mask-flag
  (fiveam:signals error
    (eval '(defmachine interrupt-both-masks-test
             (register pc :width 8) (register ia :width 8) (register a :width 8)
             (stack sp :width 8 :depth 4) (flags z)
             (memory ram :width 8 :addr-width 8)
             (interrupts :vector ia :message a :save (pc) :mask-when zerop :mask-flag z)))))

(fiveam:test defmachine-rejects-interrupts-with-no-stack-and-no-explicit-one
  (fiveam:signals error
    (eval '(defmachine interrupt-no-stack-test
             (register pc :width 8) (register ia :width 8) (register a :width 8)
             (memory ram :width 8 :addr-width 8)
             (interrupts :vector ia :message a :save (pc))))))

(fiveam:test defmachine-rejects-interrupts-with-ambiguous-stack
  (fiveam:signals error
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
    (fiveam:is (equal (list (cons (device-at m 0) :hello)) (machine-interrupt-queue m)))))

(fiveam:test signal-interrupt-enqueues-with-no-device
  (let ((m (make-machine 'interrupt-test-machine)))
    (setf (sref m 'ia) #x0100)
    (signal-interrupt m 42)
    (fiveam:is (equal (list (cons nil 42)) (machine-interrupt-queue m)))))

(fiveam:test signal-interrupt-with-zero-vector-drops-the-signal-by-default
  (let ((m (make-machine 'interrupt-test-machine)))
    (setf (sref m 'ia) 0)
    (signal-interrupt m 42)
    (fiveam:is (null (machine-interrupt-queue m)))))

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
    (fiveam:is (= 1 (length (machine-interrupt-queue m))))))

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
    (fiveam:is (equal (list (cons nil 1)) (machine-interrupt-queue m)))))

(fiveam:test overflow-drop-oldest-policy-evicts-the-head
  (let ((m (make-machine 'interrupt-overflow-drop-oldest-test-machine)))
    (setf (sref m 'ia) 1)
    (signal-interrupt m 1)
    (fiveam:finishes (signal-interrupt m 2))
    (fiveam:is (equal (list (cons nil 2)) (machine-interrupt-queue m)))))

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
    (fiveam:is (= 1 (length (machine-interrupt-queue m)))) ; still queued
    (fiveam:is (/= 7 (sref m 'a)))
    (setf (flag m 'iaq) nil)
    (step-machine m)
    (fiveam:is (null (machine-interrupt-queue m)))
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
    (fiveam:is (= 1 (length (machine-interrupt-queue m))))
    (reset m)
    (fiveam:is (null (machine-interrupt-queue m)))))

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

;;; INTERRUPT-RETURN restoring a saved flag (#22's boolean-coercion gotcha)

;; A separate fixture purely to exercise a FLAG in :SAVE -- INTERRUPT-TEST-
;; MACHINE above only saves PC/B (both registers). #22: (SETF FLAG) treats
;; its VALUE as a Lisp boolean, so a naive (SETF PLACE (STACK-POP ...))
;; would set a saved flag whose popped word happens to be 0 back to true
;; (0 is non-NIL) -- INTERRUPT-RETURN must special-case a :SAVE flag place.
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

(fiveam:test interrupt-return-restores-a-saved-flag-as-a-boolean-not-the-raw-word
  (let ((m (make-machine 'interrupt-flag-save-test-machine)))
    (setf (sref m 'ia) #x10 (flag m 'z) nil) ; Z starts false -- pushes as 0
    (load-program m (list #x00) :origin 0) ; nop
    (signal-interrupt m 0)
    (step-machine m) ; delivers: pushes z(=0); a<-0; pc<-#x10
    (load-program m (list #x01) :origin #x10) ; rfi
    (step-machine m)
    (fiveam:is (zerop (flag m 'z))) ; restored to false, not clobbered true by a bare SETF FLAG
    (fiveam:is (zerop (stack-depth m 'sp)))))

;;; INT-style software interrupt end-to-end

(fiveam:test device-raised-signal-delivers-one-step-after-its-own-tick
  (let ((m (make-machine 'interrupt-test-machine)))
    (setf (sref m 'ia) #x0010)
    (setf *armed* t)
    (load-program m (list #x00) :origin 0) ; nop -- its own TICK raises the signaller's signal
    (step-machine m) ; ticks the signaller mid-step; too late for this step's own delivery check
    (fiveam:is (= 1 (sref m 'pc)))
    (fiveam:is (= 1 (length (machine-interrupt-queue m))))
    (fiveam:is (/= #xde (sref m 'a)))
    (step-machine m) ; now delivers, then executes the implicit nop at the vector
    (fiveam:is (null (machine-interrupt-queue m)))
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
    (fiveam:is (= 1 (length (machine-interrupt-queue m))))
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
    (fiveam:is (= 1 (length (machine-interrupt-queue m))))
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
    (fiveam:is (= 1 (length (machine-interrupt-queue m))))
    (fiveam:is (= 1 (sref m 'pc)))
    (setf (sref m 'b) 77) ; change b after enqueueing, before delivery snapshots it
    (load-program m (list #x01) :origin #x0010) ; rfi at the handler
    (setf (sref m 'pc) 1) ; load-program above reset pc to #x10 -- put it back where int left it
    (step-machine m) ; delivers: pushes pc(1), b(77); a<-55 (int's own signalled data); pc<-#x10; runs rfi
    (fiveam:is (= 55 (sref m 'a))) ; int's own message, untouched by rfi
    (fiveam:is (= 77 (sref m 'b))) ; restored to what it was just before delivery
    (fiveam:is (= 1 (sref m 'pc))) ; restored to where int had left off
    (fiveam:is (zerop (stack-depth m 'sp)))))
