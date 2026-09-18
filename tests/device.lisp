;;;; tests/device.lisp
;;;; #108: the device bus -- declaration, enumeration, attach/detach,
;;;; ticking, and the interrupt signal seam.
;;;;
;;;; DEVICE-TEST-MACHINE is its own fixture rather than an addition to
;;;; SUITES.LISP's shared TEST-MACHINE, which the storage and semantics
;;;; tests depend on and shouldn't have to account for a device bus.

(in-package #:lasm)

(fiveam:def-suite device :in lasm)
(fiveam:in-suite device)

;;; Fixture

;; A counter device: INIT seeds STATE with a fresh cons (ticks . messages),
;; TICK accumulates elapsed cycles, RECEIVE bumps a message count, DETACH
;; logs itself. Log entries below let a test assert not just "it worked" but
;; the exact machine/device/argument each hook actually saw.
(defvar *device-log* nil
  "Hook calls seen by DEVICE-TEST-MACHINE's declared devices below -- reset
at the start of each test that reads it.")

(defun %counter-init (machine device)
  (declare (ignore machine device))
  (cons 0 0))

(defun %counter-tick (machine device cycles)
  (declare (ignore machine))
  (incf (car (device-state device)) cycles))

(defun %counter-receive (machine device)
  (declare (ignore machine))
  (incf (cdr (device-state device))))

(defun %counter-detach (machine device)
  (declare (ignore machine))
  (cl:push (list :detach (device-index device)) *device-log*))

(defmachine device-test-machine
  (register pc :width 16)
  (register a :width 16)
  (register b :width 16)
  (register c :width 16)
  (memory ram :width 8 :addr-width 16)
  (device counter :id 1 :version 1 :manufacturer 7
          :init %counter-init :tick %counter-tick
          :receive %counter-receive :detach %counter-detach)
  (device silent :id 2 :version 0 :manufacturer 0))

;; HWN/HWQ/HWI-shaped instructions, matching DCPU-16's own trio, driving the
;; three bus operators directly (they are deliberately not bound inside
;; WITH-MACHINE-BINDINGS -- see semantics.lisp) -- enough for the ticking/
;; ordering tests below to run under STEP-MACHINE rather than calling the
;; bus API standalone.
(definstruction device-test-machine hwn
  (encoding (opcode #x00))
  (semantics (set! a (device-count machine))))

(definstruction device-test-machine hwq
  (encoding (opcode #x01))
  (semantics (multiple-value-bind (id version manufacturer) (device-info machine (sref machine 'a))
               (set! a id) (set! b version) (set! c manufacturer))))

(definstruction device-test-machine hwi
  (encoding (opcode #x02))
  (semantics (device-send machine (sref machine 'a))))

(definstruction device-test-machine nop
  (encoding (opcode #x03))
  (semantics nil)
  (cycles 3))

(definstruction device-test-machine boom
  (encoding (opcode #x04))
  (semantics (trap :halt))
  (cycles 2))

;; #90: 2 declared cycles plus 3 extra.
(definstruction device-test-machine pen
  (encoding (opcode #x05))
  (semantics (extra-cycles 3))
  (cycles 2))

(definstruction device-test-machine xboom
  (encoding (opcode #x06))
  (semantics (extra-cycles 4) (trap :halt))
  (cycles 1))

(definstruction device-test-machine bad
  (encoding (opcode #xff))
  (semantics nil))

;;; Declaration order / enumeration

(fiveam:test declared-devices-get-fixed-indices-in-declaration-order
  (let ((m (make-machine 'device-test-machine)))
    (fiveam:is (= 2 (device-count m)))
    (fiveam:is (eq (find-device m 'counter) (device-at m 0)))
    (fiveam:is (eq (find-device m 'silent) (device-at m 1)))))

(fiveam:test device-info-returns-declared-identity
  (let ((m (make-machine 'device-test-machine)))
    (multiple-value-bind (id version manufacturer) (device-info m 0)
      (fiveam:is (= 1 id))
      (fiveam:is (= 1 version))
      (fiveam:is (= 7 manufacturer)))))

;;; Send / receive

(fiveam:test device-send-reaches-receive-hook
  (let ((m (make-machine 'device-test-machine)))
    (device-send m 0)
    (device-send m 0)
    (fiveam:is (= 2 (cdr (device-state (device-at m 0)))))))

(fiveam:test device-send-on-no-receive-hook-is-a-no-op
  (let ((m (make-machine 'device-test-machine)))
    (fiveam:finishes (device-send m 1))))

;;; Ticking

(fiveam:test step-machine-ticks-devices-once-with-the-steps-own-cost
  (let ((m (make-machine 'device-test-machine)))
    (load-program m (list #x03)) ; nop, cycles 3
    (step-machine m)
    (fiveam:is (= 3 (car (device-state (device-at m 0)))))))

(fiveam:test step-machine-does-not-tick-devices-on-decode-failure
  (let ((m (make-machine 'device-test-machine)))
    (load-program m (list #xf0)) ; unregistered opcode
    (step-machine m)
    (fiveam:is (= 0 (car (device-state (device-at m 0)))))))

(fiveam:test step-machine-still-ticks-devices-on-a-trapping-instruction
  (let ((m (make-machine 'device-test-machine)))
    (load-program m (list #x04)) ; boom, cycles 2, traps
    (fiveam:signals lasm-trap (step-machine m))
    (fiveam:is (= 2 (car (device-state (device-at m 0)))))))

(fiveam:test run-ticks-devices-across-multiple-steps
  (let ((m (make-machine 'device-test-machine)))
    (load-program m (list #x03 #x03 #x04)) ; nop, nop, boom
    (run m)
    (fiveam:is (= 8 (car (device-state (device-at m 0))))))) ; 3 + 3 + 2

(fiveam:test debug-step-ticks-devices
  (let* ((m (make-machine 'device-test-machine))
         (session (progn (load-program m (list #x03)) (make-debug-session m))))
    (debug-step session)
    (fiveam:is (= 3 (car (device-state (device-at m 0)))))))

;;; HWN/HWQ/HWI under STEP-MACHINE

(fiveam:test hwn-hwq-hwi-round-trip-under-step-machine
  (let ((m (make-machine 'device-test-machine)))
    (load-program m (list #x00)) ; hwn -> a = device-count
    (step-machine m)
    (fiveam:is (= 2 (sref m 'a)))
    ;; HWQ/HWI both read the target index from A -- point it at a real
    ;; device (0), not the count HWN just left there.
    (setf (sref m 'a) 0 (sref m 'pc) 0)
    (load-program m (list #x01)) ; hwq[a=0]
    (step-machine m)
    (fiveam:is (= 1 (sref m 'a))) ; counter's id
    (fiveam:is (= 1 (sref m 'b))) ; counter's version
    (fiveam:is (= 7 (sref m 'c))) ; counter's manufacturer
    (setf (sref m 'a) 0 (sref m 'pc) 0)
    (load-program m (list #x02)) ; hwi[a=0] -> counter receive
    (step-machine m)
    (fiveam:is (= 1 (cdr (device-state (device-at m 0)))))))

;;; Attach / detach

(fiveam:test attach-device-appends-after-every-declared-device
  (let ((m (make-machine 'device-test-machine)))
    (let ((index (attach-device m 'host-clock :id 9 :version 1 :manufacturer 2)))
      (fiveam:is (= 2 index))
      (fiveam:is (= 3 (device-count m)))
      (fiveam:is (eq (find-device m 'host-clock) (device-at m 2))))))

(fiveam:test detach-device-leaves-a-hole-not-reused-by-a-later-attach
  (let ((m (make-machine 'device-test-machine)))
    (detach-device m 0)
    (fiveam:is (= 2 (device-count m))) ; unchanged -- a hole, not a shrink
    (fiveam:is (equal '((:detach 0)) *device-log*))
    (let ((index (attach-device m 'another)))
      (fiveam:is (= 2 index)) ; appended after the bus's current end, not into the hole
      (fiveam:is (= 3 (device-count m))))))

(fiveam:test no-such-device-on-a-detached-hole
  (let ((m (make-machine 'device-test-machine)))
    (detach-device m 0)
    (fiveam:signals no-such-device (device-at m 0))))

(fiveam:test no-such-device-on-an-out-of-range-index
  (let ((m (make-machine 'device-test-machine)))
    (fiveam:signals no-such-device (device-at m 99))))

(fiveam:test attach-device-rejects-a-fresh-name-colliding-with-a-register
  (let ((m (make-machine 'device-test-machine)))
    (fiveam:signals error (attach-device m 'pc))))

;; A separate fixture purely for the register-*alias* branch of
;; %DEVICE-NAME-TAKEN-P -- DEVICE-TEST-MACHINE above declares no aliased
;; register, so this is the only coverage of that branch (distinct from the
;; bare-element-name branch REGISTER above already exercises).
(defmachine device-alias-collision-test-machine
  (register pc :width 8)
  (register bank :width 8 :names (bank0 bank1))
  (memory ram :width 8 :addr-width 8)
  (device counter))

(fiveam:test attach-device-rejects-a-fresh-name-colliding-with-a-register-alias
  (let ((m (make-machine 'device-alias-collision-test-machine)))
    (fiveam:signals error (attach-device m 'bank0))))

(fiveam:test attach-device-rejects-a-fresh-name-colliding-with-an-attached-device
  (let ((m (make-machine 'device-test-machine)))
    (attach-device m 'extra)
    (fiveam:signals error (attach-device m 'extra))))

(fiveam:test attach-device-by-a-declared-name-instantiates-a-second-instance
  ;; NAME naming an already-declared device is deliberately NOT a collision
  ;; -- it attaches a fresh, freshly-INIT'd instance of that same descriptor
  ;; (machine.lisp's DEFMACHINE docstring), distinct from #108's namespace
  ;; check, which only guards a *fresh* name.
  (let ((m (make-machine 'device-test-machine)))
    (let ((index (attach-device m 'silent)))
      (fiveam:is (= 2 index))
      (fiveam:is (= 3 (device-count m)))
      (fiveam:is (not (eq (device-at m 1) (device-at m 2)))))))

;;; Reset

(fiveam:test reset-restores-the-declared-bus-and-reruns-init
  (let ((m (make-machine 'device-test-machine)))
    (device-send m 0)
    (attach-device m 'host-clock :id 9)
    (detach-device m 1)
    (fiveam:is (= 3 (device-count m)))
    (reset m)
    (fiveam:is (= 2 (device-count m)))
    (fiveam:is (eq (find-device m 'counter) (device-at m 0)))
    (fiveam:is (eq (find-device m 'silent) (device-at m 1)))
    (fiveam:is (= 0 (cdr (device-state (device-at m 0))))))) ; INIT reran, fresh state

;;; Interrupt seam (#109 installs the real queue on top of this)

(fiveam:test device-signal-reaches-an-installed-interrupt-hook
  (let* ((m (make-machine 'device-test-machine))
         (seen nil))
    (setf (machine-interrupt-hook m) (lambda (machine device data)
                                        (declare (ignore machine))
                                        (setf seen (list (device-index device) data))))
    (device-signal m (device-at m 0) :wake)
    (fiveam:is (equal (list 0 :wake) seen))))

(fiveam:test device-signal-with-no-hook-installed-is-dropped
  (let ((m (make-machine 'device-test-machine)))
    (fiveam:finishes (device-signal m (device-at m 0)))))

;;; DEFMACHINE-time namespace rejection

(fiveam:test defmachine-rejects-duplicate-device-name
  (fiveam:signals error
    (eval '(defmachine device-dup-name-test
             (register pc :width 8)
             (memory ram :width 8 :addr-width 8)
             (device dup)
             (device dup)))))

(fiveam:test defmachine-rejects-device-name-colliding-with-register
  (fiveam:signals error
    (eval '(defmachine device-register-collision-test
             (register pc :width 8)
             (memory ram :width 8 :addr-width 8)
             (device pc)))))

(fiveam:test defmachine-rejects-device-name-colliding-with-region
  (fiveam:signals error
    (eval '(defmachine device-region-collision-test
             (register pc :width 8)
             (memory ram :width 8 :addr-width 8 (region io #x00 #x0F :kind :device))
             (device io)))))

(fiveam:test step-machine-ticks-devices-for-extra-cycles
  (let ((m (make-machine 'device-test-machine)))
    (load-program m (list #x05))
    (step-machine m)
    (fiveam:is (= 5 (car (device-state (device-at m 0)))))))

(defvar *tick-log* nil)

(defun %log-tick (machine device cycles)
  (declare (ignore machine device))
  (cl:push cycles *tick-log*))

(defmachine tick-log-machine
  (register pc :width 16)
  (memory ram :width 8 :addr-width 16)
  (device probe :id 3 :version 0 :manufacturer 0 :tick %log-tick))

(definstruction tick-log-machine pen
  (encoding (opcode #x05))
  (semantics (extra-cycles 3))
  (cycles 2))

(fiveam:test step-machine-ticks-extra-cycles-separately-from-the-declared-cost
  (let ((m (make-machine 'tick-log-machine))
        (*tick-log* nil))
    (load-program m (list #x05))
    (step-machine m)
    (fiveam:is (equal '(2 3) (reverse *tick-log*)))))

(fiveam:test step-machine-skips-the-extra-tick-when-the-instruction-traps
  (let ((m (make-machine 'device-test-machine)))
    (load-program m (list #x06))
    (fiveam:signals lasm-trap (step-machine m))
    (fiveam:is (= 1 (car (device-state (device-at m 0)))))
    (fiveam:is (= 5 (machine-cycles m)))))
