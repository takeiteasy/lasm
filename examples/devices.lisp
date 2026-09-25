;;;; examples/devices.lisp
;;;;
;;;; #108: the device bus -- a small machine with two declared devices (a
;;;; countdown clock, an output port) enumerated and messaged HWN/HWQ/HWI-
;;;; style, plus one host-attached device to show runtime attach/detach.
;;;; The port is also memory-mapped (#158): a :DEVICE region bound to it
;;;; routes MREF through the same device's :WRITE hook.
;;;;
;;;; Run with:  sbcl --script examples/devices.lisp

(load (merge-pathnames "boot.lisp" *load-pathname*))

(in-package #:lasm)

;;; The clock: TICK counts down from its own STATE (an INIT-seeded cons cell
;;; holding the remaining count), and DEVICE-SIGNAL's when it hits zero --
;;; dropped here, since this machine declares no (interrupts ...) clause
;;; and so installs no interrupt hook; see examples/interrupts.lisp for a
;;; machine where the same DEVICE-SIGNAL call is actually delivered.
(defun clock-init (machine device)
  (declare (ignore machine device))
  (cons 3 nil)) ; 3 ticks remaining

(defun clock-tick (machine device cycles)
  (declare (ignore cycles))
  (let ((remaining (device-state device)))
    (when (and (plusp (car remaining)) (zerop (decf (car remaining))))
      ;; An unsigned integer, not a keyword -- a delivered signal's data is
      ;; written into the :MESSAGE register via SETF SREF (WRAP-VALUE,
      ;; storage.lisp), which requires one. Dropped on this machine either
      ;; way (see above), but examples/interrupts.lisp shows the same call
      ;; on a machine that actually delivers it.
      (device-signal machine device #xe0))))

;;; The output port: RECEIVE (an HWI-style message) prints whatever's in the
;;; A register at the time -- an instruction's semantics decide what "the
;;; message" means, the bus API itself carries no payload of its own.
(defun port-receive (machine device)
  (declare (ignore device))
  (format t "  port received: ~D~%" (sref machine 'a)))

;;; #158: the same port answers memory writes too, via a region bound with
;;; :DEVICE. WRITE gets the device and the absolute address.
(defun port-write (machine device address value)
  (declare (ignore machine device))
  (format t "  port written at $~4,'0X: ~D~%" address value))

(defmachine devfoo
  (register pc :width 16)
  (register a :width 16)
  (register b :width 16)
  (register c :width 16)
  (memory ram :width 8 :addr-width 16
    (region port-io #xFF00 #xFF00 :kind :device :device port))
  (device clock :id #x0001 :version 1 :manufacturer #x1000
          :init clock-init :tick clock-tick)
  (device port :id #x0002 :version 1 :manufacturer #x1000
          :receive port-receive :write port-write))

;; HWN/HWQ/HWI, DCPU-16-style -- deliberately not bound inside WITH-MACHINE-
;; BINDINGS (see semantics.md), so an instruction's semantics call the bus
;; API directly with MACHINE, same as MREF.
(definstruction devfoo hwn
  (encoding (opcode #x00))
  (semantics (set! a (device-count machine))))

(definstruction devfoo hwq
  (encoding (opcode #x01))
  (semantics (multiple-value-bind (id version manufacturer) (device-info machine a)
               (set! a id) (set! b version) (set! c manufacturer))))

(definstruction devfoo hwi
  (encoding (opcode #x02))
  (semantics (device-send machine a)))

(definstruction devfoo nop
  (encoding (opcode #x03))
  (semantics nil)
  (cycles 1))

(definstruction devfoo hlt
  (encoding (opcode #xff))
  (semantics (trap :halt)))

(let ((m (make-machine 'devfoo)))
  (format t "Declared devices: ~D~%" (device-count m))
  (assert (= 2 (device-count m)))

  ;; HWQ against the clock (index 0).
  (load-program m (list #x00      ; hwn -> a = device-count
                         #x03))   ; nop (a=0 already, HWQ can read it)
  (run m :max-steps 2)
  (setf (sref m 'a) 0 (sref m 'pc) 0)
  (load-program m (list #x01 #xff)) ; hwq[a=0]; hlt
  (run m)
  (format t "Clock identity: id=~4,'0X version=~D manufacturer=~4,'0X~%"
          (sref m 'a) (sref m 'b) (sref m 'c))
  (assert (= #x0001 (sref m 'a)))

  ;; HWI against the port (index 1) -- prints whatever A holds.
  (setf (sref m 'a) 1 (sref m 'pc) 0)
  (load-program m (list #x02 #xff)) ; hwi[a=1]; hlt
  (format t "~%Sending a message to the port:~%")
  (run m)

  ;; The same port, memory-mapped: one object, bus- and address-addressed.
  (format t "~%Writing to the port's memory-mapped address:~%")
  (setf (mref m 'ram #xFF00) 42)

  ;; Tick the clock down to expiry -- DEVICE-SIGNAL fires but is dropped:
  ;; this machine declares no (interrupts ...) clause, so MAKE-MACHINE
  ;; installed no hook. examples/interrupts.lisp shows the same call
  ;; actually delivered.
  (setf (sref m 'pc) 0)
  (load-program m (list #x03 #x03 #x03 #xff)) ; nop*3, hlt
  (run m)
  (format t "~%Clock ticked down: ~D remaining~%" (car (device-state (device-at m 0))))
  (assert (= 0 (car (device-state (device-at m 0)))))

  ;; A host-attached device, appended after every declared one.
  (let ((index (attach-device m 'host-sensor :id #x0003 :version 1)))
    (format t "~%Attached host-sensor at index ~D; device-count now ~D~%" index (device-count m))
    (assert (= 2 index))
    (assert (= 3 (device-count m)))
    (detach-device m index)
    (format t "Detached it -- device-count stays ~D (a hole, not a shrink)~%" (device-count m))
    (assert (= 3 (device-count m)))
    (handler-case
        (progn (device-at m index) (error "expected no-such-device"))
      (no-such-device () (format t "device-at on the vacated index signals, as expected~%"))))

  (format t "~%All assertions passed.~%"))
