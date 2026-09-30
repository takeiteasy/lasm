;;;; timer.lisp
;;;; An interval timer mapped at $D000. It raises an IRQ, or an NMI, every PERIOD
;;;; CPU cycles. Hooks are named in the `defdevice` of 6502.lisp; see
;;;; docs/devices.md.
;;;;
;;;;   $D000  PERIOD low     cycles between interrupts; 0 stops the timer
;;;;   $D001  PERIOD high
;;;;   $D002  control        bit 0 enable, bit 1 raise NMI instead of IRQ
;;;;   $D003  count          interrupts raised, modulo 256; a write clears it
;;;;
;;;; Writing PERIOD or the control byte restarts the countdown.

(in-package #:mos6502)

(defconstant +timer-base+ #xd000)
(defconstant +timer-enable+ 1)
(defconstant +timer-nmi+ 2)

(defstruct timer
  (period 0)
  (control 0)
  (elapsed 0)
  (count 0))

(defun timer-init (machine device)
  (declare (ignore machine device))
  (make-timer))

(defun timer-read (machine device address)
  (declare (ignore machine))
  (let ((timer (device-state device)))
    (ecase (- address +timer-base+)
      (0 (ldb (byte 8 0) (timer-period timer)))
      (1 (ldb (byte 8 8) (timer-period timer)))
      (2 (timer-control timer))
      (3 (timer-count timer)))))

(defun timer-write (machine device address value)
  (declare (ignore machine))
  (let ((timer (device-state device)))
    (ecase (- address +timer-base+)
      (0 (setf (ldb (byte 8 0) (timer-period timer)) value
               (timer-elapsed timer) 0))
      (1 (setf (ldb (byte 8 8) (timer-period timer)) value
               (timer-elapsed timer) 0))
      (2 (setf (timer-control timer) (logand value 3)
               (timer-elapsed timer) 0))
      (3 (setf (timer-count timer) 0)))))

(defun timer-tick (machine device cycles)
  (let ((timer (device-state device)))
    (when (and (logtest +timer-enable+ (timer-control timer)) (plusp (timer-period timer)))
      (incf (timer-elapsed timer) cycles)
      (loop while (>= (timer-elapsed timer) (timer-period timer))
            do (decf (timer-elapsed timer) (timer-period timer))
               (setf (timer-count timer) (wrap-value (1+ (timer-count timer)) 8))
               (signal-interrupt machine 1 :device device
                                           :non-maskable (logtest +timer-nmi+ (timer-control timer)))))))

(defun timer-save (machine device)
  (declare (ignore machine))
  (let ((timer (device-state device)))
    (list (timer-period timer) (timer-control timer) (timer-elapsed timer) (timer-count timer))))

(defun timer-load (machine device data)
  (declare (ignore machine))
  (destructuring-bind (period control elapsed count) data
    (setf (device-state device)
          (make-timer :period period :control control :elapsed elapsed :count count))))
