;;;; devices.lisp
;;;; The Generic Clock and Generic Keyboard. HWI hands a device its command in
;;;; A and its argument in B, and it answers in C. Hooks are named in the
;;;; `device` clauses of dcpu16.lisp; see docs/devices.md.

(in-package #:dcpu16)

(defun cell (machine index)
  (regref machine 'reg index))

(defun (setf cell) (value machine index)
  (setf (regref machine 'reg index) value))

;;; Generic Clock: ticks 60/B times a second.
;;;   A=0  start ticking with divisor B (B=0 stops)
;;;   A=1  C = ticks since the last A=0
;;;   A=2  interrupt with message B on each tick (B=0 disables)
(defstruct clock
  (divisor 0)
  (cycles-per-tick 1)
  (cycles 0)
  (ticks 0)
  (message 0))

(defun clock-init (machine device)
  (declare (ignore machine device))
  (make-clock))

(defun clock-tick (machine device cycles)
  (let ((clock (device-state device)))
    (when (plusp (clock-divisor clock))
      (incf (clock-cycles clock) cycles)
      (loop while (>= (clock-cycles clock) (clock-cycles-per-tick clock))
            do (decf (clock-cycles clock) (clock-cycles-per-tick clock))
               (setf (clock-ticks clock) (wrap-value (1+ (clock-ticks clock)) 16))
               (when (plusp (clock-message clock))
                 (device-signal machine device (clock-message clock)))))))

(defun clock-receive (machine device)
  (let ((clock (device-state device))
        (divisor (cell machine 1)))
    (case (cell machine 0)
      (0 (let ((hertz (machine-descriptor-clock-speed (machine-descriptor machine))))
           (setf (clock-divisor clock) divisor
                 (clock-cycles-per-tick clock) (max 1 (ceiling (* hertz divisor) 60))
                 (clock-cycles clock) 0
                 (clock-ticks clock) 0)))
      (1 (setf (cell machine 2) (clock-ticks clock)))
      (2 (setf (clock-message clock) divisor)))))

;;; Generic Keyboard. Key codes: 0x10 backspace, 0x11 return, 0x12 insert,
;;; 0x13 delete, 0x20-0x7f ASCII, 0x80-0x83 up/down/left/right, 0x90 shift,
;;; 0x91 control.
;;;   A=0  clear the key buffer
;;;   A=1  C = next typed key, or 0 if the buffer is empty
;;;   A=2  C = 1 if key B is pressed, else 0
;;;   A=3  interrupt with message B on key events (B=0 disables)
(defconstant +buffer-size+ 64)

(defstruct keyboard
  (buffer nil)
  (pressed (make-array 256 :element-type 'bit :initial-element 0))
  (message 0))

(defun keyboard-init (machine device)
  (declare (ignore machine device))
  (make-keyboard))

(defun keyboard-receive (machine device)
  (let ((keyboard (device-state device))
        (argument (cell machine 1)))
    (case (cell machine 0)
      (0 (setf (keyboard-buffer keyboard) nil))
      (1 (setf (cell machine 2) (or (cl:pop (keyboard-buffer keyboard)) 0)))
      (2 (setf (cell machine 2)
               (if (and (< argument 256) (= 1 (bit (keyboard-pressed keyboard) argument))) 1 0)))
      (3 (setf (keyboard-message keyboard) argument)))))

(defun key-event (machine key pressed)
  (unless (< -1 key 256)
    (error "Key ~S is outside 0-255" key))
  (let* ((device (find-device machine 'keyboard))
         (keyboard (device-state device)))
    (setf (bit (keyboard-pressed keyboard) key) (if pressed 1 0))
    (when (and pressed (< (length (keyboard-buffer keyboard)) +buffer-size+))
      (setf (keyboard-buffer keyboard) (append (keyboard-buffer keyboard) (list key))))
    (when (plusp (keyboard-message keyboard))
      (device-signal machine device (keyboard-message keyboard)))))

(defun key-down (machine key)
  "Host input: KEY is pressed."
  (key-event machine key t))

(defun key-up (machine key)
  "Host input: KEY is released."
  (key-event machine key nil))
