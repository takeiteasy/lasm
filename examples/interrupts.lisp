;;;; examples/interrupts.lisp
;;;;
;;;; #109: a small ANIMA-16-shaped machine declaring an (interrupts ...)
;;;; clause -- a device's own signal delivered through the auto-installed
;;;; hook, a software INT-style instruction raising one directly, masking
;;;; via a flag, and RFI restoring exactly what delivery pushed.
;;;;
;;;; Run with:  sbcl --script examples/interrupts.lisp

(load (merge-pathnames "boot.lisp" *load-pathname*))

(in-package #:lasm)

;;; A clock device: TICK counts down, DEVICE-SIGNAL's on expiry -- this
;;; time actually delivered, since INTFOO declares (interrupts ...).
(defun clock-init (machine device)
  (declare (ignore machine device))
  (cons 2 nil)) ; 2 ticks remaining

(defun clock-tick (machine device cycles)
  (declare (ignore cycles))
  (let ((remaining (device-state device)))
    (when (and (plusp (car remaining)) (zerop (decf (car remaining))))
      (device-signal machine device #xc10c)))) ; "clock" signal data

(defmachine intfoo
  (register pc :width 16)
  (register ia :width 16)  ; interrupt vector
  (register a :width 16)   ; interrupt message
  (register b :width 16)
  (stack sp :width 16 :depth 32)
  (flags iaq)              ; interrupt-mask flag
  (memory ram :width 8 :addr-width 16)
  (device clock :init clock-init :tick clock-tick)
  (interrupts :vector ia :message a :save (pc b) :mask-flag iaq))

(definstruction intfoo nop (encoding (opcode #x00)) (semantics nil) (cycles 1))
(definstruction intfoo hlt (encoding (opcode #xff)) (semantics (trap :halt)))
(definstruction intfoo rfi (encoding (opcode #x01)) (semantics (interrupt-return)))
;; INT-style: raise a software interrupt carrying B's current value.
(definstruction intfoo int
  (encoding (opcode #x02))
  (semantics (signal-interrupt machine b)))
;; SIF-style: mask/unmask, DCPU-16's IAS/IAP shape simplified to one flag.
(definstruction intfoo mask (encoding (opcode #x03)) (semantics (set-flags! (iaq t))))
(definstruction intfoo unmask (encoding (opcode #x04)) (semantics (set-flags! (iaq nil))))

(let ((m (make-machine 'intfoo)))
  (setf (sref m 'ia) #x0100) ; handler address

  ;; The clock's own DEVICE-SIGNAL, delivered through the auto-installed
  ;; hook -- two NOPs tick it down to expiry; delivery happens at the top
  ;; of the step *after* the tick that raised it (the queue is checked
  ;; before that step's own fetch, not mid-step), so the third step lands
  ;; at the handler.
  (load-program m (list #x00 #x00 #x00) :origin 0) ; nop, nop, nop
  (load-program m (list #x00) :origin #x0100)       ; handler: nop
  (format t "Ticking the clock down to expiry...~%")
  (dotimes (i 3) (step-machine m))
  (format t "Delivered: pc=~4,'0X a=~4,'0X~%" (sref m 'pc) (sref m 'a))
  (assert (= (1+ #x0100) (sref m 'pc))) ; vector, plus the handler's own nop
  (assert (= #xc10c (sref m 'a)))

  ;; A software INT from inside a fresh program, immediately followed by
  ;; RFI -- the interrupt-return operator undoes exactly what delivery
  ;; pushed (:save is (pc b), so B is restored and PC returns to the INT
  ;; instruction's own successor).
  (reset m)
  (setf (sref m 'ia) #x0100 (sref m 'b) 7)
  (load-program m (list #x02) :origin 0)           ; int -- signals b (7)
  (load-program m (list #x01) :origin #x0100)       ; handler: rfi
  (setf (sref m 'pc) 0) ; LOAD-PROGRAM also sets pc -- the second call above left it at #x0100
  (format t "~%Raising a software interrupt (int)...~%")
  (step-machine m) ; runs int -- enqueues; too late for this step's own delivery
  (setf (sref m 'b) 99) ; change b after enqueueing, before delivery snapshots it
  (step-machine m) ; delivers (pushes pc=1, b=99; a<-7; pc<-#x0100), then runs rfi
  (format t "After int+rfi: pc=~D a=~D b=~D~%" (sref m 'pc) (sref m 'a) (sref m 'b))
  (assert (= 7 (sref m 'a)))    ; int's own signalled message, untouched by rfi
  (assert (= 99 (sref m 'b)))   ; restored to what it was just before delivery
  (assert (= 1 (sref m 'pc)))   ; back where int left off

  ;; Masking: a signal raised while IAQ is set stays queued, undelivered,
  ;; until unmasked.
  (reset m)
  (setf (sref m 'ia) #x0100)
  (load-program m (list #x00 #x04 #x00) :origin 0) ; nop, unmask, nop
  (setf (flag m 'iaq) t) ; masked from the start
  (signal-interrupt m 42)
  (format t "~%Masking: signalling while masked...~%")
  (step-machine m) ; nop -- still masked, signal stays queued
  (format t "Still masked -- a=~D (unchanged), queue depth=~D~%" (sref m 'a) 1)
  (assert (/= 42 (sref m 'a)))
  (step-machine m) ; unmask
  (step-machine m) ; nop -- delivery happens here, at the top of this step
  (format t "Unmasked -- delivered: a=~D pc=~4,'0X~%" (sref m 'a) (sref m 'pc))
  (assert (= 42 (sref m 'a)))

  (format t "~%All assertions passed.~%"))
