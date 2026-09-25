;;;; examples/interrupts.lisp
;;;;
;;;; #109: a small ANIMA-16-shaped machine declaring an (interrupts ...)
;;;; clause -- a device's own signal delivered through the auto-installed
;;;; hook, a software INT-style instruction raising one directly, masking
;;;; via a flag, and RFI restoring exactly what delivery pushed.
;;;;
;;;; #166: a second machine, INTFOO-PTR, repeats the same shape with a
;;;; register-indexed stack instead of a lasm :stack element -- the
;;;; convention real ANIMA-16 actually uses -- and shows the same SP backing
;;;; an ordinary JSR/RET pair outside any interrupt path.
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

;;; #166: the same shape again, but with a register-indexed stack instead of
;;; a lasm :stack element -- real ANIMA-16's SP is a plain register pushed/
;;; popped by hand into RAM, not a hidden lasm-native stack. (stack-pointer
;;; ...) binds SP that way; INTFOO-PTR's :interrupts :stack names it, and the
;;; same SP backs an ordinary JSR/RET pair -- not an interrupt path at all --
;;; showing the binding works independently of (interrupts ...).

(defmachine intfoo-ptr
  (register pc :width 16)
  (register ia :width 16)
  (register a :width 16)
  (register b :width 16)
  (register sp :width 16)
  (flags iaq)
  (memory ram :width 16 :addr-width 16 :cell-width 16)
  (stack-pointer sp :memory ram :grows :down)
  (interrupts :vector ia :message a :save (pc b) :mask-flag iaq))

(definstruction intfoo-ptr nop (encoding (opcode #x00)) (semantics nil) (cycles 1))
(definstruction intfoo-ptr rfi (encoding (opcode #x01)) (semantics (interrupt-return)))
(definstruction intfoo-ptr int
  (encoding (opcode #x02))
  (semantics (signal-interrupt machine b)))
;; JSR/RET: an ordinary call/return pair sharing INTFOO-PTR's interrupt SP --
;; PUSH/POP resolve it the same way they'd resolve a (stack ...) element.
(definstruction intfoo-ptr jsr
  (encoding (opcode #x05))
  (semantics (push pc sp)))
(definstruction intfoo-ptr ret
  (encoding (opcode #x06))
  (semantics (set! pc (pop sp))))

(let ((m (make-machine 'intfoo-ptr)))
  (setf (sref m 'ia) #x0100 (sref m 'b) 7)
  (load-program m (list #x02) :origin 0)           ; int -- signals b (7)
  (load-program m (list #x01) :origin #x0100)       ; handler: rfi
  (setf (sref m 'pc) 0)
  (format t "~%Register-indexed stack: raising a software interrupt (int)...~%")
  (step-machine m) ; runs int -- enqueues
  (step-machine m) ; delivers: pushes pc=1, b=7 into descending RAM cells; a<-7; pc<-#x0100; then runs the rfi found there, popping both back off
  (format t "RAM[FFFF]=~D (saved pc) RAM[FFFE]=~D (saved b) sp=~4,'0X~%"
          (mref m 'ram #xffff) (mref m 'ram #xfffe) (sref m 'sp))
  (assert (= 1 (mref m 'ram #xffff))) ; pc pushed first, at the higher address
  (assert (= 7 (mref m 'ram #xfffe))) ; b pushed second
  (assert (= 1 (sref m 'pc)))         ; rfi restored pc
  (assert (zerop (sref m 'sp)))       ; rfi's pops undid delivery's pushes in the same step

  ;; The same SP, used by JSR/RET -- no interrupt involved.
  (reset m)
  (load-program m (list #x05 #x00 #x06) :origin 0) ; jsr (pushes pc=1), nop, ret (pops back to 1)
  (format t "~%Same SP, JSR/RET (no interrupt path)...~%")
  (step-machine m) ; jsr
  (format t "After jsr: sp=~4,'0X RAM[FFFF]=~D~%" (sref m 'sp) (mref m 'ram #xffff))
  (assert (= #xffff (sref m 'sp)))
  (assert (= 1 (mref m 'ram #xffff)))
  (setf (sref m 'pc) 2) ; jump to ret as if the called routine returned control here
  (step-machine m) ; ret
  (format t "After ret: pc=~D sp=~4,'0X~%" (sref m 'pc) (sref m 'sp))
  (assert (= 1 (sref m 'pc)))
  (assert (zerop (sref m 'sp)))

  (format t "~%All register-indexed-stack assertions passed.~%"))

;;; #161: priority and nesting. Devices declare a :priority; a higher-priority
;;; signal is delivered ahead of a lower one already queued, and :nesting
;;; :priority lets it preempt a running handler only when it strictly
;;; outranks that handler. Handler depth is tracked until interrupt-return.

(defmachine intfoo-prio
  (register pc :width 16)
  (register ia :width 16)
  (register a :width 16)
  (stack sp :width 16 :depth 8)
  (memory ram :width 8 :addr-width 16)
  (device timer :priority 1)
  (device disk :priority 3)
  (interrupts :vector ia :message a :save (pc) :nesting :priority))

(definstruction intfoo-prio nop (encoding (opcode #x00)) (semantics nil) (cycles 1))
(definstruction intfoo-prio rfi (encoding (opcode #x01)) (semantics (interrupt-return)))

(let ((m (make-machine 'intfoo-prio)))
  (setf (sref m 'ia) #x0100)
  (load-program m (list #x00 #x00 #x00 #x01) :origin #x0100) ; handler: nops, then rfi
  (setf (sref m 'pc) 0)
  (format t "~%Priority: the timer (1) signals first, then the disk (3)...~%")
  (device-signal m (device-at m 0) #x11)
  (device-signal m (device-at m 1) #x33)
  (step-machine m)
  (format t "First delivered: a=~2,'0X depth=~D~%" (sref m 'a) (machine-interrupt-depth m))
  (assert (= #x33 (sref m 'a)))       ; the disk jumped the queue
  (assert (= 1 (machine-interrupt-depth m)))
  (step-machine m)
  (assert (= 1 (machine-interrupt-depth m))) ; the timer (1) cannot preempt the disk handler (3)
  (format t "Timer held while the disk handler runs: depth=~D~%" (machine-interrupt-depth m))
  (step-machine m)
  (step-machine m)                    ; rfi
  (assert (zerop (machine-interrupt-depth m)))
  (step-machine m)                    ; the timer is delivered now
  (assert (= #x11 (sref m 'a)))
  (format t "Timer delivered after return: a=~2,'0X~%" (sref m 'a))

  (format t "~%All priority assertions passed.~%"))

;;; #305: a priority mask (the 68k's IPL shape) and a non-maskable signal.
;;; Only signals above the MASK register's level deliver; a non-maskable one
;;; ignores it.

(defmachine intlevel
  (register pc :width 16)
  (register ia :width 16)
  (register a :width 16)
  (register mask :width 8)
  (stack sp :width 16 :depth 8)
  (memory ram :width 8 :addr-width 16)
  (interrupts :vector ia :message a :save (pc) :mask-level mask))

(definstruction intlevel nop (encoding (opcode #x00)) (semantics nil) (cycles 1))

(let ((m (make-machine 'intlevel)))
  (load-program m (list #x00 #x00 #x00) :origin 0)
  (load-program m (list #x00) :origin #x0100)
  (setf (sref m 'ia) #x0100 (sref m 'mask) 3)
  (signal-interrupt m 1 :priority 3)
  (step-machine m)
  (format t "~%Level mask 3: priority 3 held, queue depth ~D~%" (length (machine-interrupt-queue m)))
  (assert (/= 1 (sref m 'a)))
  (signal-interrupt m 2 :priority 1 :non-maskable t)
  (step-machine m)
  (format t "Non-maskable priority 1 delivered: a=~D~%" (sref m 'a))
  (assert (= 2 (sref m 'a))))
