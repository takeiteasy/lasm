;;;; interrupt.lisp
;;;; #109: interrupt delivery -- masking, and the step-machine hook that
;;;; pulls a pending signal off the queue and dispatches it.
;;;;
;;;; #108's device-signal/machine-interrupt-hook seam is unchanged by this
;;;; file: MAKE-MACHINE (storage.lisp) auto-installs #'%default-interrupt-
;;;; hook when a machine declares (interrupts ...), and that hook -- along
;;;; with %enqueue-interrupt, the shared enqueue mechanics -- lives in
;;;; storage.lisp itself, not here, since MAKE-MACHINE needs it and
;;;; LASM.ASD is :serial t with this file loading after storage.lisp. What
;;;; lives here instead is everything that needs pieces declared in later
;;;; files -- device.lisp's tick-devices, storage.lisp's stack-push/pop --
;;;; plus the one function a software INT-style instruction calls directly.

(in-package #:lasm)

;;; Software-raised interrupts

(defun signal-interrupt (machine data &optional device)
  "The public, DEVICE-optional entry point for raising an interrupt --
called directly by an INT-style instruction's semantics, MACHINE passed
explicitly, the same convention as DEVICE-INFO/DEVICE-SEND (docs/
devices.md). DEVICE-SIGNAL (device.lisp) reaches the same queue through
MACHINE-INTERRUPT-HOOK instead, for a device rather than an instruction
raising its hand -- both funnel into %ENQUEUE-INTERRUPT (storage.lisp)."
  (%enqueue-interrupt machine (cons device data)))

;;; Masking

(defun %interrupt-masked-p (machine interrupts)
  "T when INTERRUPTS' :MASK-WHEN or :MASK-FLAG says MACHINE currently
rejects delivery. Masking gates delivery only, never enqueueing -- a
masked machine still queues incoming signals (subject to :QUEUE/
:ON-OVERFLOW), it just doesn't pop and deliver the head until unmasked."
  (cond
    ((interrupt-descriptor-mask-when interrupts)
     (and (funcall (interrupt-descriptor-mask-when interrupts) machine) t))
    ((interrupt-descriptor-mask-flag interrupts)
     (plusp (flag machine (interrupt-descriptor-mask-flag interrupts))))
    (t nil)))

;;; Delivery

(defun deliver-pending-interrupt (machine pc)
  "Pop and deliver MACHINE's oldest pending interrupt, if any and if
unmasked -- called at the top of STEP-MACHINE (emulator.lisp), before that
step's own fetch, so both single-stepping (DEBUG-STEP) and every RUN
variant see delivery the same way. PC is the already-%RESOLVE-PC'd register
name STEP-MACHINE is about to fetch through, so delivery sets the same
register STEP-MACHINE reads next, honoring any :PC override the same way
STEP-MACHINE itself does. Pushes every :SAVE place in declared order,
writes the signal's DATA into :MESSAGE, sets :VECTOR's value into PC, adds
:CYCLES to MACHINE-CYCLES, and ticks devices with that delivery cost -- but
only when it's non-zero, so the default :CYCLES 0 doesn't add a second,
redundant TICK-DEVICES call to every delivering step. Does nothing when
the machine declares no (interrupts ...) clause, the queue is empty, or
the queue's head is currently masked.

#110: also clears MACHINE-IDLE (storage.lisp) -- delivery is the only thing
that wakes an idling machine; a masked machine's queue still fills, but it
stays idle until unmasked, same as delivery itself."
  (let ((interrupts (machine-descriptor-interrupts (machine-descriptor machine))))
    (when (and interrupts
               (machine-interrupt-queue machine)
               (not (%interrupt-masked-p machine interrupts)))
      (let* ((entry (cl:pop (machine-interrupt-queue machine)))
             (data (cdr entry))
             (stack (interrupt-descriptor-stack-name interrupts)))
        (dolist (place (interrupt-descriptor-save interrupts))
          (stack-push machine stack (sref machine place)))
        (setf (sref machine (interrupt-descriptor-message interrupts)) data)
        (setf (sref machine pc) (sref machine (interrupt-descriptor-vector interrupts)))
        (setf (machine-idle machine) nil)
        (let ((cost (interrupt-descriptor-cycles interrupts)))
          (incf (machine-cycles machine) cost)
          (unless (zerop cost)
            (tick-devices machine cost)))))))
