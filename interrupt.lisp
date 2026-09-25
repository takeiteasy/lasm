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

(defun signal-interrupt (machine data &key device priority (non-maskable nil non-maskable-p))
  "The public, DEVICE-optional entry point for raising an interrupt --
called directly by an INT-style instruction's semantics, MACHINE passed
explicitly, the same convention as DEVICE-INFO/DEVICE-SEND (docs/
devices.md). DEVICE-SIGNAL (device.lisp) reaches the same queue through
MACHINE-INTERRUPT-HOOK instead, for a device rather than an instruction
raising its hand -- both funnel into %ENQUEUE-INTERRUPT (storage.lisp).

#161: PRIORITY (an integer, higher delivers first) defaults to DEVICE's
declared :PRIORITY, or 0 for a software-raised signal. #305: NON-MASKABLE
ignores every mask; it defaults to DEVICE's declared :NON-MASKABLE."
  (%enqueue-interrupt machine (list device data (or priority (%device-interrupt-priority device))
                                    (if non-maskable-p
                                        (and non-maskable t)
                                        (%device-interrupt-non-maskable device)))))

;;; Masking

(defun %interrupt-mask-state (machine interrupts)
  "What currently holds back maskable signals: T when :MASK-WHEN/:MASK-FLAG masks
everything, otherwise the #305 mask level (:MASK-LEVEL/:MASK-LEVEL-WHEN) -- an
integer holding back priorities at or below it -- or NIL. Evaluated once per
delivery attempt. Masking gates delivery only, never enqueueing -- a masked
machine still queues incoming signals (subject to :QUEUE/:ON-OVERFLOW)."
  (cond
    ((and (interrupt-descriptor-mask-when interrupts)
          (funcall (interrupt-descriptor-mask-when interrupts) machine))
     t)
    ((and (interrupt-descriptor-mask-flag interrupts)
          (plusp (flag machine (interrupt-descriptor-mask-flag interrupts))))
     t)
    ((interrupt-descriptor-mask-level-when interrupts)
     (funcall (interrupt-descriptor-mask-level-when interrupts) machine))
    ((interrupt-descriptor-mask-level interrupts)
     (%interrupt-place machine (interrupt-descriptor-mask-level interrupts)))))

(defun %next-deliverable-interrupt (machine interrupts)
  "The highest-priority pending entry that is not masked, oldest first within
a priority, or NIL. A non-maskable entry is never masked."
  (let ((mask (%interrupt-mask-state machine interrupts)))
    (dolist (bucket (machine-interrupt-buckets machine))
      (let ((entry (%bucket-head bucket (or (eq mask t)
                                            (and mask (<= (bucket-priority bucket) mask))))))
        (when entry (return entry))))))

;;; Nesting

(defun %interrupt-tracks-depth-p (interrupts)
  (or (eq (interrupt-descriptor-nesting interrupts) :priority)
      (interrupt-descriptor-max-depth interrupts)))

(defun %interrupt-nesting-blocked-p (machine interrupts priority)
  "T when a signal of PRIORITY may not start a handler now (#161): the
handler depth is at :MAX-DEPTH, or :NESTING :PRIORITY and it does not
outrank the running handler."
  (let ((active (machine-interrupt-active machine))
        (max-depth (interrupt-descriptor-max-depth interrupts)))
    (or (and max-depth (>= (length active) max-depth))
        (and active
             (eq (interrupt-descriptor-nesting interrupts) :priority)
             (<= priority (first active))))))

(defun %interrupt-returned (machine)
  "Leave the innermost running handler. INTERRUPT-RETURN always calls this,
since a child machine can add nesting to a parent's compiled RFI."
  (cl:pop (machine-interrupt-active machine)))

(defun machine-interrupt-depth (machine)
  "How many interrupt handlers are running, on a machine whose (interrupts
...) declares :NESTING :PRIORITY or :MAX-DEPTH; always 0 otherwise."
  (length (machine-interrupt-active machine)))

;;; Delivery

(defun %enter-delivery-level (machine interrupts)
  "Switch MACHINE to INTERRUPTS' :DELIVER-LEVEL (#301), if it declares one.
The caller has already read the :SAVE places, so a saved level register keeps
the interrupted level."
  (let ((level (interrupt-descriptor-deliver-level interrupts)))
    (when level
      (let ((privilege (machine-descriptor-privilege (machine-descriptor machine))))
        (setf (%sref machine (privilege-descriptor-level privilege))
              (nth (position level (privilege-descriptor-levels privilege))
                   (privilege-descriptor-values privilege)))))))

(defun deliver-pending-interrupt (machine pc &optional memory)
  "Pop and deliver MACHINE's oldest pending interrupt, if any and if
unmasked -- called at the top of STEP-MACHINE (emulator.lisp), before that
step's own fetch, so both single-stepping (DEBUG-STEP) and every RUN
variant see delivery the same way. PC is the already-%RESOLVE-PC'd register
name STEP-MACHINE is about to fetch through, so delivery sets the same
register STEP-MACHINE reads next, honoring any :PC override the same way
STEP-MACHINE itself does. Pushes every :SAVE place in declared order,
writes the signal's DATA into :MESSAGE, sets :VECTOR's value (:NMI-VECTOR's for a
non-maskable signal that has one, #311) into PC, adds
:CYCLES to MACHINE-CYCLES, and ticks devices with that delivery cost -- but
only when it's non-zero, so the default :CYCLES 0 doesn't add a second,
redundant TICK-DEVICES call to every delivering step. Does nothing when
the machine declares no (interrupts ...) clause, the queue is empty, or
every queued signal is currently masked.

#301: a violation while delivering is located at the interrupted instruction
(MEMORY selects where to look up its source line and label).

#161, #305: the delivered signal is the highest-priority unmasked one, and it
is held back while :NESTING/:MAX-DEPTH forbid another handler
(%INTERRUPT-NESTING-BLOCKED-P), which even a non-maskable signal obeys.

#110: also clears MACHINE-IDLE (storage.lisp) -- delivery is the only thing
that wakes an idling machine; a masked machine's queue still fills, but it
stays idle until unmasked, same as delivery itself."
  (let* ((interrupts (machine-descriptor-interrupts (machine-descriptor machine)))
         (entry (and interrupts
                     (plusp (machine-interrupt-count machine))
                     (%next-deliverable-interrupt machine interrupts))))
    (when (and entry (not (%interrupt-nesting-blocked-p machine interrupts (pending-priority entry))))
      (%pop-pending machine entry)
      (let* ((data (pending-data entry))
             (stack (interrupt-descriptor-stack-name interrupts))
             (interrupted (%sref machine pc))
             (saved (mapcar (lambda (place) (%interrupt-place machine place))
                            (interrupt-descriptor-save interrupts))))
        (handler-bind ((runtime-location
                         (lambda (c) (%locate-runtime-condition c machine interrupted memory))))
          (%enter-delivery-level machine interrupts)
          ;; #166: a :POINTER stack pushes through SP-PUSH instead of
          ;; STACK-PUSH -- STACK names the bound register, and its
          ;; STACK-POINTER-DESCRIPTOR (resolved at DEFMACHINE time) carries
          ;; which memory and which growth direction to use.
          (if (eq (interrupt-descriptor-stack-kind interrupts) :pointer)
              (let ((sp (gethash stack (machine-descriptor-stack-pointers (machine-descriptor machine)))))
                (dolist (value saved)
                  (sp-push machine stack (stack-pointer-descriptor-memory sp)
                           (stack-pointer-descriptor-grows sp) value)))
              (dolist (value saved)
                (stack-push machine stack value)))
          (setf (%interrupt-place machine (interrupt-descriptor-message interrupts)) data)
          (setf (sref machine pc) (%interrupt-place machine (%interrupt-vector interrupts (pending-non-maskable entry)))))
        (when (interrupt-descriptor-mask-on-deliver interrupts)
          (setf (flag machine (interrupt-descriptor-mask-flag interrupts)) t))
        (when (interrupt-descriptor-mask-level-on-deliver interrupts)
          (setf (%interrupt-place machine (interrupt-descriptor-mask-level interrupts)) (pending-priority entry)))
        (when (%interrupt-tracks-depth-p interrupts)
          (cl:push (pending-priority entry) (machine-interrupt-active machine)))
        (setf (machine-idle machine) nil)
        (let ((cost (interrupt-descriptor-cycles interrupts)))
          (incf (machine-cycles machine) cost)
          (unless (zerop cost)
            (tick-devices machine cost)))))))
