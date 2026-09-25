;;;; device.lisp
;;;; #108: the device bus -- enumeration, attach/detach, the CPU-facing
;;;; message and tick API, and the interrupt signal seam.
;;;;
;;;; A device is addressed by instruction and bus index (HWN/HWQ/HWI-style).
;;;; It may also be memory-mapped: a #107 :DEVICE region naming it with
;;;; :DEVICE (#158) routes MREF through its :READ/:WRITE hooks. A region's own
;;;; :READ/:WRITE remain for a peripheral that needs no bus identity.
;;;;
;;;; DEVICE-SIGNAL below is unchanged by #109's interrupt subsystem --
;;;; it still only calls whatever MACHINE-INTERRUPT-HOOK is installed, and
;;;; drops the signal when none is. What changed is what's installed there:
;;;; MAKE-MACHINE (storage.lisp) now auto-wires the hook to the real
;;;; delivery queue on any machine declaring an (interrupts ...) clause
;;;; (machine.lisp) -- see docs/interrupts.md for the queue/masking/
;;;; overflow model this seam now feeds. A device's :SAVE/:LOAD hooks feed
;;;; snapshots (snapshot.lisp).

(in-package #:lasm)

;;; Namespace

(defun %device-name-taken-p (machine name)
  "T when NAME already names something on MACHINE -- a storage element, a
register alias, a #107 region, or another device (declared or attached).
ATTACH-DEVICE checks a fresh name against this before appending it, so a
runtime-attached device's name can't shadow anything DEFMACHINE's own SEEN
table (machine.lisp) would have rejected at DEFMACHINE time."
  (let ((descriptor (machine-descriptor machine)))
    (or (nth-value 1 (gethash name (machine-descriptor-table descriptor)))
        (nth-value 1 (gethash (symbol-name name) (machine-descriptor-register-aliases descriptor)))
        (some (lambda (element)
                (find name (storage-element-regions element) :key #'memory-region-name))
              (machine-descriptor-elements descriptor))
        (find name (machine-devices machine)
              :key (lambda (d) (and d (device-descriptor-name (device-descriptor d))))))))

;;; Attach / detach

(defun attach-device (machine name &key id version manufacturer init tick receive detach save load)
  "Attach a device to MACHINE's bus and return its (fixed) index.

NAME either names a DEVICE-DESCRIPTOR already declared on MACHINE's own
machine (a (device ...) clause, machine.lisp) -- attaching a second, freshly
INIT'd instance of it, every :ID/:VERSION/... keyword here then ignored --
or is a fresh symbol, checked against MACHINE's whole namespace exactly as a
declared device's name is (%DEVICE-NAME-TAKEN-P), with the identity/hooks
given inline the same way a (device ...) clause's keywords are.

Appended after every existing bus entry, declared or attached -- indices are
never reused (DETACH-DEVICE leaves a hole rather than shrinking the bus), so
an index returned here stays valid until this device itself is detached."
  (let* ((descriptor (machine-descriptor machine))
         (declared (find name (machine-descriptor-devices descriptor) :key #'device-descriptor-name))
         (device-descriptor
           (or declared
               (progn
                 (when (%device-name-taken-p machine name)
                   (%emulator-usage-error "attach-device ~S on machine ~S: name already in use"
                          name (machine-descriptor-name descriptor)))
                 (make-device-descriptor :name name :id (or id 0) :version (or version 0)
                                          :manufacturer (or manufacturer 0)
                                          :init init :tick tick :receive receive :detach detach
                                          :save save :load load)))))
    (let ((devices (machine-devices machine)))
      (vector-push-extend (%instantiate-device machine device-descriptor (fill-pointer devices))
                           devices))))

(defun detach-device (machine index)
  "Remove the device at bus INDEX on MACHINE, calling its :DETACH hook (if
any) first. Leaves a hole -- every other device's index is unaffected, and
DEVICE-COUNT does not shrink -- so a program that cached this index (or a
later one) never has it silently start addressing a different device.
Signals NO-SUCH-DEVICE on an already-vacant or out-of-range INDEX."
  (let ((device (device-at machine index)))
    (let ((detach (device-descriptor-detach (device-descriptor device))))
      (when detach (funcall detach machine device)))
    (setf (aref (machine-devices machine) index) nil))
  (values))

;;; Enumeration

(defun device-count (machine)
  "The size of MACHINE's bus, holes included -- the high-water index bound.
What an HWN-style instruction's semantics return."
  (fill-pointer (machine-devices machine)))

(defun device-at (machine index)
  "The DEVICE at bus INDEX on MACHINE. Signals NO-SUCH-DEVICE when INDEX is
out of range or names a detached hole."
  (let ((devices (machine-devices machine)))
    (unless (and (>= index 0) (< index (fill-pointer devices)) (aref devices index))
      (error 'no-such-device :machine (machine-descriptor-name (machine-descriptor machine))
                              :index index))
    (aref devices index)))

(defun find-device (machine name)
  "The first live device on MACHINE's bus whose descriptor is named NAME, or
NIL. NAME may be a declared device (machine.lisp) or one attached at
runtime (ATTACH-DEVICE) -- both share one namespace."
  (find name (machine-devices machine)
        :key (lambda (d) (and d (device-descriptor-name (device-descriptor d))))))

;;; CPU-facing API

(defun device-info (machine index)
  "(VALUES ID VERSION MANUFACTURER) for the device at bus INDEX on MACHINE --
an HWQ-style instruction's own semantics decide which registers each value
lands in. Signals NO-SUCH-DEVICE via DEVICE-AT."
  (let ((descriptor (device-descriptor (device-at machine index))))
    (values (device-descriptor-id descriptor)
            (device-descriptor-version descriptor)
            (device-descriptor-manufacturer descriptor))))

(defun device-send (machine index)
  "Send an HWI-style message to the device at bus INDEX on MACHINE, calling
its :RECEIVE hook -- a no-op when it declares none. Signals NO-SUCH-DEVICE
via DEVICE-AT."
  (let ((device (device-at machine index)))
    (let ((receive (device-descriptor-receive (device-descriptor device))))
      (when receive (funcall receive machine device))))
  (values))

;;; Ticking

(defun tick-devices (machine cycles)
  "Call every live device's :TICK hook on MACHINE with CYCLES -- the elapsed
cost of the instruction step that just ran (STEP-MACHINE, emulator.lisp),
zero-cost when a device declares no :TICK. Called once per STEP-MACHINE for the
instruction's declared cost, and again for any EXTRA-CYCLES (#90), in
bus index order, including a hole-skipping pass -- not once per hole."
  (loop for device across (machine-devices machine)
        when device
          do (let ((tick (device-descriptor-tick (device-descriptor device))))
               (when tick (funcall tick machine device cycles))))
  (values))

;;; Interrupt seam (#109's queue is what's installed here now)

(defun device-signal (machine device &optional data)
  "The #108 interrupt seam, unchanged: calls MACHINE-INTERRUPT-HOOK
(storage.lisp) with MACHINE, DEVICE and DATA when one is installed,
otherwise drops the signal. On a machine declaring an (interrupts ...)
clause, that hook is #109's real delivery queue -- see docs/interrupts.md
for the full model (SIGNAL-INTERRUPT, masking, overflow policy). This
function itself still only gives a device a way to raise its hand."
  (let ((hook (machine-interrupt-hook machine)))
    (when hook (funcall hook machine device data)))
  (values))
