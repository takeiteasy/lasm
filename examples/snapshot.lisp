;;;; examples/snapshot.lisp
;;;;
;;;; #112: save a machine's state to a file and restore it into a fresh
;;;; machine, including a device that opts in with :save/:load.
;;;;
;;;; Run with:  sbcl --script examples/snapshot.lisp

(load (merge-pathnames "boot.lisp" *load-pathname*))

(in-package #:lasm)

(defun ticker-init (machine device)
  (declare (ignore machine device))
  (list 0))

(defun ticker-tick (machine device cycles)
  (declare (ignore machine))
  (incf (car (device-state device)) cycles))

(defun ticker-save (machine device)
  (declare (ignore machine))
  (copy-list (device-state device)))

(defun ticker-load (machine device data)
  (declare (ignore machine))
  (setf (device-state device) (copy-list data)))

(defmachine snapshot-demo
  (register a :width 8)
  (stack s :width 8 :depth 4)
  (memory ram :width 8 :addr-width 8)
  (flags z)
  (device ticker :init ticker-init :tick ticker-tick :save ticker-save :load ticker-load))

(let ((original (make-machine 'snapshot-demo))
      (restored (make-machine 'snapshot-demo))
      (path (merge-pathnames "snapshot-demo.snap" (uiop:temporary-directory))))
  (setf (sref original 'a) 42)
  (stack-push original 's 7)
  (%poke original 'ram #x10 99)
  (tick-devices original 5)

  (write-snapshot (machine-snapshot original) path :format :binary)
  (restore-snapshot restored (read-snapshot path))

  (format t "a = ~D, stack depth = ~D, ram[#x10] = ~D, ticker = ~S~%"
          (sref restored 'a) (stack-depth restored 's)
          (mpeek restored 'ram #x10) (device-state (device-at restored 0)))
  (delete-file path))
