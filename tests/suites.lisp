;;;; tests/suites.lisp
;;;; Root fiveam suite and the shared TEST-MACHINE fixture used across the
;;;; other test files in this directory (storage.lisp, semantics.lisp).

(in-package #:lasm)

(fiveam:def-suite lasm)

;; Threads, on the hosts the tests run on.
(defun %make-thread (function &optional (name "thread"))
  #+sbcl (sb-thread:make-thread function :name name)
  #+ecl (mp:process-run-function name function))

(defun %join-thread (thread)
  #+sbcl (sb-thread:join-thread thread)
  #+ecl (mp:process-join thread))

(defun %make-lock ()
  #+sbcl (sb-thread:make-mutex)
  #+ecl (mp:make-lock))

(defmacro %with-lock ((lock) &body body)
  #+sbcl `(sb-thread:with-mutex (,lock) ,@body)
  #+ecl `(mp:with-lock (,lock) ,@body))

(defun %call-wrapped (name wrapper thunk)
  "Call THUNK with the function NAME replaced by one that calls WRAPPER with the original
function and its arguments."
  (let ((original (fdefinition name)))
    (setf (fdefinition name) (lambda (&rest args) (apply wrapper original args)))
    (unwind-protect (funcall thunk)
      (setf (fdefinition name) original))))

(defun %pending (machine)
  "MACHINE's pending interrupts as (DEVICE DATA PRIORITY NON-MASKABLE) lists, in delivery order."
  (let (entries)
    (map-pending-interrupts (lambda (&rest entry) (cl:push entry entries)) machine)
    (nreverse entries)))

;; Defined here, then used both by storage.lisp's tests (in this same file's
;; suite tree) and by a WITH-MACHINE form in semantics.lisp -- this is the
;; compile-time check that DEFMACHINE's descriptor is available at
;; macroexpansion time, not only after loading (see the EVAL-WHEN in
;; machine.lisp).
(defmachine test-machine
  (register a :width 8)
  (register wide :width 16)
  (register bank :width 8 :names (bank0 bank1 bank2 bank3)) ; #72: aliased banked register
  (stack s :width 8 :depth 4)
  (memory ram :width 8 :addr-width 8)
  (memory wram :width 8 :addr-width 4 :cell-width 16)
  (flags z n c v))
