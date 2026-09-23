;;;; tests/examples.lisp
;;;; Runs every examples/**/*.lisp script and fails on any non-zero exit.

(in-package #:lasm)

(fiveam:def-suite examples :in lasm)
(fiveam:in-suite examples)

(defun %lasm-path (relative)
  (merge-pathnames relative (asdf:system-source-directory :lasm)))

(defun %example-scripts ()
  (remove "boot" (directory (%lasm-path (make-pathname :directory '(:relative "examples" :wild-inferiors)
                                                       :name :wild :type "lisp")))
          :key #'pathname-name :test #'string=))

(defun %host-cores ()
  (or (ignore-errors
       (parse-integer (uiop:run-program '("getconf" "_NPROCESSORS_ONLN") :output :string)
                      :junk-allowed t))
      4))

(defun %sbcl (&rest args)
  (list* (namestring sb-ext:*runtime-pathname*) args))

;; Booting Quicklisp dominates each example's run time, so examples start from
;; a core that has already loaded boot.lisp.
(defun %save-example-core (core)
  (multiple-value-bind (out err status)
      (uiop:run-program (%sbcl "--non-interactive" "--no-sysinit" "--no-userinit"
                               "--load" (namestring (%lasm-path "examples/boot.lisp"))
                               "--eval" (format nil "(sb-ext:save-lisp-and-die ~S)" (namestring core)))
                        :output nil :error-output :string :ignore-error-status t)
    (declare (ignore out))
    (unless (zerop status)
      (error "Saving the example core failed:~%~A" err))))

;; Examples are standalone scripts that redefine globals, so each gets its own SBCL.
(defun %run-examples (core scripts)
  "Returns a (SCRIPT EXIT-STATUS STDERR) list per script."
  (let ((queue scripts)
        (results '())
        (lock (sb-thread:make-mutex)))
    (flet ((worker ()
             (loop for script = (sb-thread:with-mutex (lock) (cl:pop queue))
                   while script
                   do (multiple-value-bind (out err status)
                          (handler-case
                              (uiop:run-program (%sbcl "--core" (namestring core) "--script" (namestring script))
                                                :output nil :error-output :string :ignore-error-status t)
                            (error (e) (values nil (princ-to-string e) -1)))
                        (declare (ignore out))
                        (sb-thread:with-mutex (lock)
                          (cl:push (list script status err) results))))))
      (mapc #'sb-thread:join-thread
            (loop repeat (%host-cores) collect (sb-thread:make-thread #'worker :name "example"))))
    results))

(fiveam:test every-example-script-exits-cleanly
  (let ((scripts (%example-scripts)))
    (fiveam:is (plusp (length scripts)))
    (uiop:with-temporary-file (:pathname core :type "core")
      (%save-example-core core)
      (loop for (script status err) in (%run-examples core scripts)
            do (fiveam:is (zerop status) "~A exited ~D:~%~A"
                          (enough-namestring script (asdf:system-source-directory :lasm))
                          status err)))))
