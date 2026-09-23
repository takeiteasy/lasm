;;;; tests/examples.lisp
;;;; Runs every examples/**/*.lisp script and fails on any non-zero exit.

(in-package #:lasm)

(fiveam:def-suite examples :in lasm)
(fiveam:in-suite examples)

;; TODO: fixed job count waited on oldest-first; size from the host core count and
;; reap whichever script finishes first if the example set grows or slows.
(defparameter *example-jobs* 8
  "Scripts run concurrently; more than the core count slows the whole run.")

(defun %example-scripts ()
  (remove "boot" (directory (merge-pathnames (make-pathname :directory '(:relative "examples" :wild-inferiors)
                                                            :name :wild :type "lisp")
                                             (asdf:system-source-directory :lasm)))
          :key #'pathname-name :test #'string=))

;; Examples are standalone scripts that redefine globals, so each gets its own SBCL.
(defun %launch-example (script)
  (let ((log (uiop:tmpize-pathname (merge-pathnames "lasm-example.log" (uiop:temporary-directory)))))
    (list script log
          (uiop:launch-program (list (namestring sb-ext:*runtime-pathname*) "--script" (namestring script))
                               :output nil :error-output log :if-error-output-exists :supersede))))

(defun %check-example (run)
  (destructuring-bind (script log process) run
    (let ((status (uiop:wait-process process)))
      (fiveam:is (zerop status) "~A exited ~D:~%~A"
                 (enough-namestring script (asdf:system-source-directory :lasm))
                 status (uiop:read-file-string log))
      (uiop:delete-file-if-exists log))))

(fiveam:test every-example-script-exits-cleanly
  (let ((scripts (%example-scripts))
        (running '()))
    (fiveam:is (plusp (length scripts)))
    (dolist (script scripts)
      (when (= (length running) *example-jobs*)
        (%check-example (cl:pop running)))
      (setf running (append running (list (%launch-example script)))))
    (mapc #'%check-example running)))
