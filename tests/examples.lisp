;;;; tests/examples.lisp
;;;; Runs every examples/**/*.lisp script, and bench/ scripts when LASM_BENCH=1,
;;;; failing on any non-zero exit.

(in-package #:lasm)

(fiveam:def-suite examples :in lasm)
(fiveam:in-suite examples)

(defun %lasm-path (relative)
  (merge-pathnames relative (asdf:system-source-directory :lasm)))

(defun %example-scripts ()
  "Standalone scripts; examples/cli/ holds machine files for the command line."
  (remove-if (lambda (path)
               (or (string= "boot" (pathname-name path))
                   (member "cli" (pathname-directory path) :test #'string=)))
             (directory (%lasm-path (make-pathname :directory '(:relative "examples" :wild-inferiors)
                                                   :name :wild :type "lisp")))))

(defun %host-cores ()
  (or (ignore-errors
       (parse-integer (uiop:run-program '("getconf" "_NPROCESSORS_ONLN") :output :string)
                      :junk-allowed t))
      4))

(defun %sbcl (&rest args)
  #+sbcl (list* (namestring sb-ext:*runtime-pathname*) args)
  #-sbcl (error "Not SBCL: ~S" args))

(defun %save-example-core (core)
  (multiple-value-bind (out err status)
      (uiop:run-program (%sbcl "--non-interactive" "--no-sysinit" "--no-userinit"
                               "--load" (namestring (%lasm-path "examples/boot.lisp"))
                               "--eval" (format nil "(sb-ext:save-lisp-and-die ~S)" (namestring core)))
                        :output nil :error-output :string :ignore-error-status t)
    (declare (ignore out))
    (unless (zerop status)
      (error "Saving the example core failed:~%~A" err))))

(defun %newest-lasm-source-date ()
  (reduce #'max (mapcar #'file-write-date
                        (list* (asdf:system-source-file :lasm)
                               (%lasm-path "examples/boot.lisp")
                               (mapcar #'asdf:component-pathname
                                       (asdf:component-children (asdf:find-system :lasm)))))))

;; Booting Quicklisp dominates each script's run time, so scripts start from a
;; core that has already loaded boot.lisp. Quicklisp dependency updates don't
;; invalidate it; delete the core after one.
(defun %example-core ()
  "The core the scripts start from, or NIL on a host that cannot save one."
  #-sbcl nil
  #+sbcl
  (let ((core (merge-pathnames "examples.core"
                               (asdf:apply-output-translations (asdf:system-source-directory :lasm)))))
    (unless (and (probe-file core) (<= (%newest-lasm-source-date) (file-write-date core)))
      (ensure-directories-exist core)
      (let ((tmp (uiop:tmpize-pathname core)))
        (unwind-protect
             (progn (%save-example-core tmp)
                    (uiop:rename-file-overwriting-target tmp core))
          (uiop:delete-file-if-exists tmp))))
    core))

(defun %script-command (core script args)
  #+sbcl (apply #'%sbcl "--core" core "--script" (namestring script) args)
  #+ecl (list* "ecl" "--norc" "--shell" (namestring script) args))

;; Scripts are standalone and redefine globals, so each gets its own Lisp.
(defun %run-scripts (runs)
  "RUNS is a list of (SCRIPT . ARGS). Returns a (SCRIPT EXIT-STATUS STDERR) list per run."
  (let ((core (let ((core (%example-core))) (and core (namestring core))))
        (queue runs)
        (results '())
        (lock (%make-lock)))
    (flet ((worker ()
             (loop for (script . args) = (%with-lock (lock) (cl:pop queue))
                   while script
                   do (multiple-value-bind (out err status)
                          (handler-case
                              (uiop:with-temporary-file (:pathname err-file)
                                (let ((status (nth-value 2 (uiop:run-program (%script-command core script args)
                                                                             :output nil :error-output err-file
                                                                             :ignore-error-status t))))
                                  (values nil (uiop:read-file-string err-file) status)))
                            (error (e) (values nil (princ-to-string e) -1)))
                        (declare (ignore out))
                        (%with-lock (lock)
                          (cl:push (list script status err) results))))))
      (mapc #'%join-thread
            (loop repeat (%host-cores) collect (%make-thread #'worker "script"))))
    results))

(defun %check-scripts (runs)
  (loop for (script status err) in (%run-scripts runs)
        do (fiveam:is (zerop status) "~A exited ~D:~%~A"
                      (enough-namestring script (asdf:system-source-directory :lasm))
                      status err)))

(fiveam:test every-example-script-exits-cleanly
  (let ((scripts (%example-scripts)))
    (fiveam:is (plusp (length scripts)))
    (%check-scripts (mapcar #'list scripts))))

(fiveam:test debugger-history-bench-exits-cleanly
  (%check-scripts `((,(%lasm-path "bench/debugger-history.lisp") "1000"))))

(fiveam:test bench-scripts-exit-cleanly
  (if (uiop:getenv "LASM_BENCH")
      (let ((star (namestring (%lasm-path "../star/star.asd"))))
        (fiveam:is (probe-file star) "LASM_BENCH needs STAR at ~A" star)
        (%check-scripts `((,(%lasm-path "bench/cpu-scaling.lisp") ,star "1" "1")
                          (,(%lasm-path "bench/memory-audit.lisp") ,star))))
      (fiveam:skip "Set LASM_BENCH=1 to run bench/ scripts against ../star")))
