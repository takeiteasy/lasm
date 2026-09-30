;;;; Runs the whole suite and exits nonzero on any failure or error:
;;;;   ros -L sbcl-bin -Q -l tests/ci.lisp

;; Without this an error leaves some implementations at a REPL that exits 0 on end of input.
(handler-bind ((error (lambda (condition)
                        (format *error-output* "~&tests/ci.lisp: ~A~%" condition)
                        (uiop:quit 1))))
  (load (merge-pathnames "../bench/boot.lisp" *load-truename*))
  (asdf:load-system :lasm/test)
  (let ((results (fiveam:run (uiop:find-symbol* :lasm :lasm))))
    (fiveam:explain! results)
    (uiop:quit (if (fiveam:results-status results) 0 1))))
