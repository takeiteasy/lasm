;;;; Runs the whole suite and exits nonzero on any failure:
;;;;   ros -L sbcl-bin -Q -l tests/ci.lisp

(load (merge-pathnames "../bench/boot.lisp" *load-truename*))
(asdf:load-system :lasm/test)

(let ((results (fiveam:run (uiop:find-symbol* :lasm :lasm))))
  (fiveam:explain! results)
  (uiop:quit (if (fiveam:results-status results) 0 1)))
