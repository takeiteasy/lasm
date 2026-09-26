;;;; examples/language.lisp
;;;;
;;;; The source language (#319): a small Lisp compiled to items through a
;;;; backend. The machine and backend are examples/cli/callfoo.lisp; the same
;;;; program as a file is examples/cli/fact.lsp.
;;;;
;;;; Run with:  sbcl --script examples/language.lisp

(load (merge-pathnames "boot.lisp" *load-pathname*))

(in-package #:lasm)

(load (merge-pathnames "cli/callfoo.lisp" *load-pathname*))

(defparameter *source*
  '((defun fact (n)
      (if (< n 2)
          1
          (* n (fact (- n 1)))))
    (defun main () (fact 5))))

(let ((items (compile-program *source* :backend 'callfoo-lang-abi)))
  (format t "~&Compiled to items, rendered as source text:~%~A"
          (render-items items :backend 'callfoo-lang-abi))
  (let ((machine (make-machine 'callfoo)))
    (load-program machine (assemble-items items :backend 'callfoo-lang-abi))
    (setf (sref machine 'sp) #x100)
    (run machine)
    (format t "~%fact 5 = ~D~%" (regref machine 'r 0))
    (assert (= 120 (regref machine 'r 0)))))

;; Source text is read without evaluation, and a compile error names the form.
(let ((program (read-source-from-string "(defun main () (+ 1 oops))")))
  (handler-case (compile-source program :backend 'callfoo-lang-abi)
    (program-compile-error (e)
      (format t "~%~A~%" e)
      (assert (search "unknown variable oops" (program-compile-error-detail e))))))

(format t "~%All assertions passed.~%")
