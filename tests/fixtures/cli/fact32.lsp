;; Factorial past 16 bits: 10! is 3628800.
;;
;;   lasm run fact32.lsp -m quadfoo.lisp
;;
;; The result is left in the wa word, registers a to d.
(:program (:backend quadfoo-lang-abi))

(defun fact (n)
  (if (< n 2) (return 1))
  (* n (fact (- n 1))))

(defun main ()
  (fact 10))
