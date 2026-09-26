;; Factorial, a loop and a global, in the small source language (docs/language.md).
;;
;;   lasm run fact.lsp -m callfoo.lisp --backend callfoo-lang-abi
;;
;; The result is left in register a.
(:program (:backend callfoo-lang-abi))

(defvar calls 0)

(defun fact (n)
  (set calls (+ calls 1))
  (if (< n 2)
      1
      (* n (fact (- n 1)))))

(defun sum-to (n)
  (let ((total 0) (i 1))
    (while (<= i n)
      (set total (+ total i))
      (set i (+ i 1)))
    total))

(defun main ()
  (+ (fact 5) (sum-to 10)))
