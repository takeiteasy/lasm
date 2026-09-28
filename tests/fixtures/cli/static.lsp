;; Static frames (docs/static-frames.md): no recursion, so every local and
;; argument has a fixed address, and the backend needs no stack slots.
;;
;;   lasm run static.lsp -m callfoo.lisp --backend callfoo-lang-abi --frames static
;;
;; The result is left in register a.
(:program (:backend callfoo-lang-abi :frames static))

(defun square (n) (* n n))

(defun sum-of-squares (a b)
  (+ (square a) (square b)))

(defun main ()
  (sum-of-squares 3 4))    ; 25
