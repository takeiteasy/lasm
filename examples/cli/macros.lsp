;; Compile-time macros (docs/language.md#macros): a template substitution,
;; a &rest splice, and hygiene keeping a template's own `let` name from
;; capturing a caller's variable of the same name.
;;
;;   lasm run macros.lsp -m callfoo.lisp --backend callfoo-lang-abi
;;
;; The result is left in register a.
(:program (:backend callfoo-lang-abi))

(defmacro inc (v) (set v (+ v 1)))

(defmacro unless (c &rest body) (if c 0 (progn body)))

(defmacro swap (a b) (let ((tmp a)) (set a b) (set b tmp)))

(defun main ()
  (let ((count 0) (tmp 1) (y 2))
    (inc count)
    (unless 0 (inc count) (inc count))
    (swap tmp y)
    (+ (* tmp 100) y count)))
