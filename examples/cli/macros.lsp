;; Compile-time macros (docs/language.md#macros): a body evaluated at
;; compile time, quasiquote/unquote to build the expansion, a &rest splice,
;; hygiene keeping a template's own `let` name from capturing a caller's
;; variable of the same name, and a defun-for-syntax helper.
;;
;;   lasm run macros.lsp -m callfoo.lisp --backend callfoo-lang-abi
;;
;; The result is left in register a.
(:program (:backend callfoo-lang-abi))

(defmacro inc (v) `(set ,v (+ ,v 1)))

(defmacro unless (c &rest body) `(if ,c 0 (progn ,@body)))

(defmacro swap (a b) `(let ((tmp ,a)) (set ,a ,b) (set ,b tmp)))

;; A macro can decide its expansion from its (unevaluated) argument: a
;; literal integer doubles itself; anything else increments.
(defmacro double-or-inc (n) (if (integerp n) `(+ ,n ,n) `(+ ,n 1)))

;; A defun-for-syntax helper's own arguments are evaluated first, like an
;; ordinary function -- unlike a macro's.
(defun-for-syntax sum-of (xs) (if (null xs) 0 (+ (car xs) (sum-of (cdr xs)))))
(defmacro total (&rest xs) (sum-of xs))

(defun main ()
  (let ((count 0) (tmp 1) (y 2))
    (inc count)
    (unless 0 (inc count) (inc count))
    (swap tmp y)
    (+ (* tmp 100) y count (double-or-inc 5) (total 1 2 3))))
