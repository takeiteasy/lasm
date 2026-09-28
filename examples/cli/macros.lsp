;; Compile-time macros (docs/language.md#macros): a body evaluated at
;; compile time, quasiquote/unquote to build the expansion, a &rest splice,
;; hygiene keeping a template's names and a caller's apart, a macro that
;; defines a macro, a defun-for-syntax helper, mapcar over a lambda, and
;; unmark reaching the caller's variable.
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

;; A template's free name is the global, even under a caller's local `hits`.
(defvar hits 0)
(defun hit-count () hits)
(defmacro hit () `(set hits (+ hits 1)))

;; A macro that defines a macro: ,',n puts n's value into the inner template.
(defmacro defadder (name n) `(defmacro ,name (x) `(+ ,x ,',n)))
(defadder add5 5)

;; mapcar with a compile-time lambda builds one term per argument.
(defmacro scaled (k &rest xs) `(+ ,@(mapcar (lambda (x) `(* ,k ,x)) xs)))

;; A quoted or template name is the macro's own; unmark reaches the caller's.
(defmacro bump-mine () `(set ,(unmark 'count) (+ ,(unmark 'count) 1)))

(defun main ()
  (let ((count 0) (tmp 1) (y 2))
    (inc count)
    (unless 0 (inc count) (inc count))
    (swap tmp y)
    (+ (* tmp 100) y (progn (bump-mine) count) (double-or-inc 5) (total 1 2 3)
       (add5 0) (let ((hits 0)) (hit) (+ hits (hit-count)))
       (scaled 10 1 2 3))))
