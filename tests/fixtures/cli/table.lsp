;; Function values, indirect calls, arrays and strings (#365, #366) in the
;; small source language (docs/language.md).
;;
;;   lasm run table.lsp -m callfoo.lisp --backend callfoo-lang-abi
;;
;; The result is left in register a.
(:program (:backend callfoo-lang-abi))

(defun add-one (x) (+ x 1))
(defun double (x) (* x 2))

;; A table of function values; funcall dispatches through a computed index.
(defarray ops ((function add-one) (function double)))

(defun apply-op (index n) (funcall (aref ops index) n))

(defstring greeting "hi")

(defun sum-string (s len)
  (let ((total 0) (i 0))
    (while (< i len)
      (set total (+ total (aref s i)))
      (set i (+ i 1)))
    total))

(defun main ()
  (+ (apply-op 0 10)   ; add-one 10 = 11
     (apply-op 1 10)   ; double 10 = 20
     (sum-string greeting 2)))  ; #\h + #\i = 209
