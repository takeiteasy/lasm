(load (merge-pathnames "boot.lisp" *load-pathname*))

(in-package #:lasm)

(defmachine scoped-counter-example
  (register pc :width 16)
  (memory ram :width 8 :addr-width 16))

(deflexer dollar-counter-example
  (number-formats (:hex "$" "0x") (:dec :default))
  (label-suffix ":")
  (local-label-prefix ".")
  (ident-chars :alnum "_.")
  (location-counter "$"))

(let ((assembly (assemble "loop:
.next: .byte $, .next, loop.next
loop.next: .byte $FF"
                          :machine 'scoped-counter-example
                          :lexer 'dollar-counter-example)))
  (assert (equalp #(0 0 3 255) (assembly-cells assembly)))
  (assert (= 0 (symbol-info-value (assembly-symbol assembly ".next" :scope "loop"))))
  (assert (= 3 (symbol-info-value (assembly-symbol assembly "loop.next"))))
  (format t "~A" (symbols-text assembly)))
