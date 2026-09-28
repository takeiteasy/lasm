;;;; Tests an example system: sbcl --script tests/run-example.lisp ASD SYSTEM

(load (merge-pathnames "../bench/boot.lisp" *load-pathname*))

;; ECL's --shell leaves UIOP:COMMAND-LINE-ARGUMENTS empty; the raw list has
;; the two arguments last on both.
(destructuring-bind (asd system) (last (uiop:raw-command-line-arguments) 2)
  (asdf:load-asd (truename asd))
  (asdf:test-system system))
