;;;; Tests an example system: sbcl --script tests/run-example.lisp ASD SYSTEM

(load (merge-pathnames "../bench/boot.lisp" *load-pathname*))

(destructuring-bind (asd system) (uiop:command-line-arguments)
  (asdf:load-asd (truename asd))
  (asdf:test-system system))
