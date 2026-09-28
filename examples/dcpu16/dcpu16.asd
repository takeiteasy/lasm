;;;; dcpu16.asd

(asdf:defsystem #:dcpu16
  :description "A DCPU-16 v1.7 emulator written with the lasm Lisp DSL"
  :author "George Watson <gigolo@hotmail.co.uk>"
  :license "GPL-3.0-or-later"
  :depends-on (#:lasm)
  :serial t
  :components ((:file "package")
               (:file "dcpu16")
               (:file "devices"))
  :in-order-to ((test-op (test-op #:dcpu16/test))))

(asdf:defsystem #:dcpu16/test
  :description "Tests for the DCPU-16 example"
  :author "George Watson <gigolo@hotmail.co.uk>"
  :license "GPL-3.0-or-later"
  :depends-on (#:dcpu16 #:fiveam)
  :components ((:file "test"))
  :perform (test-op (op c)
             (unless (uiop:symbol-call :dcpu16/test :run-tests)
               (error "dcpu16 tests failed"))))
