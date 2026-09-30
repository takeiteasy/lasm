;;;; 6502.asd

(asdf:defsystem "6502"
  :description "A MOS 6502 machine, and a .lasm program that runs on it"
  :author "George Watson <gigolo@hotmail.co.uk>"
  :license "GPL-3.0-or-later"
  :depends-on (#:lasm)
  :serial t
  :components ((:file "package")
               (:file "timer")
               (:file "6502")
               (:static-file "demo.lasm"))
  :in-order-to ((test-op (test-op "6502/test"))))

(asdf:defsystem "6502/test"
  :description "Tests for the 6502 example"
  :author "George Watson <gigolo@hotmail.co.uk>"
  :license "GPL-3.0-or-later"
  :depends-on ("6502" #:fiveam)
  :components ((:file "test"))
  :perform (test-op (op c)
             (unless (uiop:symbol-call :mos6502/test :run-tests)
               (error "6502 tests failed"))))
