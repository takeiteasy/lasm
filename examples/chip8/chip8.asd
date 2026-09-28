;;;; chip8.asd

(asdf:defsystem #:chip8
  :description "A CHIP-8 emulator written in the lasm .lsp language"
  :author "George Watson <gigolo@hotmail.co.uk>"
  :license "GPL-3.0-or-later"
  :depends-on (#:lasm)
  :serial t
  :components ((:file "package")
               (:file "host")
               (:static-file "chip8.lsp")
               (:file "chip8"))
  :in-order-to ((test-op (test-op #:chip8/test))))

(asdf:defsystem #:chip8/test
  :description "Tests for the CHIP-8 example"
  :author "George Watson <gigolo@hotmail.co.uk>"
  :license "GPL-3.0-or-later"
  :depends-on (#:chip8 #:fiveam)
  :components ((:file "test"))
  :perform (test-op (op c)
             (unless (uiop:symbol-call :chip8/test :run-tests)
               (error "chip8 tests failed"))))
