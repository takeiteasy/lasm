;;;; lasm.asd
;;;; System definition for lasm

(asdf:defsystem #:lasm
  :description "A Lisp library and DSL for building fantasy assemblers and CPU emulators"
  :author "George Watson <gigolo@hotmail.co.uk>"
  :license "GPL-3.0-or-later"
  :version "0.1.0"
  :serial t
  :components ((:file "package")
               (:file "storage")
               (:file "machine")
               (:file "semantics")))

(asdf:defsystem #:lasm/test
  :description "Tests for lasm"
  :author "George Watson <gigolo@hotmail.co.uk>"
  :license "GPL-3.0-or-later"
  :depends-on (#:lasm #:fiveam)
  :serial t
  :components ((:file "tests")))
