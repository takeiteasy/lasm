;;;; lasm.asd
;;;; System definition for lasm

(asdf:defsystem #:lasm
  :description "A Lisp library and DSL for building fantasy assemblers and CPU emulators"
  :author "George Watson <gigolo@hotmail.co.uk>"
  :license "GPL-3.0-or-later"
  :version "0.1.0"
  ;; #75: lasm's first dependency -- RUN-FOR-DURATION's :THROTTLE T path
  ;; (emulator.lisp) needs a monotonic clock finer than CL:GET-INTERNAL-
  ;; REAL-TIME's portable-but-coarse resolution to pace cycle-accurate
  ;; execution against a declared CLOCK-SPEED. Resolves via
  ;; ~/quicklisp/local-projects, not the Quicklisp dist -- see
  ;; docs/getting-started.md.
  :depends-on (#:trivial-high-precision-timer)
  :serial t
  :components ((:file "package")
               (:file "storage")
               (:file "diagnostic")
               (:file "machine")
               (:file "device")
               (:file "interrupt")
               (:file "semantics")
               (:file "lexer")
               (:file "parser")
               (:file "mode")
               (:file "instruction")
               (:file "decoder")
               (:file "directive")
               (:file "include")
               (:file "macro")
               (:file "assembler")
               (:file "emulator")
               (:file "disassembler")
               (:file "listing")
               (:file "debugger")))

(asdf:defsystem #:lasm/test
  :description "Tests for lasm"
  :author "George Watson <gigolo@hotmail.co.uk>"
  :license "GPL-3.0-or-later"
  ;; #171: closer-mop walks condition-class-precedence-lists to check
  ;; every slot reader is exported -- test-only, #:lasm stays dependency-free.
  :depends-on (#:lasm #:fiveam #:closer-mop)
  :pathname "tests/"
  :serial t
  :components ((:file "suites")
               (:file "package")
               (:file "storage")
               (:file "device")
               (:file "interrupt")
               (:file "semantics")
               (:file "lexer")
               (:file "parser")
               (:file "mode")
               (:file "instruction")
               (:file "directive")
               (:file "include")
               (:file "macro")
               (:file "assembler")
               (:file "diagnostic")
               (:file "emulator")
               (:file "disassembler")
               (:file "listing")
               (:file "debugger")))
