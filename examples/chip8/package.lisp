;;;; package.lisp

;;; A package that uses #:lasm can define machines and backends directly.
;;; LASM's PUSH and POP are stack operators, so they are imported over the CL
;;; functions. See docs/machine-model.md#your-own-package.
(defpackage #:chip8
  (:use #:cl #:lasm)
  (:shadowing-import-from #:lasm #:push #:pop)
  (:export #:load-chip8 #:run-chip8 #:key-down #:key-up
           #:v-reg #:i-reg #:chip8-pc #:delay-timer #:sound-timer
           #:memory-byte #:pixel #:display-rows))
