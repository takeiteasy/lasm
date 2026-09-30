;;;; package.lisp

;;; The system is named "6502", which is not a symbol, so the package is mos6502.
;;; A package that uses #:lasm can define machines and backends directly. LASM's
;;; PUSH and POP are stack operators, so they are imported over the CL functions.
;;; See docs/machine-model.md#your-own-package.
(defpackage #:mos6502
  (:use #:cl #:lasm)
  (:shadowing-import-from #:lasm #:push #:pop)
  (:export #:mos6502 #:mos6502-lang #:*demo* #:assemble-6502 #:load-6502 #:reset-6502 #:run-6502
           #:reg #:status #:flag-set-p #:ram #:word-at #:demo-symbol))
