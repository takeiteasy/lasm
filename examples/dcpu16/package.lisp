;;;; package.lisp

;;; A package that uses #:lasm can define machines directly: clause names such
;;; as REGISTER and ENCODING need no import. LASM's PUSH and POP are stack
;;; operators, so they are imported over the CL functions.
;;; See docs/machine-model.md#your-own-package.
(defpackage #:dcpu16
  (:use #:cl #:lasm)
  (:shadowing-import-from #:lasm #:push #:pop)
  (:export #:dcpu16 #:reg #:ram #:sp #:ex #:ia
           #:load-dcpu16 #:run-dcpu16 #:reg-value #:key-down #:key-up))
