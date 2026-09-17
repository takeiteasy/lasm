;;;; tests/suites.lisp
;;;; Root fiveam suite and the shared TEST-MACHINE fixture used across the
;;;; other test files in this directory (storage.lisp, semantics.lisp).

(in-package #:lasm)

(fiveam:def-suite lasm)

;; Defined here, then used both by storage.lisp's tests (in this same file's
;; suite tree) and by a WITH-MACHINE form in semantics.lisp -- this is the
;; compile-time check that DEFMACHINE's descriptor is available at
;; macroexpansion time, not only after loading (see the EVAL-WHEN in
;; machine.lisp).
(defmachine test-machine
  (register a :width 8)
  (register wide :width 16)
  (register bank :width 8 :names (bank0 bank1 bank2 bank3)) ; #72: aliased banked register
  (stack s :width 8 :depth 4)
  (memory ram :width 8 :addr-width 8)
  (memory wram :width 8 :addr-width 4 :cell-width 16)
  (flags z n c v))
