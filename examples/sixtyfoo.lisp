;;;; examples/sixtyfoo.lisp
;;;;
;;;; The M0 target: a trivial register machine (SIXTYFOO) with no
;;;; instructions defined yet -- this
;;;; exercises the storage model and semantics vocabulary directly through
;;;; WITH-MACHINE, to prove the storage model holds together on its own
;;;; before M1 adds DEFINSTRUCTION/lexer/assembler/emulator on top of it.
;;;;
;;;; Run with:  sbcl --script examples/sixtyfoo.lisp
;;;; (or, from a REPL with lasm already loaded: (load "examples/sixtyfoo.lisp"))

(load (merge-pathnames "boot.lisp" *load-pathname*))

(in-package #:lasm)

(defmachine sixtyfoo
  (register a :width 8)
  (register x :width 8)
  (stack s :width 8 :depth 256)
  (memory ram :width 8 :addr-width 16)
  (flags z n c v))

(with-machine (m sixtyfoo)
  ;; Registers
  (set! a 200)
  (set! x 10)
  (format t "A = ~D, X = ~D~%" a x)

  ;; Width masking: 200 + 100 wraps mod 256
  (set! a (+ a 100))
  (set-flags! (c (> (+ 200 100) 255)) (z (zero? a)) (n (bit-set? a 7)))
  (format t "After A += 100 (wraps): A = ~D  flags C=~D Z=~D N=~D~%"
          a (flag m 'c) (flag m 'z) (flag m 'n))

  ;; Stack
  (push a s)
  (push x s)
  (format t "Stack depth after two pushes: ~D~%" (stack-depth s))
  (let ((top (pop s)))
    (format t "Popped ~D, stack depth now ~D~%" top (stack-depth s)))

  ;; Memory
  (setf (mref m #x1000) 42)
  (format t "RAM[#x1000] = ~D~%" (mref m #x1000))

  (format t "Final state: A=~D X=~D~%" a x))
