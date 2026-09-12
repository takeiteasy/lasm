;;;; tests/semantics.lisp
;;;; fiveam tests for the semantics vocabulary (semantics.lisp).

(in-package #:lasm)

(fiveam:def-suite semantics :in lasm)
(fiveam:in-suite semantics)

(fiveam:test with-machine-symbol-resolution-and-vocabulary
  (with-machine (m test-machine)
    (set! a 10)
    (fiveam:is (= 10 a))
    (set! a (+ a 5))
    (fiveam:is (= 15 a))
    (push a s)
    (fiveam:is (= 15 (pop s)))
    (set-flags! (z (zero? a)) (n (bit-set? a 3)))
    (fiveam:is (= 0 (flag m 'z)))
    (fiveam:is (= 1 (flag m 'n)))))

(fiveam:test trap-signals-lasm-trap
  (with-machine (m test-machine)
    (fiveam:is (eq 'test-machine (machine-descriptor-name (machine-descriptor m))))
    (fiveam:signals lasm-trap (trap :illegal-opcode))))
