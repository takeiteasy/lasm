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

;; #57: TEST-MACHINE declares exactly one stack (S), so PUSH/POP may omit the
;; stack name and resolve to it -- mirroring emulator.lisp's %RESOLVE-MEMORY
;; convention for the sole :memory element. This makes the design draft's own
;; mockup (LASM-plan.md sec. 3.3), (push (+ (pop) (pop))), compile and run as
;; written for any single-stack machine.

(fiveam:test push-pop-default-to-the-sole-stack
  (with-machine (m test-machine)
    (push 3 s)
    (push 4)                           ; explicit and implicit forms mix freely
    (fiveam:is (= 4 (pop)))
    (fiveam:is (= 3 (pop s)))))

(fiveam:test push-pop-implicit-form-matches-design-draft
  ;; The draft's own form, verbatim. Evaluation order is left-to-right, so
  ;; the first (pop) is the top of stack -- irrelevant for +, load-bearing
  ;; for -/ /.
  (with-machine (m test-machine)
    (push 3 s)
    (push 4 s)
    (push (+ (pop) (pop)))
    (fiveam:is (= 7 (pop)))))

;; Two-stack and no-stack machines keep PUSH/POP's stack name mandatory --
;; the ambiguity is caught at macroexpansion time, so it must be provoked
;; through EVAL of a quoted form rather than a literal form in this file
;; (which would fail to compile the test file itself).

(defmachine two-stack-test-machine
  (stack s1 :width 8 :depth 4)
  (stack s2 :width 8 :depth 4))

(defmachine no-stack-test-machine
  (register a :width 8))

(fiveam:test push-with-no-stack-name-errors-on-multiple-stacks
  (fiveam:signals error
    (eval '(with-machine (m two-stack-test-machine) (push 1)))))

(fiveam:test pop-with-no-stack-name-errors-on-multiple-stacks
  (fiveam:signals error
    (eval '(with-machine (m two-stack-test-machine) (pop)))))

(fiveam:test push-with-no-stack-name-errors-on-no-stack
  (fiveam:signals error
    (eval '(with-machine (m no-stack-test-machine) (push 1)))))

(fiveam:test pop-with-no-stack-name-errors-on-no-stack
  (fiveam:signals error
    (eval '(with-machine (m no-stack-test-machine) (pop)))))
