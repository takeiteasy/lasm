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

(fiveam:test set-flags-accepts-integer-results
  (with-machine (m test-machine)
    (set-flags! (z 1) (n 0))
    (fiveam:is (= 1 (flag m 'z)))
    (fiveam:is (= 0 (flag m 'n)))))

(fiveam:test trap-signals-lasm-trap
  (with-machine (m test-machine)
    (fiveam:is (eq 'test-machine (machine-descriptor-name (machine-descriptor m))))
    (fiveam:signals lasm-trap (trap :illegal-opcode))))

;; #13: banked registers (:count > 1) bind as a MACROLET taking a run-time
;; index, e.g. (bank i), rather than a plain symbol-macro -- TEST-MACHINE's
;; BANK element is :width 8 :count 4.

(fiveam:test with-machine-banked-register-indexed-read-write
  (with-machine (m test-machine)
    (let ((i 2))
      (set! (bank i) 7)
      (fiveam:is (= 7 (bank i)))
      (fiveam:is (= 7 (bank 2))))
    (fiveam:is (= 0 (bank 0)))))

(fiveam:test with-machine-banked-register-cells-are-independent
  (with-machine (m test-machine)
    (set! (bank 0) 1)
    (set! (bank 1) 2)
    (fiveam:is (= 1 (bank 0)))
    (fiveam:is (= 2 (bank 1)))))

;; #72: BANK's :names (bank0 bank1 bank2 bank3) each bind as an ordinary
;; symbol-macro with their bank index baked in -- an alternative to (bank
;; idx) for a compile-time-known index, reading and writing the same cells.

(fiveam:test with-machine-register-alias-reads-and-writes-its-bank-cell
  (with-machine (m test-machine)
    (set! bank1 7)
    (fiveam:is (= 7 bank1))
    (fiveam:is (= 7 (bank 1)))
    (fiveam:is (= 0 bank0))))

(fiveam:test with-machine-register-alias-and-indexed-form-share-state
  (with-machine (m test-machine)
    (set! (bank 2) 5)
    (fiveam:is (= 5 bank2))
    (set! bank2 (+ bank2 1))
    (fiveam:is (= 6 (bank 2)))))

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

(fiveam:test fixed-stack-accessors-use-the-sole-stack
  (with-machine (m test-machine)
    (push 11)
    (push 22)
    (fiveam:is (= 2 (stack-depth)))
    (fiveam:is (= 22 (stack-ref 0)))
    (setf (stack-ref 1) 33)
    (fiveam:is (= 33 (stack-ref 1)))
    (setf (stack-pointer) 1)
    (fiveam:is (= 1 (stack-depth)))
    (setf (stack-pointer) 2)
    (fiveam:is (= 22 (pop)))
    (fiveam:is (= 33 (pop)))))

;; Two-stack and no-stack machines keep PUSH/POP's stack name mandatory --
;; the ambiguity is caught at macroexpansion time, so it must be provoked
;; through EVAL of a quoted form rather than a literal form in this file
;; (which would fail to compile the test file itself).

(defmachine two-stack-test-machine
  (stack s1 :width 8 :depth 4)
  (stack s2 :width 8 :depth 4))

(defmachine no-stack-test-machine
  (register a :width 8))

(fiveam:test fixed-stack-accessors-accept-explicit-names
  (with-machine (m two-stack-test-machine)
    (push 4 s1)
    (push 7 s2)
    (fiveam:is (= 1 (stack-depth s1)))
    (fiveam:is (= 7 (stack-ref 0 s2)))
    (setf (stack-ref 0 s1) 9)
    (setf (stack-pointer s2) 0)
    (fiveam:is (= 0 (stack-depth s2)))
    (fiveam:is (= 9 (pop s1)))))

(fiveam:test fixed-stack-accessors-require-a-default-or-explicit-name
  (dolist (machine '(two-stack-test-machine no-stack-test-machine pointer-only-test-machine))
    (dolist (form '((stack-depth) (stack-ref 0) (stack-pointer)
                    (setf (stack-ref 0) 1) (setf (stack-pointer) 1)))
      (fiveam:signals error
        (eval `(with-machine (m ,machine) ,form))))))

(fiveam:test fixed-stack-accessors-reject-a-pointer-register
  (with-machine (m pointer-only-test-machine)
    (fiveam:signals unknown-storage (stack-depth sp))
    (fiveam:signals unknown-storage (stack-pointer sp))
    (fiveam:signals unknown-storage (stack-ref 0 sp))))

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

;;; #166: PUSH/POP against a (stack-pointer ...)-bound register -- works with
;;; no (interrupts ...) clause at all, unlike the interrupt-delivery-only
;;; binding an earlier design considered (see #166's plan).

(defmachine pointer-only-test-machine
  (register sp :width 16)
  (memory ram :width 16 :addr-width 16 :cell-width 16)
  (stack-pointer sp :memory ram :grows :down))

(fiveam:test push-pop-on-a-stack-pointer-register-with-no-interrupts-clause
  (with-machine (m pointer-only-test-machine)
    (push 11 sp)
    (push 22 sp)
    (fiveam:is (= 22 (pop sp)))
    (fiveam:is (= 11 (pop sp)))))

;; No :stack element declared -- the sole stack-pointer becomes PUSH/POP's
;; bare default, same as the sole :stack element does above.
(fiveam:test push-pop-default-to-the-sole-stack-pointer-when-no-stack-element
  (with-machine (m pointer-only-test-machine)
    (push 5)
    (fiveam:is (= 5 (pop)))))

;; A machine declaring BOTH a :stack element and a stack-pointer -- the
;; :stack element still wins the bare default, unchanged from before #166.
(defmachine stack-and-pointer-test-machine
  (register sp :width 8)
  (stack s :width 8 :depth 4)
  (memory ram :width 8 :addr-width 8)
  (stack-pointer sp :memory ram))

(fiveam:test bare-push-pop-still-defaults-to-the-stack-element-over-a-pointer
  (with-machine (m stack-and-pointer-test-machine)
    (push 9)                            ; no name -- must hit S, not SP
    (fiveam:is (= 1 (stack-depth)))
    (fiveam:is (zerop (sref m 'sp)))
    (fiveam:is (= 9 (pop)))))

(fiveam:test push-pop-name-the-pointer-explicitly-on-a-mixed-machine
  (with-machine (m stack-and-pointer-test-machine)
    (push 9 sp)
    (fiveam:is (zerop (stack-depth)))
    (fiveam:is (/= 0 (sref m 'sp)))
    (fiveam:is (= 9 (pop sp)))))
