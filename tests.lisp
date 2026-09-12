;;;; tests.lisp
;;;; fiveam test suite for lasm (M0: storage model + semantics vocabulary).

(in-package #:lasm)

(fiveam:def-suite lasm)
(fiveam:in-suite lasm)

;; A test machine defined here, then used by tests below and by a
;; WITH-MACHINE form later in this same file -- this is the compile-time
;; check that DEFMACHINE's descriptor is available at macroexpansion time,
;; not only after loading (see the EVAL-WHEN in machine.lisp).
(defmachine test-machine
  (register a :width 8)
  (register wide :width 16)
  (stack s :width 8 :depth 4)
  (memory ram :width 8 :addr-width 8)
  (memory wram :width 8 :addr-width 4 :cell-width 16)
  (flags z n c v))

(fiveam:test register-read-write
  (let ((m (make-machine 'test-machine)))
    (setf (sref m 'a) 5)
    (fiveam:is (= 5 (sref m 'a)))))

(fiveam:test register-width-masking
  (let ((m (make-machine 'test-machine)))
    (setf (sref m 'a) 300) ; wraps mod 256
    (fiveam:is (= 44 (sref m 'a)))
    (setf (sref m 'a) -1) ; masks to unsigned
    (fiveam:is (= 255 (sref m 'a)))))

(fiveam:test signed-value-accessor
  (fiveam:is (= -1 (signed-value 255 8)))
  (fiveam:is (= 127 (signed-value 127 8)))
  (fiveam:is (= -128 (signed-value 128 8))))

(fiveam:test flags-set-and-clear
  (let ((m (make-machine 'test-machine)))
    (setf (flag m 'z) t)
    (fiveam:is (= 1 (flag m 'z)))
    (setf (flag m 'z) nil)
    (fiveam:is (= 0 (flag m 'z)))))

(fiveam:test stack-lifo
  (let ((m (make-machine 'test-machine)))
    (stack-push m 's 1)
    (stack-push m 's 2)
    (fiveam:is (= 2 (stack-depth m 's)))
    (fiveam:is (= 2 (stack-pop m 's)))
    (fiveam:is (= 1 (stack-pop m 's)))
    (fiveam:is (= 0 (stack-depth m 's)))))

(fiveam:test stack-overflow-signalled
  (let ((m (make-machine 'test-machine)))
    (dotimes (i 4) (stack-push m 's i))
    (fiveam:signals stack-overflow (stack-push m 's 99))))

(fiveam:test stack-underflow-signalled
  (let ((m (make-machine 'test-machine)))
    (fiveam:signals stack-underflow (stack-pop m 's))))

(fiveam:test memory-read-write
  (let ((m (make-machine 'test-machine)))
    (setf (mref m 'ram #x10) 7)
    (fiveam:is (= 7 (mref m 'ram #x10)))))

(fiveam:test memory-address-out-of-range
  (let ((m (make-machine 'test-machine)))
    (fiveam:signals address-out-of-range (mref m 'ram 256))
    (fiveam:signals address-out-of-range (setf (mref m 'ram 256) 1))))

(fiveam:test memory-non-default-cell-width
  (let ((m (make-machine 'test-machine)))
    (setf (mref m 'wram 0) 65535)
    (fiveam:is (= 65535 (mref m 'wram 0)))
    (setf (mref m 'wram 0) 70000) ; wraps mod 2^16
    (fiveam:is (= (mod 70000 65536) (mref m 'wram 0)))))

(fiveam:test unknown-storage-signalled
  (let ((m (make-machine 'test-machine)))
    (fiveam:signals unknown-storage (sref m 'nope))
    (fiveam:signals unknown-storage (mref m 'nope 0))
    (fiveam:signals unknown-storage (stack-push m 'nope 1))))

(fiveam:test defmachine-rejects-duplicate-names
  (fiveam:signals error
    (eval '(defmachine dup-test
            (register a :width 8)
            (register a :width 8)))))

(fiveam:test defmachine-rejects-unknown-clause
  (fiveam:signals error
    (eval '(defmachine bad-clause-test
            (bogus a :width 8)))))

(fiveam:test defmachine-rejects-non-positive-width
  (fiveam:signals error
    (eval '(defmachine bad-width-test
            (register a :width 0)))))

(fiveam:test reset-zeroes-all-storage
  (let ((m (make-machine 'test-machine)))
    (setf (sref m 'a) 9)
    (stack-push m 's 1)
    (setf (mref m 'ram 0) 3)
    (setf (flag m 'z) t)
    (reset m)
    (fiveam:is (= 0 (sref m 'a)))
    (fiveam:is (= 0 (stack-depth m 's)))
    (fiveam:is (= 0 (mref m 'ram 0)))
    (fiveam:is (= 0 (flag m 'z)))))

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
