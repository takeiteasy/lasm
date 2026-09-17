;;;; tests/storage.lisp
;;;; fiveam tests for the M0 storage model (storage.lisp, machine.lisp).

(in-package #:lasm)

(fiveam:def-suite storage :in lasm)
(fiveam:in-suite storage)

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

;;; STACK-REF / (SETF STACK-REF) (#50) -- top-relative, unsigned indexed
;;; access: offset 0 is the top (what STACK-POP would return), 1 is one
;;; below that, and so on.

(fiveam:test stack-ref-reads-top-relative
  (let ((m (make-machine 'test-machine)))
    (stack-push m 's 10)
    (stack-push m 's 20)
    (stack-push m 's 30)
    (fiveam:is (= 30 (stack-ref m 's 0)))
    (fiveam:is (= 20 (stack-ref m 's 1)))
    (fiveam:is (= 10 (stack-ref m 's 2)))))

(fiveam:test stack-ref-does-not-disturb-depth
  (let ((m (make-machine 'test-machine)))
    (stack-push m 's 1)
    (stack-push m 's 2)
    (stack-ref m 's 0)
    (fiveam:is (= 2 (stack-depth m 's)))
    (fiveam:is (= 2 (stack-pop m 's)))))

(fiveam:test stack-ref-setf-writes-and-wraps
  (let ((m (make-machine 'test-machine)))
    (stack-push m 's 1)
    (stack-push m 's 2)
    (setf (stack-ref m 's 1) 300) ; wraps mod 256, same width as PUSH
    (fiveam:is (= 44 (stack-ref m 's 1)))
    (fiveam:is (= 2 (stack-pop m 's)))
    (fiveam:is (= 44 (stack-pop m 's)))))

(fiveam:test stack-ref-out-of-range-signalled
  (let ((m (make-machine 'test-machine)))
    (stack-push m 's 1)
    (fiveam:signals stack-index-out-of-range (stack-ref m 's 1))
    (fiveam:signals stack-index-out-of-range (stack-ref m 's -1))
    (fiveam:signals stack-index-out-of-range (setf (stack-ref m 's 1) 99))))

(fiveam:test stack-ref-out-of-range-on-empty-stack-signalled
  (let ((m (make-machine 'test-machine)))
    (fiveam:signals stack-index-out-of-range (stack-ref m 's 0))))

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

;;; REGREF / (SETF REGREF) (#13) -- indexed access into a banked (:count >
;;; 1) register. TEST-MACHINE's BANK element is :width 8 :count 4.

(fiveam:test regref-independent-cells
  (let ((m (make-machine 'test-machine)))
    (setf (regref m 'bank 0) 10)
    (setf (regref m 'bank 1) 20)
    (fiveam:is (= 10 (regref m 'bank 0)))
    (fiveam:is (= 20 (regref m 'bank 1)))
    (fiveam:is (= 0 (regref m 'bank 2)))))

(fiveam:test regref-width-masking
  (let ((m (make-machine 'test-machine)))
    (setf (regref m 'bank 0) 300) ; wraps mod 256, same width as SREF
    (fiveam:is (= 44 (regref m 'bank 0)))))

(fiveam:test regref-index-out-of-range-signalled
  (let ((m (make-machine 'test-machine)))
    (fiveam:signals register-index-out-of-range (regref m 'bank 4))
    (fiveam:signals register-index-out-of-range (regref m 'bank -1))
    (fiveam:signals register-index-out-of-range (setf (regref m 'bank 4) 1))))

(fiveam:test regref-on-scalar-register-treats-it-as-a-one-element-bank
  ;; REGREF is not restricted to :count > 1 elements -- a scalar register is
  ;; a valid size-1 bank, so index 0 works and index 1 is out of range.
  (let ((m (make-machine 'test-machine)))
    (setf (sref m 'a) 9)
    (fiveam:is (= 9 (regref m 'a 0)))
    (fiveam:signals register-index-out-of-range (regref m 'a 1))))

(fiveam:test sref-on-banked-register-signalled
  ;; SREF is the scalar accessor -- a banked register has no single cell 0
  ;; answer, so it must error rather than silently alias every index.
  (let ((m (make-machine 'test-machine)))
    (fiveam:signals unknown-storage (sref m 'bank))
    (fiveam:signals unknown-storage (setf (sref m 'bank) 1))))

(fiveam:test reset-zeroes-banked-register
  (let ((m (make-machine 'test-machine)))
    (setf (regref m 'bank 0) 1)
    (setf (regref m 'bank 3) 2)
    (reset m)
    (fiveam:is (= 0 (regref m 'bank 0)))
    (fiveam:is (= 0 (regref m 'bank 3)))))

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

;;; #72: :names on a banked register clause.

(fiveam:test defmachine-names-derives-count
  (eval '(defmachine names-derive-count-test
          (register v :width 8 :names (v0 v1 v2))))
  (let ((element (descriptor-element (find-machine-descriptor 'names-derive-count-test) 'v)))
    (fiveam:is (= 3 (storage-element-count element)))
    (fiveam:is (equal '(v0 v1 v2) (storage-element-names element)))))

(fiveam:test defmachine-rejects-names-count-mismatch
  (fiveam:signals error
    (eval '(defmachine bad-names-count-test
            (register v :width 8 :count 4 :names (v0 v1 v2))))))

(fiveam:test defmachine-rejects-duplicate-alias-within-clause
  (fiveam:signals error
    (eval '(defmachine dup-alias-test
            (register v :width 8 :names (v0 v1 v0))))))

(fiveam:test defmachine-rejects-alias-colliding-with-element-name
  (fiveam:signals error
    (eval '(defmachine alias-element-collision-test
            (register a :width 8)
            (register v :width 8 :names (v0 a))))))

(fiveam:test defmachine-register-aliases-table-populated
  (eval '(defmachine names-table-test
          (register reg :width 16 :names (a b c))))
  (let ((descriptor (find-machine-descriptor 'names-table-test)))
    (fiveam:is (= 0 (gethash "A" (machine-descriptor-register-aliases descriptor))))
    (fiveam:is (= 1 (gethash "b" (machine-descriptor-register-aliases descriptor))))
    (fiveam:is (= 2 (gethash "C" (machine-descriptor-register-aliases descriptor))))))

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
