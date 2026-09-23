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
    (fiveam:is (= 0 (flag m 'z)))
    (setf (flag m 'z) 0)
    (fiveam:is (= 0 (flag m 'z)))
    (setf (flag m 'z) 1)
    (fiveam:is (= 1 (flag m 'z)))
    (setf (flag m 'z) -2)
    (fiveam:is (= 1 (flag m 'z)))))

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

(fiveam:test stack-pointer-reads-and-moves-stack-depth
  (let ((m (make-machine 'test-machine)))
    (stack-push m 's 10)
    (stack-push m 's 20)
    (fiveam:is (= 2 (stack-pointer m 's)))
    (setf (stack-pointer m 's) 1)
    (fiveam:is (= 1 (stack-depth m 's)))
    (fiveam:is (= 10 (stack-ref m 's 0)))
    (setf (stack-pointer m 's) 2)
    (fiveam:is (= 20 (stack-pop m 's)))
    (fiveam:is (= 10 (stack-pop m 's)))))

(fiveam:test stack-pointer-validates-range-without-changing-state
  (let ((m (make-machine 'test-machine)))
    (stack-push m 's 7)
    (handler-case (setf (stack-pointer m 's) 5)
      (stack-pointer-out-of-range (condition)
        (fiveam:is (eq 's (storage-error-name condition)))
        (fiveam:is (= 5 (stack-pointer-out-of-range-value condition)))))
    (dolist (value '(-1 5 1.5 nil))
      (fiveam:signals stack-pointer-out-of-range
        (setf (stack-pointer m 's) value))
      (fiveam:is (= 1 (stack-pointer m 's))))
    (setf (stack-pointer m 's) 4)
    (fiveam:signals stack-overflow (stack-push m 's 8))
    (fiveam:is (= 4 (stack-pointer m 's)))))

;;; #166: SP-PUSH/SP-POP -- register-indexed push/pop for a (stack-pointer
;;; ...) clause. SP is deliberately wider than RAM's :addr-width, to exercise
;;; the addr-width masking SP-PUSH/SP-POP apply when indexing.

(defmachine sp-storage-test-machine
  (register sp :width 32)
  (memory ram :width 8 :addr-width 8)
  (stack-pointer sp :memory ram :grows :down))

(fiveam:test sp-push-pop-down-pre-decrements-then-post-increments
  (let ((m (make-machine 'sp-storage-test-machine)))
    (setf (sref m 'sp) 10)
    (sp-push m 'sp 'ram :down 1)
    (fiveam:is (= 9 (sref m 'sp)))
    (fiveam:is (= 1 (mref m 'ram 9)))
    (sp-push m 'sp 'ram :down 2)
    (fiveam:is (= 8 (sref m 'sp)))
    (fiveam:is (= 2 (sp-pop m 'sp 'ram :down)))
    (fiveam:is (= 9 (sref m 'sp)))
    (fiveam:is (= 1 (sp-pop m 'sp 'ram :down)))
    (fiveam:is (= 10 (sref m 'sp)))))

(fiveam:test sp-push-pop-up-stores-then-post-increments
  (let ((m (make-machine 'sp-storage-test-machine)))
    (setf (sref m 'sp) 10)
    (sp-push m 'sp 'ram :up 1)
    (fiveam:is (= 11 (sref m 'sp)))
    (fiveam:is (= 1 (mref m 'ram 10)))
    (fiveam:is (= 1 (sp-pop m 'sp 'ram :up)))
    (fiveam:is (= 10 (sref m 'sp)))))

(fiveam:test sp-push-wraps-at-zero-when-growing-down
  ;; SP itself is a plain 32-bit register, so decrementing past 0 wraps to
  ;; its own width (SETF SREF's WRAP-VALUE), not to RAM's narrower
  ;; :addr-width -- the write still lands correctly since the indexed
  ;; address is separately masked to :addr-width by SP-PUSH.
  (let ((m (make-machine 'sp-storage-test-machine)))
    (setf (sref m 'sp) 0)
    (sp-push m 'sp 'ram :down 7)
    (fiveam:is (= #xffffffff (sref m 'sp)))
    (fiveam:is (= 7 (mref m 'ram #xff)))))

(fiveam:test sp-push-masks-the-indexed-address-to-addr-width
  ;; SP (32-bit) holds 256 -- one past RAM's 8-bit address space (0-255) --
  ;; so the write must land at RAM[0], not signal ADDRESS-OUT-OF-RANGE.
  (let ((m (make-machine 'sp-storage-test-machine)))
    (setf (sref m 'sp) 257)
    (sp-push m 'sp 'ram :up 9)
    (fiveam:is (= 9 (mref m 'ram 1)))))

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

;; #110: IDLE state -- machine state like INTERRUPT-QUEUE, so RESET clears
;; it unconditionally too.
(fiveam:test reset-clears-idle
  (let ((m (make-machine 'test-machine)))
    (setf (machine-idle m) t)
    (reset m)
    (fiveam:is (not (machine-idle-p m)))))

;;; #107: region-mapped memory (ROM/RAM/MMIO). REGION-TEST-MACHINE dedicates
;;; a distinct address range to each region kind so the tests below can
;;; exercise them independently: 0-15 plain (no region), 16-31 :RAM (named
;;; but behaviorally identical to plain), 32-47 :ROM (:ON-WRITE :IGNORE, the
;;; default), 48-63 :ROM :ON-WRITE :ERROR, 64-79 :DEVICE with both handlers,
;;; 80-95 :DEVICE with neither.

(defvar *device-log* nil
  "Addresses/values seen by REGION-TEST-MACHINE's device handlers below --
reset at the start of each test that reads it.")

(defun %region-test-device-read (machine address)
  (declare (ignore machine))
  (cl:push (list :read address) *device-log*)
  #xAA)

(defun %region-test-device-write (machine address value)
  (declare (ignore machine))
  (cl:push (list :write address value) *device-log*))

(defmachine region-test-machine
  (register pc :width 8)
  (memory ram :width 8 :addr-width 8
    (region plain-rom #x20 #x2F :kind :rom)
    (region strict-rom #x30 #x3F :kind :rom :on-write :error)
    (region io #x40 #x4F :kind :device
            :read %region-test-device-read :write %region-test-device-write)
    (region blind-io #x50 #x5F :kind :device)))

(fiveam:test region-ram-behaves-as-plain-memory
  (let ((m (make-machine 'region-test-machine)))
    (setf (mref m 'ram 0) 7)   ; no region at all
    (fiveam:is (= 7 (mref m 'ram 0)))))

(fiveam:test region-rom-ignores-writes
  (let ((m (make-machine 'region-test-machine)))
    (%poke m 'ram #x20 42) ; burn in a value directly, bypassing region policy
    (setf (mref m 'ram #x20) 99) ; dropped
    (fiveam:is (= 42 (mref m 'ram #x20)))))

(fiveam:test region-rom-on-write-error
  (let ((m (make-machine 'region-test-machine)))
    (fiveam:signals memory-write-protected (setf (mref m 'ram #x30) 1))))

(fiveam:test region-device-routes-reads-and-writes
  (let ((*device-log* nil)
        (m (make-machine 'region-test-machine)))
    (fiveam:is (= #xAA (mref m 'ram #x40)))
    (setf (mref m 'ram #x41) 5)
    (fiveam:is (equal (list (list :write #x41 5) (list :read #x40)) *device-log*))
    ;; backing storage is never touched by a device region -- MPEEK bypasses
    ;; the region entirely and reads the raw (never-written) cell
    (fiveam:is (= 0 (mpeek m 'ram #x41)))))

(fiveam:test region-device-write-value-wraps-to-cell-width
  ;; A :DEVICE region's :WRITE receives the same cell-width-wrapped value
  ;; every other memory write receives, not the raw unwrapped argument.
  (let ((*device-log* nil)
        (m (make-machine 'region-test-machine)))
    (setf (mref m 'ram #x41) 300) ; wraps mod 256, same as plain memory
    (fiveam:is (equal (list (list :write #x41 44)) *device-log*))))

(fiveam:test region-device-without-handlers
  (let ((m (make-machine 'region-test-machine)))
    (fiveam:is (= 0 (mref m 'ram #x50)))
    (setf (mref m 'ram #x50) 99) ; discarded, no error
    (fiveam:is (= 0 (mref m 'ram #x50)))))

(fiveam:test mpeek-bypasses-device-handlers
  (let ((*device-log* nil)
        (m (make-machine 'region-test-machine)))
    (fiveam:is (= 0 (mpeek m 'ram #x40)))
    (fiveam:is (null *device-log*))))

(fiveam:test poke-writes-through-rom
  (let ((m (make-machine 'region-test-machine)))
    (%poke m 'ram #x30 7) ; strict-rom, :on-write :error -- %POKE bypasses it
    (fiveam:is (= 7 (mref m 'ram #x30)))))

(fiveam:test load-program-burns-into-rom
  ;; Regression for the #107/LOAD-PROGRAM collision: a program assembled at
  ;; a ROM region's origin must still load, since LOAD-PROGRAM burns cells in
  ;; via %POKE rather than storing them via (SETF MREF).
  (let ((m (make-machine 'region-test-machine)))
    (load-program m (vector 1 2 3) :origin #x20)
    (fiveam:is (= 1 (mref m 'ram #x20)))
    (fiveam:is (= 2 (mref m 'ram #x21)))
    (fiveam:is (= 3 (mref m 'ram #x22)))))

(fiveam:test reset-zeroes-regioned-memory
  (let ((m (make-machine 'region-test-machine)))
    (%poke m 'ram #x20 42)
    (reset m)
    (fiveam:is (= 0 (mref m 'ram #x20)))))

(fiveam:test defmachine-rejects-overlapping-regions
  (fiveam:signals error
    (eval '(defmachine overlap-region-test
            (memory ram :width 8 :addr-width 8
              (region a 0 15 :kind :rom)
              (region b 10 20 :kind :rom))))))

(fiveam:test defmachine-rejects-duplicate-region-name
  (fiveam:signals error
    (eval '(defmachine dup-region-test
            (memory ram :width 8 :addr-width 8
              (region a 0 15 :kind :rom)
              (region a 16 31 :kind :rom))))))

(fiveam:test defmachine-rejects-region-name-colliding-with-element
  (fiveam:signals error
    (eval '(defmachine region-element-collision-test
            (memory ram :width 8 :addr-width 8
              (region ram 0 15 :kind :rom))))))

(fiveam:test defmachine-rejects-region-out-of-address-range
  (fiveam:signals error
    (eval '(defmachine region-range-test
            (memory ram :width 8 :addr-width 8
              (region a 0 256 :kind :rom))))))

(fiveam:test defmachine-rejects-inverted-region-range
  (fiveam:signals error
    (eval '(defmachine inverted-region-test
            (memory ram :width 8 :addr-width 8
              (region a 15 0 :kind :rom))))))

(fiveam:test defmachine-rejects-unknown-region-kind
  (fiveam:signals error
    (eval '(defmachine bad-kind-region-test
            (memory ram :width 8 :addr-width 8
              (region a 0 15 :kind :bogus))))))

(fiveam:test defmachine-rejects-on-write-on-non-rom
  (fiveam:signals error
    (eval '(defmachine bad-on-write-region-test
            (memory ram :width 8 :addr-width 8
              (region a 0 15 :kind :ram :on-write :error))))))

(fiveam:test defmachine-rejects-read-write-on-non-device
  (fiveam:signals error
    (eval '(defmachine bad-read-region-test
            (memory ram :width 8 :addr-width 8
              (region a 0 15 :kind :rom :read (lambda (m a) (declare (ignore m a)) 0)))))))

(defun %region-test-wide-read (machine address)
  (declare (ignore machine address))
  #x1FF)

(defmachine region-read-mask-machine
  (register pc :width 8)
  (memory ram :width 8 :addr-width 4
    (region io 0 3 :kind :device :read %region-test-wide-read)))

(fiveam:test region-device-read-masks-to-cell-width
  (let ((m (make-machine 'region-read-mask-machine)))
    (fiveam:is (= #xFF (mref m 'ram 0)))))
