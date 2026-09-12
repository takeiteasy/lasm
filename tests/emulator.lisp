;;;; tests/emulator.lisp
;;;; fiveam tests for the M1 emulator loop (emulator.lisp), plus the M1
;;;; milestone's end-to-end counter-loop regression test.

(in-package #:lasm)

(fiveam:def-suite emulator :in lasm)
(fiveam:in-suite emulator)

;; A dedicated fixture with an HLT instruction (via TRAP) so RUN has a clean
;; way to stop -- INSTR-TEST-MACHINE (tests/instruction.lisp) has no such
;; instruction.
(defmachine emu-test-machine
  (register x :width 8)
  (register pc :width 16)
  (memory ram :width 8 :addr-width 16)
  (flags z))

(definstruction emu-test-machine ldx
  (modes immediate)
  (encoding (opcode #xA2) (operand :mode))
  (semantics (set! x operand) (set-flags! (z (zero? x)))))

(definstruction emu-test-machine dex
  (encoding (opcode #xCA))
  (semantics (set! x (wrap-value (1- x) 8)) (set-flags! (z (zero? x)))))

(definstruction emu-test-machine bne
  (modes absolute)
  (encoding (opcode #xD0) (operand :mode))
  (semantics (when (zerop z) (set! pc operand))))

(definstruction emu-test-machine sta
  (modes absolute)
  (encoding (opcode #x8D) (operand :mode))
  (semantics (setf (mref machine 'ram operand) x)))

(definstruction emu-test-machine hlt
  (encoding (opcode #x00))
  (semantics (trap :halt)))

;;; load-program

(fiveam:test load-program-places-bytes-and-sets-pc
  (let ((m (make-machine 'emu-test-machine))
        (a (assemble "ldx #10" :machine 'emu-test-machine :origin #x100)))
    (load-program m a)
    (fiveam:is (= #xA2 (mref m 'ram #x100)))
    (fiveam:is (= 10 (mref m 'ram #x101)))
    (fiveam:is (= #x100 (sref m 'pc)))))

(fiveam:test load-program-accepts-raw-byte-sequence
  (let ((m (make-machine 'emu-test-machine)))
    (load-program m (list #xEA) :origin #x10)
    (fiveam:is (= #xEA (mref m 'ram #x10)))
    (fiveam:is (= #x10 (sref m 'pc)))))

;;; step-machine

(fiveam:test step-machine-advances-pc-and-executes
  (let ((m (make-machine 'emu-test-machine))
        (a (assemble "ldx #10" :machine 'emu-test-machine)))
    (load-program m a)
    (let ((descriptor (step-machine m)))
      (fiveam:is (eq (find-instruction 'emu-test-machine 'ldx) descriptor))
      (fiveam:is (= 2 (sref m 'pc)))
      (fiveam:is (= 10 (sref m 'x))))))

(fiveam:test step-machine-branch-overrides-pc-increment
  (let ((m (make-machine 'emu-test-machine))
        ;; "target" (address 6) is well past bne's own post-fetch increment
        ;; (address 3) -- if step-machine's increment ran after semantics
        ;; instead of before, this would observe pc=3, not 6.
        (a (assemble "bne target
sta $2000
target: hlt" :machine 'emu-test-machine)))
    (load-program m a)
    ;; z starts at 0 (fresh machine), so bne's (zerop z) condition holds and
    ;; the branch to "target" is taken -- its semantics' (set! pc operand)
    ;; must win over step-machine's own post-fetch increment.
    (step-machine m)
    (fiveam:is (= 6 (sref m 'pc)))))

(fiveam:test step-machine-branch-not-taken-falls-through
  (let ((m (make-machine 'emu-test-machine))
        (a (assemble "sta $2000
bne skip
hlt
skip: hlt" :machine 'emu-test-machine)))
    (load-program m a)
    (setf (flag m 'z) t)  ; z set -> bne's branch condition (zerop z) is false
    (step-machine m)      ; sta, address 0 -> 3
    (step-machine m)      ; bne, not taken -> falls through to address 6
    (fiveam:is (= 6 (sref m 'pc)))))

(fiveam:test step-machine-decode-failure-on-unknown-opcode
  (let ((m (make-machine 'emu-test-machine)))
    (load-program m (list #xFF))
    (fiveam:is (eq :decode-failure (step-machine m)))))

;;; run

(fiveam:test run-stops-on-trap
  (let ((m (make-machine 'emu-test-machine))
        (a (assemble "ldx #5
hlt" :machine 'emu-test-machine)))
    (load-program m a)
    (multiple-value-bind (reason steps condition) (run m)
      (fiveam:is (eq :trap reason))
      (fiveam:is (= 2 steps))
      (fiveam:is (eq :halt (lasm-trap-tag condition)))
      (fiveam:is (= 5 (sref m 'x))))))

(fiveam:test run-stops-on-decode-failure
  (let ((m (make-machine 'emu-test-machine)))
    (load-program m (list #xFF))
    (multiple-value-bind (reason steps) (run m)
      (fiveam:is (eq :decode-failure reason))
      (fiveam:is (= 0 steps)))))

(fiveam:test run-stops-on-max-steps
  (let ((m (make-machine 'emu-test-machine))
        (a (assemble "loop: dex
bne loop" :machine 'emu-test-machine)))
    (load-program m a)
    (multiple-value-bind (reason steps) (run m :max-steps 4)
      (fiveam:is (eq :max-steps reason))
      (fiveam:is (= 4 steps)))))

;;; M1 milestone target: end-to-end counter loop, assembled and run

(fiveam:test counter-loop-end-to-end
  (let* ((m (make-machine 'emu-test-machine))
         (a (assemble "        ldx #10
loop:   dex
        bne loop
        sta $1000
        hlt" :machine 'emu-test-machine)))
    (load-program m a)
    (multiple-value-bind (reason steps) (run m)
      (fiveam:is (eq :trap reason))
      ;; 1 ldx + 10 dex + 10 bne + 1 sta + 1 hlt -- proves the loop actually
      ;; looped ten times rather than falling through.
      (fiveam:is (= 23 steps))
      (fiveam:is (= 0 (sref m 'x)))
      (fiveam:is (= 0 (mref m 'ram #x1000))))))
