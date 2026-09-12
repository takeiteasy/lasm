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

;; RELATIVE mode (#23): same "branch if Z clear" condition as BNE above, just
;; RELATIVE instead of ABSOLUTE -- kept as a separate mnemonic so BNE's
;; existing ABSOLUTE-mode tests/byte expectations elsewhere in this file are
;; undisturbed.
(definstruction emu-test-machine bra
  (modes relative)
  (encoding (opcode #x90) (operand :mode))
  (semantics (when (zerop z) (set! pc (+ pc operand)))))

(definstruction emu-test-machine sta
  (modes absolute)
  (encoding (opcode #x8D) (operand :mode))
  (semantics (setf (mref machine 'ram operand) x)))

(definstruction emu-test-machine hlt
  (encoding (opcode #x00))
  (semantics (trap :halt)))

;; Multi-mode (mode.lisp, #18): LDA's IMMEDIATE and ZERO-PAGE variants share
;; one mnemonic but distinct opcodes/semantics -- proves opcode decode
;; (FIND-INSTRUCTION-BY-OPCODE) stays 1:1 per variant once a mnemonic
;; registers more than one INSTRUCTION-DESCRIPTOR.
(definstruction emu-test-machine lda
  (modes
    (immediate (opcode #xA1) (semantics (set! x operand)))
    (zero-page (opcode #xA6) (semantics (set! x (mref machine 'ram operand)))))
  (semantics (set! x operand)))

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

;;; RELATIVE mode (#23) -- BRA, kept separate from the existing ABSOLUTE-mode
;;; BNE tests above.

(fiveam:test step-machine-relative-branch-taken-forward
  ;; bra target (address 0, 2 bytes) / sta $2000 (address 2, 3 bytes) /
  ;; target: hlt (address 5) -- BRA is narrower than BNE's ABSOLUTE encoding,
  ;; so target lands at 5, not 6 as in the BNE test above.
  (let ((m (make-machine 'emu-test-machine))
        (a (assemble "bra target
sta $2000
target: hlt" :machine 'emu-test-machine)))
    (load-program m a)
    (step-machine m)
    (fiveam:is (= 5 (sref m 'pc)))))

(fiveam:test step-machine-relative-branch-taken-backward-sign-extends
  ;; target: dex (address 0, 1 byte) / bra target (address 1, 2 bytes) --
  ;; next-pc after BRA is 3, target is 0, so the encoded offset is -3 (#xFD);
  ;; taking the branch must land back on pc=0, proving STEP-MACHINE
  ;; sign-extends the fetched operand rather than adding it as unsigned 253.
  ;; (No NOP on this fixture -- DEX stands in as the no-operand filler.)
  (let ((m (make-machine 'emu-test-machine))
        (a (assemble "target: dex
bra target" :machine 'emu-test-machine)))
    (load-program m a)
    (step-machine m)  ; dex, address 0 -> 1, x wraps to 255 so z clears
    (step-machine m)  ; bra, taken (z=0) -> back to 0
    (fiveam:is (= 0 (sref m 'pc)))))

(fiveam:test step-machine-relative-branch-not-taken-falls-through
  ;; sta $2000 (address 0, 3 bytes) / bra skip (address 3, 2 bytes) / hlt
  ;; (address 5) / skip: hlt (address 6) -- not taken falls through to the
  ;; plain HLT at 5, one byte earlier than the BNE test above since BRA
  ;; encodes narrower.
  (let ((m (make-machine 'emu-test-machine))
        (a (assemble "sta $2000
bra skip
hlt
skip: hlt" :machine 'emu-test-machine)))
    (load-program m a)
    (setf (flag m 'z) t)  ; z set -> bra's branch condition (zerop z) is false
    (step-machine m)      ; sta, address 0 -> 3
    (step-machine m)      ; bra, not taken -> falls through to address 5
    (fiveam:is (= 5 (sref m 'pc)))))

(fiveam:test counter-loop-end-to-end-with-relative-branch
  (let* ((m (make-machine 'emu-test-machine))
         (a (assemble "        ldx #10
loop:   dex
        bra loop
        sta $1000
        hlt" :machine 'emu-test-machine)))
    (load-program m a)
    (multiple-value-bind (reason steps) (run m)
      (fiveam:is (eq :trap reason))
      (fiveam:is (= 23 steps))
      (fiveam:is (= 0 (sref m 'x)))
      (fiveam:is (= 0 (mref m 'ram #x1000))))))

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

;;; Multi-mode opcode decode (mode.lisp, #18)

(fiveam:test step-machine-decodes-each-mode-variant-independently
  (let ((m (make-machine 'emu-test-machine))
        (a (assemble "lda #5
lda $10" :machine 'emu-test-machine)))
    (setf (mref m 'ram #x10) 99)
    (load-program m a)
    (let ((first-descriptor (step-machine m)))
      (fiveam:is (eq (find-instruction 'emu-test-machine 'lda :mode 'immediate) first-descriptor))
      (fiveam:is (= 5 (sref m 'x))))
    (let ((second-descriptor (step-machine m)))
      (fiveam:is (eq (find-instruction 'emu-test-machine 'lda :mode 'zero-page) second-descriptor))
      (fiveam:is (= 99 (sref m 'x))))))

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
