;;;; tests/assembler.lisp
;;;; fiveam tests for the assembler pass (assembler.lisp), including M2's
;;;; multi-mode selection. Reuses the INSTR-TEST-MACHINE fixture and its
;;;; LDX/ADC/BNE/NOP/JMPFAR/LDA instructions from tests/instruction.lisp.

(in-package #:lasm)

(fiveam:def-suite assembler :in lasm)
(fiveam:in-suite assembler)

(fiveam:test label-binds-to-its-address
  (let ((a (assemble "start: ldx #10
sta: nop" :machine 'instr-test-machine)))
    (fiveam:is (= 0 (gethash "start" (assembly-symbols a))))
    (fiveam:is (= 2 (gethash "sta" (assembly-symbols a))))))

(fiveam:test forward-label-reference-resolves
  ;; bne .loop appears before .loop: is bound -- only a layout pass gets this.
  (let ((a (assemble "bne target
target: nop" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#xD0 3 0 #xEA) (assembly-bytes a)))))

(fiveam:test backward-label-reference-resolves
  (let ((a (assemble "target: nop
bne target" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#xEA #xD0 0 0) (assembly-bytes a)))))

(fiveam:test origin-offsets-bytes-and-symbols
  (let ((a (assemble "start: nop
bne start" :machine 'instr-test-machine :origin #x200)))
    (fiveam:is (= #x200 (assembly-origin a)))
    (fiveam:is (= #x200 (gethash "start" (assembly-symbols a))))
    (fiveam:is (equalp #(#xEA #xD0 0 2) (assembly-bytes a)))))

(fiveam:test duplicate-label-signals-assembly-error
  (fiveam:signals assembly-error
    (assemble "a: nop
a: nop" :machine 'instr-test-machine)))

(fiveam:test undefined-label-signals-unresolved-label
  (fiveam:signals unresolved-label
    (assemble "bne nowhere" :machine 'instr-test-machine)))

(fiveam:test missing-operand-signals-assembly-error
  (fiveam:signals assembly-error
    (assemble "ldx" :machine 'instr-test-machine)))

(fiveam:test unexpected-operand-signals-assembly-error
  (fiveam:signals assembly-error
    (assemble "nop #10" :machine 'instr-test-machine)))

(fiveam:test too-many-operands-signals-assembly-error
  (fiveam:signals assembly-error
    (assemble "ldx #10, #20" :machine 'instr-test-machine)))

(fiveam:test bare-label-line-emits-no-bytes
  (let ((a (assemble "start:
nop" :machine 'instr-test-machine)))
    (fiveam:is (= 0 (gethash "start" (assembly-symbols a))))
    (fiveam:is (equalp #(#xEA) (assembly-bytes a)))))

(fiveam:test unknown-mnemonic-signals-unknown-instruction
  (fiveam:signals unknown-instruction
    (assemble "frobnicate" :machine 'instr-test-machine)))

(fiveam:test no-operand-instruction-encodes-alone
  (let ((a (assemble "nop" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#xEA) (assembly-bytes a)))))

(fiveam:test immediate-and-absolute-mix
  (let ((a (assemble "ldx #10
adc target
target: nop" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#xA2 10 #x6D 5 0 #xEA) (assembly-bytes a)))))

;;; M2 multi-mode selection (mode.lisp, #18) -- LDA declares
;;; immediate/zero-page/absolute (tests/instruction.lisp), zero-page and
;;; absolute sharing identical operand syntax and differing only by width.

(fiveam:test constant-operand-picks-narrowest-fitting-mode
  (let ((a (assemble "lda $10" :machine 'instr-test-machine)))
    ;; zero-page (opcode #x11), not absolute (#x12) -- 2 bytes total
    (fiveam:is (equalp #(#x11 #x10) (assembly-bytes a)))))

(fiveam:test constant-operand-too-wide-for-zero-page-picks-absolute
  (let ((a (assemble "lda $1000" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#x12 #x00 #x10) (assembly-bytes a)))))

(fiveam:test label-operand-picks-widest-mode-even-when-zero-page-would-fit
  ;; "target" resolves to address 3 (comfortably zero-page), but a
  ;; label-bearing operand always takes the widest syntax-matching mode --
  ;; this is what buys one-pass layout instead of a relaxation loop.
  (let ((a (assemble "lda target
target: nop" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#x12 3 0 #xEA) (assembly-bytes a)))
    (fiveam:is (= 3 (gethash "target" (assembly-symbols a))))))

(fiveam:test immediate-operand-still-selects-immediate-mode-among-variants
  (let ((a (assemble "lda #7" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#x10 7) (assembly-bytes a)))))

(fiveam:test no-matching-mode-signals-assembly-error
  ;; ADC only declares ABSOLUTE -- an immediate-syntax operand matches none
  ;; of its variants.
  (fiveam:signals assembly-error
    (assemble "adc #10" :machine 'instr-test-machine)))
