;;;; test.lisp

(defpackage #:mos6502/test
  (:use #:cl #:lasm #:mos6502)
  (:shadowing-import-from #:lasm #:push #:pop)
  (:export #:run-tests))

(in-package #:mos6502/test)

(fiveam:def-suite 6502-tests)
(fiveam:in-suite 6502-tests)

(defun run-tests ()
  (fiveam:run! '6502-tests))

(defun cells (source)
  (coerce (assembly-cells (assemble-6502 source)) 'list))

(defun run-source (source &key (max-steps 10000))
  "The machine after SOURCE has run to a JAM appended to it, with S at $FF."
  (let ((machine (load-6502 (assemble-6502 (format nil "~A~%jam" source)))))
    (setf (reg machine 's) #xff)
    (fiveam:is (eq :halted (run-6502 machine :max-steps max-steps)) "~A" source)
    machine))

(defun regs (machine)
  "A, X and Y."
  (list (reg machine 'a) (reg machine 'x) (reg machine 'y)))

(defun bit-of (machine name)
  (if (flag-set-p machine name) 1 0))

(defun cycles (source before &key (start #x200))
  "The cycles of the instruction after the BEFORE that come first in SOURCE."
  (let ((machine (load-6502 (assemble-6502 source))))
    (setf (reg machine 'pc) start)
    (dotimes (i before)
      (step-machine machine))
    (nth-value 1 (step-machine machine))))

;;; Encoding. The opcode of the 8 x 8 groups is aaabbbcc, so the expected bytes
;;; come from that layout and not from the tables that define the instructions.

(defparameter +operands+
  '((imm "#$10" (16)) (zp "$10" (16)) (zpx "$10,x" (16)) (zpy "$10,y" (16))
    (abs "$1234" (52 18)) (absx "$1234,x" (52 18)) (absy "$1234,y" (52 18))
    (indx "($10,x)" (16)) (indy "($10),y" (16)) (acc "a" ()))
  "Each mode, an operand that uses it, and the operand's bytes.")

(defparameter +opcode-groups+
  '((1 (("ora" 0) ("and" 1) ("eor" 2) ("adc" 3) ("sta" 4) ("lda" 5) ("cmp" 6) ("sbc" 7))
     ((indx 0) (zp 1) (imm 2) (abs 3) (indy 4) (zpx 5) (absy 6) (absx 7)))
    (2 (("asl" 0) ("rol" 1) ("lsr" 2) ("ror" 3) ("stx" 4) ("ldx" 5) ("dec" 6) ("inc" 7))
     ((imm 0) (zp 1) (acc 2) (abs 3) (zpx 5) (absx 7)))
    (0 (("bit" 1) ("sty" 4) ("ldy" 5) ("cpy" 6) ("cpx" 7))
     ((imm 0) (zp 1) (abs 3) (zpx 5) (absx 7))))
  "Each group's cc bits, its mnemonics with their aaa bits, and its modes with their bbb bits.")

(defparameter +missing-modes+
  '(("sta" imm) ("asl" imm) ("rol" imm) ("lsr" imm) ("ror" imm) ("stx" imm) ("dec" imm) ("inc" imm)
    ("stx" acc) ("ldx" acc) ("dec" acc) ("inc" acc) ("stx" zpx) ("ldx" zpx) ("stx" absx) ("dec" acc)
    ("ldx" absx)
    ("bit" imm) ("bit" zpx) ("bit" absx) ("sty" imm) ("sty" absx) ("cpy" zpx) ("cpy" absx) ("cpx" zpx) ("cpx" absx))
  "Combinations the 6502 does not have.")

(fiveam:test the-opcode-matrix
  (loop for (cc mnemonics modes) in +opcode-groups+
        do (loop for (mnemonic aaa) in mnemonics
                 do (loop for (mode bbb) in modes
                          unless (member (list mnemonic mode) +missing-modes+ :test #'equal)
                            do (destructuring-bind (text operand-text bytes) (cons mode (rest (assoc mode +operands+)))
                                 (declare (ignore text))
                                 (let ((source (format nil "~A ~A" mnemonic operand-text)))
                                   (fiveam:is (equal (cons (+ cc (* 4 bbb) (* 32 aaa)) bytes) (cells source))
                                              "~A" source)))))))

(fiveam:test the-indexed-modes-the-matrix-shifts
  (fiveam:is (equal '(#xb6 16) (cells "ldx $10,y")))
  (fiveam:is (equal '(#x96 16) (cells "stx $10,y")))
  (fiveam:is (equal '(#xbe 52 18) (cells "ldx $1234,y")))
  (fiveam:is (equal '(#xae 52 18) (cells "ldx $1234")))
  (fiveam:is (equal '(#x86 16) (cells "stx $10")))
  (fiveam:is (equal '(#x99 52 18) (cells "sta $1234,y"))))

(defparameter +implied+
  '(("brk" #x00) ("php" #x08) ("clc" #x18) ("plp" #x28) ("sec" #x38) ("rti" #x40) ("pha" #x48)
    ("cli" #x58) ("rts" #x60) ("pla" #x68) ("sei" #x78) ("dey" #x88) ("txa" #x8a) ("tya" #x98)
    ("txs" #x9a) ("tay" #xa8) ("tax" #xaa) ("clv" #xb8) ("tsx" #xba) ("iny" #xc8) ("dex" #xca)
    ("cld" #xd8) ("inx" #xe8) ("nop" #xea) ("sed" #xf8) ("jam" #x02))
  "Instructions without an operand, with opcodes from the 6502 reference.")

(fiveam:test implied-instructions
  (loop for (mnemonic opcode) in +implied+
        do (fiveam:is (equal (list opcode) (cells mnemonic)) "~A" mnemonic)))

(fiveam:test jumps-and-branches
  (fiveam:is (equal '(#x4c 52 18) (cells "jmp $1234")))
  (fiveam:is (equal '(#x6c 52 18) (cells "jmp ($1234)")))
  (fiveam:is (equal '(#x20 52 18) (cells "jsr $1234")))
  (loop for (mnemonic opcode) in '(("bpl" #x10) ("bmi" #x30) ("bvc" #x50) ("bvs" #x70)
                                   ("bcc" #x90) ("bcs" #xb0) ("bne" #xd0) ("beq" #xf0))
        do (fiveam:is (equal (list opcode 1 #xea #xea) (cells (format nil "~A skip~%nop~%skip: nop" mnemonic)))
                      "~A" mnemonic)))

(fiveam:test a-branch-offset-counts-from-the-next-instruction
  (fiveam:is (equal '(#xca #xd0 #xfd) (cells (format nil "loop: dex~%bne loop"))) "backwards")
  (fiveam:is (equal '(#xf0 1 #xea #xea) (cells (format nil "beq skip~%nop~%skip: nop"))) "forwards"))

(fiveam:test a-branch-out-of-range-is-an-error
  (fiveam:signals assembly-error (assemble-6502 (format nil "bne far~%.res 200~%far: nop"))))

;;; Choosing zero page or absolute. See docs/assembler.md#choosing-a-mode.

(fiveam:test an-operand-that-fits-a-byte-is-zero-page
  (fiveam:is (equal '(#xa5 16) (cells "lda $10")))
  (fiveam:is (equal '(#xad 0 1) (cells "lda $100")))
  (fiveam:is (equal '(#xb5 16) (cells "lda $10,x")))
  (fiveam:is (equal '(#xbd 0 1) (cells "lda $100,x"))))

(fiveam:test a-forward-label-widens-when-it-must
  (fiveam:is (equal '(#xa5 4 #xea) (cells (format nil "lda near~%near = 4~%nop")))))

(fiveam:test a-suffix-forces-a-mode
  (fiveam:is (equal '(#xad 16 0) (cells "lda.w $10")))
  (fiveam:is (equal '(#xa5 16) (cells "lda.z $10"))))

(fiveam:test an-operand-is-little-endian
  (fiveam:is (equal '(#x8d #x34 #x12) (cells "sta $1234")))
  (fiveam:is (equal '(#x4c #x00 #x02) (cells "jmp $200"))))

(fiveam:test an-expression-in-parentheses-is-not-indirect
  (fiveam:is (equal '(#x8d #x35 #x12) (cells "sta $1234+1")))
  (fiveam:is (equal '(#xa9 4) (cells "lda #(1+3)")))
  (fiveam:is (equal '(#x6c 52 18) (cells "jmp ($1234)"))))

;;; Registers and flags

(fiveam:test a-load-sets-the-zero-and-negative-flags
  (fiveam:is (= #x22 (status (run-source "lda #0"))))
  (fiveam:is (= #xa0 (status (run-source "lda #$80"))))
  (fiveam:is (= #x20 (status (run-source "ldx #1"))))
  (fiveam:is (equal '(0 5 9) (regs (run-source (format nil "ldx #5~%ldy #9"))))))

(fiveam:test transfers
  (fiveam:is (equal '(7 7 0) (regs (run-source (format nil "lda #7~%tax")))))
  (fiveam:is (equal '(7 0 7) (regs (run-source (format nil "lda #7~%tay")))))
  (fiveam:is (equal '(9 9 0) (regs (run-source (format nil "ldx #9~%txa")))))
  (fiveam:is (equal '(9 0 9) (regs (run-source (format nil "ldy #9~%tya")))))
  (fiveam:is (= #xa0 (status (run-source (format nil "ldx #$80~%txa")))) "a transfer sets N and Z")
  (let ((machine (run-source (format nil "ldx #$80~%lda #1~%txs"))))
    (fiveam:is (= #x80 (reg machine 's)))
    (fiveam:is (= #x20 (status machine)) "TXS sets no flags"))
  (let ((machine (run-source (format nil "tsx"))))
    (fiveam:is (= #xff (reg machine 'x)))
    (fiveam:is (= #xa0 (status machine)))))

(fiveam:test increment-and-decrement-wrap
  (fiveam:is (equal '(0 0 0) (regs (run-source (format nil "ldx #$ff~%inx~%ldy #1~%dey")))))
  (fiveam:is (= #x22 (status (run-source (format nil "ldx #$ff~%inx")))))
  (fiveam:is (equal '(0 #xff 0) (regs (run-source (format nil "ldx #0~%dex")))))
  (let ((machine (run-source (format nil "lda #$0f~%sta $10~%inc $10~%inc $10~%dec $10~%ldx $10"))))
    (fiveam:is (= #x10 (reg machine 'x)))))

(fiveam:test add-with-carry
  (fiveam:is (= #x23 (status (run-source (format nil "clc~%lda #$ff~%adc #1")))) "255 + 1: Z and C")
  (fiveam:is (= #xe0 (status (run-source (format nil "clc~%lda #$50~%adc #$50")))) "80 + 80 overflows: N and V")
  (fiveam:is (= #xa1 (status (run-source (format nil "clc~%lda #$90~%adc #$f0")))) "a carry without an overflow")
  (fiveam:is (= 3 (reg (run-source (format nil "sec~%lda #1~%adc #1")) 'a)) "the carry in is added"))

(fiveam:test subtract-with-borrow
  (fiveam:is (= #x20 (status (run-source (format nil "sec~%lda #$50~%sbc #$f0")))))
  (fiveam:is (= #xe0 (status (run-source (format nil "sec~%lda #$50~%sbc #$b0")))) "80 - -80 overflows")
  (fiveam:is (= #x23 (status (run-source (format nil "sec~%lda #5~%sbc #5")))) "no borrow: C is set")
  (fiveam:is (= 4 (reg (run-source (format nil "clc~%lda #5~%sbc #0")) 'a)) "the borrow is subtracted"))

(fiveam:test decimal-mode
  (loop for (source a carry) in '(("clc~%lda #$12~%adc #$34" #x46 0)
                                  ("clc~%lda #$46~%adc #$55" #x01 1)
                                  ("sec~%lda #$46~%adc #$55" #x02 1)
                                  ("clc~%lda #$99~%adc #$01" #x00 1)
                                  ("clc~%lda #$09~%adc #$01" #x10 0)
                                  ("sec~%lda #$46~%sbc #$12" #x34 1)
                                  ("sec~%lda #$12~%sbc #$21" #x91 0)
                                  ("clc~%lda #$32~%sbc #$02" #x29 1)
                                  ("sec~%lda #$00~%sbc #$01" #x99 0))
        do (let ((machine (run-source (format nil "sed~%~A" (format nil source)))))
             (fiveam:is (= a (reg machine 'a)) "~A" source)
             (fiveam:is (eq (= 1 carry) (flag-set-p machine 'c)) "~A carry" source)))
  (fiveam:is (= #x0a (reg (run-source (format nil "clc~%lda #$05~%adc #$05")) 'a)) "binary without D"))

(fiveam:test compare
  (fiveam:is (= #x23 (status (run-source (format nil "lda #5~%cmp #5")))) "equal: Z and C")
  (fiveam:is (= #xa0 (status (run-source (format nil "lda #5~%cmp #6")))) "less: N, no C")
  (fiveam:is (= #x21 (status (run-source (format nil "lda #6~%cmp #5")))) "greater: C")
  (fiveam:is (= #x23 (status (run-source (format nil "ldx #9~%cpx #9")))))
  (fiveam:is (= #x21 (status (run-source (format nil "ldy #9~%cpy #1")))))
  (fiveam:is (= #x20 (reg (run-source (format nil "lda #$20~%cmp #$21")) 'a)) "a compare does not change A"))

(fiveam:test bit-tests-memory-against-a
  (let ((machine (run-source (format nil "lda #$c0~%sta $10~%lda #$01~%bit $10"))))
    (fiveam:is (= #xe2 (status machine)) "A and M is 0: Z; bits 7 and 6 of M: N and V")
    (fiveam:is (= 1 (reg machine 'a))))
  (fiveam:is (= #x20 (status (run-source (format nil "lda #$3f~%sta $10~%lda #$01~%bit $10"))))))

(fiveam:test logic
  (fiveam:is (= #x0c (reg (run-source (format nil "lda #$0f~%and #$3c")) 'a)))
  (fiveam:is (= #x3f (reg (run-source (format nil "lda #$0f~%ora #$3c")) 'a)))
  (fiveam:is (= #x33 (reg (run-source (format nil "lda #$0f~%eor #$3c")) 'a)))
  (fiveam:is (= #x22 (status (run-source (format nil "lda #$f0~%and #$0f"))))))

(fiveam:test shifts-and-rotates
  (let ((machine (run-source (format nil "lda #$81~%asl a"))))
    (fiveam:is (equal '(2 1) (list (reg machine 'a) (bit-of machine 'c)))))
  (let ((machine (run-source (format nil "lda #$81~%lsr a"))))
    (fiveam:is (equal '(#x40 1) (list (reg machine 'a) (bit-of machine 'c)))))
  (let ((machine (run-source (format nil "sec~%lda #$01~%ror a"))))
    (fiveam:is (equal '(#x80 1 1) (list (reg machine 'a) (bit-of machine 'c) (bit-of machine 'n)))))
  (let ((machine (run-source (format nil "clc~%lda #$80~%rol a"))))
    (fiveam:is (equal '(0 1 1) (list (reg machine 'a) (bit-of machine 'c) (bit-of machine 'z)))))
  (let ((machine (run-source (format nil "sec~%lda #$40~%rol a"))))
    (fiveam:is (= #x81 (reg machine 'a))))
  (let ((machine (run-source (format nil "lda #$0f~%sta $10~%asl $10~%lsr $10~%lsr $10~%ldx $10"))))
    (fiveam:is (= 7 (reg machine 'x)))))

;;; Loads and stores

(fiveam:test stores-and-loads-through-every-register
  (let ((machine (run-source (format nil "ldx #3~%ldy #4~%stx $10~%sty $11~%stx $20,y~%sty $30,x~%ldx $10~%ldy $11"))))
    (fiveam:is (equal '(0 3 4) (regs machine)))
    (fiveam:is (= 3 (ram machine #x24)))
    (fiveam:is (= 4 (ram machine #x33))))
  (let ((machine (run-source (format nil "ldy #5~%ldx #9~%stx $1234~%sty $1235~%ldx $1235~%ldy $1234"))))
    (fiveam:is (equal '(0 5 9) (regs machine))))
  (let ((machine (run-source (format nil "lda #7~%sta $10~%ldy #2~%ldx $0e,y~%lda #8~%sta $1002~%ldx $1000,y~%ldy $1000,x"))))
    (fiveam:is (equal '(8 8 0) (regs machine)) "ldx zp,y, ldx abs,y and ldy abs,x")))

;;; Addressing

(fiveam:test zero-page-indexing-wraps-within-the-page
  (let ((machine (run-source (format nil "ldx #$ff~%lda #7~%sta $10,x"))))
    (fiveam:is (= 7 (ram machine #x0f)))
    (fiveam:is (= 0 (ram machine #x10f)))))

(fiveam:test absolute-indexing-carries-into-the-page
  (let ((machine (run-source (format nil "ldx #$ff~%lda #7~%sta $1001,x~%ldy #$10~%sta $1100,y"))))
    (fiveam:is (= 7 (ram machine #x1100)))
    (fiveam:is (= 7 (ram machine #x1110)))))

(fiveam:test indexed-indirect-takes-its-pointer-from-zero-page-plus-x
  (let ((machine (run-source (format nil "lda #$00~%sta $24~%lda #$10~%sta $25~%lda #$77~%sta $1000~%ldx #4~%lda ($20,x)"))))
    (fiveam:is (= #x77 (reg machine 'a)))))

(fiveam:test indirect-indexed-adds-y-to-the-pointer
  (let ((machine (run-source (format nil "lda #$f0~%sta $30~%lda #$10~%sta $31~%lda #$55~%sta $1110~%ldy #$20~%lda ($30),y"))))
    (fiveam:is (= #x55 (reg machine 'a))))
  (let ((machine (run-source (format nil "lda #$66~%ldy #1~%sta ($30),y")))) 
    (fiveam:is (= #x66 (ram machine 1)) "the pointer is zero, so Y = 1 addresses cell 1")))

(fiveam:test a-zero-page-pointer-wraps-to-the-start-of-the-page
  (let ((machine (run-source (format nil "lda #$34~%sta $ff~%lda #$12~%sta $00~%lda #$5a~%sta $1234~%ldy #0~%lda ($ff),y"))))
    (fiveam:is (= #x5a (reg machine 'a)))))

;;; The stack is page 1

(fiveam:test push-and-pull
  (let ((machine (run-source (format nil "lda #$42~%pha~%lda #0~%pla"))))
    (fiveam:is (= #x42 (reg machine 'a)))
    (fiveam:is (= #xff (reg machine 's)))
    (fiveam:is (= #x42 (ram machine #x1ff)) "PHA writes at $0100 + S, then decrements S"))
  (fiveam:is (= #xfe (reg (run-source "pha") 's))))

(fiveam:test php-pushes-the-break-bit-and-plp-ignores-it
  (let ((machine (run-source (format nil "sec~%sed~%php~%clc~%cld~%plp"))))
    (fiveam:is (= #x39 (ram machine #x1ff)) "N V 1 B D I Z C")
    (fiveam:is (flag-set-p machine 'c))
    (fiveam:is (flag-set-p machine 'd))
    (fiveam:is (= #xff (reg machine 's)))
    (fiveam:is (= #x29 (status machine)) "B is not a flag")))

(fiveam:test a-call-pushes-the-address-of-its-last-byte
  (let ((machine (run-source (format nil "jsr sub~%ldx #1~%jam~%sub: ldy #2~%rts"))))
    (fiveam:is (equal '(0 1 2) (regs machine)))
    (fiveam:is (= #xff (reg machine 's)))
    (fiveam:is (equal '(2 2) (list (ram machine #x1ff) (ram machine #x1fe))) "$0202, high byte first")))

(fiveam:test nested-calls-return-in-order
  (fiveam:is (equal '(3 0 0)
                    (regs (run-source (format nil "jsr outer~%jam~%outer: jsr inner~%adc #1~%rts~%inner: lda #2~%rts"))))
             "the ADC after the inner call runs after it returns"))

(fiveam:test jmp-indirect-takes-its-high-byte-from-the-start-of-the-page
  (let ((machine (load-6502 (assemble-6502 (format nil "lda #$34~%sta $10ff~%lda #$12~%sta $1000~%lda #$99~%sta $1100~%jmp ($10ff)~%.org $1234~%ldx #5~%jam")))))
    (setf (reg machine 's) #xff)
    (fiveam:is (eq :halted (run-6502 machine)))
    (fiveam:is (= 5 (reg machine 'x)) "the target is $1234, not $9934")))

(fiveam:test brk-goes-through-the-vector-and-rti-returns
  (let ((machine (load-6502 (assemble-6502 (format nil "lda #1~%brk~%nop~%ldx #7~%jam~%.org $300~%handler: ldy #9~%rti~%.org $fffe~%.word handler")))))
    (setf (reg machine 's) #xff)
    (fiveam:is (eq :halted (run-6502 machine)))
    (fiveam:is (equal '(1 7 9) (regs machine)) "RTI returns past the padding byte")
    (fiveam:is (= #xff (reg machine 's)))))

(fiveam:test brk-pushes-the-return-address-and-status
  (let ((machine (load-6502 (assemble-6502 (format nil "sec~%brk~%nop~%.org $300~%handler: jam~%.org $fffe~%.word handler"))))) 
    (setf (reg machine 's) #xff)
    (run-6502 machine)
    (fiveam:is (= #xfc (reg machine 's)))
    (fiveam:is (equal '(#x02 #x03 #x31) (list (ram machine #x1ff) (ram machine #x1fe) (ram machine #x1fd)))
               "return address $0203 and P with B, bit 5 and C")))

(fiveam:test reset-reads-the-vector
  (let ((machine (load-6502 (assemble-6502 (format nil ".org $400~%start: jam~%.org $fffc~%.word start")))))
    (reset-6502 machine)
    (fiveam:is (= #x400 (reg machine 'pc)))
    (fiveam:is (= #xfd (reg machine 's)))
    (fiveam:is (flag-set-p machine 'i))))

(fiveam:test the-vectors-are-rom-and-survive-a-machine-reset
  (let ((machine (load-6502 (assemble-6502 (format nil ".org $400~%start: jam~%.org $fffc~%.word start")))))
    (lasm:reset machine)
    (fiveam:is (= #x400 (reg machine 'pc)) "reset reads the vector it kept")
    (fiveam:is (= #x400 (lasm:machine-reset-pc machine)))
    (fiveam:is (= 0 (ram machine #x400)) "RAM was cleared")))

(fiveam:test the-stack-is-page-one-and-stores-before-it-decrements
  (let ((machine (load-6502 (assemble-6502 (format nil "lda #$5a~%pha~%jam")))))
    (setf (reg machine 's) #xff)
    (run-6502 machine)
    (fiveam:is (= #x5a (ram machine #x1ff)))
    (fiveam:is (= #xfe (reg machine 's)))))

;;; Cycles. See docs/emulator.md#dynamic-cycle-costs.

(fiveam:test an-indexed-read-costs-a-cycle-across-a-page
  (fiveam:is (= 4 (cycles (format nil "ldx #1~%lda $1000,x") 1)))
  (fiveam:is (= 5 (cycles (format nil "ldx #1~%lda $10ff,x") 1)))
  (fiveam:is (= 5 (cycles (format nil "ldx #1~%sta $1000,x") 1)) "a write always pays it")
  (fiveam:is (= 3 (cycles "lda $10" 0)))
  (fiveam:is (= 6 (cycles "lda ($10,x)" 0))))

(fiveam:test a-taken-branch-costs-more
  (fiveam:is (= 2 (cycles (format nil "sec~%bcc skip~%skip: nop") 1)))
  (fiveam:is (= 3 (cycles (format nil "clc~%bcc skip~%skip: nop") 1)))
  (fiveam:is (= 4 (cycles (format nil ".org $2fb~%clc~%bcc skip~%nop~%nop~%nop~%skip: nop") 1 :start #x2fb))
             "landing on another page"))

;;; demo.lasm. See docs/examples.md#6502.

(defvar *demo-assembly* nil)

(defun demo-machine ()
  (setf *demo-assembly* (assemble-items-file *demo*))
  (let ((machine (load-6502 *demo-assembly*)))
    (reset-6502 machine)
    (fiveam:is (eq :halted (run-6502 machine)))
    machine))

(defun result (machine label &optional (width 1))
  (let ((address (demo-symbol *demo-assembly* label)))
    (if (= width 2) (word-at machine address) (ram machine address))))

(fiveam:test the-demo-runs-from-the-reset-vector
  (let ((machine (demo-machine)))
    (fiveam:is (= 36 (result machine "sum")) "absolute,X")
    (fiveam:is (= 1 (result machine "first")) "(zp,X)")
    (fiveam:is (= 1 (result machine "landed")) "JMP (vector)")
    (fiveam:is (equal '(72 69 76 76 79 0)
                      (loop for i below 6 collect (ram machine (+ i (demo-symbol *demo-assembly* "buffer")))))
               "(zp),Y copied HELLO")
    (fiveam:is (= 2100 (result machine "product" 2)) "300 * 7 through the backend's call")
    (fiveam:is (= 2105 (result machine "total" 2)) "and its :add")
    (fiveam:is (= 4 (result machine "bcd")) "decimal 58 + 46")
    (fiveam:is (= 1 (result machine "bcd_carry")))
    (fiveam:is (= 66 (result machine "pulled")))
    (fiveam:is (= #x35 (result machine "status_byte")) "PHP with C, I and B")
    (fiveam:is (= #xff (result machine "stack_top")) "the stack is balanced")
    (fiveam:is (= 2 (result machine "irq_count")) "BRK went through the vector twice")
    (fiveam:is (= #xb5 (result machine "irq_status")) "N, bit 5, B, I and C as BRK pushed them")
    (fiveam:is (= #xff (reg machine 's)))))

(fiveam:test the-demo-is-the-items-a-text-assembler-would-render
  (let* ((program (read-items *demo*))
         (text (render-items (items-program-items program) :backend 'mos6502-lang)))
    (fiveam:is (equalp (assembly-cells (assemble-items-file *demo*))
                       (assembly-cells (assemble text :cpu 'mos6502 :origin #x200))))))

;;; A .lsp program on the backend. Every value is a word in a pair of zero-page
;;; cells, and locals and arguments have fixed addresses.

(defparameter +program+
  "(defvar total 0)
   (defun add (a b) (+ a b))
   (defun triangle (n)
     (let ((i 0) (sum 0))
       (while (< i n) (set i (+ i 1)) (set sum (+ sum i)))
       sum))
   (defun main () (set total (add 30 40)) (+ total (triangle 10)))")

(defun run-program (source &key (optimize :size))
  (let* ((program (compile-source (read-source-from-string source) :backend 'mos6502-lang :optimize optimize))
         (machine (load-6502 (assemble-items (items-program-items program) :backend 'mos6502-lang :origin #x200))))
    (fiveam:is (eq :halted (run-6502 machine)))
    machine))

(fiveam:test a-lsp-program-runs-with-static-frames
  (fiveam:is (= 125 (word-at (run-program +program+) 2)))
  (fiveam:is (= 125 (word-at (run-program +program+ :optimize :speed) 2))))

(fiveam:test a-lsp-program-with-memory-and-comparisons
  (loop for (source . expected) in '(("(defun main () (- 5))" . 65531)
                                     ("(defun main () (+ (< 3 5) (= 4 4) (/= 4 4)))" . 2)
                                     ("(defun main () (< (- 1) 2))" . 1)
                                     ("(defun main () (poke 8192 7000) (poke 8194 (+ (peek 8192) 1)) (peek 8194))" . 7001)
                                     ("(defarray arr (10 20 300)) (defun main () (+ (aref arr 0) (aref arr 2)))" . 310)
                                     ("(defun main () (let ((x 3)) (if (< x 5) 100 200)))" . 100)
                                     ("(defun f (x) (+ x 1)) (defvar g 0) (defun main () (set g (function f)) (funcall g 41))" . 42))
        do (fiveam:is (= expected (word-at (run-program source) 2)) "~A" source)))

(fiveam:test an-operation-the-backend-lacks-is-a-compile-error
  (fiveam:signals error (run-program "(defun main () (* 6 7))")))
