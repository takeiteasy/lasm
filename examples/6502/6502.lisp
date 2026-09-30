;;;; 6502.lisp
;;;; A MOS 6502 machine, and a backend that lets the .lsp compiler target it.
;;;; demo.lasm is a program for it. Load it with (asdf:load-system "6502"); see
;;;; docs/examples.md.

#|
The MOS 6502 (NMOS, documented opcodes)

Registers   A, X, Y   8-bit accumulator and index registers
            S         8-bit stack pointer; the stack is page $01 ($0100-$01FF)
            PC        16-bit program counter
Flags       P         N V - B D I Z C, one register over the flags:
                      bit 7 N negative, 6 V overflow, 5 always 1, 4 B break
                      (0 in P; PHP and BRK push it set), 3 D decimal,
                      2 I interrupt disable, 1 Z zero, 0 C carry
Memory      64K of 8-bit cells; a 16-bit operand is little-endian
Stack       PHA and PHP push at $0100+S then decrement S; PLA and PLP increment
            S then pull. JSR pushes the address of its last byte, high byte
            first; RTS pulls it and adds one.
Vectors     $FFFA NMI, $FFFC reset, $FFFE IRQ/BRK, each a little-endian address
Interrupts  IRQ (held off while I is set) and NMI (never) take 7 cycles: push PC
            then P with B clear, set I, jump through the vector. BRK does the
            same with B set. RTI pulls P then PC.
Timer       $D000-$D003, a memory-mapped interval timer; see timer.lisp

Addressing modes
  immediate   #$10       one operand byte
  zero page   $10        a byte address in page 0
  zero page,X $10,X      (also ,Y) the sum wraps within page 0
  absolute    $1234      a 16-bit address
  absolute,X  $1234,X    (also ,Y) a page crossed costs a cycle on a read
  indirect    ($1234)    JMP only; a pointer at $xxFF takes its high byte from $xx00
  (zp,X)      ($10,X)    the pointer is at zero page $10+X
  (zp),Y      ($10),Y    the pointer is at zero page $10; Y is added to it
  relative    label      branches: a signed byte from the next instruction
  accumulator A          the shift and rotate instructions

Instructions (56)
  Load/store   LDA LDX LDY STA STX STY        Transfer  TAX TAY TXA TYA TSX TXS
  Arithmetic   ADC SBC (decimal when D = 1)   Compare   CMP CPX CPY BIT
  Logic        AND ORA EOR                    Shift     ASL LSR ROL ROR
  Increment    INC DEC INX INY DEX DEY        Stack     PHA PHP PLA PLP
  Branch       BCC BCS BEQ BMI BNE BPL BVC BVS
  Jump         JMP JSR RTS BRK RTI            Flags     CLC SEC CLD SED CLI SEI CLV
  NOP
JAM ($02) is not an instruction of the documented set; a real NMOS 6502 locks up
on it, and here it stops the machine.
|#

(in-package #:mos6502)

;;; The assembly syntax: `$` hex, `%` binary, `;` comments and `lda.w` to force a
;;; mode. See docs/lexer.md.
(deflexer mos6502-syntax
  (comment-styles (";" :line))
  (number-formats (:hex "$" "0x") (:bin "%" "0b") (:dec :default))
  (label-suffix ":")
  (local-label-prefix ".")
  (string-delim "\"")
  (ident-chars :alnum "_.")
  (mode-suffix-separator "."))

;;; Devices. The timer's hooks live in timer.lisp. See docs/devices.md.
(defdevice timer :id 1 :version 1
  :init timer-init :tick timer-tick :read timer-read :write timer-write
  :save timer-save :load timer-load)

;;; Storage. P is a register over the flags: bit 5 reads 1 and bit 4 (B) reads 0,
;;; and a write to P ignores both. S is an 8-bit register over page $01 that
;;; stores before it decrements. The vectors at $FFFA-$FFFF are ROM, so a reset
;;; keeps them, and PC starts at the word at $FFFC. IRQ and NMI save PC then P
;;; and jump through $FFFE and $FFFA, as the hardware does; the timer is mapped
;;; at $D000. See docs/machine-model.md and docs/interrupts.md.
;;; TODO: IRQ-DATA only receives the signal's data, which nothing reads; drop it
;;; once :message is optional (#462).
(defmachine mos6502
  (register a :width 8)
  (register x :width 8)
  (register y :width 8)
  (register s :width 8)
  (register pc :width 16)
  (flags n v d i z c)
  (status-register p (n v 1 0 d i z c))
  (register irq-data :width 8)
  (memory ram :width 8 :addr-width 16
    (region timer-io #xd000 #xd003 :kind :device :device timer)
    (region vectors #xfffa #xffff :kind :rom))
  (devices timer)
  (stack-pointer s :memory ram :base #x100 :grows :down :push :post)
  (reset-pc (ram #xfffc))
  (interrupts :vector (ram #xfffe) :nmi-vector (ram #xfffa) :message irq-data
              :save (pc p) :stack s :mask-flag i :mask-on-deliver t
              :cycles 7 :on-overflow :drop :drop-on-zero-vector nil))

;;; Addressing modes. The built-in `immediate`, `zero-page`, `absolute` and
;;; `relative` are the 6502's own. The rest are local to this ISA: a local mode
;;; shadows a global one of the same name, as `indirect-y` does here with a
;;; one-byte operand. Modes with the same syntax compete by the operand's value,
;;; so an instruction lists its zero-page mode before its absolute one. See
;;; docs/modes.md.
(defmode (zero-page-x (:isa mos6502)) expr "," "X" :width 1)
(defmode (zero-page-y (:isa mos6502)) expr "," "Y" :width 1)
(defmode (absolute-x (:isa mos6502)) expr "," "X")
(defmode (absolute-y (:isa mos6502)) expr "," "Y")
(defmode (indirect-x (:isa mos6502)) "(" expr "," "X" ")" :width 1)
(defmode (indirect-y (:isa mos6502)) "(" expr ")" "," "Y" :width 1)
(defmode (indirect (:isa mos6502)) "(" expr ")")
(defmode (accumulator (:isa mos6502)) "A")

;;; Helpers the instructions call. A word is two cells, low first.
(defun word-at (machine address)
  (logior (mref machine 'ram address)
          (ash (mref machine 'ram (logand (1+ address) #xffff)) 8)))

(defun zero-page-word (machine address)
  "The word at zero-page ADDRESS; its high byte wraps within the page."
  (logior (mref machine 'ram address)
          (ash (mref machine 'ram (logand (1+ address) #xff)) 8)))

(defun indirect-jump-target (machine pointer)
  "JMP (POINTER): a pointer at $xxFF takes its high byte from $xx00."
  (logior (mref machine 'ram pointer)
          (ash (mref machine 'ram (logior (logand pointer #xff00) (logand (1+ pointer) #xff))) 8)))

(defun signed-byte-of (byte)
  (if (>= byte 128) (- byte 256) byte))

;;; Decimal mode. NMOS ADC takes A and C from the packed-BCD sum, N and V from
;;; its signed intermediate and Z from the binary sum; SBC's flags are the
;;; binary ones. See http://www.6502.org/tutorials/decimal_mode.html.
(defun decimal-add (accumulator operand carry)
  "(VALUES A C N V) of a decimal ADC."
  (let* ((low (+ (logand accumulator 15) (logand operand 15) carry))
         (low (if (>= low 10) (+ (logand (+ low 6) 15) 16) low))
         (signed (+ (signed-byte-of (logand accumulator #xf0)) (signed-byte-of (logand operand #xf0)) low))
         (high (+ (logand accumulator #xf0) (logand operand #xf0) low))
         (high (if (>= high #xa0) (+ high #x60) high)))
    (values (logand high 255) (if (>= high #x100) 1 0)
            (if (logbitp 7 signed) 1 0) (if (< -129 signed 128) 0 1))))

(defun decimal-subtract (accumulator operand carry)
  "The A of a decimal SBC."
  (let* ((low (+ (- (logand accumulator 15) (logand operand 15)) carry -1))
         (low (if (minusp low) (- (logand (- low 6) 15) 16) low))
         (high (+ (- (logand accumulator #xf0) (logand operand #xf0)) low))
         (high (if (minusp high) (- high #x60) high)))
    (logand high 255)))

(defun add-with-carry (accumulator operand carry decimal)
  "(VALUES A C N V Z) of ADC."
  (let* ((sum (+ accumulator operand carry))
         (result (logand sum 255))
         (zero (if (zerop result) 1 0)))
    (if decimal
        (multiple-value-bind (dec-result dec-carry negative overflow) (decimal-add accumulator operand carry)
          (values dec-result dec-carry negative overflow zero))
        (values result (if (> sum 255) 1 0) (ash result -7)
                (if (logbitp 7 (logand (logxor accumulator sum) (logxor operand sum))) 1 0)
                zero))))

(defun subtract-with-borrow (accumulator operand carry decimal)
  "(VALUES A C N V Z) of SBC."
  (let* ((difference (- accumulator operand (- 1 carry)))
         (result (logand difference 255)))
    (values (if decimal (decimal-subtract accumulator operand carry) result)
            (if (minusp difference) 0 1) (ash result -7)
            (if (logbitp 7 (logand (logxor accumulator operand) (logxor accumulator difference))) 1 0)
            (if (zerop result) 1 0))))

;;; Semantics macros. A body reads and writes registers and flags by name, so
;;; nothing here may bind a variable called a, x, y, s, n, v, d, i, z or c.
(defmacro flag-of (test)
  `(if ,test 1 0))

(defmacro set-nz (form)
  (let ((result (gensym "RESULT")))
    `(let ((,result ,form))
       (set! n (ash (logand ,result 255) -7))
       (set! z (flag-of (zerop (logand ,result 255)))))))

;;; An instruction is written once and defined for each of its addressing modes
;;; (docs/instructions.md). WITH-OPERAND wraps the body for one mode: `value`
;;; reads the operand, `(store v)` writes it back, and `address` is where it is.
;;; A read through an indexed mode costs a cycle when it crosses a page.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun operand-semantics (mode kind body)
    (flet ((in-memory (bindings &optional penalty)
             `(let* (,@bindings)
                ,@(when (and penalty (eq kind :read))
                    `((when (page-crossed? ,penalty address) (elapse 1))))
                (symbol-macrolet ((value (mref machine 'ram address)))
                  (flet ((store (result) (set! (mref machine 'ram address) result)))
                    (declare (ignorable #'store))
                    ,@body)))))
      (ecase mode
        (immediate `(symbol-macrolet ((value operand)) ,@body))
        (accumulator `(symbol-macrolet ((value a))
                        (flet ((store (result) (set! a result)))
                          (declare (ignorable #'store))
                          ,@body)))
        ((zero-page absolute) (in-memory '((address operand))))
        (zero-page-x (in-memory '((address (logand (+ operand x) 255)))))
        (zero-page-y (in-memory '((address (logand (+ operand y) 255)))))
        (absolute-x (in-memory '((address (logand (+ operand x) #xffff))) 'operand))
        (absolute-y (in-memory '((address (logand (+ operand y) #xffff))) 'operand))
        (indirect-x (in-memory '((address (zero-page-word machine (logand (+ operand x) 255))))))
        (indirect-y (in-memory '((base (zero-page-word machine operand))
                                 (address (logand (+ base y) #xffff)))
                               'base))))))

(defmacro defop (mnemonic kind modes &body body)
  "MODES is ((MODE OPCODE CYCLES)...). KIND is :read, :write or :modify."
  `(definstruction mos6502 ,mnemonic
     (modes ,@(loop for (mode opcode cycles) in modes
                    collect `(,mode (opcode ,opcode) (cycles ,cycles)
                                    (semantics ,(operand-semantics mode kind body)))))))

(defmacro defimplied (mnemonic opcode cycles &body body)
  `(definstruction mos6502 ,mnemonic
     (encoding (opcode ,opcode))
     (cycles ,cycles)
     (semantics ,@body)))

;;; Loads, stores and transfers.
(defmacro defloader (mnemonic register modes)
  `(defop ,mnemonic :read ,modes
     (let ((result value))
       (set! ,register result)
       (set-nz result))))

(defloader lda a ((immediate #xa9 2) (zero-page #xa5 3) (zero-page-x #xb5 4) (absolute #xad 4)
                  (absolute-x #xbd 4) (absolute-y #xb9 4) (indirect-x #xa1 6) (indirect-y #xb1 5)))
(defloader ldx x ((immediate #xa2 2) (zero-page #xa6 3) (zero-page-y #xb6 4) (absolute #xae 4)
                  (absolute-y #xbe 4)))
(defloader ldy y ((immediate #xa0 2) (zero-page #xa4 3) (zero-page-x #xb4 4) (absolute #xac 4)
                  (absolute-x #xbc 4)))

(defmacro defstorer (mnemonic register modes)
  `(defop ,mnemonic :write ,modes
     (set! (mref machine 'ram address) ,register)))

(defstorer sta a ((zero-page #x85 3) (zero-page-x #x95 4) (absolute #x8d 4) (absolute-x #x9d 5)
                  (absolute-y #x99 5) (indirect-x #x81 6) (indirect-y #x91 6)))
(defstorer stx x ((zero-page #x86 3) (zero-page-y #x96 4) (absolute #x8e 4)))
(defstorer sty y ((zero-page #x84 3) (zero-page-x #x94 4) (absolute #x8c 4)))

(defmacro deftransfer (mnemonic opcode from to)
  `(defimplied ,mnemonic ,opcode 2
     (set! ,to ,from)
     ,@(unless (eq to 's) `((set-nz ,to)))))

(deftransfer tax #xaa a x)
(deftransfer tay #xa8 a y)
(deftransfer txa #x8a x a)
(deftransfer tya #x98 y a)
(deftransfer tsx #xba s x)
(deftransfer txs #x9a x s)

;;; Arithmetic. D selects packed-BCD arithmetic.
(defop adc :read ((immediate #x69 2) (zero-page #x65 3) (zero-page-x #x75 4) (absolute #x6d 4)
                  (absolute-x #x7d 4) (absolute-y #x79 4) (indirect-x #x61 6) (indirect-y #x71 5))
  (multiple-value-bind (result carry negative overflow zero) (add-with-carry a value c (= d 1))
    (set! a result) (set! c carry) (set! n negative) (set! v overflow) (set! z zero)))

(defop sbc :read ((immediate #xe9 2) (zero-page #xe5 3) (zero-page-x #xf5 4) (absolute #xed 4)
                  (absolute-x #xfd 4) (absolute-y #xf9 4) (indirect-x #xe1 6) (indirect-y #xf1 5))
  (multiple-value-bind (result carry negative overflow zero) (subtract-with-borrow a value c (= d 1))
    (set! a result) (set! c carry) (set! n negative) (set! v overflow) (set! z zero)))

(defmacro defcompare (mnemonic register modes)
  `(defop ,mnemonic :read ,modes
     (let ((operand-value value))
       (set! c (flag-of (>= ,register operand-value)))
       (set-nz (- ,register operand-value)))))

(defcompare cmp a ((immediate #xc9 2) (zero-page #xc5 3) (zero-page-x #xd5 4) (absolute #xcd 4)
                   (absolute-x #xdd 4) (absolute-y #xd9 4) (indirect-x #xc1 6) (indirect-y #xd1 5)))
(defcompare cpx x ((immediate #xe0 2) (zero-page #xe4 3) (absolute #xec 4)))
(defcompare cpy y ((immediate #xc0 2) (zero-page #xc4 3) (absolute #xcc 4)))

(defop bit :read ((zero-page #x24 3) (absolute #x2c 4))
  (let ((operand-value value))
    (set! z (flag-of (zerop (logand a operand-value))))
    (set! n (ldb (byte 1 7) operand-value))
    (set! v (ldb (byte 1 6) operand-value))))

;;; Logic.
(defmacro deflogic (mnemonic function modes)
  `(defop ,mnemonic :read ,modes
     (let ((result (,function a value)))
       (set! a result)
       (set-nz result))))

(deflogic and logand ((immediate #x29 2) (zero-page #x25 3) (zero-page-x #x35 4) (absolute #x2d 4)
                      (absolute-x #x3d 4) (absolute-y #x39 4) (indirect-x #x21 6) (indirect-y #x31 5)))
(deflogic ora logior ((immediate #x09 2) (zero-page #x05 3) (zero-page-x #x15 4) (absolute #x0d 4)
                      (absolute-x #x1d 4) (absolute-y #x19 4) (indirect-x #x01 6) (indirect-y #x11 5)))
(deflogic eor logxor ((immediate #x49 2) (zero-page #x45 3) (zero-page-x #x55 4) (absolute #x4d 4)
                      (absolute-x #x5d 4) (absolute-y #x59 4) (indirect-x #x41 6) (indirect-y #x51 5)))

;;; Shifts and rotates. The bit shifted out goes to C.
(defmacro defshift (mnemonic modes carry-out result-form)
  `(defop ,mnemonic :modify ,modes
     (let* ((byte value)
            (result (logand ,result-form 255)))
       (set! c ,carry-out)
       (store result)
       (set-nz result))))

(defshift asl ((accumulator #x0a 2) (zero-page #x06 5) (zero-page-x #x16 6) (absolute #x0e 6) (absolute-x #x1e 7))
  (ldb (byte 1 7) byte) (ash byte 1))
(defshift lsr ((accumulator #x4a 2) (zero-page #x46 5) (zero-page-x #x56 6) (absolute #x4e 6) (absolute-x #x5e 7))
  (ldb (byte 1 0) byte) (ash byte -1))
(defshift rol ((accumulator #x2a 2) (zero-page #x26 5) (zero-page-x #x36 6) (absolute #x2e 6) (absolute-x #x3e 7))
  (ldb (byte 1 7) byte) (logior (ash byte 1) c))
(defshift ror ((accumulator #x6a 2) (zero-page #x66 5) (zero-page-x #x76 6) (absolute #x6e 6) (absolute-x #x7e 7))
  (ldb (byte 1 0) byte) (logior (ash byte -1) (ash c 7)))

;;; Increment and decrement.
(defmacro defstep (mnemonic delta modes)
  `(defop ,mnemonic :modify ,modes
     (let ((result (logand (+ value ,delta) 255)))
       (store result)
       (set-nz result))))

(defstep inc 1 ((zero-page #xe6 5) (zero-page-x #xf6 6) (absolute #xee 6) (absolute-x #xfe 7)))
(defstep dec -1 ((zero-page #xc6 5) (zero-page-x #xd6 6) (absolute #xce 6) (absolute-x #xde 7)))

(defmacro defregister-step (mnemonic opcode register delta)
  `(defimplied ,mnemonic ,opcode 2
     (set! ,register (logand (+ ,register ,delta) 255))
     (set-nz ,register)))

(defregister-step inx #xe8 x 1)
(defregister-step iny #xc8 y 1)
(defregister-step dex #xca x -1)
(defregister-step dey #x88 y -1)

;;; Flags and the stack.
(defimplied clc #x18 2 (set! c 0))
(defimplied sec #x38 2 (set! c 1))
(defimplied cli #x58 2 (set! i 0))
(defimplied sei #x78 2 (set! i 1))
(defimplied cld #xd8 2 (set! d 0))
(defimplied sed #xf8 2 (set! d 1))
(defimplied clv #xb8 2 (set! v 0))
(defimplied nop #xea 2)

(defimplied pha #x48 3 (push a))
(defimplied php #x08 3 (push (logior p #x10)))
(defimplied pla #x68 4 (set! a (pop)) (set-nz a))
(defimplied plp #x28 4 (set! p (pop)))

;;; Branches. The offset is signed and counts from the next instruction, so it
;;; is added to a PC that has already moved past it. A taken branch costs a
;;; cycle, and another when it lands on a different page.
(defmacro defbranch (mnemonic opcode condition)
  `(definstruction mos6502 ,mnemonic
     (modes relative)
     (encoding (opcode ,opcode) (operand :mode))
     (cycles 2)
     (semantics
       (when ,condition
         (let ((target (logand (+ pc operand) #xffff)))
           (elapse (if (page-crossed? pc target) 2 1))
           (set! pc target))))))

(defbranch bcc #x90 (= c 0))
(defbranch bcs #xb0 (= c 1))
(defbranch bne #xd0 (= z 0))
(defbranch beq #xf0 (= z 1))
(defbranch bpl #x10 (= n 0))
(defbranch bmi #x30 (= n 1))
(defbranch bvc #x50 (= v 0))
(defbranch bvs #x70 (= v 1))

;;; Jumps and calls. JSR pushes the address of its last byte, and BRK the address
;;; of the byte after its padding byte. BRK goes through the $FFFE vector by hand,
;;; because it pushes B set, which an `interrupts` delivery does not. RTI undoes
;;; BRK, IRQ and NMI alike.
(definstruction mos6502 jmp
  (modes (absolute (opcode #x4c) (cycles 3) (semantics (set! pc operand)))
         (indirect (opcode #x6c) (cycles 5) (semantics (set! pc (indirect-jump-target machine operand))))))

(definstruction mos6502 jsr
  (modes absolute)
  (encoding (opcode #x20) (operand :mode))
  (cycles 6)
  (semantics
    (push (logand (1- pc) #xffff) :width 16)
    (set! pc operand)))

(defimplied rts #x60 6 (set! pc (logand (1+ (pop :width 16)) #xffff)))

(defimplied brk #x00 7
  (push (logand (1+ pc) #xffff) :width 16)
  (push (logior p #x10))
  (set! i 1)
  (set! pc (word-at machine #xfffe)))

(defimplied rti #x40 6 (interrupt-return))

;;; A real NMOS 6502 locks up on $02. Here it stops the machine, so a program
;;; ends on it and BRK stays a real interrupt.
(defimplied jam #x02 1 (trap :halt))

;;; The backend. A .lsp word is 16 bits, and the 6502 has three 8-bit registers, so
;;; a word lives in a pair of zero-page cells (docs/register-pairs.md#memory-halves).
;;; A and Y are in no role list, so a template uses them freely. Locals and
;;; arguments have fixed addresses (docs/static-frames.md): the stack only holds
;;; return addresses, so a function cannot recurse. The word instructions the
;;; language needs but the 6502 lacks, such as `:mul`, `:div` and `:shl`, are left
;;; out; a program that uses one is a compile error naming the form.
;;; See docs/backends.md.
(defbackend mos6502-lang (:isa mos6502)
  (registers :pairs ((w0 #x03 #x02) (w1 #x05 #x04) (w2 #x07 #x06)
                     (w3 #x09 #x08) (w4 #x0b #x0a) (w5 #x0d #x0c))
             :return (w0) :scratch (w0 w1) :caller-saved (w2 w3) :callee-saved (w4 w5)
             :program-counter pc :operand zp)
  (call :args (w2 w3) :return-address-slots 0)
  (frame :static t)
  (operands (zp zero-page) (zpx zero-page-x) (zpy zero-page-y)
            (imm immediate) (abs absolute) (absx absolute-x) (absy absolute-y)
            (indx indirect-x) (indy indirect-y) (ind indirect) (acc accumulator))
  (ops (:const (r v) (lda (imm (:lo v))) (sta (:lo r)) (lda (imm (:hi v))) (sta (:hi r)))
       (:move (d s) (lda (:lo s)) (sta (:lo d)) (lda (:hi s)) (sta (:hi d)))
       (:peek-label (d label) (lda (abs label)) (sta (:lo d)) (lda (abs (+ label 1))) (sta (:hi d)))
       (:poke-label (label s) (lda (:lo s)) (sta (abs label)) (lda (:hi s)) (sta (abs (+ label 1))))
       ;; The word at the address in a pair, through (zp),Y with Y = 0 then 1.
       (:peek (d a) (ldy (imm 0)) (lda (indy (:lo a))) (tax) (iny) (lda (indy (:lo a)))
              (sta (zp (:hi d))) (txa) (sta (zp (:lo d))))
       (:poke (a s) (ldy (imm 0)) (lda (zp (:lo s))) (sta (indy (:lo a))) (iny)
              (lda (zp (:hi s))) (sta (indy (:lo a))))
       (:add (d s) (clc) (lda (:lo d)) (adc (:lo s)) (sta (:lo d)) (lda (:hi d)) (adc (:hi s)) (sta (:hi d)))
       (:sub (d s) (sec) (lda (:lo d)) (sbc (:lo s)) (sta (:lo d)) (lda (:hi d)) (sbc (:hi s)) (sta (:hi d)))
       (:and (d s) (lda (:lo d)) (and (:lo s)) (sta (:lo d)) (lda (:hi d)) (and (:hi s)) (sta (:hi d)))
       (:or (d s) (lda (:lo d)) (ora (:lo s)) (sta (:lo d)) (lda (:hi d)) (ora (:hi s)) (sta (:hi d)))
       (:xor (d s) (lda (:lo d)) (eor (:lo s)) (sta (:lo d)) (lda (:hi d)) (eor (:hi s)) (sta (:hi d)))
       ;; A comparison leaves 1 or 0 in the pair. LDA sets Z, so the BNE and BEQ
       ;; after LDA #1 and LDA #0 always jump.
       (:eq (d s) (lda (:lo d)) (cmp (:lo s)) (bne no) (lda (:hi d)) (cmp (:hi s)) (bne no)
            (lda (imm 1)) (bne done) (:label no) (lda (imm 0)) (:label done)
            (sta (:lo d)) (lda (imm 0)) (sta (:hi d)))
       (:ne (d s) (lda (:lo d)) (cmp (:lo s)) (bne yes) (lda (:hi d)) (cmp (:hi s)) (bne yes)
            (lda (imm 0)) (beq done) (:label yes) (lda (imm 1)) (:label done)
            (sta (:lo d)) (lda (imm 0)) (sta (:hi d)))
       ;; Signed less-than: after the subtraction N differs from V when d < s.
       (:lt (d s) (sec) (lda (:lo d)) (sbc (:lo s)) (lda (:hi d)) (sbc (:hi s))
            (bvc same) (eor (imm #x80)) (:label same) (bmi yes)
            (lda (imm 0)) (beq done) (:label yes) (lda (imm 1)) (:label done)
            (sta (:lo d)) (lda (imm 0)) (sta (:hi d)))
       (:jump (target) (jmp target))
       (:branch-zero (r target) (lda (:lo r)) (ora (:hi r)) (bne skip) (jmp target) (:label skip))
       ;; JMP (indirect) takes a fixed address, so a computed target is copied to the
       ;; call vector at 14 and 15 and called through a trampoline.
       (:call ((f zp)) (lda (:lo f)) (sta (zp 14)) (lda (:hi f)) (sta (zp 15))
              (jsr trampoline) (jmp after) (:label trampoline) (jmp (ind 14)) (:label after))
       (:call (f) (jsr f))
       (:return () (rts))
       (:halt () (jam))))

;;; Driving the machine from Lisp. The demo is a .lasm program: an items program
;;; that names its backend, so it mixes raw 6502 instructions with the backend's
;;; operations and calls. See docs/items.md#lasm-files.
(defparameter *demo* (asdf:system-relative-pathname "6502" "demo.lasm")
  "The demo program's path.")

(defun assemble-6502 (source &key (origin #x200))
  "Assemble the assembly text SOURCE, with `$` hex and `;` comments."
  (assemble source :cpu 'mos6502 :lexer 'mos6502-syntax :origin origin))

(defun load-6502 (assembly)
  "A machine with ASSEMBLY loaded; PC starts at its origin."
  (let ((machine (make-machine 'mos6502)))
    (load-program machine assembly)
    machine))

(defun reset-6502 (machine)
  "What the reset line does: S = $FD, I = 1, PC from the vector at $FFFC."
  (setf (sref machine 's) #xfd
        (flag machine 'i) 1
        (sref machine 'pc) (machine-reset-pc machine))
  machine)

(defun run-6502 (machine &key (max-steps 1000000))
  "Run MACHINE until JAM, or MAX-STEPS instructions. Returns :HALTED or :RUNNING;
any other reason the machine stopped is an error."
  (let ((reason (run machine :max-steps max-steps)))
    (case reason
      (:trap :halted)
      (:max-steps :running)
      (t (error "The 6502 stopped: ~S" reason)))))

(defun place (name)
  "The register or flag NAME, a symbol or string of any package, as this machine names it."
  (intern (string-upcase (string name)) '#:mos6502))

(defun reg (machine name)
  "The register NAME, one of A X Y S PC P."
  (sref machine (place name)))

(defun (setf reg) (value machine name)
  (setf (sref machine (place name)) value))

(defun flag-set-p (machine name)
  (= 1 (flag machine (place name))))

(defun status (machine)
  "The status register P: the flags as PHP pushes them, with B clear."
  (reg machine 'p))

(defun ram (machine address)
  (mref machine 'ram address))

(defun demo-symbol (assembly name)
  "The address of the label NAME in ASSEMBLY."
  (gethash name (assembly-symbols assembly)))
