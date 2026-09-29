;;;; dcpu16.lisp
;;;; A DCPU-16 v1.7 machine: storage, operand syntax, encodings and semantics.
;;;; Load it with (asdf:load-system :dcpu16); see docs/examples.md.

#|
DCPU-16 SPECIFICATION (v1.7)

Machine
  16-bit words. 0x10000 words of RAM, each address one word.
  Registers: A B C X Y Z I J, PC (program counter), SP (stack pointer),
             EX (extra/excess), IA (interrupt address).
  Whenever the CPU reads a word it reads [PC], then increments PC.

Instruction word (MSB first)
  Basic:    aaaaaa bbbbb ooooo    a: 6-bit value, b: 5-bit value, o: opcode
  Special:  aaaaaa ooooo 00000    o: special opcode, a: 6-bit value
  `a` is handled before `b`. "Next word" operands add a word to the
  instruction, `a`'s first. Instructions are 1 to 3 words long.

Values (C is the extra cycles to look the value up)
  C  VALUE      MEANING
  0  0x00-0x07  register (A B C X Y Z I J)
  0  0x08-0x0f  [register]
  1  0x10-0x17  [register + next word]
  0  0x18       PUSH ([--SP]) in b, POP ([SP++]) in a
  0  0x19       PEEK ([SP])
  1  0x1a       PICK n ([SP + next word])
  0  0x1b       SP
  0  0x1c       PC
  0  0x1d       EX
  1  0x1e       [next word]
  1  0x1f       next word (literal)
  0  0x20-0x3f  literal -1..30 (only in a)
  Writing to a literal is silently ignored.

Basic opcodes (C is the cycle cost)
  C  VAL   NAME    EFFECT
  1  0x01  SET b,a b = a
  2  0x02  ADD b,a b = b+a; EX = 1 on overflow, else 0
  2  0x03  SUB b,a b = b-a; EX = 0xffff on underflow, else 0
  2  0x04  MUL b,a b = b*a; EX = (b*a)>>16 (unsigned)
  2  0x05  MLI b,a like MUL, signed
  3  0x06  DIV b,a b = b/a; EX = ((b<<16)/a)&0xffff; if a==0, b and EX = 0
  3  0x07  DVI b,a like DIV, signed, rounding towards 0
  3  0x08  MOD b,a b = b%a; if a==0, b = 0
  3  0x09  MDI b,a like MOD, signed (MDI -7, 16 == -7)
  1  0x0a  AND b,a b = b&a
  1  0x0b  BOR b,a b = b|a
  1  0x0c  XOR b,a b = b^a
  1  0x0d  SHR b,a b = b>>>a; EX = ((b<<16)>>a)&0xffff (logical shift)
  1  0x0e  ASR b,a b = b>>a; EX = ((b<<16)>>>a)&0xffff (arithmetic shift)
  1  0x0f  SHL b,a b = b<<a; EX = ((b<<a)>>16)&0xffff
  2+ 0x10  IFB b,a perform next instruction only if (b&a) != 0
  2+ 0x11  IFC b,a ... only if (b&a) == 0
  2+ 0x12  IFE b,a ... only if b == a
  2+ 0x13  IFN b,a ... only if b != a
  2+ 0x14  IFG b,a ... only if b > a
  2+ 0x15  IFA b,a ... only if b > a (signed)
  2+ 0x16  IFL b,a ... only if b < a
  2+ 0x17  IFU b,a ... only if b < a (signed)
  3  0x1a  ADX b,a b = b+a+EX; EX = 1 on overflow, else 0
  3  0x1b  SBX b,a b = b-a+EX; EX = 0xffff on underflow, else 0
  2  0x1e  STI b,a b = a, then I and J increase by 1
  2  0x1f  STD b,a b = a, then I and J decrease by 1
  The IF instructions cost one more cycle when the test fails and the next
  instruction is skipped. Skipping an IF instruction skips one more
  instruction, at one more cycle, so conditionals chain.

Special opcodes (C is the cycle cost)
  C  VAL   NAME   EFFECT
  3  0x01  JSR a  push the address of the next instruction, then PC = a
  4  0x08  INT a  trigger a software interrupt with message a
  1  0x09  IAG a  a = IA
  1  0x0a  IAS a  IA = a
  3  0x0b  RFI a  disable interrupt queueing, pop A, then pop PC
  2  0x0c  IAQ a  if a is nonzero, queue interrupts; if zero, trigger them
  2  0x10  HWN a  a = number of connected devices
  4  0x11  HWQ a  A,B = device id (low, high), C = version,
                   X,Y = manufacturer (low, high)
  4+ 0x12  HWI a  send an interrupt to device a

Interrupts
  At most one interrupt is triggered between instructions; the rest wait in a
  queue of up to 256 (more and the DCPU-16 "catches fire"). When IA is not 0, a
  triggered interrupt turns on queueing, pushes PC, then A, and sets PC = IA and
  A = the message. When IA is 0 a triggered interrupt does nothing.

Hardware
  Up to 65535 devices, enumerated with HWN, HWQ and HWI.
  Devices here: the Generic Clock and Generic Keyboard (devices.lisp).
|#

(in-package #:dcpu16)

;;; Devices. Their hooks live in devices.lisp. See docs/devices.md.
(defdevice clock :id #x12d0b402 :version 1 :manufacturer #x1c6c8b36
  :init clock-init :tick clock-tick :receive clock-receive)
(defdevice keyboard :id #x30cf7406 :version 1 :manufacturer #x1c6c8b36
  :init keyboard-init :receive keyboard-receive)

;;; Storage. REG is a bank of eight registers whose :NAMES let source write
;;; `a` for register 0. SP is a plain register that STACK-POINTER turns into a
;;; downward-growing stack in RAM, so PUSH, POP and interrupt delivery use it.
;;; See docs/machine-model.md.
(defmachine dcpu16
  (register reg :width 16 :names (a b c x y z i j))
  (register pc :width 16)
  (register sp :width 16)
  (register ex :width 16)
  (register ia :width 16)
  (flags queueing)
  (memory ram :width 16 :addr-width 16 :cell-width 16)
  (stack-pointer sp :memory ram :grows :down)
  ;; Delivery pushes PC then A and sets A to the message; RFI pops in reverse.
  ;; Queueing is on while a handler runs. See docs/interrupts.md.
  (interrupts :vector ia :message (reg 0) :save (pc (reg 0)) :stack sp
              :queue 256 :on-overflow :trap
              :mask-flag queueing :mask-on-deliver t)
  ;; `a`'s next word comes before `b`'s. See docs/word-instructions.md.
  (instruction-word :width 16
    (field av 6)
    (field bv 5)
    (field opcode 5)
    (extra-word-order av bv))
  (clock-speed 100000)
  (undefined-opcode :fault)
  (devices clock keyboard))

;;; Operand syntax. Each `defmode` is one way to write a value; a register-
;;; qualified hole makes `[a]` a register lookup and `[5]` a memory address.
;;; `a` and `b` accept different sets, so each slot is its own `one-of`.
;;; See docs/modes.md and docs/operand-modes.md.
(defmode d-reg (expr :register reg))
(defmode d-regind "[" (expr :register reg) "]")
(defmode d-idx "[" (expr :register reg) "+" expr "]"
  :spelling ("[" (hole 1) "+" (hole 0) "]"))
(defmode d-mem "[" expr "]")
(defmode d-lit expr)
(defmode d-sp "sp")
(defmode d-pc "pc")
(defmode d-ex "ex")
(defmode d-peek "peek" :spelling ("[" "sp" "]"))
(defmode d-pick "pick" expr :spelling ("[" "sp" "+" (hole 0) "]"))
(defmode d-pop "pop")
(defmode d-push "push")

(defmode d-common
  (one-of d-reg d-regind d-idx d-mem d-lit d-sp d-pc d-ex d-peek d-pick))

;; A basic instruction is `op b, a`; a special one is `op a`.
(defmode d-ba
  (one-of (b-slot d-push d-common)) ","
  (one-of (a-slot d-pop d-common)))
(defmode d-a
  (one-of (a-slot d-pop d-common)))

;;; Operand encoding. Each alternative fills the 6-bit `a` field or 5-bit `b`
;;; field with its own value range; those without a hole pin a constant with
;;; `field-value`. `a` also packs -1..30 into the field itself (`:wrap 16` makes
;;; 0xffff pack as -1), and any other literal spends the next word. See
;;; docs/word-instructions.md.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun operand-clauses (slot field reg regind idx off mem lit pick)
    `((for-choice (,slot d-common d-reg)
        (operand ,reg :field ,field
          (variant (choice (d-common d-reg)) inline :range (0 7) :bias #x00)))
      (for-choice (,slot d-common d-regind)
        (operand ,regind :field ,field
          (variant (choice (d-common d-regind)) inline :range (0 7) :bias #x08)))
      (for-choice (,slot d-common d-idx)
        (operand ,idx :field ,field
          (variant (choice (d-common d-idx)) inline :range (0 7) :bias #x10))
        (operand ,off :trailing-word))
      (for-choice (,slot d-common d-mem)
        (operand ,mem :field ,field
          (variant (choice (d-common d-mem)) (extra-word :escape #x1e))))
      (for-choice (,slot d-common d-pick)
        (operand ,pick :field ,field
          (variant (choice (d-common d-pick)) (extra-word :escape #x1a))))
      (for-choice (,slot d-common d-lit)
        (operand ,lit :field ,field
          ,@(when (eq slot 'a-slot)
              '((variant (choice (d-common d-lit)) inline :range (-1 30) :bias 33 :wrap 16)))
          (variant (choice (d-common d-lit)) (extra-word :escape #x1f))))
      (for-choice (,slot d-common d-peek) (field-value ,field #x19))
      (for-choice (,slot d-common d-sp) (field-value ,field #x1b))
      (for-choice (,slot d-common d-pc) (field-value ,field #x1c))
      (for-choice (,slot d-common d-ex) (field-value ,field #x1d))
      (for-choice (,slot ,(if (eq slot 'a-slot) 'd-pop 'd-push))
        (field-value ,field #x18)))))

;;; Operand access. An operand resolves to a place: (VALUES KIND DATUM) with
;;; KIND one of :REG :MEM :SP :PC :EX :LIT. Resolving runs PUSH's and POP's
;;; SP change once, and costs one cycle for each next word, so an operand can
;;; then be read and written any number of times.
(defun place-get (machine kind datum)
  (ecase kind
    (:reg (regref machine 'reg datum))
    (:mem (mref machine 'ram datum))
    (:sp (sref machine 'sp))
    (:pc (sref machine 'pc))
    (:ex (sref machine 'ex))
    (:lit datum)))

(defun place-put (machine kind datum value)
  (ecase kind
    (:reg (setf (regref machine 'reg datum) value))
    (:mem (setf (mref machine 'ram datum) value))
    (:sp (setf (sref machine 'sp) value))
    (:pc (setf (sref machine 'pc) value))
    (:ex (setf (sref machine 'ex) value))
    (:lit nil)))

(defmacro wrap (value)
  `(wrap-value ,value 16))

;; `choice-case` dispatches on which alternative the operand matched.
(defmacro resolve-operand (slot stack-form literal-form reg regind idx off mem pick)
  `(choice-case ,slot
     ,stack-form
     (d-common
      (choice-case (,slot d-common)
        (d-reg (values :reg ,reg))
        (d-regind (values :mem (regref machine 'reg ,regind)))
        (d-idx (values :mem (wrap (+ (regref machine 'reg ,idx) ,off))))
        (d-mem (values :mem (wrap ,mem)))
        (d-lit ,literal-form)
        (d-sp (values :sp 0))
        (d-pc (values :pc 0))
        (d-ex (values :ex 0))
        (d-peek (values :mem (sref machine 'sp)))
        (d-pick (values :mem (wrap (+ (sref machine 'sp) ,pick))))))))

(defmacro a-place ()
  `(resolve-operand a-slot
     (d-pop (let ((address (sref machine 'sp)))
              (setf (sref machine 'sp) (wrap (1+ address)))
              (values :mem address)))
     (values :lit (wrap alit))
     areg aregind aidx aoff amem apick))

(defmacro b-place ()
  `(resolve-operand b-slot
     (d-push (values :mem (setf (sref machine 'sp) (wrap (1- (sref machine 'sp))))))
     (values :lit (wrap blit))
     breg bregind bidx boff bmem bpick))

;; SRC is the value of `a`, DST is the value of `b`, and (STORE v) writes `b`.
;; Each next word costs a cycle.
(defmacro with-operands (&body body)
  `(multiple-value-bind (a-kind a-datum) (progn (elapse (1- (instruction-size))) (a-place))
     (let ((src (place-get machine a-kind a-datum)))
       (declare (ignorable src))
       (multiple-value-bind (b-kind b-datum) (b-place)
         (symbol-macrolet ((dst (place-get machine b-kind b-datum)))
           (flet ((store (value) (place-put machine b-kind b-datum value)))
             (declare (ignorable #'store))
             ,@body))))))

(defmacro defbasic (name opcode cycles &body semantics)
  `(definstruction dcpu16 ,name
     (modes d-ba)
     (encoding
       (opcode ,opcode)
       ,@(operand-clauses 'b-slot 'bv 'breg 'bregind 'bidx 'boff 'bmem 'blit 'bpick)
       ,@(operand-clauses 'a-slot 'av 'areg 'aregind 'aidx 'aoff 'amem 'alit 'apick))
     (cycles ,cycles)
     (semantics (with-operands ,@semantics))))

;; A special opcode sits in the `b` field of an opcode-0 word, so each one
;; pins `bv` with `field-value`.
(defmacro defspecial (name code cycles &body semantics)
  `(definstruction dcpu16 ,name
     (modes d-a)
     (encoding
       (opcode 0)
       (field-value bv ,code)
       ,@(operand-clauses 'a-slot 'av 'areg 'aregind 'aidx 'aoff 'amem 'alit 'apick))
     (cycles ,cycles)
     (semantics
       (elapse (1- (instruction-size)))
       (multiple-value-bind (a-kind a-datum) (a-place)
         (let ((src (place-get machine a-kind a-datum)))
           (declare (ignorable src))
           (flet ((store (value) (place-put machine a-kind a-datum value)))
             (declare (ignorable #'store))
             ,@semantics))))))

;;; Arithmetic.
(defmacro signed (value)
  `(signed-value ,value 16))

(defmacro set-ex (value)
  `(set! ex (logand ,value #xffff)))

(defbasic set #x01 1
  (store src))

(defbasic add #x02 2
  (let ((sum (+ dst src)))
    (store (wrap sum))
    (set-ex (if (> sum #xffff) 1 0))))

(defbasic sub #x03 2
  (let ((difference (- dst src)))
    (store (wrap difference))
    (set-ex (if (minusp difference) #xffff 0))))

(defbasic mul #x04 2
  (let ((product (* dst src)))
    (store (wrap product))
    (set-ex (ash product -16))))

(defbasic mli #x05 2
  (let ((product (* (signed dst) (signed src))))
    (store (wrap product))
    (set-ex (ash product -16))))

(defbasic div #x06 3
  (cond ((zerop src) (store 0) (set-ex 0))
        (t (let ((dividend dst))
             (store (wrap (floor dividend src)))
             (set-ex (floor (ash dividend 16) src))))))

(defbasic dvi #x07 3
  (cond ((zerop src) (store 0) (set-ex 0))
        (t (let ((dividend (signed dst)) (divisor (signed src)))
             (store (wrap (truncate dividend divisor)))
             (set-ex (truncate (ash dividend 16) divisor))))))

(defbasic mod #x08 3
  (store (if (zerop src) 0 (mod dst src))))

(defbasic mdi #x09 3
  (store (if (zerop src) 0 (wrap (rem (signed dst) (signed src))))))

(defbasic and #x0a 1
  (store (logand dst src)))

(defbasic bor #x0b 1
  (store (logior dst src)))

(defbasic xor #x0c 1
  (store (logxor dst src)))

(defbasic shr #x0d 1
  (let ((value dst))
    (store (wrap (ash value (- src))))
    (set-ex (ash (ash value 16) (- src)))))

(defbasic asr #x0e 1
  (let ((value dst))
    (store (wrap (ash (signed value) (- src))))
    (set-ex (ash (logand (ash (signed value) 16) #xffffffff) (- src)))))

(defbasic shl #x0f 1
  (let ((shifted (ash dst (min src 32))))
    (store (wrap shifted))
    (set-ex (ash shifted -16))))

(defbasic adx #x1a 3
  (let ((sum (+ dst src ex)))
    (store (wrap sum))
    (set-ex (if (> sum #xffff) 1 0))))

(defbasic sbx #x1b 3
  (let ((difference (+ (- dst src) ex)))
    (store (wrap difference))
    (set-ex (if (minusp difference) #xffff 0))))

(defbasic sti #x1e 2
  (store src)
  (set! i (wrap (1+ i)))
  (set! j (wrap (1+ j))))

(defbasic std #x1f 2
  (store src)
  (set! i (wrap (1- i)))
  (set! j (wrap (1- j))))

;;; Conditionals. A failed test skips the next instruction and, while the
;;; skipped one is itself an IF, the one after it, at one cycle each. The
;;; skipped instruction is decoded but not run, so its PUSH or POP does nothing.
(defmacro defconditional (name opcode test)
  `(defbasic ,name ,opcode 2
     (unless ,test
       (loop for skipped = (skip-instruction)
             do (elapse 1)
             while (and (instruction-descriptor-p skipped)
                        (string= "IF" (instruction-descriptor-name skipped) :end2 2))))))

(defconditional ifb #x10 (/= 0 (logand dst src)))
(defconditional ifc #x11 (= 0 (logand dst src)))
(defconditional ife #x12 (= dst src))
(defconditional ifn #x13 (/= dst src))
(defconditional ifg #x14 (> dst src))
(defconditional ifa #x15 (> (signed dst) (signed src)))
(defconditional ifl #x16 (< dst src))
(defconditional ifu #x17 (< (signed dst) (signed src)))

;;; Special instructions. `push` and `pop` are lasm's stack operators; SP is
;;; the machine's stack pointer.
(defspecial jsr #x01 3
  (push pc sp)
  (set! pc src))

;; Delivery and the trap frame are the `interrupts` clause above.
(defspecial int #x08 4
  (signal-interrupt machine src))

(defspecial iag #x09 1
  (store ia))

(defspecial ias #x0a 1
  (set! ia src))

(defspecial rfi #x0b 3
  (set-flags! (queueing nil))
  (interrupt-return))

(defspecial iaq #x0c 2
  (set-flags! (queueing (/= src 0))))

;;; Hardware. A device number that names no device reads as all zeros.
(defspecial hwn #x10 2
  (store (device-count machine)))

(defspecial hwq #x11 4
  (multiple-value-bind (id version manufacturer)
      (handler-case (device-info machine src)
        (no-such-device () (values 0 0 0)))
    (setf (regref machine 'reg 0) (ldb (byte 16 0) id)
          (regref machine 'reg 1) (ldb (byte 16 16) id)
          (regref machine 'reg 2) version
          (regref machine 'reg 3) (ldb (byte 16 0) manufacturer)
          (regref machine 'reg 4) (ldb (byte 16 16) manufacturer))))

(defspecial hwi #x12 4
  (handler-case (device-send machine src)
    (no-such-device () nil)))

;;; API.
(defun load-dcpu16 (source)
  "A machine with the assembled SOURCE loaded at address 0."
  (let ((machine (make-machine 'dcpu16)))
    (load-program machine (assemble source :cpu 'dcpu16))
    machine))

(defun reg-value (machine name)
  "The value of register NAME, one of :A :B :C :X :Y :Z :I :J."
  (regref machine 'reg (position name '(:a :b :c :x :y :z :i :j))))

(defun run-dcpu16 (machine &key (max-steps 100000))
  "Step MACHINE until an instruction leaves PC where it found it, the way DCPU-16
programs halt (`:end set pc, end`), and return the steps taken."
  (loop for steps from 1 to max-steps
        for pc = (sref machine 'pc)
        do (step-machine machine)
        when (= pc (sref machine 'pc))
          do (return steps)
        finally (return max-steps)))
