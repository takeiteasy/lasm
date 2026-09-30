;;;; tests/fixtures/cli/quadfoo.lisp
;;;; #423: an 8-bit machine whose 32-bit language words live in four registers
;;;; (docs/register-words.md). Sixteen 8-bit registers, 8-bit cells, a carry chain
;;;; (ADD/ADC, SUB/SBC, CP/CPC), one-byte pushes and a two-byte return address, so
;;;; a backend spells each language operation as its four parts' instructions. The
;;;; MULQ family and LDQ/STQ work on whole words at once. Little-endian, like
;;;; memory's default.
;;;;
;;;;   lasm run fact32.lsp -m quadfoo.lisp --backend quadfoo-lang-abi

(defmachine quadfoo
  (register pc :width 16)
  (register sp :width 16)
  (register r :width 8 :names (a b c d e f g h i j k l m n o p))
  (flags cf zf nf vf)
  (memory ram :width 8 :addr-width 16)
  (stack-pointer sp :memory ram :grows :down))

(defmode qf-reg (expr :register r))
(defmode qf-imm "#" expr)
(defmode qf-rr (expr :register r) "," (expr :register r))
(defmode qf-ri (expr :register r) "," "#" expr)
(defmode qf-rs (expr :register r) "," "[" "sp" "+" expr "]")
(defmode qf-sr "[" "sp" "+" expr "]" "," (expr :register r))
(defmode qf-sp-idx "[" "sp" "+" expr "]")
(defmode qf-sp "sp")
(defmode qf-spi "sp" "," "#" expr)
(defmode qf-rrt (expr :register r) "," (expr :register r) "," expr)
(defmode qf-r4t (expr :register r) "," (expr :register r) "," (expr :register r) "," (expr :register r) "," expr)
(defmode qf-r6 (expr :register r) "," (expr :register r) "," (expr :register r) "," (expr :register r) "," (expr :register r) "," (expr :register r))
(defmode qf-r8 (expr :register r) "," (expr :register r) "," (expr :register r) "," (expr :register r) "," (expr :register r) "," (expr :register r) "," (expr :register r) "," (expr :register r))

(definstruction quadfoo hlt
  (encoding (opcode 0))
  (semantics (trap :halt)))

(definstruction quadfoo ldi (modes qf-ri)
  (encoding (opcode 1) (operand dst :width 1) (operand value :width 1))
  (semantics (set! (r dst) (logand value 255))))

(definstruction quadfoo movv (modes qf-rr)
  (encoding (opcode 2) (operand dst :width 1) (operand src :width 1))
  (semantics (set! (r dst) (r src))))

(definstruction quadfoo lds (modes qf-rs)
  (encoding (opcode 3) (operand dst :width 1) (operand offset :width 1))
  (semantics (set! (r dst) (mref machine 'ram (wrap-value (+ sp offset) 16)))))

(definstruction quadfoo sts (modes qf-sr)
  (encoding (opcode 4) (operand offset :width 1) (operand src :width 1))
  (semantics (set! (mref machine 'ram (wrap-value (+ sp offset) 16)) (r src))))

(definstruction quadfoo pushr (modes qf-reg)
  (encoding (opcode 5) (operand src :width 1))
  (semantics (push (r src) sp)))

(definstruction quadfoo popr (modes qf-reg)
  (encoding (opcode 6) (operand dst :width 1))
  (semantics (set! (r dst) (pop sp))))

(definstruction quadfoo subs (modes qf-spi)
  (encoding (opcode 7) (operand :mode))
  (semantics (set! sp (wrap-value (- sp operand) 16))))

(definstruction quadfoo adds (modes qf-spi)
  (encoding (opcode 8) (operand :mode))
  (semantics (set! sp (wrap-value (+ sp operand) 16))))

;; The return address goes on the stack a byte at a time, high byte first, so
;; it lies in memory as a little-endian word.
(definstruction quadfoo call (modes absolute)
  (encoding (opcode 9) (operand :mode))
  (semantics
    (push (logand (ash pc -8) 255) sp)
    (push (logand pc 255) sp)
    (set! pc operand)))

(definstruction quadfoo callp (modes qf-rr)
  (encoding (opcode 10) (operand hi :width 1) (operand lo :width 1))
  (semantics
    (push (logand (ash pc -8) 255) sp)
    (push (logand pc 255) sp)
    (set! pc (logior (ash (r hi) 8) (r lo)))))

(definstruction quadfoo ret
  (encoding (opcode 11))
  (semantics
    (let* ((lo (pop sp)) (hi (pop sp)))
      (set! pc (logior lo (ash hi 8))))))

(definstruction quadfoo jmp (modes absolute)
  (encoding (opcode 12) (operand :mode))
  (semantics (set! pc operand)))

;; Jump when all four registers of a word are zero.
(definstruction quadfoo jzq (modes qf-r4t)
  (encoding (opcode 13) (operand p3 :width 1) (operand p2 :width 1) (operand p1 :width 1) (operand p0 :width 1)
            (operand target :width 2))
  (semantics (when (and (zerop (r p3)) (zerop (r p2)) (zerop (r p1)) (zerop (r p0))) (set! pc target))))

;; A word through the address in two registers, four cells little-endian.
(definstruction quadfoo ldq (modes qf-r6)
  (encoding (opcode 14) (operand d3 :width 1) (operand d2 :width 1) (operand d1 :width 1) (operand d0 :width 1)
            (operand ah :width 1) (operand al :width 1))
  (semantics
    (let ((address (logior (ash (r ah) 8) (r al))))
      (set! (r d0) (mref machine 'ram address))
      (set! (r d1) (mref machine 'ram (wrap-value (+ address 1) 16)))
      (set! (r d2) (mref machine 'ram (wrap-value (+ address 2) 16)))
      (set! (r d3) (mref machine 'ram (wrap-value (+ address 3) 16))))))

(definstruction quadfoo stq (modes qf-r6)
  (encoding (opcode 15) (operand ah :width 1) (operand al :width 1)
            (operand s3 :width 1) (operand s2 :width 1) (operand s1 :width 1) (operand s0 :width 1))
  (semantics
    (let ((address (logior (ash (r ah) 8) (r al))))
      (set! (mref machine 'ram address) (r s0))
      (set! (mref machine 'ram (wrap-value (+ address 1) 16)) (r s1))
      (set! (mref machine 'ram (wrap-value (+ address 2) 16)) (r s2))
      (set! (mref machine 'ram (wrap-value (+ address 3) 16)) (r s3)))))

;; The carry chain: ADD then ADC, SUB then SBC, CP then CPC.
(definstruction quadfoo add (modes qf-rr)
  (encoding (opcode 16) (operand dst :width 1) (operand src :width 1))
  (semantics
    (let ((sum (+ (r dst) (r src))))
      (set! (r dst) (logand sum 255))
      (set! cf (if (> sum 255) 1 0)))))

(definstruction quadfoo adc (modes qf-rr)
  (encoding (opcode 17) (operand dst :width 1) (operand src :width 1))
  (semantics
    (let ((sum (+ (r dst) (r src) cf)))
      (set! (r dst) (logand sum 255))
      (set! cf (if (> sum 255) 1 0)))))

(definstruction quadfoo sub (modes qf-rr)
  (encoding (opcode 18) (operand dst :width 1) (operand src :width 1))
  (semantics
    (let ((difference (- (r dst) (r src))))
      (set! (r dst) (logand difference 255))
      (set! cf (if (minusp difference) 1 0)))))

(definstruction quadfoo sbc (modes qf-rr)
  (encoding (opcode 19) (operand dst :width 1) (operand src :width 1))
  (semantics
    (let ((difference (- (r dst) (r src) cf)))
      (set! (r dst) (logand difference 255))
      (set! cf (if (minusp difference) 1 0)))))

(defmacro qf-defcompare (mnemonic opcode carry-in zero-in)
  "MNEMONIC dst, src sets the flags from dst - src, and, for the second half of a word, the borrow
and the zero flag of the first."
  `(definstruction quadfoo ,mnemonic (modes qf-rr)
     (encoding (opcode ,opcode) (operand dst :width 1) (operand src :width 1))
     (semantics
       (let* ((x (r dst)) (y (r src))
              (sx (if (>= x 128) (- x 256) x))
              (sy (if (>= y 128) (- y 256) y))
              (borrow ,carry-in)
              (difference (- x y borrow))
              (signed (- sx sy borrow))
              (result (logand difference 255)))
         (set! cf (if (minusp difference) 1 0))
         (set! zf (if (and (zerop result) ,zero-in) 1 0))
         (set! nf (if (>= result 128) 1 0))
         (set! vf (if (or (< signed -128) (> signed 127)) 1 0))))))

(qf-defcompare cp 20 0 t)
(qf-defcompare cpc 21 cf (= zf 1))

(defmacro qf-defsetter (mnemonic opcode condition)
  `(definstruction quadfoo ,mnemonic (modes qf-reg)
     (encoding (opcode ,opcode) (operand dst :width 1))
     (semantics (set! (r dst) (if ,condition 1 0)))))

(qf-defsetter seteq 22 (= zf 1))
(qf-defsetter setne 23 (= zf 0))
(qf-defsetter setlt 24 (/= nf vf))
(qf-defsetter setge 25 (= nf vf))

(defmacro qf-defbitwise (mnemonic opcode function)
  `(definstruction quadfoo ,mnemonic (modes qf-rr)
     (encoding (opcode ,opcode) (operand dst :width 1) (operand src :width 1))
     (semantics (set! (r dst) (,function (r dst) (r src))))))

(qf-defbitwise andr 26 logand)
(qf-defbitwise orr 27 logior)
(qf-defbitwise xorr 28 logxor)

;; MNEMONIC d3, d2, d1, d0, s3, s2, s1, s0 sets the word d to EXPRESSION of the words x and y, signed as sx and sy.
(defmacro qf-defword (mnemonic opcode expression)
  `(definstruction quadfoo ,mnemonic (modes qf-r8)
     (encoding (opcode ,opcode) (operand d3 :width 1) (operand d2 :width 1) (operand d1 :width 1) (operand d0 :width 1)
               (operand s3 :width 1) (operand s2 :width 1) (operand s1 :width 1) (operand s0 :width 1))
     (semantics
       (let* ((x (logior (ash (r d3) 24) (ash (r d2) 16) (ash (r d1) 8) (r d0)))
              (y (logior (ash (r s3) 24) (ash (r s2) 16) (ash (r s1) 8) (r s0)))
              (sx (if (>= x #x80000000) (- x #x100000000) x))
              (sy (if (>= y #x80000000) (- y #x100000000) y))
              (result (wrap-value ,expression 32)))
         (declare (ignorable sx sy))
         (set! (r d3) (logand (ash result -24) 255))
         (set! (r d2) (logand (ash result -16) 255))
         (set! (r d1) (logand (ash result -8) 255))
         (set! (r d0) (logand result 255))))))

(qf-defword mulq 29 (* sx sy))
(qf-defword divq 30 (if (zerop sy) 0 (truncate sx sy)))
(qf-defword modq 31 (if (zerop sy) 0 (rem sx sy)))
(qf-defword shlq 32 (ash x y))
(qf-defword shrq 33 (ash x (- y)))

;; Every value is a word of four registers: wa is the accumulator, wb the right
;; operand. A word goes on the stack a byte at a time, high byte first, and a
;; frame slot is four cells, low byte first, so (:part k slot) is the cell at the
;; slot's offset plus k. The return address is two cells, not a word, and a
;; computed call reads the low two parts of its target.
(defbackend quadfoo-lang-abi (:isa quadfoo)
  (registers :words ((wa a b c d) (wb e f g h) (wc i j k l) (wd m n o p))
             :return (wa) :scratch (wa wb) :callee-saved (wc wd)
             :stack-pointer sp :program-counter pc :operand reg)
  (call :args :stack :order :right-to-left :cleanup :caller :return-address-cells 2)
  (frame :grows :down :slot sp-idx :offsets :cells :counts :cells)
  (operands (reg qf-reg) (imm qf-imm) (sp-idx qf-sp-idx) (sp qf-sp))
  (ops (:push (x) (pushr (:hi x)) (pushr (:part 2 x)) (pushr (:part 1 x)) (pushr (:lo x)))
       (:pop (x) (popr (:lo x)) (popr (:part 1 x)) (popr (:part 2 x)) (popr (:hi x)))
       (:move (d (s sp-idx))
        (lds (:lo d) (:lo s)) (lds (:part 1 d) (:part 1 s)) (lds (:part 2 d) (:part 2 s)) (lds (:hi d) (:hi s)))
       (:move (d s)
        (movv (:lo d) (:lo s)) (movv (:part 1 d) (:part 1 s)) (movv (:part 2 d) (:part 2 s)) (movv (:hi d) (:hi s)))
       (:alloc (n) (subs (sp) (imm n)))
       (:free (n) (adds (sp) (imm n)))
       (:call ((f reg)) (callp (:part 1 f) (:lo f)))
       (:call (f) (call f))
       (:return () (ret))
       (:const (r v)
        (ldi (:lo r) (imm (:lo v))) (ldi (:part 1 r) (imm (:part 1 v)))
        (ldi (:part 2 r) (imm (:part 2 v))) (ldi (:hi r) (imm (:hi v))))
       (:get (r slot)
        (lds (:lo r) (:lo slot)) (lds (:part 1 r) (:part 1 slot)) (lds (:part 2 r) (:part 2 slot)) (lds (:hi r) (:hi slot)))
       (:set (slot r)
        (sts (:lo slot) (:lo r)) (sts (:part 1 slot) (:part 1 r)) (sts (:part 2 slot) (:part 2 r)) (sts (:hi slot) (:hi r)))
       (:peek (d a) (ldq (reg (:hi d)) (reg (:part 2 d)) (reg (:part 1 d)) (reg (:lo d)) (reg (:part 1 a)) (reg (:lo a))))
       (:poke (a s) (stq (reg (:part 1 a)) (reg (:lo a)) (reg (:hi s)) (reg (:part 2 s)) (reg (:part 1 s)) (reg (:lo s))))
       (:jump (target) (jmp target))
       (:branch-zero (r target) (jzq (:hi r) (:part 2 r) (:part 1 r) (:lo r) target))
       (:halt () (hlt))
       (:add (d s) (add (:lo d) (:lo s)) (adc (:part 1 d) (:part 1 s)) (adc (:part 2 d) (:part 2 s)) (adc (:hi d) (:hi s)))
       (:sub (d s) (sub (:lo d) (:lo s)) (sbc (:part 1 d) (:part 1 s)) (sbc (:part 2 d) (:part 2 s)) (sbc (:hi d) (:hi s)))
       (:and (d s) (andr (:lo d) (:lo s)) (andr (:part 1 d) (:part 1 s)) (andr (:part 2 d) (:part 2 s)) (andr (:hi d) (:hi s)))
       (:or (d s) (orr (:lo d) (:lo s)) (orr (:part 1 d) (:part 1 s)) (orr (:part 2 d) (:part 2 s)) (orr (:hi d) (:hi s)))
       (:xor (d s) (xorr (:lo d) (:lo s)) (xorr (:part 1 d) (:part 1 s)) (xorr (:part 2 d) (:part 2 s)) (xorr (:hi d) (:hi s)))
       (:mul (d s) (mulq (:hi d) (:part 2 d) (:part 1 d) (:lo d) (:hi s) (:part 2 s) (:part 1 s) (:lo s)))
       (:div (d s) (divq (:hi d) (:part 2 d) (:part 1 d) (:lo d) (:hi s) (:part 2 s) (:part 1 s) (:lo s)))
       (:mod (d s) (modq (:hi d) (:part 2 d) (:part 1 d) (:lo d) (:hi s) (:part 2 s) (:part 1 s) (:lo s)))
       (:shl (d s) (shlq (:hi d) (:part 2 d) (:part 1 d) (:lo d) (:hi s) (:part 2 s) (:part 1 s) (:lo s)))
       (:shr (d s) (shrq (:hi d) (:part 2 d) (:part 1 d) (:lo d) (:hi s) (:part 2 s) (:part 1 s) (:lo s)))
       (:eq (d s) (cp (:lo d) (:lo s)) (cpc (:part 1 d) (:part 1 s)) (cpc (:part 2 d) (:part 2 s)) (cpc (:hi d) (:hi s))
        (seteq (:lo d)) (ldi (:part 1 d) (imm 0)) (ldi (:part 2 d) (imm 0)) (ldi (:hi d) (imm 0)))
       (:ne (d s) (cp (:lo d) (:lo s)) (cpc (:part 1 d) (:part 1 s)) (cpc (:part 2 d) (:part 2 s)) (cpc (:hi d) (:hi s))
        (setne (:lo d)) (ldi (:part 1 d) (imm 0)) (ldi (:part 2 d) (imm 0)) (ldi (:hi d) (imm 0)))
       (:lt (d s) (cp (:lo d) (:lo s)) (cpc (:part 1 d) (:part 1 s)) (cpc (:part 2 d) (:part 2 s)) (cpc (:hi d) (:hi s))
        (setlt (:lo d)) (ldi (:part 1 d) (imm 0)) (ldi (:part 2 d) (imm 0)) (ldi (:hi d) (imm 0)))
       (:ge (d s) (cp (:lo d) (:lo s)) (cpc (:part 1 d) (:part 1 s)) (cpc (:part 2 d) (:part 2 s)) (cpc (:hi d) (:hi s))
        (setge (:lo d)) (ldi (:part 1 d) (imm 0)) (ldi (:part 2 d) (imm 0)) (ldi (:hi d) (imm 0)))
       (:gt (d s) (cp (:lo s) (:lo d)) (cpc (:part 1 s) (:part 1 d)) (cpc (:part 2 s) (:part 2 d)) (cpc (:hi s) (:hi d))
        (setlt (:lo d)) (ldi (:part 1 d) (imm 0)) (ldi (:part 2 d) (imm 0)) (ldi (:hi d) (imm 0)))
       (:le (d s) (cp (:lo s) (:lo d)) (cpc (:part 1 s) (:part 1 d)) (cpc (:part 2 s) (:part 2 d)) (cpc (:hi s) (:hi d))
        (setge (:lo d)) (ldi (:part 1 d) (imm 0)) (ldi (:part 2 d) (imm 0)) (ldi (:hi d) (imm 0)))))

;; The first argument goes in wb. wc is a second scratch word, so a stack argument
;; can pass through it while a computed call target sits in wa.
(defbackend quadfoo-lang-reg-abi (:extends quadfoo-lang-abi)
  (registers :scratch (wa wc) :caller-saved (wb) :callee-saved (wd))
  (call :args (wb) :order :right-to-left :cleanup :caller :return-address-cells 2))
