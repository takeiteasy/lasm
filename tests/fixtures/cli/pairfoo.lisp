;;;; tests/fixtures/cli/pairfoo.lisp
;;;; #416: an 8-bit machine whose 16-bit language words live in register pairs
;;;; (docs/register-pairs.md). Eight 8-bit registers, 8-bit cells, a carry
;;;; chain (ADD/ADC, SUB/SBC, CP/CPC) and one-byte pushes, so a backend spells
;;;; each language operation as its halves' instructions. The MULW family and
;;;; LDW/STW work on two register pairs at once. Little-endian, like memory's
;;;; default.
;;;;
;;;;   lasm run fact.lsp -m pairfoo.lisp --backend pairfoo-lang-abi

(defmachine pairfoo
  (register pc :width 16)
  (register sp :width 16)
  (register r :width 8 :names (a b c d e f g h))
  (flags cf zf nf vf)
  (memory ram :width 8 :addr-width 16)
  (stack-pointer sp :memory ram :grows :down))

(defmode pf-reg (expr :register r))
(defmode pf-imm "#" expr)
(defmode pf-rr (expr :register r) "," (expr :register r))
(defmode pf-ri (expr :register r) "," "#" expr)
(defmode pf-rs (expr :register r) "," "[" "sp" "+" expr "]")
(defmode pf-sr "[" "sp" "+" expr "]" "," (expr :register r))
(defmode pf-sp-idx "[" "sp" "+" expr "]")
(defmode pf-sp "sp")
(defmode pf-spi "sp" "," "#" expr)
(defmode pf-rrt (expr :register r) "," (expr :register r) "," expr)
(defmode pf-rrrr (expr :register r) "," (expr :register r) "," (expr :register r) "," (expr :register r))

(definstruction pairfoo hlt
  (encoding (opcode 0))
  (semantics (trap :halt)))

(definstruction pairfoo ldi (modes pf-ri)
  (encoding (opcode 1) (operand dst :width 1) (operand value :width 1))
  (semantics (set! (r dst) (logand value 255))))

(definstruction pairfoo movv (modes pf-rr)
  (encoding (opcode 2) (operand dst :width 1) (operand src :width 1))
  (semantics (set! (r dst) (r src))))

(definstruction pairfoo lds (modes pf-rs)
  (encoding (opcode 3) (operand dst :width 1) (operand offset :width 1))
  (semantics (set! (r dst) (mref machine 'ram (wrap-value (+ sp offset) 16)))))

(definstruction pairfoo sts (modes pf-sr)
  (encoding (opcode 4) (operand offset :width 1) (operand src :width 1))
  (semantics (set! (mref machine 'ram (wrap-value (+ sp offset) 16)) (r src))))

(definstruction pairfoo pushr (modes pf-reg)
  (encoding (opcode 5) (operand src :width 1))
  (semantics (push (r src) sp)))

(definstruction pairfoo popr (modes pf-reg)
  (encoding (opcode 6) (operand dst :width 1))
  (semantics (set! (r dst) (pop sp))))

(definstruction pairfoo subs (modes pf-spi)
  (encoding (opcode 7) (operand :mode))
  (semantics (set! sp (wrap-value (- sp operand) 16))))

(definstruction pairfoo adds (modes pf-spi)
  (encoding (opcode 8) (operand :mode))
  (semantics (set! sp (wrap-value (+ sp operand) 16))))

;; The return address goes on the stack a byte at a time, high byte first, so
;; it lies in memory as a little-endian word.
(definstruction pairfoo call (modes absolute)
  (encoding (opcode 9) (operand :mode))
  (semantics
    (push (logand (ash pc -8) 255) sp)
    (push (logand pc 255) sp)
    (set! pc operand)))

(definstruction pairfoo callp (modes pf-rr)
  (encoding (opcode 10) (operand hi :width 1) (operand lo :width 1))
  (semantics
    (push (logand (ash pc -8) 255) sp)
    (push (logand pc 255) sp)
    (set! pc (logior (ash (r hi) 8) (r lo)))))

(definstruction pairfoo ret
  (encoding (opcode 11))
  (semantics
    (let* ((lo (pop sp)) (hi (pop sp)))
      (set! pc (logior lo (ash hi 8))))))

(definstruction pairfoo jmp (modes absolute)
  (encoding (opcode 12) (operand :mode))
  (semantics (set! pc operand)))

;; Jump when both registers of a pair are zero.
(definstruction pairfoo jzp (modes pf-rrt)
  (encoding (opcode 13) (operand hi :width 1) (operand lo :width 1) (operand target :width 2))
  (semantics (when (and (zerop (r hi)) (zerop (r lo))) (set! pc target))))

;; A word through the address in a pair, two cells little-endian.
(definstruction pairfoo ldw (modes pf-rrrr)
  (encoding (opcode 14) (operand dh :width 1) (operand dl :width 1) (operand ah :width 1) (operand al :width 1))
  (semantics
    (let* ((address (logior (ash (r ah) 8) (r al)))
           (lo (mref machine 'ram address))
           (hi (mref machine 'ram (wrap-value (1+ address) 16))))
      (set! (r dl) lo)
      (set! (r dh) hi))))

(definstruction pairfoo stw (modes pf-rrrr)
  (encoding (opcode 15) (operand ah :width 1) (operand al :width 1) (operand sh :width 1) (operand sl :width 1))
  (semantics
    (let ((address (logior (ash (r ah) 8) (r al))))
      (set! (mref machine 'ram address) (r sl))
      (set! (mref machine 'ram (wrap-value (1+ address) 16)) (r sh)))))

;; The carry chain: ADD then ADC, SUB then SBC, CP then CPC.
(definstruction pairfoo add (modes pf-rr)
  (encoding (opcode 16) (operand dst :width 1) (operand src :width 1))
  (semantics
    (let ((sum (+ (r dst) (r src))))
      (set! (r dst) (logand sum 255))
      (set! cf (if (> sum 255) 1 0)))))

(definstruction pairfoo adc (modes pf-rr)
  (encoding (opcode 17) (operand dst :width 1) (operand src :width 1))
  (semantics
    (let ((sum (+ (r dst) (r src) cf)))
      (set! (r dst) (logand sum 255))
      (set! cf (if (> sum 255) 1 0)))))

(definstruction pairfoo sub (modes pf-rr)
  (encoding (opcode 18) (operand dst :width 1) (operand src :width 1))
  (semantics
    (let ((difference (- (r dst) (r src))))
      (set! (r dst) (logand difference 255))
      (set! cf (if (minusp difference) 1 0)))))

(definstruction pairfoo sbc (modes pf-rr)
  (encoding (opcode 19) (operand dst :width 1) (operand src :width 1))
  (semantics
    (let ((difference (- (r dst) (r src) cf)))
      (set! (r dst) (logand difference 255))
      (set! cf (if (minusp difference) 1 0)))))

(defmacro defcompare (mnemonic opcode carry-in zero-in)
  "MNEMONIC dst, src sets the flags from dst - src, and, for the second half of a word, the borrow
and the zero flag of the first."
  `(definstruction pairfoo ,mnemonic (modes pf-rr)
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

(defcompare cp 20 0 t)
(defcompare cpc 21 cf (= zf 1))

(defmacro defsetter (mnemonic opcode condition)
  `(definstruction pairfoo ,mnemonic (modes pf-reg)
     (encoding (opcode ,opcode) (operand dst :width 1))
     (semantics (set! (r dst) (if ,condition 1 0)))))

(defsetter seteq 22 (= zf 1))
(defsetter setne 23 (= zf 0))
(defsetter setlt 24 (/= nf vf))
(defsetter setge 25 (= nf vf))

(defmacro defbitwise (mnemonic opcode function)
  `(definstruction pairfoo ,mnemonic (modes pf-rr)
     (encoding (opcode ,opcode) (operand dst :width 1) (operand src :width 1))
     (semantics (set! (r dst) (,function (r dst) (r src))))))

(defbitwise andr 26 logand)
(defbitwise orr 27 logior)
(defbitwise xorr 28 logxor)

;; MNEMONIC dh, dl, sh, sl sets the pair dh:dl to EXPRESSION of the words x and y, signed as sx and sy.
(defmacro defword (mnemonic opcode expression)
  `(definstruction pairfoo ,mnemonic (modes pf-rrrr)
     (encoding (opcode ,opcode) (operand dh :width 1) (operand dl :width 1)
               (operand sh :width 1) (operand sl :width 1))
     (semantics
       (let* ((x (logior (ash (r dh) 8) (r dl))) (y (logior (ash (r sh) 8) (r sl)))
              (sx (if (>= x 32768) (- x 65536) x))
              (sy (if (>= y 32768) (- y 65536) y))
              (result (wrap-value ,expression 16)))
         (declare (ignorable sx sy))
         (set! (r dh) (ash result -8))
         (set! (r dl) (logand result 255))))))

(defword mulw 29 (* sx sy))
(defword divw 30 (if (zerop sy) 0 (truncate sx sy)))
(defword modw 31 (if (zerop sy) 0 (rem sx sy)))
(defword shlw 32 (ash x y))
(defword shrw 33 (ash x (- y)))

;; Every value is a pair: ab is the accumulator, cd the right operand. A word
;; goes on the stack a byte at a time, high byte first, and a frame slot is
;; two cells, low byte first, so (:lo slot) is the cell at the slot's offset.
(defbackend pairfoo-lang-abi (:isa pairfoo)
  (registers :pairs ((ab a b) (cd c d) (ef e f) (gh g h))
             :return (ab) :scratch (ab cd) :callee-saved (ef gh)
             :stack-pointer sp :program-counter pc :operand reg)
  (call :args :stack :order :right-to-left :cleanup :caller :return-address-slots 1)
  (frame :grows :down :slot sp-idx :offsets :cells :counts :cells)
  (operands (reg pf-reg) (imm pf-imm) (sp-idx pf-sp-idx) (sp pf-sp))
  (ops (:push (x) (pushr (:hi x)) (pushr (:lo x)))
       (:pop (x) (popr (:lo x)) (popr (:hi x)))
       (:move (d (s sp-idx)) (lds (:lo d) (:lo s)) (lds (:hi d) (:hi s)))
       (:move (d s) (movv (:lo d) (:lo s)) (movv (:hi d) (:hi s)))
       (:alloc (n) (subs (sp) (imm n)))
       (:free (n) (adds (sp) (imm n)))
       (:call ((f reg)) (callp (:hi f) (:lo f)))
       (:call (f) (call f))
       (:return () (ret))
       (:const (r v) (ldi (:lo r) (imm (:lo v))) (ldi (:hi r) (imm (:hi v))))
       (:get (r slot) (lds (:lo r) (:lo slot)) (lds (:hi r) (:hi slot)))
       (:set (slot r) (sts (:lo slot) (:lo r)) (sts (:hi slot) (:hi r)))
       (:peek (d a) (ldw (reg (:hi d)) (reg (:lo d)) (reg (:hi a)) (reg (:lo a))))
       (:poke (a s) (stw (reg (:hi a)) (reg (:lo a)) (reg (:hi s)) (reg (:lo s))))
       (:jump (target) (jmp target))
       (:branch-zero (r target) (jzp (:hi r) (:lo r) target))
       (:halt () (hlt))
       (:add (d s) (add (:lo d) (:lo s)) (adc (:hi d) (:hi s)))
       (:sub (d s) (sub (:lo d) (:lo s)) (sbc (:hi d) (:hi s)))
       (:and (d s) (andr (:lo d) (:lo s)) (andr (:hi d) (:hi s)))
       (:or (d s) (orr (:lo d) (:lo s)) (orr (:hi d) (:hi s)))
       (:xor (d s) (xorr (:lo d) (:lo s)) (xorr (:hi d) (:hi s)))
       (:mul (d s) (mulw (:hi d) (:lo d) (:hi s) (:lo s)))
       (:div (d s) (divw (:hi d) (:lo d) (:hi s) (:lo s)))
       (:mod (d s) (modw (:hi d) (:lo d) (:hi s) (:lo s)))
       (:shl (d s) (shlw (:hi d) (:lo d) (:hi s) (:lo s)))
       (:shr (d s) (shrw (:hi d) (:lo d) (:hi s) (:lo s)))
       (:eq (d s) (cp (:lo d) (:lo s)) (cpc (:hi d) (:hi s)) (seteq (:lo d)) (ldi (:hi d) (imm 0)))
       (:ne (d s) (cp (:lo d) (:lo s)) (cpc (:hi d) (:hi s)) (setne (:lo d)) (ldi (:hi d) (imm 0)))
       (:lt (d s) (cp (:lo d) (:lo s)) (cpc (:hi d) (:hi s)) (setlt (:lo d)) (ldi (:hi d) (imm 0)))
       (:ge (d s) (cp (:lo d) (:lo s)) (cpc (:hi d) (:hi s)) (setge (:lo d)) (ldi (:hi d) (imm 0)))
       (:gt (d s) (cp (:lo s) (:lo d)) (cpc (:hi s) (:hi d)) (setlt (:lo d)) (ldi (:hi d) (imm 0)))
       (:le (d s) (cp (:lo s) (:lo d)) (cpc (:hi s) (:hi d)) (setge (:lo d)) (ldi (:hi d) (imm 0)))))

;; The first argument goes in cd. ef is a second scratch pair, so a stack argument
;; can pass through it while a computed call target sits in ab.
(defbackend pairfoo-lang-reg-abi (:extends pairfoo-lang-abi)
  (registers :scratch (ab ef) :caller-saved (cd) :callee-saved (gh))
  (call :args (cd) :order :right-to-left :cleanup :caller :return-address-slots 1))
