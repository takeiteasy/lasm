;;;; examples/cli/callfoo.lisp
;;;; A machine with register names, a memory stack and a call instruction, and
;;;; the backend a front end targets it through (#113):
;;;;
;;;;   lasm run double.lasm -m callfoo.lisp

(defmachine callfoo
  (register pc :width 16)
  (register sp :width 16)
  (register r :width 16 :names (a b c d))
  (memory ram :width 16 :addr-width 16)
  (stack-pointer sp :memory ram :grows :down))

(defmode call-reg (expr :register r))
(defmode call-imm "#" expr)
(defmode call-sp-idx "[" "sp" "+" expr "]")
(defmode call-sp "sp")
(defmode call-rr (expr :register r) "," (expr :register r))
(defmode call-ri (expr :register r) "," "#" expr)
(defmode call-rs (expr :register r) "," "[" "sp" "+" expr "]")
(defmode call-spi "sp" "," "#" expr)
(defmode call-sr "[" "sp" "+" expr "]" "," (expr :register r))

(definstruction callfoo ldi (modes call-ri)
  (encoding (opcode 1) (operand dst :width 1) (operand value :width 1))
  (semantics (set! (r dst) value)))

(definstruction callfoo mov (modes call-rr)
  (encoding (opcode 2) (operand dst :width 1) (operand src :width 1))
  (semantics (set! (r dst) (r src))))

(definstruction callfoo lds (modes call-rs)
  (encoding (opcode 3) (operand dst :width 1) (operand offset :width 1))
  (semantics (set! (r dst) (mref machine 'ram (wrap-value (+ sp offset) 16)))))

(definstruction callfoo push (modes call-imm)
  (encoding (opcode 4) (operand :mode))
  (semantics (push operand sp)))

(definstruction callfoo add (modes call-rr)
  (encoding (opcode 5) (operand dst :width 1) (operand src :width 1))
  (semantics (set! (r dst) (wrap-value (+ (r dst) (r src)) 16))))

(definstruction callfoo adds (modes call-spi)
  (encoding (opcode 6) (operand :mode))
  (semantics (set! sp (wrap-value (+ sp operand) 16))))

(definstruction callfoo call (modes absolute)
  (encoding (opcode 7) (operand :mode))
  (semantics (push pc sp) (set! pc operand)))

(definstruction callfoo ret
  (encoding (opcode 8))
  (semantics (set! pc (pop sp))))

(definstruction callfoo hlt
  (encoding (opcode 0))
  (semantics (trap :halt)))

;; What convention lowering (docs/conventions.md) emits: a push that takes a
;; register, an immediate or a stack slot, a pop, a register move and a stack
;; adjustment.
(definstruction callfoo pushv
  (modes
    (call-reg (opcode 9) (operand src :width 1) (semantics (push (r src) sp)))
    (call-imm (opcode 10) (operand :mode) (semantics (push operand sp)))
    (call-sp-idx (opcode 11) (operand offset :width 1)
                 (semantics (push (mref machine 'ram (wrap-value (+ sp offset) 16)) sp)))))

(definstruction callfoo movv
  (modes
    (call-rr (opcode 12) (operand dst :width 1) (operand src :width 1)
             (semantics (set! (r dst) (r src))))
    (call-ri (opcode 13) (operand dst :width 1) (operand value :width 1)
             (semantics (set! (r dst) value)))
    (call-rs (opcode 68) (operand dst :width 1) (operand offset :width 1)
             (semantics (set! (r dst) (mref machine 'ram (wrap-value (+ sp offset) 16)))))))

(definstruction callfoo xchg (modes call-rr)
  (encoding (opcode 25) (operand lhs :width 1) (operand rhs :width 1))
  (semantics (let ((old (r lhs))) (set! (r lhs) (r rhs)) (set! (r rhs) old))))

(definstruction callfoo callr (modes call-reg)
  (encoding (opcode 24) (operand target :width 1))
  (semantics (push pc sp) (set! pc (r target))))

(definstruction callfoo popr (modes call-reg)
  (encoding (opcode 14) (operand dst :width 1))
  (semantics (set! (r dst) (pop sp))))

(definstruction callfoo subs (modes call-spi)
  (encoding (opcode 15) (operand :mode))
  (semantics (set! sp (wrap-value (- sp operand) 16))))

(definstruction callfoo sts (modes call-sr)
  (encoding (opcode 17) (operand offset :width 1) (operand src :width 1))
  (semantics (set! (mref machine 'ram (wrap-value (+ sp offset) 16)) (r src))))

;; Return and remove n cells: for a backend whose callee cleans up.
(definstruction callfoo retn (modes call-imm)
  (encoding (opcode 16) (operand :mode))
  (semantics (set! pc (pop sp)) (set! sp (wrap-value (+ sp operand) 16))))

;; What the language compiler (docs/language.md) emits: memory access through
;; a register, jumps, and arithmetic and comparisons that leave their result in
;; the first register. Values are signed where it matters.
(defmode call-ind "[" (expr :register r) "]")
(defmode call-rind (expr :register r) "," "[" (expr :register r) "]")
(defmode call-indr "[" (expr :register r) "]" "," (expr :register r))
(defmode call-rt (expr :register r) "," expr)

(definstruction callfoo ldx (modes call-rind)
  (encoding (opcode 64) (operand dst :width 1) (operand addr :width 1))
  (semantics (set! (r dst) (mref machine 'ram (r addr)))))

(definstruction callfoo stx (modes call-indr)
  (encoding (opcode 65) (operand addr :width 1) (operand src :width 1))
  (semantics (set! (mref machine 'ram (r addr)) (r src))))

(definstruction callfoo jmp (modes absolute)
  (encoding (opcode 66) (operand :mode))
  (semantics (set! pc operand)))

(definstruction callfoo jz (modes call-rt)
  (encoding (opcode 67) (operand src :width 1) (operand target :width 1))
  (semantics (when (zerop (r src)) (set! pc target))))

(defmacro defarith (mnemonic opcode expression)
  "MNEMONIC dst, src sets dst to EXPRESSION of the signed values x and y."
  `(definstruction callfoo ,mnemonic (modes call-rr)
     (encoding (opcode ,opcode) (operand dst :width 1) (operand src :width 1))
     (semantics
       (let* ((x (r dst)) (y (r src))
              (sx (if (>= x 32768) (- x 65536) x))
              (sy (if (>= y 32768) (- y 65536) y)))
         (declare (ignorable sx sy))
         (set! (r dst) (wrap-value ,expression 16))))))

(defarith subr 69 (- x y))
(defarith mulr 70 (* sx sy))
(defarith divr 71 (if (zerop sy) 0 (truncate sx sy)))
(defarith modr 72 (if (zerop sy) 0 (rem sx sy)))
(defarith andr 73 (logand x y))
(defarith orr 74 (logior x y))
(defarith xorr 75 (logxor x y))
(defarith shlr 76 (ash x y))
(defarith shrr 77 (ash x (- y)))
(defarith seq 78 (if (= x y) 1 0))
(defarith sne 79 (if (/= x y) 1 0))
(defarith slt 80 (if (< sx sy) 1 0))
(defarith sgt 81 (if (> sx sy) 1 0))
(defarith sle 82 (if (<= sx sy) 1 0))
(defarith sge 83 (if (>= sx sy) 1 0))

;; Arguments go on the stack right to left and the caller removes them; a
;; function finds its first argument above the return address.
(defbackend callfoo-abi (:machine callfoo)
  (registers :return (a) :scratch (a b) :callee-saved (c d)
             :stack-pointer sp :program-counter pc :operand reg)
  (call :args :stack :order :right-to-left :cleanup :caller :return-address-slots 1)
  (frame :grows :down :slot sp-idx)
  (operands (reg call-reg) (imm call-imm) (sp-idx call-sp-idx) (sp call-sp))
  (ops (:load (r v) (ldi r (imm v)))
       (:add (d s) (add d s))
       (:push (x) (pushv x))
       (:pop (x) (popr x))
       (:move (d s) (movv d s))
       (:alloc (n) (subs (sp) (imm n)))
       (:free (n) (adds (sp) (imm n)))
       (:call (f) (call f))
       (:return () (ret))))

;; The first two arguments go in b and c, and a is free to break an argument
;; swap.
(defbackend callfoo-reg-abi (:machine callfoo)
  (registers :return (a) :scratch (a) :caller-saved (b c) :callee-saved (d)
             :stack-pointer sp :program-counter pc :operand reg)
  (call :args (b c) :order :right-to-left :cleanup :caller :return-address-slots 1)
  (frame :grows :down :slot sp-idx)
  (operands (reg call-reg) (imm call-imm) (sp-idx call-sp-idx) (sp call-sp))
  (ops (:push (x) (pushv x))
       (:pop (x) (popr x))
       (:move (d s) (movv d s))
       (:alloc (n) (subs (sp) (imm n)))
       (:free (n) (adds (sp) (imm n)))
       (:call (f) (call f))
       (:return () (ret))))

;; callfoo-abi with what the language compiler needs. :peek and :poke take
;; register names, which the templates put inside a bracket operand.
(defbackend callfoo-lang-abi (:extends callfoo-abi)
  (operands (ind call-ind))
  (ops (:const (r v) (ldi r (imm v)))
       (:get (r slot) (lds r slot))
       (:set (slot r) (sts slot r))
       (:peek (d a) (ldx (reg d) (ind a)))
       (:poke (a s) (stx (ind a) (reg s)))
       (:jump (target) (jmp target))
       (:branch-zero (r target) (jz r target))
       (:halt () (hlt))
       (:sub (d s) (subr d s))
       (:mul (d s) (mulr d s))
       (:div (d s) (divr d s))
       (:mod (d s) (modr d s))
       (:and (d s) (andr d s))
       (:or (d s) (orr d s))
       (:xor (d s) (xorr d s))
       (:shl (d s) (shlr d s))
       (:shr (d s) (shrr d s))
       (:eq (d s) (seq d s))
       (:ne (d s) (sne d s))
       (:lt (d s) (slt d s))
       (:gt (d s) (sgt d s))
       (:le (d s) (sle d s))
       (:ge (d s) (sge d s))))
