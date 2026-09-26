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
             (semantics (set! (r dst) value)))))

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
