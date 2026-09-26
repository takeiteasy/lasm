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

;; Arguments go on the stack right to left and the caller removes them; a
;; function finds its first argument above the return address.
(defbackend callfoo-abi (:machine callfoo)
  (registers :return (a) :scratch (a b) :callee-saved (c d)
             :stack-pointer sp :program-counter pc)
  (call :args :stack :order :right-to-left :cleanup :caller :return-address-slots 1)
  (frame :grows :down)
  (operands (reg call-reg) (imm call-imm) (sp-idx call-sp-idx) (sp call-sp))
  (ops (:load (r v) (ldi r (imm v)))
       (:add (d s) (add d s))
       (:call (f) (call f))
       (:return () (ret))))
