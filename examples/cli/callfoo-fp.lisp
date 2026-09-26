;;;; examples/cli/callfoo-fp.lisp
;;;; callfoo with a frame pointer, and the backend extending callfoo-abi for it
;;;; (#321, #323). The CLI file defines callfoo too, so name the machine:
;;;;
;;;;   lasm run framed.lasm -m callfoo-fp.lisp --machine-name callfoo-fp

(load (merge-pathnames "callfoo.lisp" *load-pathname*))

;; Locals and arguments are addressed from fp, so a function's slots do not
;; depend on how deep its stack is.
(defmachine (callfoo-fp (:extends callfoo))
  (register fp :width 16))

(defmode call-fp-idx "[" "fp" "+" expr "]")
(defmode call-rf (expr :register r) "," "[" "fp" "+" expr "]")
(defmode call-fr "[" "fp" "+" expr "]" "," (expr :register r))

(definstruction callfoo-fp pushfp
  (encoding (opcode 18))
  (semantics (push fp sp)))

(definstruction callfoo-fp popfp
  (encoding (opcode 19))
  (semantics (set! fp (pop sp))))

(definstruction callfoo-fp movfs
  (encoding (opcode 20))
  (semantics (set! fp sp)))

(definstruction callfoo-fp movsf
  (encoding (opcode 21))
  (semantics (set! sp fp)))

(definstruction callfoo-fp ldf (modes call-rf)
  (encoding (opcode 22) (operand dst :width 1) (operand offset :width 1))
  (semantics (set! (r dst) (mref machine 'ram (wrap-value (+ fp offset) 16)))))

(definstruction callfoo-fp stf (modes call-fr)
  (encoding (opcode 23) (operand offset :width 1) (operand src :width 1))
  (semantics (set! (mref machine 'ram (wrap-value (+ fp offset) 16)) (r src))))

(defbackend callfoo-fp-abi (:extends callfoo-abi :machine callfoo-fp)
  (frame :pointer fp :slot fp-idx :stack-slot sp-idx)
  (operands (fp-idx call-fp-idx))
  (ops (:enter () (pushfp) (movfs))
       (:leave () (movsf) (popfp))))

;; A frame slot is also pushed and moved into a register, as an argument.
(definstruction callfoo-fp pushv
  (modes
    (call-reg (opcode 9) (operand src :width 1) (semantics (push (r src) sp)))
    (call-imm (opcode 10) (operand :mode) (semantics (push operand sp)))
    (call-sp-idx (opcode 11) (operand offset :width 1)
                 (semantics (push (mref machine 'ram (wrap-value (+ sp offset) 16)) sp)))
    (call-fp-idx (opcode 84) (operand offset :width 1)
                 (semantics (push (mref machine 'ram (wrap-value (+ fp offset) 16)) sp)))))

(definstruction callfoo-fp movv
  (modes
    (call-rr (opcode 12) (operand dst :width 1) (operand src :width 1)
             (semantics (set! (r dst) (r src))))
    (call-ri (opcode 13) (operand dst :width 1) (operand value :width 1)
             (semantics (set! (r dst) value)))
    (call-rs (opcode 68) (operand dst :width 1) (operand offset :width 1)
             (semantics (set! (r dst) (mref machine 'ram (wrap-value (+ sp offset) 16)))))
    (call-rf (opcode 85) (operand dst :width 1) (operand offset :width 1)
             (semantics (set! (r dst) (mref machine 'ram (wrap-value (+ fp offset) 16)))))))

;; callfoo-lang-abi on the frame-pointer machine: slots are read and written
;; through fp.
(defbackend callfoo-lang-fp-abi (:extends callfoo-fp-abi)
  (operands (ind call-ind))
  (ops (:const (r v) (ldi r (imm v)))
       (:get (r slot) (ldf r slot))
       (:set (slot r) (stf slot r))
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
