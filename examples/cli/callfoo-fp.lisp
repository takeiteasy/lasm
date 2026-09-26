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
  (frame :pointer fp :slot fp-idx)
  (operands (fp-idx call-fp-idx))
  (ops (:enter () (pushfp) (movfs))
       (:leave () (movsf) (popfp))))
