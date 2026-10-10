;;;; tests/fixtures/cli/quadfoo-fp.lisp
;;;; quadfoo with a 16-bit frame pointer: :enter pushes two cells, not a
;;;; four-cell word, so the backend says (frame :pointer-cells 2).
;;;;
;;;;   lasm run fact32.lsp -m quadfoo-fp.lisp --cpu quadfoo-fp --backend quadfoo-lang-fp-abi

(load (merge-pathnames "quadfoo.lisp" *load-pathname*))

(defmachine (quadfoo-fp (:extends quadfoo))
  (register fp :width 16))

(defmode qf-fp-idx "[" "fp" "+" expr "]")
(defmode qf-rf (expr :register r) "," "[" "fp" "+" expr "]")
(defmode qf-fr "[" "fp" "+" expr "]" "," (expr :register r))

(definstruction quadfoo-fp pushfp
  (encoding (opcode 40))
  (semantics
    (push (logand (ash fp -8) 255) sp)
    (push (logand fp 255) sp)))

(definstruction quadfoo-fp popfp
  (encoding (opcode 41))
  (semantics
    (let* ((lo (pop sp)) (hi (pop sp)))
      (set! fp (logior lo (ash hi 8))))))

(definstruction quadfoo-fp movfs
  (encoding (opcode 42))
  (semantics (set! fp sp)))

(definstruction quadfoo-fp movsf
  (encoding (opcode 43))
  (semantics (set! sp fp)))

(definstruction quadfoo-fp ldf (modes qf-rf)
  (encoding (opcode 44) (operand dst :width 1) (operand offset :width 1))
  (semantics (set! (r dst) (mref machine 'ram (wrap-value (+ fp offset) 16)))))

(definstruction quadfoo-fp stf (modes qf-fr)
  (encoding (opcode 45) (operand offset :width 1) (operand src :width 1))
  (semantics (set! (mref machine 'ram (wrap-value (+ fp offset) 16)) (r src))))

;; Slots are read and written through fp; :stack-slot serves a function without a frame.
(defbackend quadfoo-lang-fp-abi (:extends quadfoo-lang-abi :isa quadfoo-fp)
  (frame :pointer fp :pointer-cells 2 :slot fp-idx :stack-slot sp-idx)
  (operands (fp-idx qf-fp-idx))
  (ops (:enter () (pushfp) (movfs))
       (:leave () (movsf) (popfp))
       (:move (d (s fp-idx))
        (ldf (:lo d) (:lo s)) (ldf (:part 1 d) (:part 1 s)) (ldf (:part 2 d) (:part 2 s)) (ldf (:hi d) (:hi s)))
       (:move (d (s sp-idx))
        (lds (:lo d) (:lo s)) (lds (:part 1 d) (:part 1 s)) (lds (:part 2 d) (:part 2 s)) (lds (:hi d) (:hi s)))
       (:move (d s)
        (movv (:lo d) (:lo s)) (movv (:part 1 d) (:part 1 s)) (movv (:part 2 d) (:part 2 s)) (movv (:hi d) (:hi s)))
       (:get (r (slot fp-idx))
        (ldf (:lo r) (:lo slot)) (ldf (:part 1 r) (:part 1 slot)) (ldf (:part 2 r) (:part 2 slot)) (ldf (:hi r) (:hi slot)))
       (:get (r slot)
        (lds (:lo r) (:lo slot)) (lds (:part 1 r) (:part 1 slot)) (lds (:part 2 r) (:part 2 slot)) (lds (:hi r) (:hi slot)))
       (:set (slot r)
        (stf (:lo slot) (:lo r)) (stf (:part 1 slot) (:part 1 r)) (stf (:part 2 slot) (:part 2 r)) (stf (:hi slot) (:hi r)))))
