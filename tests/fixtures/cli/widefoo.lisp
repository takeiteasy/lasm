;;;; tests/fixtures/cli/widefoo.lisp
;;;; #368: a machine whose registers (16 bits) are wider than its memory cells
;;;; (8 bits), so a language word spans two cells (BACKEND-WORD-CELLS). Modeled
;;;; on callfoo.lisp, with (stack-pointer sp ... :width 16) (#167) giving the
;;;; frame slots LDS/STS address, through STACK-REF, that same two-cell split;
;;;; LDW/STW combine two cells by hand for a peek/poke through a computed
;;;; address, little-endian like the memory's own default.
;;;;
;;;;   lasm run fact.lsp -m widefoo.lisp

(defmachine widefoo
  (register pc :width 16)
  (register sp :width 16)
  (register r :width 16 :names (a b c d))
  (memory ram :width 8 :addr-width 16)
  (stack-pointer sp :memory ram :grows :down :width 16))

(defmode wf-reg (expr :register r))
(defmode wf-imm "#" expr)
(defmode wf-rr (expr :register r) "," (expr :register r))
(defmode wf-ri (expr :register r) "," "#" expr)
(defmode wf-so (expr :register r) "," "[" "sp" "+" expr "]")
(defmode wf-os "[" "sp" "+" expr "]" "," (expr :register r))
(defmode wf-sp-idx "[" "sp" "+" expr "]")
(defmode wf-sp "sp")
(defmode wf-spi "sp" "," "#" expr)
(defmode wf-ind "[" (expr :register r) "]")
(defmode wf-rind (expr :register r) "," "[" (expr :register r) "]")
(defmode wf-indr "[" (expr :register r) "]" "," (expr :register r))
(defmode wf-rt (expr :register r) "," expr)

(definstruction widefoo ldi (modes wf-ri)
  (encoding (opcode 1) (operand dst :width 1) (operand value :width 2))
  (semantics (set! (r dst) value)))

(definstruction widefoo movv (modes wf-rr)
  (encoding (opcode 2) (operand dst :width 1) (operand src :width 1))
  (semantics (set! (r dst) (r src))))

;; #167: a frame slot is the stack pointer's own :width, so STACK-REF already
;; reads/writes it as one two-cell word.
(definstruction widefoo lds (modes wf-so)
  (encoding (opcode 3) (operand dst :width 1) (operand offset :width 1))
  (semantics (set! (r dst) (stack-ref offset sp))))

(definstruction widefoo sts (modes wf-os)
  (encoding (opcode 4) (operand offset :width 1) (operand src :width 1))
  (semantics (setf (stack-ref offset sp) (r src))))

;; #167: PUSH takes a frame slot (its own two-cell width) as well as a
;; register, since convention lowering spills arguments and saves through it.
(definstruction widefoo pushv
  (modes
    (wf-reg (opcode 5) (operand src :width 1) (semantics (push (r src) sp)))
    (wf-sp-idx (opcode 21) (operand offset :width 1)
               (semantics (push (stack-ref offset sp) sp)))))

(definstruction widefoo popr (modes wf-reg)
  (encoding (opcode 6) (operand dst :width 1))
  (semantics (set! (r dst) (pop sp))))

(definstruction widefoo subs (modes wf-spi)
  (encoding (opcode 7) (operand :mode))
  (semantics (set! sp (wrap-value (- sp operand) 16))))

(definstruction widefoo adds (modes wf-spi)
  (encoding (opcode 8) (operand :mode))
  (semantics (set! sp (wrap-value (+ sp operand) 16))))

(definstruction widefoo call (modes absolute)
  (encoding (opcode 9) (operand :mode))
  (semantics (push pc sp) (set! pc operand)))

(definstruction widefoo callr (modes wf-reg)
  (encoding (opcode 10) (operand target :width 1))
  (semantics (push pc sp) (set! pc (r target))))

(definstruction widefoo ret
  (encoding (opcode 11))
  (semantics (set! pc (pop sp))))

(definstruction widefoo hlt
  (encoding (opcode 0))
  (semantics (trap :halt)))

(definstruction widefoo jmp (modes absolute)
  (encoding (opcode 12) (operand :mode))
  (semantics (set! pc operand)))

(definstruction widefoo jz (modes wf-rt)
  (encoding (opcode 13) (operand src :width 1) (operand target :width 2))
  (semantics (when (zerop (r src)) (set! pc target))))

;; A word through a computed address, two cells combined little-endian, since
;; RAM's own cells are narrower than a word (#368) -- MREF only reaches one.
(definstruction widefoo ldw (modes wf-rind)
  (encoding (opcode 14) (operand dst :width 1) (operand addr :width 1))
  (semantics
    (set! (r dst)
          (let ((lo (mref machine 'ram (r addr)))
                (hi (mref machine 'ram (wrap-value (1+ (r addr)) 16))))
            (logior lo (ash hi 8))))))

(definstruction widefoo stw (modes wf-indr)
  (encoding (opcode 15) (operand addr :width 1) (operand src :width 1))
  (semantics
    (let ((value (r src)))
      (set! (mref machine 'ram (r addr)) (logand value #xFF))
      (set! (mref machine 'ram (wrap-value (1+ (r addr)) 16)) (logand (ash value -8) #xFF)))))

;; #379: a byte is one 8-bit cell here, so its byte address is its cell address.
(definstruction widefoo ldb (modes wf-rind)
  (encoding (opcode 23) (operand dst :width 1) (operand addr :width 1))
  (semantics (set! (r dst) (mref machine 'ram (r addr)))))

(definstruction widefoo stb (modes wf-indr)
  (encoding (opcode 24) (operand addr :width 1) (operand src :width 1))
  (semantics (set! (mref machine 'ram (r addr)) (logand (r src) #xFF))))

(defmacro defarith (mnemonic opcode expression)
  "MNEMONIC dst, src sets dst to EXPRESSION of the signed values x and y."
  `(definstruction widefoo ,mnemonic (modes wf-rr)
     (encoding (opcode ,opcode) (operand dst :width 1) (operand src :width 1))
     (semantics
       (let* ((x (r dst)) (y (r src))
              (sx (if (>= x 32768) (- x 65536) x))
              (sy (if (>= y 32768) (- y 65536) y)))
         (declare (ignorable sx sy))
         (set! (r dst) (wrap-value ,expression 16))))))

(defarith addr 16 (+ x y))
(defarith subr 17 (- x y))
(defarith mulr 18 (* sx sy))
(defarith seq 19 (if (= x y) 1 0))
(defarith slt 20 (if (< sx sy) 1 0))
;; #368: SHL, so a computed AREF/ASET index can scale by the (power-of-two)
;; word size without needing :mul.
(defarith shlr 22 (ash x y))

(defbackend widefoo-lang-abi (:isa widefoo)
  (registers :return (a) :scratch (a b) :callee-saved (c d)
             :stack-pointer sp :program-counter pc :operand reg)
  (call :args :stack :order :right-to-left :cleanup :caller :return-address-slots 1)
  (frame :grows :down :slot sp-idx :counts :cells)
  (operands (reg wf-reg) (imm wf-imm) (sp-idx wf-sp-idx) (ind wf-ind) (sp wf-sp))
  (ops (:const (r v) (ldi r (imm v)))
       (:get (r slot) (lds r slot))
       (:set (slot r) (sts slot r))
       (:peek (d a) (ldw (reg d) (ind a)))
       (:poke (a s) (stw (ind a) (reg s)))
       (:peek-byte (d a) (ldb (reg d) (ind a)))
       (:poke-byte (a s) (stb (ind a) (reg s)))
       (:jump (target) (jmp target))
       (:branch-zero (r target) (jz r target))
       (:halt () (hlt))
       (:add (d s) (addr d s))
       (:sub (d s) (subr d s))
       (:mul (d s) (mulr d s))
       (:eq (d s) (seq d s))
       (:lt (d s) (slt d s))
       (:shl (d s) (shlr d s))
       (:push (x) (pushv x))
       (:pop (x) (popr x))
       (:move (d s) (movv d s))
       ;; #385: SUBS/ADDS move SP by cells, so (frame :counts :cells) hands
       ;; them a cell count; LDS/STS/PUSH/POP size a slot themselves (#167).
       (:alloc (n) (subs (sp) (imm n)))
       (:free (n) (adds (sp) (imm n)))
       (:call ((f reg)) (callr f))
       (:call (f) (call f))
       (:return () (ret))))
