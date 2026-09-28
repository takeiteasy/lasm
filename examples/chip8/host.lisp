;;;; host.lisp
;;;; The machine that runs the compiled emulator, and the backend that lets the
;;;; .lsp compiler target it. Not CHIP-8 itself: chip8.lsp is the CHIP-8
;;;; interpreter, and this is only enough of a computer to run a program
;;;; written in the language. See docs/language.md#backend-requirements.

(in-package #:chip8)

;;; 16-bit registers and 16-bit cells. A .lsp value is one register wide, and
;;; CHIP-8 needs 16 bits for an opcode and 12 for an address.
;;; TODO: an 8-bit host once register-pair words land; see docs/examples.md#limitations.
(defmachine host
  (register pc :width 16)
  (register sp :width 16)
  (register r :width 16 :names (a b c d))
  (memory ram :width 16 :addr-width 16)
  (stack-pointer sp :memory ram :grows :down))

;;; Operand syntax: a register, an immediate, a stack slot, and an address held
;;; in a register.
(defmode h-reg (expr :register r))
(defmode h-imm "#" expr)
(defmode h-slot "[" "sp" "+" expr "]")
(defmode h-ind "[" (expr :register r) "]")
(defmode h-rr (expr :register r) "," (expr :register r))
(defmode h-ri (expr :register r) "," "#" expr)
(defmode h-rs (expr :register r) "," "[" "sp" "+" expr "]")
(defmode h-sr "[" "sp" "+" expr "]" "," (expr :register r))
(defmode h-rind (expr :register r) "," "[" (expr :register r) "]")
(defmode h-indr "[" (expr :register r) "]" "," (expr :register r))
(defmode h-addr expr)
(defmode h-rt (expr :register r) "," expr)
(defmode h-sp "sp")
(defmode h-sp-imm "sp" "," "#" expr)

;;; Values are unsigned words. Comparison, division and remainder read them as
;;; signed two's complement, so program code must not order or divide a value
;;; that may have its top bit set, such as a CHIP-8 opcode: mask and shift it.
(defun signed (word)
  (if (>= word 32768) (- word 65536) word))

;;; Moves. `ldb`/`stb` address bytes: the byte address is the cell address
;;; times two, an even one the low byte of its cell.
(definstruction host hlt
  (encoding (opcode 0))
  (semantics (trap :halt)))

(definstruction host ldi (modes h-ri)
  (encoding (opcode 1) (operand dst :width 1) (operand value :width 1))
  (semantics (set! (r dst) value)))

(definstruction host mov (modes h-rr)
  (encoding (opcode 2) (operand dst :width 1) (operand src :width 1))
  (semantics (set! (r dst) (r src))))

(definstruction host lds (modes h-rs)
  (encoding (opcode 3) (operand dst :width 1) (operand offset :width 1))
  (semantics (set! (r dst) (mref machine 'ram (wrap-value (+ sp offset) 16)))))

(definstruction host sts (modes h-sr)
  (encoding (opcode 4) (operand offset :width 1) (operand src :width 1))
  (semantics (set! (mref machine 'ram (wrap-value (+ sp offset) 16)) (r src))))

(definstruction host ldx (modes h-rind)
  (encoding (opcode 5) (operand dst :width 1) (operand addr :width 1))
  (semantics (set! (r dst) (mref machine 'ram (r addr)))))

(definstruction host stx (modes h-indr)
  (encoding (opcode 6) (operand addr :width 1) (operand src :width 1))
  (semantics (set! (mref machine 'ram (r addr)) (r src))))

(definstruction host ldb (modes h-rind)
  (encoding (opcode 7) (operand dst :width 1) (operand addr :width 1))
  (semantics
    (set! (r dst)
          (let ((word (mref machine 'ram (truncate (r addr) 2))))
            (if (evenp (r addr)) (logand word #xFF) (logand (ash word -8) #xFF))))))

(definstruction host stb (modes h-indr)
  (encoding (opcode 8) (operand addr :width 1) (operand src :width 1))
  (semantics
    (let* ((cell (truncate (r addr) 2))
           (word (mref machine 'ram cell))
           (byte (logand (r src) #xFF)))
      (set! (mref machine 'ram cell)
            (if (evenp (r addr))
                (logior (logand word #xFF00) byte)
                (logior (logand word #x00FF) (ash byte 8)))))))

;;; Control flow.
(definstruction host jmp (modes h-addr)
  (encoding (opcode 9) (operand :mode))
  (semantics (set! pc operand)))

(definstruction host jz (modes h-rt)
  (encoding (opcode 10) (operand src :width 1) (operand target :width 1))
  (semantics (when (zerop (r src)) (set! pc target))))

(definstruction host call (modes h-addr)
  (encoding (opcode 11) (operand :mode))
  (semantics (push pc sp) (set! pc operand)))

(definstruction host callr (modes h-reg)
  (encoding (opcode 12) (operand target :width 1))
  (semantics (push pc sp) (set! pc (r target))))

(definstruction host ret
  (encoding (opcode 13))
  (semantics (set! pc (pop sp))))

;;; The stack: what call lowering (docs/conventions.md) emits.
(definstruction host pushv
  (modes
    (h-reg (opcode 14) (operand src :width 1) (semantics (push (r src) sp)))
    (h-imm (opcode 15) (operand :mode) (semantics (push operand sp)))
    (h-slot (opcode 16) (operand offset :width 1)
            (semantics (push (mref machine 'ram (wrap-value (+ sp offset) 16)) sp)))))

(definstruction host popr (modes h-reg)
  (encoding (opcode 17) (operand dst :width 1))
  (semantics (set! (r dst) (pop sp))))

(definstruction host adds (modes h-sp-imm)
  (encoding (opcode 18) (operand :mode))
  (semantics (set! sp (wrap-value (+ sp operand) 16))))

(definstruction host subs (modes h-sp-imm)
  (encoding (opcode 19) (operand :mode))
  (semantics (set! sp (wrap-value (- sp operand) 16))))

(definstruction host movv
  (modes
    (h-rr (opcode 20) (operand dst :width 1) (operand src :width 1)
          (semantics (set! (r dst) (r src))))
    (h-ri (opcode 21) (operand dst :width 1) (operand value :width 1)
          (semantics (set! (r dst) value)))
    (h-rs (opcode 22) (operand dst :width 1) (operand offset :width 1)
          (semantics (set! (r dst) (mref machine 'ram (wrap-value (+ sp offset) 16)))))))

;;; Arithmetic and comparison: DST = DST op SRC, on the signed values SX and SY.
;;; A comparison leaves 1 or 0. `-ri` takes an immediate for SRC, which the
;;; compiler uses when it can (docs/language.md#backend-requirements).
(defmacro defalu (mnemonic opcode expression)
  `(progn
     (definstruction host ,mnemonic (modes h-rr)
       (encoding (opcode ,opcode) (operand dst :width 1) (operand src :width 1))
       (semantics
         (let* ((x (r dst)) (y (r src)) (sx (signed x)) (sy (signed y)))
           (declare (ignorable sx sy))
           (set! (r dst) (wrap-value ,expression 16)))))
     (definstruction host ,(intern (format nil "~Ai" mnemonic)) (modes h-ri)
       (encoding (opcode ,(+ opcode 32)) (operand dst :width 1) (operand value :width 1))
       (semantics
         (let* ((x (r dst)) (y value) (sx (signed x)) (sy (signed y)))
           (declare (ignorable sx sy))
           (set! (r dst) (wrap-value ,expression 16)))))))

(defalu addr 40 (+ x y))
(defalu subr 41 (- x y))
(defalu mulr 42 (* sx sy))
(defalu divr 43 (if (zerop sy) 0 (truncate sx sy)))
(defalu modr 44 (if (zerop sy) 0 (rem sx sy)))
(defalu andr 45 (logand x y))
(defalu orr 46 (logior x y))
(defalu xorr 47 (logxor x y))
(defalu shlr 48 (ash x y))
(defalu shrr 49 (ash x (- y)))
(defalu eqr 50 (if (= x y) 1 0))
(defalu ner 51 (if (/= x y) 1 0))
(defalu ltr 52 (if (< sx sy) 1 0))
(defalu gtr 53 (if (> sx sy) 1 0))
(defalu ler 54 (if (<= sx sy) 1 0))
(defalu ger 55 (if (>= sx sy) 1 0))

;;; The backend. Arguments go on the stack right to left and the caller removes
;;; them. `a` is the accumulator, `b` holds a right operand, and `c` and `d`
;;; survive calls. `:call` has a clause for a register target, which is how
;;; `funcall` on a computed function value compiles.
;;; See docs/backends.md.
;;; Adding the optional :branch-* operations would let a comparison in an `if`
;;; jump directly instead of computing 1 or 0 and testing it, making the
;;; compiled interpreter smaller and faster.
(defbackend host-lang (:machine host)
  (registers :return (a) :scratch (a b) :callee-saved (c d)
             :stack-pointer sp :program-counter pc :operand reg)
  (call :args :stack :order :right-to-left :cleanup :caller :return-address-slots 1)
  (frame :grows :down :slot slot)
  (operands (reg h-reg) (imm h-imm) (slot h-slot) (ind h-ind) (sp h-sp))
  (ops (:const (r v) (ldi r (imm v)))
       (:get (r s) (lds r s))
       (:set (s r) (sts s r))
       (:peek (d a) (ldx (reg d) (ind a)))
       (:poke (a s) (stx (ind a) (reg s)))
       (:peek-byte (d a) (ldb (reg d) (ind a)))
       (:poke-byte (a s) (stb (ind a) (reg s)))
       (:jump (target) (jmp target))
       (:branch-zero (r target) (jz r target))
       (:halt () (hlt))
       (:add (d s) (addr d s))
       (:sub (d s) (subr d s))
       (:mul (d s) (mulr d s))
       (:div (d s) (divr d s))
       (:mod (d s) (modr d s))
       (:and (d s) (andr d s))
       (:or (d s) (orr d s))
       (:xor (d s) (xorr d s))
       (:shl (d s) (shlr d s))
       (:shr (d s) (shrr d s))
       (:eq (d s) (eqr d s))
       (:ne (d s) (ner d s))
       (:lt (d s) (ltr d s))
       (:gt (d s) (gtr d s))
       (:le (d s) (ler d s))
       (:ge (d s) (ger d s))
       (:add-imm (d v) (addri d (imm v)))
       (:sub-imm (d v) (subri d (imm v)))
       (:and-imm (d v) (andri d (imm v)))
       (:eq-imm (d v) (eqri d (imm v)))
       (:lt-imm (d v) (ltri d (imm v)))
       (:push (x) (pushv x))
       (:pop (x) (popr x))
       (:move (d s) (movv d s))
       (:alloc (n) (subs (sp) (imm n)))
       (:free (n) (adds (sp) (imm n)))
       (:call ((f reg)) (callr f))
       (:call (f) (call f))
       (:return () (ret))))
