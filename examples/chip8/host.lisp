;;;; host.lisp
;;;; The machine that runs the compiled emulator, and the backend that lets the
;;;; .lsp compiler target it. Not CHIP-8 itself: chip8.lsp is the CHIP-8
;;;; interpreter, and this is only enough of a computer to run a program
;;;; written in the language. See docs/language.md#backend-requirements.

(in-package #:chip8)

;;; 8-bit registers and 8-bit cells. A .lsp value is a 16-bit word held in a
;;; register pair (docs/register-pairs.md), and a word in memory is two cells,
;;; low cell first. There is no SP-relative addressing: locals and arguments
;;; have fixed addresses (docs/static-frames.md), so the stack only holds
;;; return addresses.
(defmachine host
  (register pc :width 16)
  (register sp :width 16)
  (register r :width 8 :names (a b c d e f g h))
  (flags cf zf nf vf)
  (memory ram :width 8 :addr-width 16)
  (stack-pointer sp :memory ram :grows :down))

;;; Operand syntax: a register, an immediate, and two registers or a register
;;; and an immediate.
(defmode h-reg (expr :register r))
(defmode h-imm "#" expr)
(defmode h-rr (expr :register r) "," (expr :register r))
(defmode h-addr expr)
(defmode h-ri (expr :register r) "," "#" expr)
(defmode h-rrt (expr :register r) "," (expr :register r) "," expr)
(defmode h-rrrr (expr :register r) "," (expr :register r) "," (expr :register r) "," (expr :register r))
(defmode h-rrr (expr :register r) "," (expr :register r) "," (expr :register r))

(defun byte-pair (high low)
  (logior (ash high 8) low))

;;; Values are unsigned words. Comparison, division and remainder read them as
;;; signed two's complement, so program code must not order or divide a value
;;; that may have its top bit set, such as a CHIP-8 opcode: mask and shift it.
(defun signed (word)
  (if (>= word 32768) (- word 65536) word))

(definstruction host hlt
  (encoding (opcode 0))
  (semantics (trap :halt)))

;;; Moves.
(definstruction host ldi (modes h-ri)
  (encoding (opcode 1) (operand dst :width 1) (operand value :width 1))
  (semantics (set! (r dst) (logand value 255))))

(definstruction host mov (modes h-rr)
  (encoding (opcode 2) (operand dst :width 1) (operand src :width 1))
  (semantics (set! (r dst) (r src))))

;;; Memory through the address in a register pair: `ldw`/`stw` move a word, two
;;; cells low first, and `ldb`/`stb` a single cell.
(definstruction host ldw (modes h-rrrr)
  (encoding (opcode 3) (operand dh :width 1) (operand dl :width 1)
            (operand ah :width 1) (operand al :width 1))
  (semantics
    (let* ((address (byte-pair (r ah) (r al)))
           (lo (mref machine 'ram address))
           (hi (mref machine 'ram (wrap-value (1+ address) 16))))
      (set! (r dl) lo)
      (set! (r dh) hi))))

(definstruction host stw (modes h-rrrr)
  (encoding (opcode 4) (operand ah :width 1) (operand al :width 1)
            (operand sh :width 1) (operand sl :width 1))
  (semantics
    (let ((address (byte-pair (r ah) (r al))))
      (set! (mref machine 'ram address) (r sl))
      (set! (mref machine 'ram (wrap-value (1+ address) 16)) (r sh)))))

(definstruction host ldb (modes h-rrr)
  (encoding (opcode 5) (operand dst :width 1) (operand ah :width 1) (operand al :width 1))
  (semantics (set! (r dst) (mref machine 'ram (byte-pair (r ah) (r al))))))

(definstruction host stb (modes h-rrr)
  (encoding (opcode 6) (operand ah :width 1) (operand al :width 1) (operand src :width 1))
  (semantics (set! (mref machine 'ram (byte-pair (r ah) (r al))) (r src))))

;;; Control flow. A call pushes the return address a byte at a time, high byte
;;; first, so it lies in memory as a little-endian word.
(definstruction host jmp (modes h-addr)
  (encoding (opcode 7) (operand :mode))
  (semantics (set! pc operand)))

;;; Jump when both registers of a pair are zero.
(definstruction host jzp (modes h-rrt)
  (encoding (opcode 8) (operand hi :width 1) (operand lo :width 1) (operand target :width 2))
  (semantics (when (and (zerop (r hi)) (zerop (r lo))) (set! pc target))))

(definstruction host call (modes h-addr)
  (encoding (opcode 9) (operand :mode))
  (semantics
    (push (logand (ash pc -8) 255) sp)
    (push (logand pc 255) sp)
    (set! pc operand)))

(definstruction host callr (modes h-rr)
  (encoding (opcode 10) (operand hi :width 1) (operand lo :width 1))
  (semantics
    (push (logand (ash pc -8) 255) sp)
    (push (logand pc 255) sp)
    (set! pc (byte-pair (r hi) (r lo)))))

(definstruction host ret
  (encoding (opcode 11))
  (semantics
    (let* ((lo (pop sp)) (hi (pop sp)))
      (set! pc (byte-pair hi lo)))))

;;; One cell at a time, carrying between the halves of a word: ADD then ADC,
;;; SUB then SBC. `-i` takes an immediate for the second operand, which the
;;; compiler uses when it can (docs/language.md#backend-requirements).
(defmacro defcarry (mnemonic opcode expression)
  `(progn
     (definstruction host ,mnemonic (modes h-rr)
       (encoding (opcode ,opcode) (operand dst :width 1) (operand src :width 1))
       (semantics
         (let* ((x (r dst)) (y (r src)) (total ,expression))
           (set! (r dst) (logand total 255))
           (set! cf (if (or (> total 255) (minusp total)) 1 0)))))
     (definstruction host ,(intern (format nil "~Ai" mnemonic)) (modes h-ri)
       (encoding (opcode ,(+ opcode 32)) (operand dst :width 1) (operand value :width 1))
       (semantics
         (let* ((x (r dst)) (y (logand value 255)) (total ,expression))
           (set! (r dst) (logand total 255))
           (set! cf (if (or (> total 255) (minusp total)) 1 0)))))))

(defcarry add 16 (+ x y))
(defcarry adc 17 (+ x y cf))
(defcarry sub 18 (- x y))
(defcarry sbc 19 (- x y cf))

(defmacro compare-cells (x y carry-in zero-in)
  `(let* ((sx (if (>= ,x 128) (- ,x 256) ,x))
          (sy (if (>= ,y 128) (- ,y 256) ,y))
          (borrow ,carry-in)
          (difference (- ,x ,y borrow))
          (signed-difference (- sx sy borrow))
          (result (logand difference 255)))
     (set! cf (if (minusp difference) 1 0))
     (set! zf (if (and (zerop result) ,zero-in) 1 0))
     (set! nf (if (>= result 128) 1 0))
     (set! vf (if (or (< signed-difference -128) (> signed-difference 127)) 1 0))))

;;; CP and CPC set the flags from a subtraction, CPC continuing the borrow and
;;; the zero flag from the cell before it.
(defmacro defcompare (mnemonic opcode carry-in zero-in)
  `(progn
     (definstruction host ,mnemonic (modes h-rr)
       (encoding (opcode ,opcode) (operand dst :width 1) (operand src :width 1))
       (semantics
         (let* ((x (r dst)) (y (r src)))
           (compare-cells x y ,carry-in ,zero-in))))
     (definstruction host ,(intern (format nil "~Ai" mnemonic)) (modes h-ri)
       (encoding (opcode ,(+ opcode 32)) (operand dst :width 1) (operand value :width 1))
       (semantics
         (let* ((x (r dst)) (y (logand value 255)))
           (compare-cells x y ,carry-in ,zero-in))))))

(defcompare cp 20 0 t)
(defcompare cpc 21 cf (= zf 1))

;;; A comparison leaves 1 or 0 in a register.
(defmacro defsetter (mnemonic opcode condition)
  `(definstruction host ,mnemonic (modes h-reg)
     (encoding (opcode ,opcode) (operand dst :width 1))
     (semantics (set! (r dst) (if ,condition 1 0)))))

(defsetter seteq 22 (= zf 1))
(defsetter setne 23 (= zf 0))
(defsetter setlt 24 (/= nf vf))
(defsetter setge 25 (= nf vf))

(defmacro defbitwise (mnemonic opcode function)
  `(progn
     (definstruction host ,mnemonic (modes h-rr)
       (encoding (opcode ,opcode) (operand dst :width 1) (operand src :width 1))
       (semantics (set! (r dst) (,function (r dst) (r src)))))
     (definstruction host ,(intern (format nil "~Ai" mnemonic)) (modes h-ri)
       (encoding (opcode ,(+ opcode 32)) (operand dst :width 1) (operand value :width 1))
       (semantics (set! (r dst) (,function (r dst) (logand value 255)))))))

(defbitwise andr 26 logand)
(defbitwise orr 27 logior)
(defbitwise xorr 28 logxor)

;;; Multiply, divide, remainder and shifts take two register pairs, DH:DL =
;;; DH:DL op SH:SL, as a machine with word instructions might. The operands are
;;; the words X and Y, read as signed SX and SY.
(defmacro defword (mnemonic opcode expression)
  `(definstruction host ,mnemonic (modes h-rrrr)
     (encoding (opcode ,opcode) (operand dh :width 1) (operand dl :width 1)
               (operand sh :width 1) (operand sl :width 1))
     (semantics
       (let* ((x (byte-pair (r dh) (r dl))) (y (byte-pair (r sh) (r sl)))
              (sx (signed x))
              (sy (signed y))
              (result (wrap-value ,expression 16)))
         (declare (ignorable sx sy))
         (set! (r dh) (ash result -8))
         (set! (r dl) (logand result 255))))))

(defword mulw 29 (* sx sy))
(defword divw 30 (if (zerop sy) 0 (truncate sx sy)))
(defword modw 31 (if (zerop sy) 0 (rem sx sy)))
(defword shlw 32 (ash x y))
(defword shrw 33 (ash x (- y)))

;;; The backend. Every value is a register pair: `ab` is the accumulator and
;;; `cd` holds a right operand. A word is two cells, low cell first, so
;;; (:lo x) and (:hi x) name the halves. Calls store arguments straight into
;;; the callee's static frame, and `:call` has a clause for a register target,
;;; which is how `funcall` on a computed function value compiles.
;;; See docs/backends.md.
;;; Adding the optional :branch-* operations would let a comparison in an `if`
;;; jump directly instead of computing 1 or 0 and testing it, making the
;;; compiled interpreter smaller and faster.
(defbackend host-lang (:machine host)
  (registers :pairs ((ab a b) (cd c d) (ef e f) (gh g h))
             :return (ab) :scratch (ab cd) :callee-saved (ef gh)
             :stack-pointer sp :program-counter pc :operand reg)
  (call :args :stack :order :right-to-left :cleanup :caller :return-address-slots 1)
  (frame :static t :grows :down)
  (operands (reg h-reg) (imm h-imm))
  (ops (:const (r v) (ldi (:lo r) (imm (:lo v))) (ldi (:hi r) (imm (:hi v))))
       (:move (d s) (mov (:lo d) (:lo s)) (mov (:hi d) (:hi s)))
       (:peek (d a) (ldw (reg (:hi d)) (reg (:lo d)) (reg (:hi a)) (reg (:lo a))))
       (:poke (a s) (stw (reg (:hi a)) (reg (:lo a)) (reg (:hi s)) (reg (:lo s))))
       ;; The byte is read before the high half is cleared: D may be A.
       (:peek-byte (d a) (ldb (reg (:lo d)) (reg (:hi a)) (reg (:lo a))) (ldi (:hi d) (imm 0)))
       (:poke-byte (a s) (stb (reg (:hi a)) (reg (:lo a)) (reg (:lo s))))
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
       (:le (d s) (cp (:lo s) (:lo d)) (cpc (:hi s) (:hi d)) (setge (:lo d)) (ldi (:hi d) (imm 0)))
       (:add-imm (d v) (addi (:lo d) (imm (:lo v))) (adci (:hi d) (imm (:hi v))))
       (:sub-imm (d v) (subi (:lo d) (imm (:lo v))) (sbci (:hi d) (imm (:hi v))))
       (:and-imm (d v) (andri (:lo d) (imm (:lo v))) (andri (:hi d) (imm (:hi v))))
       (:eq-imm (d v) (cpi (:lo d) (imm (:lo v))) (cpci (:hi d) (imm (:hi v)))
                (seteq (:lo d)) (ldi (:hi d) (imm 0)))
       (:lt-imm (d v) (cpi (:lo d) (imm (:lo v))) (cpci (:hi d) (imm (:hi v)))
                (setlt (:lo d)) (ldi (:hi d) (imm 0)))
       (:call ((f reg)) (callr (:hi f) (:lo f)))
       (:call (f) (call f))
       (:return () (ret))))
