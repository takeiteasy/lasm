;;;; tests/fixtures/cli/zpfoo.lisp
;;;; An 8-bit accumulator machine whose 16-bit language words live in pairs of
;;;; zero-page cells (docs/register-pairs.md#memory-halves). One register, A,
;;;; with a carry chain and instructions that take zero-page addresses, so a
;;;; backend spells each language operation as its halves' loads and stores.
;;;; The word instructions (MULW, LDW...) take four cell addresses. Programs
;;;; start at #x200, above the halves.
;;;;
;;;;   lasm run fact.lsp -m zpfoo.lisp --backend zpfoo-lang-abi --origin 512

(defmachine zpfoo
  (register pc :width 16)
  (register sp :width 16)
  (register a :width 8)
  (flags cf zf nf vf)
  (memory ram :width 8 :addr-width 16)
  (stack-pointer sp :memory ram :grows :down))

(defmode zf-imm "#" expr)
(defmode zf-zp "[" expr "]")
(defmode zf-slot "[" "sp" "+" expr "]")
(defmode zf-sp "sp")
(defmode zf-spi "sp" "," "#" expr)
(defmode zf-zz "[" expr "]" "," "[" expr "]")
(defmode zf-zzt "[" expr "]" "," "[" expr "]" "," expr)
(defmode zf-zzzz "[" expr "]" "," "[" expr "]" "," "[" expr "]" "," "[" expr "]")

(definstruction zpfoo hlt
  (encoding (opcode 0))
  (semantics (trap :halt)))

(definstruction zpfoo ldi (modes zf-imm)
  (encoding (opcode 1) (operand value :width 1))
  (semantics (set! a (logand value 255))))

(definstruction zpfoo lda (modes zf-zp)
  (encoding (opcode 2) (operand cell :width 1))
  (semantics (set! a (mref machine 'ram cell))))

(definstruction zpfoo sta (modes zf-zp)
  (encoding (opcode 3) (operand cell :width 1))
  (semantics (set! (mref machine 'ram cell) a)))

(definstruction zpfoo lds (modes zf-slot)
  (encoding (opcode 4) (operand offset :width 1))
  (semantics (set! a (mref machine 'ram (wrap-value (+ sp offset) 16)))))

(definstruction zpfoo sts (modes zf-slot)
  (encoding (opcode 5) (operand offset :width 1))
  (semantics (set! (mref machine 'ram (wrap-value (+ sp offset) 16)) a)))

(definstruction zpfoo pha
  (encoding (opcode 6))
  (semantics (push a sp)))

(definstruction zpfoo pla
  (encoding (opcode 7))
  (semantics (set! a (pop sp))))

(definstruction zpfoo subs (modes zf-spi)
  (encoding (opcode 8) (operand :mode))
  (semantics (set! sp (wrap-value (- sp operand) 16))))

(definstruction zpfoo adds (modes zf-spi)
  (encoding (opcode 9) (operand :mode))
  (semantics (set! sp (wrap-value (+ sp operand) 16))))

;; The return address goes on the stack a byte at a time, high byte first, so
;; it lies in memory as a little-endian word.
(definstruction zpfoo call (modes absolute)
  (encoding (opcode 10) (operand :mode))
  (semantics
    (push (logand (ash pc -8) 255) sp)
    (push (logand pc 255) sp)
    (set! pc operand)))

;; A call through the word in two cells, hi then lo.
(definstruction zpfoo callz (modes zf-zz)
  (encoding (opcode 11) (operand hi :width 1) (operand lo :width 1))
  (semantics
    (push (logand (ash pc -8) 255) sp)
    (push (logand pc 255) sp)
    (set! pc (logior (ash (mref machine 'ram hi) 8) (mref machine 'ram lo)))))

(definstruction zpfoo ret
  (encoding (opcode 12))
  (semantics
    (let* ((lo (pop sp)) (hi (pop sp)))
      (set! pc (logior lo (ash hi 8))))))

(definstruction zpfoo jmp (modes absolute)
  (encoding (opcode 13) (operand :mode))
  (semantics (set! pc operand)))

;; Jump when both cells of a word, hi then lo, are zero.
(definstruction zpfoo jzw (modes zf-zzt)
  (encoding (opcode 14) (operand hi :width 1) (operand lo :width 1) (operand target :width 2))
  (semantics (when (and (zerop (mref machine 'ram hi)) (zerop (mref machine 'ram lo))) (set! pc target))))

(definstruction zpfoo clc
  (encoding (opcode 15))
  (semantics (set! cf 0)))

(definstruction zpfoo adc (modes zf-zp)
  (encoding (opcode 16) (operand cell :width 1))
  (semantics
    (let ((sum (+ a (mref machine 'ram cell) cf)))
      (set! a (logand sum 255))
      (set! cf (if (> sum 255) 1 0)))))

(definstruction zpfoo sbc (modes zf-zp)
  (encoding (opcode 17) (operand cell :width 1))
  (semantics
    (let ((difference (- a (mref machine 'ram cell) cf)))
      (set! a (logand difference 255))
      (set! cf (if (minusp difference) 1 0)))))

;; CMP compares A with a cell; CMPC also takes the borrow and the zero flag of the low half.
(defmacro defcompare (mnemonic opcode carry-in zero-in)
  `(definstruction zpfoo ,mnemonic (modes zf-zp)
     (encoding (opcode ,opcode) (operand cell :width 1))
     (semantics
       (let* ((x a) (y (mref machine 'ram cell))
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

(defcompare cmp 19 0 t)
(defcompare cmpc 20 cf (= zf 1))

(defmacro defsetter (mnemonic opcode condition)
  `(definstruction zpfoo ,mnemonic
     (encoding (opcode ,opcode))
     (semantics (set! a (if ,condition 1 0)))))

(defsetter seteq 21 (= zf 1))
(defsetter setne 22 (= zf 0))
(defsetter setlt 23 (/= nf vf))
(defsetter setge 24 (= nf vf))

(defmacro defbitwise (mnemonic opcode function)
  `(definstruction zpfoo ,mnemonic (modes zf-zp)
     (encoding (opcode ,opcode) (operand cell :width 1))
     (semantics (set! a (,function a (mref machine 'ram cell))))))

(defbitwise anda 25 logand)
(defbitwise ora 26 logior)
(defbitwise eor 27 logxor)

;; A word through the address in two cells, two cells little-endian: dh, dl, ah, al.
(definstruction zpfoo ldw (modes zf-zzzz)
  (encoding (opcode 28) (operand dh :width 1) (operand dl :width 1) (operand ah :width 1) (operand al :width 1))
  (semantics
    (let* ((address (logior (ash (mref machine 'ram ah) 8) (mref machine 'ram al)))
           (lo (mref machine 'ram address))
           (hi (mref machine 'ram (wrap-value (1+ address) 16))))
      (set! (mref machine 'ram dl) lo)
      (set! (mref machine 'ram dh) hi))))

(definstruction zpfoo stw (modes zf-zzzz)
  (encoding (opcode 29) (operand ah :width 1) (operand al :width 1) (operand sh :width 1) (operand sl :width 1))
  (semantics
    (let ((address (logior (ash (mref machine 'ram ah) 8) (mref machine 'ram al))))
      (set! (mref machine 'ram address) (mref machine 'ram sl))
      (set! (mref machine 'ram (wrap-value (1+ address) 16)) (mref machine 'ram sh)))))

;; MNEMONIC dh, dl, sh, sl sets the word in dh:dl to EXPRESSION of the words x and y, signed as sx and sy.
(defmacro defword (mnemonic opcode expression)
  `(definstruction zpfoo ,mnemonic (modes zf-zzzz)
     (encoding (opcode ,opcode) (operand dh :width 1) (operand dl :width 1)
               (operand sh :width 1) (operand sl :width 1))
     (semantics
       (let* ((x (logior (ash (mref machine 'ram dh) 8) (mref machine 'ram dl))) (y (logior (ash (mref machine 'ram sh) 8) (mref machine 'ram sl)))
              (sx (if (>= x 32768) (- x 65536) x))
              (sy (if (>= y 32768) (- y 65536) y))
              (result (wrap-value ,expression 16)))
         (declare (ignorable sx sy))
         (set! (mref machine 'ram dh) (ash result -8))
         (set! (mref machine 'ram dl) (logand result 255))))))

(defword mulw 30 (* sx sy))
(defword divw 31 (if (zerop sy) 0 (truncate sx sy)))
(defword modw 32 (if (zerop sy) 0 (rem sx sy)))
(defword shlw 33 (ash x y))
(defword shrw 34 (ash x (- y)))

;; Every value is a pair of zero-page cells; w3's halves are far apart, so no
;; template may assume a pair's cells are adjacent. The accumulator A is in no
;; role list, so a template uses it freely. A word goes on the stack a byte at a
;; time, high byte first, and a frame slot is two cells, low byte first.
(defbackend zpfoo-lang-abi (:isa zpfoo)
  (registers :pairs ((w0 #x11 #x10) (w1 #x13 #x12) (w2 #x15 #x14) (w3 #x20 #x30))
             :return (w0) :scratch (w0 w1) :callee-saved (w2 w3)
             :stack-pointer sp :program-counter pc :operand zp)
  (call :args :stack :order :right-to-left :cleanup :caller :return-address-slots 1)
  (frame :grows :down :slot slot :offsets :cells :counts :cells)
  (operands (zp zf-zp) (imm zf-imm) (slot zf-slot) (sp zf-sp))
  (ops (:push (x) (lda (:hi x)) (pha) (lda (:lo x)) (pha))
       (:pop (x) (pla) (sta (:lo x)) (pla) (sta (:hi x)))
       (:move (d (s slot)) (lds (:lo s)) (sta (:lo d)) (lds (:hi s)) (sta (:hi d)))
       (:move (d s) (lda (:lo s)) (sta (:lo d)) (lda (:hi s)) (sta (:hi d)))
       (:alloc (n) (subs (sp) (imm n)))
       (:free (n) (adds (sp) (imm n)))
       (:call ((f zp)) (callz (:hi f) (:lo f)))
       (:call (f) (call f))
       (:return () (ret))
       (:const (r v) (ldi (imm (:lo v))) (sta (:lo r)) (ldi (imm (:hi v))) (sta (:hi r)))
       (:get (r s) (lds (:lo s)) (sta (:lo r)) (lds (:hi s)) (sta (:hi r)))
       (:set (s r) (lda (:lo r)) (sts (:lo s)) (lda (:hi r)) (sts (:hi s)))
       (:peek (d a) (ldw (zp (:hi d)) (zp (:lo d)) (zp (:hi a)) (zp (:lo a))))
       (:poke (a s) (stw (zp (:hi a)) (zp (:lo a)) (zp (:hi s)) (zp (:lo s))))
       (:jump (target) (jmp target))
       (:branch-zero (r target) (jzw (:hi r) (:lo r) target))
       (:halt () (hlt))
       (:add (d s) (clc) (lda (:lo d)) (adc (:lo s)) (sta (:lo d)) (lda (:hi d)) (adc (:hi s)) (sta (:hi d)))
       (:sub (d s) (clc) (lda (:lo d)) (sbc (:lo s)) (sta (:lo d)) (lda (:hi d)) (sbc (:hi s)) (sta (:hi d)))
       (:and (d s) (lda (:lo d)) (anda (:lo s)) (sta (:lo d)) (lda (:hi d)) (anda (:hi s)) (sta (:hi d)))
       (:or (d s) (lda (:lo d)) (ora (:lo s)) (sta (:lo d)) (lda (:hi d)) (ora (:hi s)) (sta (:hi d)))
       (:xor (d s) (lda (:lo d)) (eor (:lo s)) (sta (:lo d)) (lda (:hi d)) (eor (:hi s)) (sta (:hi d)))
       (:mul (d s) (mulw (:hi d) (:lo d) (:hi s) (:lo s)))
       (:div (d s) (divw (:hi d) (:lo d) (:hi s) (:lo s)))
       (:mod (d s) (modw (:hi d) (:lo d) (:hi s) (:lo s)))
       (:shl (d s) (shlw (:hi d) (:lo d) (:hi s) (:lo s)))
       (:shr (d s) (shrw (:hi d) (:lo d) (:hi s) (:lo s)))
       (:eq (d s) (lda (:lo d)) (cmp (:lo s)) (lda (:hi d)) (cmpc (:hi s)) (seteq) (sta (:lo d)) (ldi (imm 0)) (sta (:hi d)))
       (:ne (d s) (lda (:lo d)) (cmp (:lo s)) (lda (:hi d)) (cmpc (:hi s)) (setne) (sta (:lo d)) (ldi (imm 0)) (sta (:hi d)))
       (:lt (d s) (lda (:lo d)) (cmp (:lo s)) (lda (:hi d)) (cmpc (:hi s)) (setlt) (sta (:lo d)) (ldi (imm 0)) (sta (:hi d)))
       (:ge (d s) (lda (:lo d)) (cmp (:lo s)) (lda (:hi d)) (cmpc (:hi s)) (setge) (sta (:lo d)) (ldi (imm 0)) (sta (:hi d)))
       (:gt (d s) (lda (:lo s)) (cmp (:lo d)) (lda (:hi s)) (cmpc (:hi d)) (setlt) (sta (:lo d)) (ldi (imm 0)) (sta (:hi d)))
       (:le (d s) (lda (:lo s)) (cmp (:lo d)) (lda (:hi s)) (cmpc (:hi d)) (setge) (sta (:lo d)) (ldi (imm 0)) (sta (:hi d)))))

;; The first argument goes in w1. w3 is a second scratch pair, so a stack argument
;; can pass through it while a computed call target sits in w0.
(defbackend zpfoo-lang-reg-abi (:extends zpfoo-lang-abi)
  (registers :scratch (w0 w3) :caller-saved (w1) :callee-saved (w2))
  (call :args (w1) :order :right-to-left :cleanup :caller :return-address-slots 1))
