;;;; tests/fixtures/cli/callfoo.lisp
;;;; A machine with register names, a memory stack and a call instruction, and
;;;; the backend a front end targets it through (#113 (old, not migrated)):
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

;; #366: byte access on a machine whose registers are wider than its cells.
;; Byte-address = word-address * 2 (ANIMA-16's LDB/STB, docs/anima16.md in
;; star): even reads/writes the low byte (bits 0-7), odd the high byte
;; (bits 8-15); STB preserves the other byte of the word.
(definstruction callfoo ldb (modes call-rind)
  (encoding (opcode 26) (operand dst :width 1) (operand addr :width 1))
  (semantics
    (set! (r dst)
          (let ((word (mref machine 'ram (truncate (r addr) 2))))
            (if (evenp (r addr)) (logand word #xFF) (logand (ash word -8) #xFF))))))

(definstruction callfoo stb (modes call-indr)
  (encoding (opcode 27) (operand addr :width 1) (operand src :width 1))
  (semantics
    (let* ((cell (truncate (r addr) 2))
           (word (mref machine 'ram cell))
           (byte (logand (r src) #xFF)))
      (set! (mref machine 'ram cell)
            (if (evenp (r addr))
                (logior (logand word #xFF00) byte)
                (logior (logand word #x00FF) (ash byte 8)))))))

;; A load and store by address, for a backend's :peek-label/:poke-label.
(definstruction callfoo ldm (modes call-rt)
  (encoding (opcode 250) (operand dst :width 1) (operand addr :width 1))
  (semantics (set! (r dst) (mref machine 'ram addr))))

(definstruction callfoo stm (modes call-rt)
  (encoding (opcode 251) (operand src :width 1) (operand addr :width 1))
  (semantics (set! (mref machine 'ram addr) (r src))))

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

;; #374: the same operations with an immediate or a stack slot as the source,
;; for the language compiler's optional :OP-imm and :OP-slot operations.
(defmacro defarith-variant (mnemonic mode opcode operand-name source expression)
  `(definstruction callfoo ,mnemonic (modes ,mode)
     (encoding (opcode ,opcode) (operand dst :width 1) (operand ,operand-name :width 1))
     (semantics
       (let* ((x (r dst)) (y ,source)
              (sx (if (>= x 32768) (- x 65536) x))
              (sy (if (>= y 32768) (- y 65536) y)))
         (declare (ignorable sx sy))
         (set! (r dst) (wrap-value ,expression 16))))))

(defmacro defarith-ri (mnemonic opcode expression)
  `(defarith-variant ,mnemonic call-ri ,opcode value value ,expression))

(defmacro defarith-rs (mnemonic opcode expression)
  `(defarith-variant ,mnemonic call-rs ,opcode offset
                     (mref machine 'ram (wrap-value (+ sp offset) 16)) ,expression))

(defmacro defarith-rm (mnemonic opcode expression)
  `(defarith-variant ,mnemonic call-rt ,opcode addr (mref machine 'ram addr) ,expression))

(defarith-ri addri 100 (+ x y))
(defarith-rs addrs 101 (+ x y))
(defarith-ri subri 102 (- x y))
(defarith-rs subrs 103 (- x y))
(defarith-ri seqri 104 (if (= x y) 1 0))
(defarith-rs seqrs 105 (if (= x y) 1 0))
(defarith-ri sltri 106 (if (< sx sy) 1 0))
(defarith-rs sltrs 107 (if (< sx sy) 1 0))

;; The same with a word at a label as the source, for :OP-label.
(defarith-rm addrm 130 (+ x y))
(defarith-rm subrm 131 (- x y))
(defarith-rm seqrm 132 (if (= x y) 1 0))
(defarith-rm sltrm 133 (if (< sx sy) 1 0))

;; #375: compare and jump, on the same source operands and signed values as
;; the operations above, for the compiler's optional :BRANCH-cmp operations.
(defmode call-rrt (expr :register r) "," (expr :register r) "," expr)
(defmode call-rit (expr :register r) "," "#" expr "," expr)
(defmode call-rst (expr :register r) "," "[" "sp" "+" expr "]" "," expr)
(defmode call-rmt (expr :register r) "," expr "," expr)

(defmacro defbranch (mnemonic mode opcode operand-name source expression)
  `(definstruction callfoo ,mnemonic (modes ,mode)
     (encoding (opcode ,opcode) (operand dst :width 1) (operand ,operand-name :width 1)
               (operand target :width 1))
     (semantics
       (let* ((x (r dst)) (y ,source)
              (sx (if (>= x 32768) (- x 65536) x))
              (sy (if (>= y 32768) (- y 65536) y)))
         (declare (ignorable sx sy))
         (when ,expression (set! pc target))))))

(defmacro defbranches (comparisons)
  `(progn
     ,@(loop for (suffix expression) in comparisons
             for offset from 0
             collect `(defbranch ,(intern (format nil "B~Ar" suffix)) call-rrt ,(+ 108 offset) src (r src) ,expression)
             collect `(defbranch ,(intern (format nil "B~Ari" suffix)) call-rit ,(+ 114 offset) value value ,expression)
             collect `(defbranch ,(intern (format nil "B~Ars" suffix)) call-rst ,(+ 120 offset) offset
                                 (mref machine 'ram (wrap-value (+ sp offset) 16)) ,expression)
             collect `(defbranch ,(intern (format nil "B~Arm" suffix)) call-rmt ,(+ 140 offset) addr
                                 (mref machine 'ram addr) ,expression))))

(defbranches ((eq (= x y)) (ne (/= x y)) (lt (< sx sy)) (gt (> sx sy)) (le (<= sx sy)) (ge (>= sx sy))))

;; Arguments go on the stack right to left and the caller removes them; a
;; function finds its first argument above the return address.
(defbackend callfoo-abi (:isa callfoo)
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
       ;; #365: a computed call target -- a register -- goes through callr;
       ;; a label still goes through call. Tried in order, so a register
       ;; operand picks the first clause and a label falls through to the
       ;; second.
       (:call ((f reg)) (callr f))
       (:call (f) (call f))
       (:return () (ret))))

;; The first two arguments go in b and c, and a is free to break an argument
;; swap.
(defbackend callfoo-reg-abi (:isa callfoo)
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
       (:call ((f reg)) (callr f))
       (:call (f) (call f))
       (:return () (ret))))

;; callfoo-abi with what the language compiler needs. :peek and :poke take
;; register names, which the templates put inside a bracket operand.
(defbackend callfoo-lang-abi (:extends callfoo-abi)
  (operands (ind call-ind))
  (ops (:const (r v) (ldi r (imm v)))
       (:get (r slot) (lds r slot))
       (:set (slot r) (sts slot r))
       (:add-imm (d v) (addri d (imm v)))
       (:add-slot (d slot) (addrs d slot))
       (:sub-imm (d v) (subri d (imm v)))
       (:sub-slot (d slot) (subrs d slot))
       (:eq-imm (d v) (seqri d (imm v)))
       (:eq-slot (d slot) (seqrs d slot))
       (:lt-imm (d v) (sltri d (imm v)))
       (:lt-slot (d slot) (sltrs d slot))
       (:peek (d a) (ldx (reg d) (ind a)))
       (:poke (a s) (stx (ind a) (reg s)))
       (:peek-byte (d a) (ldb (reg d) (ind a)))
       (:poke-byte (a s) (stb (ind a) (reg s)))
       (:jump (target) (jmp target))
       (:branch-zero (r target) (jz r target))
       (:branch-eq (a b target) (beqr a b target))
       (:branch-eq-imm (a v target) (beqri a (imm v) target))
       (:branch-eq-slot (a slot target) (beqrs a slot target))
       (:branch-ne (a b target) (bner a b target))
       (:branch-ne-imm (a v target) (bneri a (imm v) target))
       (:branch-ne-slot (a slot target) (bners a slot target))
       (:branch-lt (a b target) (bltr a b target))
       (:branch-lt-imm (a v target) (bltri a (imm v) target))
       (:branch-lt-slot (a slot target) (bltrs a slot target))
       (:branch-gt (a b target) (bgtr a b target))
       (:branch-gt-imm (a v target) (bgtri a (imm v) target))
       (:branch-gt-slot (a slot target) (bgtrs a slot target))
       (:branch-le (a b target) (bler a b target))
       (:branch-le-imm (a v target) (bleri a (imm v) target))
       (:branch-le-slot (a slot target) (blers a slot target))
       (:branch-ge (a b target) (bger a b target))
       (:branch-ge-imm (a v target) (bgeri a (imm v) target))
       (:branch-ge-slot (a slot target) (bgers a slot target))
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
