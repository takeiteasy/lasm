;;;; tests/convention.lisp
;;;; #320, #322: convention lowering -- (:function ...), (:call ...), (:return),
;;;; (:push), (:pop), (:arg i) and (:local i).

(in-package #:lasm)

(fiveam:def-suite convention :in lasm)
(fiveam:in-suite convention)

;;; Fixtures: callfoo (tests/fixtures/cli/callfoo.lisp, loaded by tests/backend.lisp)
;;; under several conventions, and a machine whose stack grows up.

(defmacro %cv-abi (name &key (args :stack) (order :right-to-left) (cleanup :caller) (alignment 1)
                          (registers '(:return (a) :callee-saved (c d))) return-pop extra-ops)
  "EXTRA-OPS add operations to the fixture's, or replace one of the same name."
  (let ((ops (append (remove-if (lambda (op) (member (first op) extra-ops :key #'first))
                                `((:add (d s) (add d s))
                                  (:push (x) (pushv x))
                                  (:pop (x) (popr x))
                                  (:move (d s) (movv d s))
                                  (:alloc (n) (subs (sp) (imm n)))
                                  (:free (n) (adds (sp) (imm n)))
                                  (:call (f) (call f))
                                  (:return () (ret))))
                     (and return-pop '((:return-pop (n) (retn (imm n)))))
                     extra-ops)))
    `(defbackend ,name (:isa callfoo)
       (registers ,@registers :stack-pointer sp :program-counter pc :operand reg)
       (call :args ,args :order ,order :cleanup ,cleanup :return-address-slots 1)
       (frame :grows :down :alignment ,alignment :slot sp-idx)
       (operands (reg call-reg) (imm call-imm) (sp-idx call-sp-idx) (sp call-sp))
       (ops ,@ops))))

(%cv-abi cv-ltr-abi :order :left-to-right)
(%cv-abi cv-callee-abi :cleanup :callee :return-pop t)
(%cv-abi cv-aligned-abi :alignment 2)
(%cv-abi cv-reg-abi :args (b c) :registers (:return (a) :caller-saved (b c) :callee-saved (d)))
(%cv-abi cv-scratch-abi :args (b c) :registers (:return (a) :scratch (b a) :caller-saved (b c) :callee-saved (d)))
(%cv-abi cv-xchg-abi :args (b c d) :registers (:return (a) :caller-saved (b c d))
         :extra-ops ((:exchange (x y) (xchg x y))))
(%cv-abi cv-xchg-scratch-abi :args (b c) :registers (:return (a) :scratch (a) :caller-saved (b c))
         :extra-ops ((:exchange (x y) (xchg x y))))
(%cv-abi cv-indirect-abi :args (b c) :registers (:return (a) :scratch (a) :caller-saved (b c))
         :extra-ops ((:call (f) (callr f))))
(%cv-abi cv-indirect-bare-abi :args (b c) :registers (:return (a) :caller-saved (b c))
         :extra-ops ((:call (f) (callr f))))
;; #365: a register target and a label each pick their own :call clause.
(%cv-abi cv-dispatch-abi :args (b c) :registers (:return (a) :scratch (a) :caller-saved (b c))
         :extra-ops ((:call ((f reg)) (callr f)) (:call (f) (call f))))
(%cv-abi cv-three-abi :args (b c d) :registers (:return (a) :scratch (a) :caller-saved (b c d)))

(defmachine cv-up
  (register pc :width 16)
  (register sp :width 16)
  (register r :width 16 :names (a b c d))
  (memory ram :width 16 :addr-width 16)
  (stack-pointer sp :memory ram :grows :up))

(definstruction cv-up pushv
  (modes
    (call-reg (opcode 9) (operand src :width 1) (semantics (push (r src) sp)))
    (call-imm (opcode 10) (operand :mode) (semantics (push operand sp)))
    (call-sp-idx (opcode 11) (operand offset :width 1)
                 (semantics (push (mref machine 'ram (wrap-value (+ sp offset) 16)) sp)))))
(definstruction cv-up popr (modes call-reg)
  (encoding (opcode 14) (operand dst :width 1))
  (semantics (set! (r dst) (pop sp))))
(definstruction cv-up lds (modes call-rs)
  (encoding (opcode 3) (operand dst :width 1) (operand offset :width 1))
  (semantics (set! (r dst) (mref machine 'ram (wrap-value (+ sp offset) 16)))))
(definstruction cv-up adds (modes call-spi)
  (encoding (opcode 6) (operand :mode))
  (semantics (set! sp (wrap-value (+ sp operand) 16))))
(definstruction cv-up subs (modes call-spi)
  (encoding (opcode 15) (operand :mode))
  (semantics (set! sp (wrap-value (- sp operand) 16))))
(definstruction cv-up call (modes absolute)
  (encoding (opcode 7) (operand :mode))
  (semantics (push pc sp) (set! pc operand)))
(definstruction cv-up ret
  (encoding (opcode 8))
  (semantics (set! pc (pop sp))))
(definstruction cv-up hlt
  (encoding (opcode 0))
  (semantics (trap :halt)))

(defbackend cv-up-abi (:isa cv-up)
  (registers :return (a) :callee-saved (c d) :stack-pointer sp :program-counter pc :operand reg)
  (call :args :stack :order :right-to-left :cleanup :caller :return-address-slots 1)
  (frame :grows :up :slot sp-idx)
  (operands (reg call-reg) (imm call-imm) (sp-idx call-sp-idx) (sp call-sp))
  (ops (:push (x) (pushv x))
       (:pop (x) (popr x))
       (:alloc (n) (adds (sp) (imm n)))
       (:free (n) (subs (sp) (imm n)))
       (:call (f) (call f))
       (:return () (ret))))

(defparameter +cv-sp+ #x800)

(defun %cv-run (items backend &key ((:cpu machine) 'callfoo) setup)
  "Assemble and run ITEMS with the stack at +CV-SP+; returns the machine."
  (let ((m (make-machine machine)))
    (load-program m (assemble-items items :backend backend))
    (setf (sref m 'sp) +cv-sp+)
    (dolist (assignment setup)
      (setf (regref m 'r (first assignment)) (second assignment)))
    (run m)
    m))

(defun %cv-a (m) (regref m 'r 0))

(defun %cv-malformed (items backend)
  (handler-case (progn (assemble-items items :backend backend) nil)
    (items-malformed (c) (items-error-detail c))))

;;; Stack arguments

(fiveam:test a-call-passes-stack-arguments-and-the-function-finds-them
  (let ((m (%cv-run '((:call f (imm 10) (imm 3)) (hlt)
                      (:function f (:args 2) (lds (reg a) (:arg 0)) (lds (reg b) (:arg 1)) (:return)))
                    'callfoo-abi)))
    (fiveam:is (= 10 (%cv-a m)))
    (fiveam:is (= 3 (regref m 'r 1)))
    (fiveam:is (= +cv-sp+ (sref m 'sp)))))

(fiveam:test left-to-right-order-pushes-the-first-argument-first
  (let ((items '((:call f (imm 10) (imm 3)) (hlt)
                 (:function f (:args 2) (lds (reg a) (:arg 0)) (lds (reg b) (:arg 1)) (:return)))))
    (let ((m (%cv-run items 'cv-ltr-abi)))
      (fiveam:is (= 10 (%cv-a m)))
      (fiveam:is (= 3 (regref m 'r 1))))
    (fiveam:is (search "pushv # 10
pushv # 3" (render-items items :backend 'cv-ltr-abi)))
    (fiveam:is (search "pushv # 3
pushv # 10" (render-items items :backend 'callfoo-abi)))))

(fiveam:test callee-cleanup-returns-and-removes-the-arguments
  (let ((items '((:call f (imm 5) (imm 6)) (hlt)
                 (:function f (:args 2) (lds (reg a) (:arg 1)) (:return)))))
    (let ((m (%cv-run items 'cv-callee-abi)))
      (fiveam:is (= 6 (%cv-a m)))
      (fiveam:is (= +cv-sp+ (sref m 'sp))))
    (fiveam:is (search "retn # 2" (render-items items :backend 'cv-callee-abi)))
    (fiveam:is (not (search "adds" (render-items items :backend 'cv-callee-abi))))))

(fiveam:test a-convention-that-needs-the-argument-count-requires-it
  (fiveam:is (search ":args" (%cv-malformed '((:function f () (:return))) 'cv-callee-abi)))
  (fiveam:is (search ":args" (%cv-malformed '((:function f () (:return))) 'cv-ltr-abi)))
  (fiveam:is (null (%cv-malformed '((:function f () (:return))) 'callfoo-abi))))

;;; Locals, saves and the tracked depth

(fiveam:test locals-are-addressed-from-the-stack-pointer
  (let ((m (%cv-run '((:call f) (hlt)
                      (:function f (:locals 2)
                        (:op :add (reg b) (reg b))
                        (movv (reg a) (imm 5)) (movv (reg b) (imm 9))
                        (sts (:local 1) (reg a)) (sts (:local 0) (reg b))
                        (lds (reg a) (:local 1))
                        (lds (reg c) (:local 0))
                        (:return)))
                    'callfoo-abi)))
    (fiveam:is (= 5 (%cv-a m)))
    (fiveam:is (= 9 (regref m 'r 2)))
    (fiveam:is (= +cv-sp+ (sref m 'sp)))))

(fiveam:test saved-registers-are-restored-and-the-frame-is-aligned
  (let* ((items '((:call f) (hlt)
                  (:function f (:locals 2 :save (c))
                    (movv (reg c) (imm 1))
                    (:return))))
         (m (%cv-run items 'cv-aligned-abi :setup '((2 7)))))
    (fiveam:is (= 7 (regref m 'r 2)))
    (fiveam:is (= +cv-sp+ (sref m 'sp)))
    ;; one saved cell and two locals pad to a multiple of two: three cells
    (fiveam:is (search "subs sp, # 3" (render-items items :backend 'cv-aligned-abi)))))

(fiveam:test tracked-pushes-and-pops-shift-argument-offsets
  (let ((items '((:call f (imm 42)) (hlt)
                 (:function f (:args 1)
                   (:push (imm 9))
                   (lds (reg a) (:arg 0))
                   (:pop (reg b))
                   (:return)))))
    (fiveam:is (= 42 (%cv-a (%cv-run items 'callfoo-abi))))
    (fiveam:is (search "[ sp + 2 ]" (render-items items :backend 'callfoo-abi)))
    (fiveam:is (search "[ sp + 1 ]" (render-items '((:function f (:args 1) (lds (reg a) (:arg 0)) (:return)))
                                                   :backend 'callfoo-abi)))))

(fiveam:test arguments-can-pass-a-functions-own-arguments-on
  (let ((m (%cv-run '((:call g (imm 1) (imm 2)) (hlt)
                      (:function g (:args 2) (:call f (:arg 1) (:arg 0)) (:return))
                      (:function f (:args 2) (lds (reg a) (:arg 0)) (:return)))
                    'callfoo-abi)))
    (fiveam:is (= 2 (%cv-a m)))
    (fiveam:is (= +cv-sp+ (sref m 'sp)))))

(fiveam:test a-frame-that-grows-up-addresses-below-the-stack-pointer
  (let ((m (%cv-run '((:call f (imm 21)) (hlt)
                      (:function f (:args 1 :locals 1 :save (c))
                        (lds (reg a) (:arg 0))
                        (:return)))
                    'cv-up-abi :cpu 'cv-up)))
    (fiveam:is (= 21 (%cv-a m)))
    (fiveam:is (= +cv-sp+ (sref m 'sp)))))

;;; Register arguments (#322)

(defparameter *cv-sum-function*
  '(:function f (:args 3)
     (lds (reg a) (:arg 2))
     (:op :add (reg a) (:arg 0))
     (:op :add (reg a) (:arg 1))
     (:return)))

(fiveam:test the-first-arguments-go-in-registers-and-the-rest-spill
  (let* ((items `((:call f (imm 1) (imm 2) (imm 3)) (hlt) ,*cv-sum-function*))
         (m (%cv-run items 'cv-reg-abi))
         (text (render-items items :backend 'cv-reg-abi)))
    (fiveam:is (= 6 (%cv-a m)))
    (fiveam:is (= +cv-sp+ (sref m 'sp)))
    (fiveam:is (search "movv b, # 1" text))
    (fiveam:is (search "movv c, # 2" text))
    (fiveam:is (= 1 (count-if (lambda (line) (search "pushv" line))
                              (uiop:split-string (subseq text 0 (search "f:" text)) :separator '(#\Newline)))))))

(fiveam:test register-moves-do-not-overwrite-a-register-a-later-move-reads
  (let ((m (%cv-run `((:call f (reg c) (imm 2) (imm 0)) (hlt) ,*cv-sum-function*)
                    'cv-reg-abi :setup '((2 40)))))
    (fiveam:is (= 42 (%cv-a m)))))

(fiveam:test a-register-move-cycle-needs-a-scratch-register
  (let ((detail (%cv-malformed `((:call f (reg c) (reg b) (imm 0)) (hlt) ,*cv-sum-function*) 'cv-reg-abi)))
    (fiveam:is (search "cycle" detail))
    (fiveam:is (search ":scratch" detail)))
  (fiveam:is (null (%cv-malformed `((:call f (reg b) (reg c) (imm 0)) (hlt) ,*cv-sum-function*) 'cv-reg-abi))))

(fiveam:test a-register-swap-goes-through-a-free-scratch-register
  (let* ((items `((:call f (reg c) (reg b) (imm 0)) (hlt)
                  (:function f (:args 2) (:op :add (reg a) (:arg 0)) (:return))))
         (m (%cv-run items 'cv-scratch-abi :setup '((1 40) (2 2))))
         (text (render-items items :backend 'cv-scratch-abi)))
    (fiveam:is (= 2 (regref m 'r 1)))
    (fiveam:is (= 40 (regref m 'r 2)))
    (fiveam:is (search "movv a, b" text))))

(fiveam:test a-swap-of-a-functions-own-arguments-is-a-swap
  (let ((m (%cv-run '((:call f (imm 1) (imm 2)) (hlt)
                      (:function f (:args 2) (:call g (:arg 1) (:arg 0)) (:return))
                      (:function g (:args 2) (:op :add (reg a) (:arg 0)) (:return)))
                    'cv-scratch-abi)))
    (fiveam:is (= 2 (regref m 'r 1)))
    (fiveam:is (= 1 (regref m 'r 2)))))

(fiveam:test a-three-register-cycle-is-broken-once
  (let ((m (%cv-run `((:call f (reg c) (reg d) (reg b)) (hlt)
                      (:function f (:args 3) (:return)))
                    'cv-three-abi :setup '((1 10) (2 20) (3 30)))))
    (fiveam:is (= 20 (regref m 'r 1)))
    (fiveam:is (= 30 (regref m 'r 2)))
    (fiveam:is (= 10 (regref m 'r 3)))))

;;; Exchange (#333)

(defun %cv-count (text needle)
  (count-if (lambda (line) (search needle line)) (uiop:split-string text :separator '(#\Newline))))

(fiveam:test a-two-register-swap-is-one-exchange
  (let* ((items `((:call f (reg c) (reg b) (imm 0)) (hlt) (:function f (:args 3) (:return))))
         (m (%cv-run items 'cv-xchg-abi :setup '((1 10) (2 20))))
         (text (render-items items :backend 'cv-xchg-abi)))
    (fiveam:is (= 20 (regref m 'r 1)))
    (fiveam:is (= 10 (regref m 'r 2)))
    (fiveam:is (= 0 (regref m 'r 3)))
    (fiveam:is (= 1 (%cv-count text "xchg")))
    (fiveam:is (= 1 (%cv-count text "movv")))))

(fiveam:test an-n-register-cycle-is-n-minus-one-exchanges
  (let* ((items `((:call f (reg c) (reg d) (reg b)) (hlt) (:function f (:args 3) (:return))))
         (m (%cv-run items 'cv-xchg-abi :setup '((1 10) (2 20) (3 30))))
         (text (render-items items :backend 'cv-xchg-abi)))
    (fiveam:is (= 20 (regref m 'r 1)))
    (fiveam:is (= 30 (regref m 'r 2)))
    (fiveam:is (= 10 (regref m 'r 3)))
    (fiveam:is (= 2 (%cv-count text "xchg")))
    (fiveam:is (= 0 (%cv-count text "movv")))))

(fiveam:test an-exchange-needs-no-scratch-register
  (let* ((items `((:call f (reg c) (reg b)) (hlt) (:function f (:args 2) (:return))))
         (text (render-items items :backend 'cv-xchg-scratch-abi)))
    (fiveam:is (= 1 (%cv-count text "xchg")))
    (fiveam:is (= 0 (%cv-count text "movv a")))))

(fiveam:test an-exchange-is-not-used-for-a-register-no-move-writes
  (let* ((items `((:call f (reg d) (reg b)) (hlt) (:function f (:args 2) (:return))))
         (m (%cv-run items 'cv-xchg-abi :setup '((1 10) (2 20) (3 30))))
         (text (render-items items :backend 'cv-xchg-abi)))
    (fiveam:is (= 30 (regref m 'r 1)))
    (fiveam:is (= 10 (regref m 'r 2)))
    (fiveam:is (= 0 (%cv-count text "xchg")))))

;;; Call targets (#335)

(fiveam:test a-target-in-an-argument-register-is-copied-before-the-moves
  (let* ((items `((:call (reg b) (imm 5)) (hlt) (:function f (:args 1) (:return))))
         (text (render-items items :backend 'cv-indirect-abi)))
    (fiveam:is (search "movv a, b" text))
    (fiveam:is (< (search "movv a, b" text) (search "movv b, # 5" text)))
    (fiveam:is (search "callr a" text))))

(fiveam:test an-indirect-call-runs-the-function-its-target-named
  (let ((m (%cv-run '((ldi (reg b) (imm f)) (:call (reg b) (imm 5)) (hlt)
                      (:function f (:args 1) (movv (reg c) (reg b)) (:return)))
                    'cv-indirect-abi)))
    (fiveam:is (= 5 (regref m 'r 2)))
    (fiveam:is (= +cv-sp+ (sref m 'sp)))))

(fiveam:test a-frame-operand-target-is-copied-too
  (let ((text (render-items '((:function g (:args 1) (:call (:arg 0) (imm 5)) (:return)))
                            :backend 'cv-indirect-abi)))
    (fiveam:is (search "movv a, b" text))
    (fiveam:is (search "callr a" text))))

(fiveam:test a-target-with-no-free-scratch-register-is-malformed
  (fiveam:is (search "call target"
                     (%cv-malformed '((:call (reg b) (imm 5))) 'cv-indirect-bare-abi))))

(fiveam:test a-target-in-a-register-no-move-writes-is-not-copied
  (let ((text (render-items '((:call (reg d) (imm 5))) :backend 'cv-indirect-abi)))
    (fiveam:is (search "callr d" text))
    (fiveam:is (= 1 (%cv-count text "movv")))))

;;; Register-target vs. label :call clauses (#365)

(fiveam:test a-register-target-and-a-label-each-pick-their-own-call-clause
  (let ((reg (render-items '((:call (reg d) (imm 5))) :backend 'cv-dispatch-abi))
        (label (render-items '((:call f (imm 5)) (:function f (:args 1) (:return)))
                             :backend 'cv-dispatch-abi)))
    (fiveam:is (search "callr d" reg))
    (fiveam:is (not (search "callr" label)))
    (fiveam:is (search "call f" label))))

(fiveam:test a-register-target-call-still-runs
  (let ((m (%cv-run '((ldi (reg b) (imm f)) (:call (reg b) (imm 5)) (hlt)
                      (:function f (:args 1) (movv (reg c) (reg b)) (:return)))
                    'cv-dispatch-abi)))
    (fiveam:is (= 5 (regref m 'r 2)))))

(fiveam:test keep-saves-caller-saved-registers-around-a-call
  (let* ((items `((:call f (imm 1) (imm 2) (imm 3) :keep (b d)) (hlt) ,*cv-sum-function*))
         (m (%cv-run items 'cv-reg-abi :setup '((1 50) (3 60))))
         (text (render-items items :backend 'cv-reg-abi)))
    (fiveam:is (= 6 (%cv-a m)))
    (fiveam:is (= 50 (regref m 'r 1)))
    (fiveam:is (= 60 (regref m 'r 3)))
    (fiveam:is (= +cv-sp+ (sref m 'sp)))
    (fiveam:is (search "pushv b" text))
    (fiveam:is (not (search "pushv d" text)))))

(fiveam:test keep-rejects-return-registers-and-a-misplaced-keep
  (fiveam:is (search "return register" (%cv-malformed '((:call f :keep (a))) 'cv-reg-abi)))
  (fiveam:is (search ":keep must end" (%cv-malformed '((:call f :keep (b) (imm 1))) 'cv-reg-abi)))
  (fiveam:is (search "neither" (%cv-malformed '((:call f :keep (b))) 'callfoo-abi))))

;;; Errors

(fiveam:test lowering-outside-a-function-is-malformed
  (fiveam:is (search "inside" (%cv-malformed '((lds (reg a) (:arg 0))) 'callfoo-abi)))
  (fiveam:is (search "inside" (%cv-malformed '((:return)) 'callfoo-abi)))
  (fiveam:is (search "nest" (%cv-malformed '((:function f () (:function g ()))) 'callfoo-abi))))

(fiveam:test frame-operands-are-range-checked
  (fiveam:is (search "argument" (%cv-malformed '((:function f (:args 1) (lds (reg a) (:arg 1)))) 'callfoo-abi)))
  (fiveam:is (search "local" (%cv-malformed '((:function f (:locals 1) (lds (reg a) (:local 1)))) 'callfoo-abi)))
  (fiveam:is (search "expected" (%cv-malformed '((:function f (:args 1) (lds (reg a) (:arg -1)))) 'callfoo-abi))))

(fiveam:test return-needs-a-balanced-stack-and-saves-need-callee-saved-registers
  (fiveam:is (search "deeper" (%cv-malformed '((:function f () (:push (imm 1)) (:return))) 'callfoo-abi)))
  (fiveam:is (search "nothing pushed" (%cv-malformed '((:function f () (:pop (reg a)))) 'callfoo-abi)))
  (fiveam:is (search "callee-saved" (%cv-malformed '((:function f (:save (b)) (:return))) 'callfoo-abi))))

(fiveam:test a-missing-hook-names-the-operation-that-needs-it
  (eval '(defbackend cv-bare-abi (:isa callfoo)
          (registers :return (a) :callee-saved (c) :stack-pointer sp :operand reg)
          (frame :slot sp-idx)
          (operands (reg call-reg) (sp-idx call-sp-idx))))
  (fiveam:is (search ":call" (%cv-malformed '((:call f)) 'cv-bare-abi)))
  (fiveam:is (search ":push" (%cv-malformed '((:push (reg a))) 'cv-bare-abi)))
  (fiveam:is (search ":alloc" (%cv-malformed '((:function f (:locals 1) (:return))) 'cv-bare-abi)))
  (eval '(defbackend cv-no-operand-abi (:isa callfoo)
          (registers :callee-saved (c) :stack-pointer sp)
          (operands (reg call-reg))))
  (fiveam:is (search ":operand" (%cv-malformed '((:function f (:save (c)) (:return))) 'cv-no-operand-abi)))
  (fiveam:is (search ":slot" (%cv-malformed '((:function f (:args 1) (lds (reg a) (:arg 0)))) 'cv-no-operand-abi))))

(fiveam:test defbackend-checks-the-operand-and-slot-kinds-and-hook-arities
  (flet ((definition-error (form)
           (handler-case (progn (eval form) nil)
             (backend-definition-error (c) (princ-to-string c)))))
    (fiveam:is (search "not a declared operand kind"
                       (definition-error '(defbackend cv-bad-abi (:isa callfoo)
                                           (registers :operand nowhere) (operands (reg call-reg))))))
    (fiveam:is (search "not a declared operand kind"
                       (definition-error '(defbackend cv-bad-abi (:isa callfoo)
                                           (frame :slot nowhere) (operands (reg call-reg))))))
    (fiveam:is (search "drop :slot"
                       (definition-error '(defbackend cv-bad-abi (:isa callfoo)
                                           (frame :static t :slot nowhere) (operands (reg call-reg))))))
    (fiveam:is (not (search "drop :slot"
                            (definition-error '(defbackend cv-bad-abi (:isa callfoo)
                                                (frame :slot nowhere) (operands (reg call-reg)))))))
    (fiveam:is (search "takes 1 parameter"
                       (definition-error '(defbackend cv-bad-abi (:isa callfoo)
                                           (operands (reg call-reg)) (ops (:push (a b) (pushv a)))))))))

;;; Frame pointer (#321)

(defmachine (cv-up-fp (:extends cv-up))
  (register fp :width 16))
(definstruction cv-up-fp pushfp (encoding (opcode 18)) (semantics (push fp sp)))
(definstruction cv-up-fp popfp (encoding (opcode 19)) (semantics (set! fp (pop sp))))
(definstruction cv-up-fp movfs (encoding (opcode 20)) (semantics (set! fp sp)))
(definstruction cv-up-fp movsf (encoding (opcode 21)) (semantics (set! sp fp)))
(definstruction cv-up-fp ldf (modes call-rf)
  (encoding (opcode 22) (operand dst :width 1) (operand offset :width 1))
  (semantics (set! (r dst) (mref machine 'ram (wrap-value (+ fp offset) 16)))))
(definstruction cv-up-fp stf (modes call-fr)
  (encoding (opcode 23) (operand offset :width 1) (operand src :width 1))
  (semantics (set! (mref machine 'ram (wrap-value (+ fp offset) 16)) (r src))))

(defbackend cv-up-fp-abi (:extends cv-up-abi :isa cv-up-fp)
  (frame :pointer fp :slot fp-idx)
  (operands (fp-idx call-fp-idx))
  (ops (:enter () (pushfp) (movfs))
       (:leave () (movsf) (popfp))))

(defbackend cv-fp-callee-abi (:extends callfoo-fp-abi)
  (call :cleanup :callee)
  (ops (:return-pop (n) (retn (imm n)))))
(defbackend cv-fp-aligned-abi (:extends callfoo-fp-abi)
  (frame :alignment 2))

(defun %cv-fp (m) (sref m 'fp))

(fiveam:test a-frame-pointer-function-saves-enters-and-leaves
  (let ((items '((:call f (imm 21)) (hlt)
                 (:function f (:args 1 :locals 1 :save (c))
                   (ldf (reg a) (:arg 0))
                   (stf (:local 0) (reg a))
                   (:op :add (reg a) (reg a))
                   (:return)))))
    (fiveam:is (search "f:
pushv c
pushfp
movfs
subs sp, # 1
ldf a, [ fp + 3 ]
stf [ fp + - 1 ], a
add a, a
movsf
popfp
popr c
ret
" (render-items items :backend 'callfoo-fp-abi)))
    (let ((m (%cv-run items 'callfoo-fp-abi :cpu 'callfoo-fp :setup '((2 77)))))
      (fiveam:is (= 42 (%cv-a m)))
      (fiveam:is (= 77 (regref m 'r 2)))
      (fiveam:is (= +cv-sp+ (sref m 'sp))))))

(fiveam:test frame-pointer-slots-do-not-depend-on-the-stack-depth
  (let ((m (%cv-run '((:call f (imm 21)) (hlt)
                      (:function f (:args 1 :locals 1)
                        (:push (imm 9)) (:push (imm 8))
                        (ldf (reg a) (:arg 0))
                        (stf (:local 0) (reg a))
                        (ldf (reg b) (:local 0))
                        (:return)))
                    'callfoo-fp-abi :cpu 'callfoo-fp)))
    (fiveam:is (= 21 (%cv-a m)))
    (fiveam:is (= 21 (regref m 'r 1)))
    (fiveam:is (= +cv-sp+ (sref m 'sp))))
  (fiveam:is (search "ldf a, [ fp + 2 ]"
                     (render-items '((:function f (:args 1) (:push (imm 9)) (ldf (reg a) (:arg 0)) (:return)))
                                   :backend 'callfoo-fp-abi))))

(fiveam:test a-frame-pointer-function-returns-at-any-depth
  (let ((m (%cv-run '((:call f) (hlt)
                      (:function f (:locals 2 :save (d))
                        (:push (imm 1))
                        (:return)))
                    'callfoo-fp-abi :cpu 'callfoo-fp :setup '((3 5)))))
    (fiveam:is (= +cv-sp+ (sref m 'sp)))
    (fiveam:is (= 5 (regref m 'r 3)))))

(fiveam:test frame-pointer-frames-nest-and-restore-the-caller-pointer
  (let ((m (%cv-run '((:call outer (imm 6)) (hlt)
                      (:function outer (:args 1 :locals 1)
                        (:call inner (imm 4))
                        (ldf (reg b) (:arg 0))
                        (:op :add (reg a) (reg b))
                        (:return))
                      (:function inner (:args 1 :locals 2)
                        (ldf (reg a) (:arg 0))
                        (:return)))
                    'callfoo-fp-abi :cpu 'callfoo-fp)))
    (fiveam:is (= 10 (%cv-a m)))
    (fiveam:is (= 0 (%cv-fp m)))
    (fiveam:is (= +cv-sp+ (sref m 'sp)))))

(fiveam:test frame-pointer-padding-counts-the-saved-pointer
  (fiveam:is (search "subs sp, # 2" (render-items '((:function f (:locals 1 :save (c)) (:return)))
                                                  :backend 'cv-fp-aligned-abi)))
  (fiveam:is (search "subs sp, # 1" (render-items '((:function f (:locals 1)) )
                                                  :backend 'cv-fp-aligned-abi)))
  (let ((m (%cv-run '((:call f (imm 7)) (hlt)
                      (:function f (:args 1 :locals 1 :save (c))
                        (ldf (reg a) (:arg 0))
                        (:return)))
                    'cv-fp-aligned-abi :cpu 'callfoo-fp)))
    (fiveam:is (= 7 (%cv-a m)))
    (fiveam:is (= +cv-sp+ (sref m 'sp)))))

(fiveam:test frame-pointer-frames-work-with-callee-cleanup
  (let ((m (%cv-run '((:call f (imm 3) (imm 4)) (hlt)
                      (:function f (:args 2 :locals 1)
                        (ldf (reg a) (:arg 1))
                        (:return)))
                    'cv-fp-callee-abi :cpu 'callfoo-fp)))
    (fiveam:is (= 4 (%cv-a m)))
    (fiveam:is (= +cv-sp+ (sref m 'sp)))))

(fiveam:test frame-pointer-frames-work-when-the-stack-grows-up
  (let ((items '((:call f (imm 21)) (hlt)
                 (:function f (:args 1 :locals 1 :save (c))
                   (ldf (reg a) (:arg 0))
                   (stf (:local 0) (reg a))
                   (ldf (reg b) (:local 0))
                   (:return)))))
    (let ((m (%cv-run items 'cv-up-fp-abi :cpu 'cv-up-fp :setup '((2 77)))))
      (fiveam:is (= 21 (%cv-a m)))
      (fiveam:is (= 21 (regref m 'r 1)))
      (fiveam:is (= 77 (regref m 'r 2)))
      (fiveam:is (= +cv-sp+ (sref m 'sp))))))

(fiveam:test frame-pointer-lowering-signals-malformed-items
  (fiveam:is (search "frame pointer"
                     (%cv-malformed '((:function f (:save (fp)) (:return))) 'callfoo-fp-abi)))
  (eval '(defbackend cv-fp-noenter-abi (:extends callfoo-abi :isa callfoo-fp)
          (frame :pointer fp :slot fp-idx)
          (operands (fp-idx call-fp-idx))))
  (fiveam:is (search ":enter" (%cv-malformed '((:function f () (:return))) 'cv-fp-noenter-abi)))
  (eval '(defbackend cv-fp-noleave-abi (:extends cv-fp-noenter-abi)
          (ops (:enter () (pushfp) (movfs)))))
  (fiveam:is (search ":leave" (%cv-malformed '((:function f () (:return))) 'cv-fp-noleave-abi))))

(fiveam:test defbackend-checks-the-frame-pointer-hook-arities
  (fiveam:is (search "takes 0 parameters"
                     (handler-case (progn (eval '(defbackend cv-bad-fp-abi (:extends callfoo-fp-abi)
                                                  (ops (:enter (x) (pushfp)))))
                                          nil)
                       (backend-definition-error (c) (princ-to-string c))))))

;;; Frame pointer opt-out (#331)

(eval '(defbackend cv-fp-nostack-abi (:extends callfoo-abi :isa callfoo-fp)
        (frame :pointer fp :slot fp-idx)
        (operands (fp-idx call-fp-idx))
        (ops (:enter () (pushfp) (movfs))
             (:leave () (movsf) (popfp)))))

(fiveam:test a-function-can-opt-out-of-the-frame-pointer
  (let ((items '((:call f (imm 21)) (hlt)
                 (:function f (:args 1 :frame nil)
                   (lds (reg a) (:arg 0))
                   (:return)))))
    (fiveam:is (search "f:
lds a, [ sp + 1 ]
ret
" (render-items items :backend 'callfoo-fp-abi)))
    (let ((m (%cv-run items 'callfoo-fp-abi :cpu 'callfoo-fp)))
      (fiveam:is (= 21 (%cv-a m)))
      (fiveam:is (= +cv-sp+ (sref m 'sp))))))

(fiveam:test an-opted-out-function-is-aligned-without-the-frame-pointer-cell
  (let ((items '((:function f (:locals 2 :save (c) :frame nil) (:return)))))
    (fiveam:is (search "subs sp, # 3" (render-items items :backend 'cv-fp-aligned-abi)))
    (fiveam:is (search "subs sp, # 2" (render-items '((:function f (:locals 2 :save (c)) (:return)))
                                                    :backend 'cv-fp-aligned-abi)))))

(fiveam:test an-opted-out-function-needs-the-stack-slot-kind
  (fiveam:is (search ":stack-slot"
                     (%cv-malformed '((:function f (:args 1 :frame nil) (lds (reg a) (:arg 0)) (:return)))
                                    'cv-fp-nostack-abi)))
  (fiveam:is (null (%cv-malformed '((:function f (:frame nil) (:return))) 'cv-fp-nostack-abi))))

(fiveam:test frame-option-checks
  (fiveam:is (search ":frame t needs" (%cv-malformed '((:function f (:frame t) (:return))) 'callfoo-abi)))
  (fiveam:is (null (%cv-malformed '((:function f (:frame nil) (:return))) 'callfoo-abi)))
  (fiveam:is (search ":frame" (%cv-malformed '((:function f (:frame 3) (:return))) 'callfoo-fp-abi)))
  (fiveam:is (search ":stack-slot"
                     (handler-case (progn (eval '(defbackend cv-bad-stack-slot-abi (:extends callfoo-abi)
                                                  (frame :stack-slot nope)))
                                          nil)
                       (backend-definition-error (c) (princ-to-string c))))))

;;; Stack depth across labels and branches (#329)

(%cv-abi cv-grab-abi :extra-ops ((:grab (n) (adds (sp) (imm n)))))

(fiveam:test a-label-must-be-reached-at-its-own-depth
  (fiveam:is (null (%cv-malformed '((:function f () (call there) (:label there) (:return))) 'callfoo-abi)))
  (fiveam:is (null (%cv-malformed '((:function f () (:label back) (:push (imm 1)) (:pop (reg b)) (call back) (:return)))
                                  'callfoo-abi)))
  (let ((detail (%cv-malformed '((:function f ()
                                   (call there) (:push (imm 1)) (:label there) (:pop (reg b)) (:return)))
                               'callfoo-abi)))
    (fiveam:is (search "reached at depth 0 but defined at depth 1" detail))))

(fiveam:test depth-declares-the-depth-after-a-jump
  (let ((items '((:function f ()
                   (:push (imm 1)) (call over) (:pop (reg b)) (:depth 1)
                   (:label over) (:pop (reg b)) (:return)))))
    (fiveam:is (null (%cv-malformed items 'callfoo-abi)))
    (fiveam:is (search "reached at depth 1 but defined at depth 0"
                       (%cv-malformed '((:function f ()
                                          (:push (imm 1)) (call over) (:pop (reg b)) (:label over) (:return)))
                                      'callfoo-abi))))
  (fiveam:is (search "(:depth n)" (%cv-malformed '((:depth 1)) 'callfoo-abi)))
  (fiveam:is (search "(:depth n)" (%cv-malformed '((:function f () (:depth -1))) 'callfoo-abi))))

(fiveam:test frame-pointer-functions-do-not-check-depth
  (fiveam:is (null (%cv-malformed '((:function f ()
                                      (call there) (:push (imm 1)) (:label there) (:pop (reg b)) (:return)))
                                  'callfoo-fp-abi)))
  (fiveam:is (search "reached at depth"
                     (%cv-malformed '((:function f (:frame nil)
                                        (call there) (:push (imm 1)) (:label there) (:pop (reg b)) (:return)))
                                    'callfoo-fp-abi))))

(fiveam:test a-raw-instruction-that-changes-the-depth-is-rejected
  (dolist (item '((pushv (imm 1)) (popr (reg b)) (subs (sp) (imm 2)) (adds (sp) (imm 2))
                  (:op :push (imm 1)) (:op :free 2)))
    (fiveam:is (search "use (:push)/(:pop)"
                       (%cv-malformed `((:function f () ,item (:return))) 'callfoo-abi))
               "~S" item))
  (fiveam:is (search "use (:push)/(:pop)"
                     (%cv-malformed '((:function f () (:op :grab 2) (:return))) 'cv-grab-abi)))
  (fiveam:is (null (%cv-malformed '((:function f (:args 1) (:op :add (reg a) (reg b)) (lds (reg a) (:arg 0)) (:return)))
                                  'callfoo-abi))))

(fiveam:test raw-stack-instructions-are-fine-outside-a-tracked-function
  (fiveam:is (null (%cv-malformed '((pushv (imm 1)) (:op :push (imm 1))) 'callfoo-abi)))
  (fiveam:is (null (%cv-malformed '((:function f () (pushv (imm 1)) (:return))) 'callfoo-fp-abi)))
  (fiveam:is (search "use (:push)/(:pop)"
                     (%cv-malformed '((:function f (:frame nil) (pushv (imm 1)) (:return))) 'callfoo-fp-abi))))

;;; Branch mnemonics and declared stack effects (#337, #338)

(defbackend cv-branches-abi (:extends callfoo-abi) (branches call))
(defbackend cv-effects-abi (:extends cv-branches-abi)
  (ops (:grab (n) :pops n (adds (sp) (imm n)))
       (:push2 (a b) :pushes 2 (pushv a) (pushv b))))

(fiveam:test only-a-branch-mnemonic-is-checked-against-the-label-depth
  (let ((data '((:function f ()
                  (:push (imm 1)) (ldi (reg a) (imm there)) (:pop (reg b))
                  (:label there) (:return)))))
    (fiveam:is (search "reached at depth 1" (%cv-malformed data 'callfoo-abi)))
    (fiveam:is (null (%cv-malformed data 'cv-branches-abi))))
  (fiveam:is (search "reached at depth 1"
                     (%cv-malformed '((:function f () (:push (imm 1)) (call there) (:pop (reg b))
                                       (:label there) (:return)))
                                    'cv-branches-abi))))

(fiveam:test an-operand-kind-name-is-not-a-label-reference
  (fiveam:is (null (%cv-malformed '((:function f ()
                                      (:label imm) (:push (imm 1)) (:pop (reg b)) (:label reg) (:return)))
                                  'callfoo-abi))))

(fiveam:test a-declared-stack-effect-is-tracked
  (let ((items '((:function f (:args 1)
                  (:op :push2 (imm 1) (imm 2)) (lds (reg a) (:arg 0))
                  (:op :grab 2) (lds (reg b) (:arg 0)) (:return)))))
    (let* ((text (render-items items :backend 'cv-effects-abi))
           (lines (remove-if-not (lambda (line) (search "lds" line)) (uiop:split-string text :separator '(#\Newline)))))
      (fiveam:is (= 2 (length lines)))
      (fiveam:is (not (string= (first lines) (second lines)))))
    (fiveam:is (null (%cv-malformed items 'cv-effects-abi)))
    (fiveam:is (search "use (:push)/(:pop)"
                       (%cv-malformed '((:function f () (:op :grab 2) (:return))) 'cv-grab-abi)))))

(fiveam:test a-declared-effect-needs-a-numeric-argument
  (fiveam:is (search "non-negative integer"
                     (%cv-malformed '((:function f () (:op :grab (reg a)) (:return))) 'cv-effects-abi))))

;;; Register names spelled by labels (#336)

(fiveam:test a-label-named-like-a-register-is-not-a-register-read
  (let ((text (render-items '((:call f (imm c) (imm b))) :backend 'cv-scratch-abi)))
    (fiveam:is (= 2 (%cv-count text "movv")))
    (fiveam:is (search "movv b, # c" text))
    (fiveam:is (search "movv c, # b" text))))

(fiveam:test a-call-target-label-named-like-a-register-is-not-copied
  (let ((text (render-items '((:call (imm b) (imm 1) (imm 2))) :backend 'cv-indirect-abi)))
    (fiveam:is (= 2 (%cv-count text "movv")))
    (fiveam:is (search "callr # b" text))))

(fiveam:test a-register-argument-cycle-is-still-broken
  (let ((text (render-items '((:call f (reg c) (reg b))) :backend 'cv-scratch-abi)))
    (fiveam:is (= 3 (%cv-count text "movv")))))

;;; Stack writers (#340)

(defbackend cv-writers-abi (:extends callfoo-abi)
  (stack-writers push)
  (ops (:stash (x) (push x))
       (:stash1 (x) :pushes 1 (push x))))

(fiveam:test a-raw-stack-writer-is-rejected-in-a-tracked-function
  (fiveam:is (search "writes the stack pointer"
                     (%cv-malformed '((:function f () (push (imm 1)) (:return))) 'callfoo-abi)))
  (fiveam:is (search "writes the stack pointer"
                     (%cv-malformed '((:function f () (push (imm 1)) (:return))) 'cv-writers-abi)))
  (fiveam:is (search "writes the stack pointer"
                     (%cv-malformed '((:function f () (:op :stash (imm 1)) (:return))) 'cv-writers-abi))))

(fiveam:test a-stack-writer-with-a-declared-effect-or-outside-tracking-is-accepted
  (fiveam:is (null (%cv-malformed '((:function f () (:op :stash1 (imm 1)) (:pop (reg a)) (:return))) 'cv-writers-abi)))
  (fiveam:is (null (%cv-malformed '((push (imm 1))) 'cv-writers-abi)))
  (fiveam:is (null (%cv-malformed '((:function f () (:depth 0) (:return))) 'cv-writers-abi))))

(defbackend cv-except-abi (:extends callfoo-abi)
  (stack-writers movv :except push))

(fiveam:test a-control-transfer-is-not-a-stack-writer
  (fiveam:is (null (%cv-malformed '((:function g () (:return)) (:function f () (call g) (:return))) 'callfoo-abi))))

(fiveam:test a-stack-writers-clause-adds-to-and-excepts-from-the-derived-list
  (fiveam:is (null (%cv-malformed '((:function f () (push (imm 1)) (:return))) 'cv-except-abi)))
  (fiveam:is (search "writes the stack pointer"
                     (%cv-malformed '((:function f () (movv (reg a) (reg b)) (:return))) 'cv-except-abi))))

;;; Stack writers by variant (#343, #344)

(defmachine (cv-addx (:extends callfoo))
  (register cvx :width 16))

(definstruction cv-addx addx
  (modes
    (call-rr (opcode 30) (operand dst :width 1) (operand src :width 1)
             (semantics (set! (r dst) (wrap-value (+ (r dst) (r src)) 16))))
    (call-spi (opcode 31) (operand :mode)
              (semantics (set! sp (wrap-value (+ sp operand) 16))))))

(definstruction cv-addx drop
  (modes
    (zero-page (opcode 32) (operand v :width 1) (semantics nil))
    (absolute (opcode 33) (operand v :width 2) (semantics (set! sp v)))))

(defbackend cv-addx-abi (:extends callfoo-abi :isa cv-addx)
  (ops (:plus (d s) (addx d s))
       (:bump (n) (addx (sp) (imm n)))))

(defun %cv-in-function (form backend)
  (%cv-malformed `((:function f () ,form (:return))) backend))

(fiveam:test only-the-variant-the-operands-select-is-a-stack-writer
  (fiveam:is (null (%cv-in-function '(addx (reg a) (reg b)) 'cv-addx-abi)))
  (fiveam:is (search "writes the stack pointer" (%cv-in-function '(addx (sp) (imm 2)) 'cv-addx-abi)))
  (fiveam:is (null (%cv-in-function '(:op :plus (reg a) (reg b)) 'cv-addx-abi)))
  (fiveam:is (search "writes the stack pointer" (%cv-in-function '(:op :bump 2) 'cv-addx-abi))))

(fiveam:test a-forced-mode-selects-the-variant-checked
  (fiveam:is (null (%cv-in-function '(drop (:force (:mode zero-page 5))) 'cv-addx-abi)))
  (fiveam:is (search "writes the stack pointer"
                     (%cv-in-function '(drop (:force (:mode absolute 5))) 'cv-addx-abi))))

(fiveam:test a-constant-operand-selects-the-variant-the-assembler-picks
  (fiveam:is (null (%cv-in-function '(drop 5) 'cv-addx-abi)))
  (fiveam:is (search "writes the stack pointer" (%cv-in-function '(drop 70000) 'cv-addx-abi))))

(defun %cv-drop-label (value &key forward)
  (let ((definition `(:directive equ lowpage ,value)))
    (%cv-malformed `(,@(unless forward (list definition))
                     (:function f () (drop lowpage) (:return))
                     ,@(when forward (list definition)))
                   'cv-addx-abi)))

(fiveam:test a-label-operand-selects-the-variant-the-assembler-chose
  (fiveam:is (null (%cv-drop-label 5)))
  (fiveam:is (search "writes the stack pointer" (%cv-drop-label 70000)))
  (fiveam:is (null (%cv-drop-label 5 :forward t)))
  (fiveam:is (search "writes the stack pointer" (%cv-drop-label 70000 :forward t))))

(defun %cv-sized-drop (value entry)
  "The message ENTRY (:render, :size-widest or :size-narrowest) gives for a DROP of a label
defined as VALUE, or of an undefined one when VALUE is NIL; NIL when it accepts."
  (let ((items `(,@(and value `((:directive equ lowpage ,value)))
                 (:function f () (drop lowpage) (:return)))))
    (handler-case
        (progn (ecase entry
                 (:render (render-items items :backend 'cv-addx-abi))
                 (:size-widest (items-size items :backend 'cv-addx-abi :assume :widest))
                 (:size-narrowest (items-size items :backend 'cv-addx-abi :assume :narrowest)))
               nil)
      (items-malformed (condition) (princ-to-string condition)))))

(fiveam:test rendering-and-sizing-judge-a-label-operand-by-the-laid-out-variant
  (dolist (entry '(:render :size-widest :size-narrowest))
    (fiveam:is (null (%cv-sized-drop 5 entry)))
    (fiveam:is (search "writes the stack pointer" (%cv-sized-drop 70000 entry)))))

(fiveam:test an-undefined-label-is-sized-by-the-assumption
  (fiveam:is (search "writes the stack pointer" (%cv-sized-drop nil :render)))
  (fiveam:is (search "writes the stack pointer" (%cv-sized-drop nil :size-widest)))
  (fiveam:is (null (%cv-sized-drop nil :size-narrowest))))

(fiveam:test an-operand-matching-no-variant-is-left-to-the-assembler
  (let ((items '((:function f () (addx (reg a) (imm 2)) (:return)))))
    (fiveam:finishes (render-items items :backend 'cv-addx-abi))
    (fiveam:is (eq :assembler (handler-case (assemble-items items :backend 'cv-addx-abi)
                                (items-malformed () :items)
                                (error () :assembler))))))

(fiveam:test stack-writers-are-listed-per-mode
  (fiveam:is (equal '("ADDS" "ADDX" "DROP" "POPR" "PUSH" "PUSHV" "SUBS")
                    (mapcar #'first (backend-stack-writers 'cv-addx-abi))))
  (fiveam:is (equal '("CALL-SPI") (rest (assoc "ADDX" (backend-stack-writers 'cv-addx-abi) :test #'string=))))
  (fiveam:is (equal '("ABSOLUTE") (rest (assoc "DROP" (backend-stack-writers 'cv-addx-abi) :test #'string=)))))

;;; A write under a CHOICE-CASE clause counts for that alternative only.

(defmachine cv-sel
  (register pc :width 16)
  (register sp :width 16)
  (register a :width 8)
  (memory ram :width 8 :addr-width 16)
  (stack-pointer sp :memory ram :grows :down))

(defmode cv-sel-sp "SP")
(defmode cv-sel-pc "PC")
(defmode cv-sel-reg expr)
(defmode cv-sel-imm "#" expr)
(defmode cv-sel-pop "POP")
(defmode cv-sel-idx "[" expr "," expr "]")
(defmode cv-sel-stk (one-of cv-sel-pop cv-sel-idx))
(defmode cv-sel-kind (one-of (kind cv-sel-sp cv-sel-pc cv-sel-reg)))
(defmode cv-sel-nested (one-of (nest cv-sel-reg cv-sel-stk)))
(defmode cv-sel-src (one-of cv-sel-reg cv-sel-imm))

(definstruction cv-sel ret
  (encoding (opcode #x00))
  (semantics (set! pc (pop sp))))

(definstruction cv-sel zap
  (modes cv-sel-kind)
  (encoding (opcode #x01)
            (sub-opcode (holes kind)
                        (variant (choice cv-sel-sp) (sub 0))
                        (variant (choice cv-sel-pc) (sub 1))
                        (variant (choice cv-sel-reg) (sub 2)))
            (for-choice (kind cv-sel-reg) (operand v :width 1)))
  (semantics (choice-case kind
               (cv-sel-sp (set! sp 0))
               (cv-sel-pc (push pc sp) (set! pc 0))
               (otherwise (set! a v)))))

(definstruction cv-sel bump
  (modes cv-sel-kind)
  (encoding (opcode #x02)
            (sub-opcode (holes kind)
                        (variant (choice cv-sel-sp) (sub 0))
                        (variant (choice cv-sel-pc) (sub 1))
                        (variant (choice cv-sel-reg) (sub 2)))
            (for-choice (kind cv-sel-reg) (operand v :width 1)))
  (semantics (choice-case kind
               (cv-sel-pc (set! pc 0))
               (otherwise (set! sp (+ sp 1))))))

(definstruction cv-sel nst
  (modes cv-sel-nested)
  (encoding (opcode #x03)
            (sub-opcode (holes nest)
                        (variant (choice cv-sel-reg) (sub 0))
                        (variant (choice (cv-sel-stk cv-sel-pop)) (sub 1))
                        (variant (choice (cv-sel-stk cv-sel-idx)) (sub 2)))
            (for-choice (nest cv-sel-reg) (operand v :width 1))
            (for-choice (nest cv-sel-stk cv-sel-idx) (operand base :width 1) (operand off :width 1)))
  (semantics (choice-case (nest cv-sel-stk)
               (cv-sel-pop (pop sp))
               (otherwise (set! a 1)))))

(definstruction cv-sel hs
  (modes cv-sel-src)
  (encoding (opcode #x04)
            (operand src :width 1
              (variant (choice cv-sel-reg) (sub 0))
              (variant (choice cv-sel-imm) (sub 1))))
  (semantics (choice-case src
               (cv-sel-reg (set! a src))
               (cv-sel-imm (set! sp src)))))

(defbackend cv-sel-abi (:isa cv-sel)
  (registers :stack-pointer sp :program-counter pc)
  (ops (:return () (ret))))

(fiveam:test a-write-under-a-choice-case-clause-counts-for-that-alternative
  (flet ((writes (source)
           (and (search "writes the stack pointer" (%cv-in-function source 'cv-sel-abi)) t)))
    (fiveam:is-true (writes '(zap "SP")))
    (fiveam:is-false (writes '(zap "PC")))
    (fiveam:is-false (writes '(zap 5)))
    (fiveam:is-true (writes '(bump 5)))
    (fiveam:is-true (writes '(bump "SP")))
    (fiveam:is-false (writes '(bump "PC")))
    (fiveam:is-true (writes '(nst "POP")))
    (fiveam:is-false (writes '(nst 5)))
    (fiveam:is-false (writes '(nst (:mode cv-sel-idx 1 2))))
    (fiveam:is-true (writes '(hs (:mode cv-sel-imm 5))))
    (fiveam:is-false (writes '(hs 5)))))

(fiveam:test choice-case-writes-carry-their-conditions
  (fiveam:is (equal '(("SP" (kind nil :in (cv-sel-sp)))
                      ("SP" (kind nil :in (cv-sel-pc)))
                      ("PC" (kind nil :in (cv-sel-pc)))
                      ("A" (kind nil :not-in (cv-sel-sp cv-sel-pc))))
                    (instruction-descriptor-written-registers (first (find-instruction-variants 'cv-sel 'zap)))))
  (fiveam:is (equal '(("PC" (kind nil :in (cv-sel-pc)))
                      ("SP" (kind nil :not-in (cv-sel-pc))))
                    (instruction-descriptor-written-registers (first (find-instruction-variants 'cv-sel 'bump)))))
  (fiveam:is (equal '(("SP" (nest ((cv-sel-stk . 0)) :in (cv-sel-pop)))
                      ("A" (nest ((cv-sel-stk . 0)) :not-in (cv-sel-pop))))
                    (instruction-descriptor-written-registers (first (find-instruction-variants 'cv-sel 'nst)))))
  (fiveam:is (equal '(("BUMP" "CV-SEL-KIND") ("HS" "CV-SEL-SRC") ("NST" "CV-SEL-NESTED") ("ZAP" "CV-SEL-KIND"))
                    (backend-stack-writers 'cv-sel-abi))))

;;; A write under a variable holding a CHOICE-CASE result, and in a local macro (#345, #347)

(defmachine cv-var
  (register pc :width 16)
  (register sp :width 16)
  (register a :width 8)
  (memory ram :width 8 :addr-width 16)
  (stack-pointer sp :memory ram :grows :down))

(defmacro cv-write-sp ()
  '(set! sp 0))

(defmacro cv-definstruction-kind (name opcode &body semantics)
  `(definstruction cv-var ,name
     (modes cv-sel-kind)
     (encoding (opcode ,opcode)
               (sub-opcode (holes kind)
                           (variant (choice cv-sel-sp) (sub 0))
                           (variant (choice cv-sel-pc) (sub 1))
                           (variant (choice cv-sel-reg) (sub 2)))
               (for-choice (kind cv-sel-reg) (operand v :width 1)))
     (semantics ,@semantics)))

(cv-definstruction-kind vwhen #x01
  (let* ((p (choice-case kind (cv-sel-sp t) (otherwise nil))))
    (when p (set! sp 0))
    (unless p (set! a 1))))

(cv-definstruction-kind vnot #x02
  (let ((p (choice-case kind (cv-sel-pc t) (otherwise nil))))
    (if (not p) (set! sp 1) (set! pc 0))))

(cv-definstruction-kind vsequential #x03
  (let* ((p (choice-case kind (cv-sel-sp t) (otherwise nil)))
         (q (when p (set! sp 0))))
    q))

(cv-definstruction-kind vcomputed #x04
  (let ((p (choice-case kind (cv-sel-sp (> a 0)) (otherwise nil))))
    (when p (set! sp 0))))

(cv-definstruction-kind vassigned #x05
  (let ((p (choice-case kind (cv-sel-sp t) (otherwise nil))))
    (setq p t)
    (when p (set! sp 0))))

(cv-definstruction-kind vshadowed #x06
  (let ((p (choice-case kind (cv-sel-sp t) (otherwise nil))))
    (let ((p nil))
      (when p (set! sp 0)))))

(cv-definstruction-kind vallcomputed #x17
  (let ((p (choice-case kind (cv-sel-sp (> a 0)) (otherwise (< a 1)))))
    (when p (set! sp 0))))

(cv-definstruction-kind vmixed #x18
  (let ((p (choice-case kind (cv-sel-sp t) (cv-sel-pc nil) (otherwise (> a 0)))))
    (if p (set! sp 0) (set! a 1))))

(cv-definstruction-kind vand #x07
  (let ((p (choice-case kind (cv-sel-sp t) (otherwise nil)))
        (q (choice-case kind (cv-sel-pc t) (otherwise nil))))
    (if (and p (not q)) (set! sp 0) (set! a 1))))

(cv-definstruction-kind vor #x08
  (let ((p (choice-case kind (cv-sel-sp t) (otherwise nil))))
    (if (or p a) (set! a 1) (set! sp 0))))

(cv-definstruction-kind vandelse #x09
  (let ((p (choice-case kind (cv-sel-sp t) (otherwise nil))))
    (if (and p a) (set! a 1) (set! sp 0))))

(cv-definstruction-kind vdirect #x0a
  (if (choice-case kind (cv-sel-sp t) (otherwise nil)) (set! sp 0) (set! a 1)))

(defmacro cv-assign-true (variable)
  `(setq ,variable t))

(cv-definstruction-kind vhidden #x0b
  (let ((p (choice-case kind (cv-sel-pc t) (otherwise nil))))
    (cv-assign-true p)
    (when p (set! sp 0))))

(cv-definstruction-kind vpsetq #x0c
  (let ((p (choice-case kind (cv-sel-pc t) (otherwise nil))))
    (psetq p t)
    (when p (set! sp 0))))

(definstruction cv-var vlocal
  (encoding (opcode #x10))
  (semantics (macrolet ((wipe () '(set! sp 0))) (wipe))))

(definstruction cv-var vnested
  (encoding (opcode #x11))
  (semantics (macrolet ((wipe () '(set! sp 0)))
               (macrolet ((again () '(wipe)))
                 (again)))))

(definstruction cv-var vglobal
  (encoding (opcode #x12))
  (semantics (cv-write-sp)))

(definstruction cv-var vshadow
  (encoding (opcode #x13))
  (semantics (macrolet ((cv-write-sp () '(set! a 0))) (cv-write-sp))))

(macrolet ((cv-outer-wipe () '(set! sp 0)))
  (definstruction cv-var vouter
    (encoding (opcode #x14))
    (semantics (cv-outer-wipe)))
  (definstruction cv-var vouter-inner
    (encoding (opcode #x15))
    (semantics (macrolet ((again () '(set! a 1))) (again) (cv-outer-wipe))))
  (definstruction cv-var vouter-shadowed
    (encoding (opcode #x16))
    (semantics (macrolet ((cv-outer-wipe () '(set! a 0))) (cv-outer-wipe)))))

(macrolet ((cv-target () ''sp))
  (definstruction cv-var vexpander
    (encoding (opcode #x19))
    (semantics (macrolet ((wipe () `(set! ,(cv-target) 0))) (wipe))))
  (definstruction cv-var vexpander-sibling
    (encoding (opcode #x1a))
    (semantics (macrolet ((cv-target () ''a)
                          (wipe () `(set! ,(cv-target) 0)))
                 (wipe)))))

(definstruction cv-var vexpander-nested
  (encoding (opcode #x1b))
  (semantics (macrolet ((cv-nested-target () ''sp))
               (macrolet ((wipe () `(set! ,(cv-nested-target) 0))) (wipe)))))

(defun %cv-var-writes (name)
  (instruction-descriptor-written-registers (first (find-instruction-variants 'cv-var name))))

(fiveam:test a-variable-holding-a-choice-case-carries-its-condition
  (fiveam:is (equal '(("SP" (kind nil :in (cv-sel-sp)))
                      ("A" (kind nil :not-in (cv-sel-sp))))
                    (%cv-var-writes 'vwhen)))
  (fiveam:is (equal '(("SP" (kind nil :not-in (cv-sel-pc)))
                      ("PC" (kind nil :in (cv-sel-pc))))
                    (%cv-var-writes 'vnot)))
  (fiveam:is (equal '(("SP" (kind nil :in (cv-sel-sp)))) (%cv-var-writes 'vsequential))))

(fiveam:test an-and-or-or-test-carries-only-the-conditions-that-hold-in-a-branch
  (fiveam:is (equal '(("SP" (kind nil :in (cv-sel-sp)) (kind nil :not-in (cv-sel-pc))) ("A"))
                    (%cv-var-writes 'vand))
             "the then branch of an AND takes its resolvable conjuncts")
  (fiveam:is (equal '(("A") ("SP" (kind nil :not-in (cv-sel-sp))))
                    (%cv-var-writes 'vor))
             "the else branch of an OR takes its negated disjuncts, the then branch nothing")
  (fiveam:is (equal '(("A" (kind nil :in (cv-sel-sp))) ("SP"))
                    (%cv-var-writes 'vandelse))
             "the else branch of an AND takes nothing")
  (fiveam:is (equal '(("SP" (kind nil :in (cv-sel-sp))) ("A" (kind nil :not-in (cv-sel-sp))))
                    (%cv-var-writes 'vdirect))))

(fiveam:test an-assignment-hidden-in-a-macro-stops-the-variable-being-followed
  (fiveam:is (equal '("SP") (assoc "SP" (%cv-var-writes 'vhidden) :test #'string=)))
  (fiveam:is (equal '("SP") (assoc "SP" (%cv-var-writes 'vpsetq) :test #'string=))))

(fiveam:test a-computed-clause-may-return-either-value
  (fiveam:is (equal '(("SP" (kind nil :in (cv-sel-sp)))) (%cv-var-writes 'vcomputed))
             "the then branch keeps the clause that computes its result")
  (fiveam:is (equal '(("SP" (kind nil :not-in (cv-sel-pc))) ("A" (kind nil :not-in (cv-sel-sp))))
                    (%cv-var-writes 'vmixed))
             "each branch excludes only the clauses that return the other value")
  (fiveam:is (equal '(("SP")) (%cv-var-writes 'vallcomputed))
             "no clause tells the branches apart"))

(fiveam:test a-variable-that-is-not-a-constant-choice-stays-unconditional
  (fiveam:is (equal '("SP") (assoc "SP" (%cv-var-writes 'vassigned) :test #'string=)))
  (fiveam:is (equal '(("SP")) (%cv-var-writes 'vshadowed))))

(fiveam:test a-local-macro-is-expanded-when-looking-for-writes
  (fiveam:is (equal '(("SP")) (%cv-var-writes 'vlocal)))
  (fiveam:is (equal '(("SP")) (%cv-var-writes 'vnested)))
  (fiveam:is (equal '(("SP")) (%cv-var-writes 'vglobal))))

(fiveam:test a-macro-bound-around-the-definstruction-is-expanded
  (fiveam:is (equal '(("SP")) (%cv-var-writes 'vouter)))
  (fiveam:is (equal '(("A") ("SP")) (%cv-var-writes 'vouter-inner)))
  (fiveam:is (equal '(("A")) (%cv-var-writes 'vouter-shadowed))))

(fiveam:test an-expander-body-can-use-the-macros-bound-around-it
  (fiveam:is (equal '(("SP")) (%cv-var-writes 'vexpander)))
  (fiveam:is (equal '(("SP")) (%cv-var-writes 'vexpander-nested)))
  (fiveam:is (equal '(("SP")) (%cv-var-writes 'vexpander-sibling))
             "a sibling macro is not visible to an expander"))

(fiveam:test a-local-macro-shadows-a-global-one-of-the-same-name
  (fiveam:is (equal '(("A")) (%cv-var-writes 'vshadow))))

(defbackend cv-var-abi (:isa cv-var)
  (registers :stack-pointer sp :program-counter pc)
  (ops (:nothing () (vlocal))))

(fiveam:test a-write-under-a-variable-counts-for-its-alternative-only
  (flet ((writes (source)
           (and (search "writes the stack pointer" (%cv-in-function source 'cv-var-abi)) t)))
    (fiveam:is-true (writes '(vwhen "SP")))
    (fiveam:is-false (writes '(vwhen "PC")))
    (fiveam:is-false (writes '(vwhen 5)))
    (fiveam:is-true (writes '(vnot 5)))
    (fiveam:is-false (writes '(vnot "PC")))
    (fiveam:is-true (writes '(vlocal)))
    (fiveam:is-false (writes '(vshadow)))))

;;; #385: (frame :offsets :cells) and (frame :counts :cells) hand the backend
;;; cell units, not slots, for a machine whose slot spans several cells.

(defbackend cv-cell-offsets-abi (:extends widefoo-lang-abi)
  (frame :grows :down :slot sp-idx :counts :slots :offsets :cells))

(defbackend cv-slot-counts-abi (:extends widefoo-lang-abi)
  (frame :grows :down :slot sp-idx :counts :slots))

(defparameter +cv-cell-items+
  '((:function f (:args 2 :locals 1) (lds (reg a) (:arg 1)) (lds (reg b) (:local 0)) (:return))
    (:call f (imm 1) (imm 2))))

(fiveam:test frame-units-default-to-slots
  (let ((text (render-items +cv-cell-items+ :backend 'cv-slot-counts-abi)))
    (fiveam:is (search "subs sp, # 1" text))
    (fiveam:is (search "lds a, [ sp + 3 ]" text))
    (fiveam:is (search "lds b, [ sp + 0 ]" text))
    (fiveam:is (search "adds sp, # 2" text))))

(fiveam:test frame-counts-cells-scale-alloc-and-free-by-the-word
  (let ((text (render-items +cv-cell-items+ :backend 'widefoo-lang-abi)))
    (fiveam:is (search "subs sp, # 2" text))
    (fiveam:is (search "adds sp, # 4" text))))

(fiveam:test frame-offsets-cells-scale-slot-distances-by-the-word
  (let ((text (render-items +cv-cell-items+ :backend 'cv-cell-offsets-abi)))
    (fiveam:is (search "lds a, [ sp + 6 ]" text))
    (fiveam:is (search "lds b, [ sp + 0 ]" text))
    (fiveam:is (search "subs sp, # 1" text))))

(defmachine cv-wide-up
  (register pc :width 16)
  (register sp :width 16)
  (register r :width 16 :names (a b c d))
  (memory ram :width 8 :addr-width 16)
  (stack-pointer sp :memory ram :grows :up :width 16))
(definstruction cv-wide-up lds (modes call-rs)
  (encoding (opcode 3) (operand dst :width 1) (operand offset :width 1))
  (semantics (set! (r dst) (mref machine 'ram (wrap-value (+ sp offset) 16)))))
(definstruction cv-wide-up adds (modes call-spi)
  (encoding (opcode 6) (operand :mode))
  (semantics (set! sp (wrap-value (+ sp operand) 16))))
(definstruction cv-wide-up subs (modes call-spi)
  (encoding (opcode 15) (operand :mode))
  (semantics (set! sp (wrap-value (- sp operand) 16))))
(definstruction cv-wide-up ret
  (encoding (opcode 8))
  (semantics (set! pc (pop sp))))

(defbackend cv-wide-up-abi (:isa cv-wide-up)
  (registers :return (a) :stack-pointer sp :program-counter pc :operand reg)
  (call :args :stack :order :right-to-left :cleanup :caller :return-address-slots 1)
  (frame :grows :up :slot sp-idx :offsets :cells :counts :cells)
  (operands (reg call-reg) (imm call-imm) (sp-idx call-sp-idx) (sp call-sp))
  (ops (:alloc (n) (adds (sp) (imm n)))
       (:free (n) (subs (sp) (imm n)))
       (:return () (ret))))

(fiveam:test frame-cell-units-count-down-from-the-top-when-the-stack-grows-up
  (let ((text (render-items '((:function f (:args 1 :locals 1) (lds (reg a) (:local 0)) (lds (reg b) (:arg 0)) (:return)))
                            :backend 'cv-wide-up-abi)))
    (fiveam:is (search "adds sp, # 2" text))
    (fiveam:is (search "lds a, [ sp + - 2 ]" text))
    (fiveam:is (search "lds b, [ sp + - 6 ]" text))))

(eval '(defbackend cv-wide-up-cells-abi (:extends cv-wide-up-abi)
        (call :args :stack :order :right-to-left :cleanup :caller :return-address-cells 1)))

(fiveam:test a-return-address-in-cells-lies-below-the-arguments-when-the-stack-grows-up
  (let ((text (render-items '((:function f (:args 1 :locals 1) (lds (reg b) (:arg 0)) (:return)))
                            :backend 'cv-wide-up-cells-abi)))
    (fiveam:is (search "lds b, [ sp + - 5 ]" text)
               "a 1-cell return address and a 2-cell local put the word argument at -5, not -6")))

(fiveam:test defbackend-rejects-a-bad-frame-unit
  (fiveam:signals backend-definition-error
    (eval '(defbackend cv-bad-unit-abi (:isa callfoo) (frame :counts :bytes))))
  (fiveam:signals backend-definition-error
    (eval '(defbackend cv-bad-unit-abi (:isa callfoo) (frame :offsets nil)))))

;;; Stack push order (#459): a :push other than the :grows default moves every slot one cell.

(defmacro %cv-def-push-machine (name fp-name abi-name fp-abi-name &key grows push)
  (let ((alloc (if (eq grows :down) 'subs 'adds))
        (free (if (eq grows :down) 'adds 'subs)))
    `(progn
       (defmachine ,name
         (register pc :width 16)
         (register sp :width 16)
         (register r :width 16 :names (a b c d))
         (memory ram :width 16 :addr-width 16)
         (stack-pointer sp :memory ram :grows ,grows :push ,push))
       (definstruction ,name pushv
         (modes
           (call-reg (opcode 9) (operand src :width 1) (semantics (push (r src) sp)))
           (call-imm (opcode 10) (operand :mode) (semantics (push operand sp)))))
       (definstruction ,name popr (modes call-reg)
         (encoding (opcode 14) (operand dst :width 1))
         (semantics (set! (r dst) (pop sp))))
       (definstruction ,name lds (modes call-rs)
         (encoding (opcode 3) (operand dst :width 1) (operand offset :width 1))
         (semantics (set! (r dst) (mref machine 'ram (wrap-value (+ sp offset) 16)))))
       (definstruction ,name adds (modes call-spi)
         (encoding (opcode 6) (operand :mode))
         (semantics (set! sp (wrap-value (+ sp operand) 16))))
       (definstruction ,name subs (modes call-spi)
         (encoding (opcode 15) (operand :mode))
         (semantics (set! sp (wrap-value (- sp operand) 16))))
       (definstruction ,name call (modes absolute)
         (encoding (opcode 7) (operand :mode))
         (semantics (push pc sp) (set! pc operand)))
       (definstruction ,name ret
         (encoding (opcode 8))
         (semantics (set! pc (pop sp))))
       (definstruction ,name hlt
         (encoding (opcode 0))
         (semantics (trap :halt)))
       (defbackend ,abi-name (:isa ,name)
         (registers :return (a) :callee-saved (c d) :stack-pointer sp :program-counter pc :operand reg)
         (call :args :stack :order :right-to-left :cleanup :caller :return-address-slots 1)
         (frame :slot sp-idx)
         (operands (reg call-reg) (imm call-imm) (sp-idx call-sp-idx) (sp call-sp))
         (ops (:push (x) (pushv x))
              (:pop (x) (popr x))
              (:alloc (n) (,alloc (sp) (imm n)))
              (:free (n) (,free (sp) (imm n)))
              (:call (f) (call f))
              (:return () (ret))))
       (defmachine (,fp-name (:extends ,name))
         (register fp :width 16))
       (definstruction ,fp-name pushfp (encoding (opcode 18)) (semantics (push fp sp)))
       (definstruction ,fp-name popfp (encoding (opcode 19)) (semantics (set! fp (pop sp))))
       (definstruction ,fp-name movfs (encoding (opcode 20)) (semantics (set! fp sp)))
       (definstruction ,fp-name movsf (encoding (opcode 21)) (semantics (set! sp fp)))
       (definstruction ,fp-name ldf (modes call-rf)
         (encoding (opcode 22) (operand dst :width 1) (operand offset :width 1))
         (semantics (set! (r dst) (mref machine 'ram (wrap-value (+ fp offset) 16)))))
       (definstruction ,fp-name stf (modes call-fr)
         (encoding (opcode 23) (operand offset :width 1) (operand src :width 1))
         (semantics (set! (mref machine 'ram (wrap-value (+ fp offset) 16)) (r src))))
       (defbackend ,fp-abi-name (:extends ,abi-name :isa ,fp-name)
         (frame :pointer fp :slot fp-idx)
         (operands (fp-idx call-fp-idx))
         (ops (:enter () (pushfp) (movfs))
              (:leave () (movsf) (popfp)))))))

(%cv-def-push-machine cv-post cv-post-fp cv-post-abi cv-post-fp-abi :grows :down :push :post)
(%cv-def-push-machine cv-pre-up cv-pre-up-fp cv-pre-up-abi cv-pre-up-fp-abi :grows :up :push :pre)

(defparameter *cv-push-stack-items*
  '((:call f (imm 21)) (hlt)
    (:function f (:args 1 :locals 1 :save (c))
      (lds (reg a) (:arg 0))
      (:return))))

(defparameter *cv-push-frame-items*
  '((:call f (imm 21)) (hlt)
    (:function f (:args 1 :locals 1 :save (c))
      (ldf (reg a) (:arg 0))
      (stf (:local 0) (reg a))
      (ldf (reg b) (:local 0))
      (:return))))

(fiveam:test a-post-push-stack-shifts-stack-slots-one-cell
  (let ((m (%cv-run *cv-push-stack-items* 'cv-post-abi :cpu 'cv-post)))
    (fiveam:is (= 21 (%cv-a m)))
    (fiveam:is (= +cv-sp+ (sref m 'sp))))
  (fiveam:is (search "lds a, [ sp + 2 ]"
                     (render-items '((:function f (:args 1) (lds (reg a) (:arg 0)) (:return))) :backend 'cv-post-abi))))

(fiveam:test a-pre-push-stack-that-grows-up-shifts-stack-slots-one-cell
  (let ((m (%cv-run *cv-push-stack-items* 'cv-pre-up-abi :cpu 'cv-pre-up)))
    (fiveam:is (= 21 (%cv-a m)))
    (fiveam:is (= +cv-sp+ (sref m 'sp))))
  (fiveam:is (search "lds a, [ sp + - 1 ]"
                     (render-items '((:function f (:args 1) (lds (reg a) (:arg 0)) (:return))) :backend 'cv-pre-up-abi))))

(fiveam:test frame-pointer-slots-follow-the-push-order
  (dolist (spec '((cv-post-fp-abi cv-post-fp) (cv-pre-up-fp-abi cv-pre-up-fp)))
    (let ((m (%cv-run *cv-push-frame-items* (first spec) :cpu (second spec) :setup '((2 77)))))
      (fiveam:is (= 21 (%cv-a m)))
      (fiveam:is (= 21 (regref m 'r 1)))
      (fiveam:is (= 77 (regref m 'r 2)))
      (fiveam:is (= +cv-sp+ (sref m 'sp))))))

(fiveam:test the-default-push-order-records-no-shift
  (fiveam:is (null (getf (backend-descriptor-frame (find-backend 'cv-up-abi)) :push)))
  (fiveam:is (eq :post (getf (backend-descriptor-frame (find-backend 'cv-post-abi)) :push)))
  (fiveam:is (eq :pre (getf (backend-descriptor-frame (find-backend 'cv-pre-up-abi)) :push))))

(defmachine cv-wide-post
  (register pc :width 16)
  (register sp :width 16)
  (register r :width 16 :names (a b c d))
  (memory ram :width 8 :addr-width 16)
  (stack-pointer sp :memory ram :grows :down :push :post :width 16))
(definstruction cv-wide-post lds (modes call-rs)
  (encoding (opcode 3) (operand dst :width 1) (operand offset :width 1))
  (semantics (set! (r dst) (mref machine 'ram (wrap-value (+ sp offset) 16)))))
(definstruction cv-wide-post ret
  (encoding (opcode 8))
  (semantics (set! pc (pop sp))))

(fiveam:test a-multi-cell-slot-over-a-shifted-push-order-needs-cell-offsets
  (fiveam:is (search ":offsets must be :cells"
                     (handler-case (eval '(defbackend cv-wide-post-slots-abi (:isa cv-wide-post)
                                           (registers :return (a) :stack-pointer sp :program-counter pc :operand reg)
                                           (frame :slot sp-idx)
                                           (operands (reg call-reg) (sp-idx call-sp-idx))))
                       (backend-definition-error (c) (princ-to-string c)))))
  (eval '(defbackend cv-wide-post-cells-abi (:isa cv-wide-post)
          (registers :return (a) :stack-pointer sp :program-counter pc :operand reg)
          (call :args :stack :order :right-to-left :cleanup :caller :return-address-slots 1)
          (frame :slot sp-idx :offsets :cells)
          (operands (reg call-reg) (sp-idx call-sp-idx))
          (ops (:return () (ret)))))
  (fiveam:is (search "lds a, [ sp + 3 ]"
                     (render-items '((:function f (:args 1) (lds (reg a) (:arg 0)) (:return)))
                                   :backend 'cv-wide-post-cells-abi))))
