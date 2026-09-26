;;;; tests/convention.lisp
;;;; #320, #322: convention lowering -- (:function ...), (:call ...), (:return),
;;;; (:push), (:pop), (:arg i) and (:local i).

(in-package #:lasm)

(fiveam:def-suite convention :in lasm)
(fiveam:in-suite convention)

;;; Fixtures: callfoo (examples/cli/callfoo.lisp, loaded by tests/backend.lisp)
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
    `(defbackend ,name (:machine callfoo)
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

(defbackend cv-up-abi (:machine cv-up)
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

(defparameter +cv-sp+ #x100)

(defun %cv-run (items backend &key (machine 'callfoo) setup)
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
                    'cv-up-abi :machine 'cv-up)))
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
  (eval '(defbackend cv-bare-abi (:machine callfoo)
          (registers :return (a) :callee-saved (c) :stack-pointer sp :operand reg)
          (frame :slot sp-idx)
          (operands (reg call-reg) (sp-idx call-sp-idx))))
  (fiveam:is (search ":call" (%cv-malformed '((:call f)) 'cv-bare-abi)))
  (fiveam:is (search ":push" (%cv-malformed '((:push (reg a))) 'cv-bare-abi)))
  (fiveam:is (search ":alloc" (%cv-malformed '((:function f (:locals 1) (:return))) 'cv-bare-abi)))
  (eval '(defbackend cv-no-operand-abi (:machine callfoo)
          (registers :callee-saved (c) :stack-pointer sp)
          (operands (reg call-reg))))
  (fiveam:is (search ":operand" (%cv-malformed '((:function f (:save (c)) (:return))) 'cv-no-operand-abi)))
  (fiveam:is (search ":slot" (%cv-malformed '((:function f (:args 1) (lds (reg a) (:arg 0)))) 'cv-no-operand-abi))))

(fiveam:test defbackend-checks-the-operand-and-slot-kinds-and-hook-arities
  (flet ((definition-error (form)
           (handler-case (progn (eval form) nil)
             (backend-definition-error (c) (princ-to-string c)))))
    (fiveam:is (search "not a declared operand kind"
                       (definition-error '(defbackend cv-bad-abi (:machine callfoo)
                                           (registers :operand nowhere) (operands (reg call-reg))))))
    (fiveam:is (search "not a declared operand kind"
                       (definition-error '(defbackend cv-bad-abi (:machine callfoo)
                                           (frame :slot nowhere) (operands (reg call-reg))))))
    (fiveam:is (search "takes 1 parameter"
                       (definition-error '(defbackend cv-bad-abi (:machine callfoo)
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

(defbackend cv-up-fp-abi (:extends cv-up-abi :machine cv-up-fp)
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
    (let ((m (%cv-run items 'callfoo-fp-abi :machine 'callfoo-fp :setup '((2 77)))))
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
                    'callfoo-fp-abi :machine 'callfoo-fp)))
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
                    'callfoo-fp-abi :machine 'callfoo-fp :setup '((3 5)))))
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
                    'callfoo-fp-abi :machine 'callfoo-fp)))
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
                    'cv-fp-aligned-abi :machine 'callfoo-fp)))
    (fiveam:is (= 7 (%cv-a m)))
    (fiveam:is (= +cv-sp+ (sref m 'sp)))))

(fiveam:test frame-pointer-frames-work-with-callee-cleanup
  (let ((m (%cv-run '((:call f (imm 3) (imm 4)) (hlt)
                      (:function f (:args 2 :locals 1)
                        (ldf (reg a) (:arg 1))
                        (:return)))
                    'cv-fp-callee-abi :machine 'callfoo-fp)))
    (fiveam:is (= 4 (%cv-a m)))
    (fiveam:is (= +cv-sp+ (sref m 'sp)))))

(fiveam:test frame-pointer-frames-work-when-the-stack-grows-up
  (let ((items '((:call f (imm 21)) (hlt)
                 (:function f (:args 1 :locals 1 :save (c))
                   (ldf (reg a) (:arg 0))
                   (stf (:local 0) (reg a))
                   (ldf (reg b) (:local 0))
                   (:return)))))
    (let ((m (%cv-run items 'cv-up-fp-abi :machine 'cv-up-fp :setup '((2 77)))))
      (fiveam:is (= 21 (%cv-a m)))
      (fiveam:is (= 21 (regref m 'r 1)))
      (fiveam:is (= 77 (regref m 'r 2)))
      (fiveam:is (= +cv-sp+ (sref m 'sp))))))

(fiveam:test frame-pointer-lowering-signals-malformed-items
  (fiveam:is (search "frame pointer"
                     (%cv-malformed '((:function f (:save (fp)) (:return))) 'callfoo-fp-abi)))
  (eval '(defbackend cv-fp-noenter-abi (:extends callfoo-abi :machine callfoo-fp)
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

(eval '(defbackend cv-fp-nostack-abi (:extends callfoo-abi :machine callfoo-fp)
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
    (let ((m (%cv-run items 'callfoo-fp-abi :machine 'callfoo-fp)))
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
