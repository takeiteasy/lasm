;;;; tests/compiler.lisp
;;;; #319: the source language compiled to items through a backend.

(in-package #:lasm)

(fiveam:def-suite compiler :in lasm)
(fiveam:in-suite compiler)

;;; Fixtures: callfoo-lang-abi (examples/cli/callfoo.lisp, loaded by
;;; tests/backend.lisp) with stack arguments, and the same operations over
;;; register arguments. callfoo-lang-fp-abi addresses slots through a frame
;;; pointer. cl-up is a machine whose stack grows up.

(defun %cl-lang-ops ()
  (loop for (name params kinds . forms) in (backend-descriptor-ops (find-backend 'callfoo-lang-abi))
        unless (or (member name '("LOAD" "PUSH" "POP" "MOVE" "ALLOC" "FREE" "CALL" "RETURN") :test #'string=)
                   (some #'identity kinds))
          collect (list* (intern name :keyword) (mapcar #'intern params) forms)))

(eval `(defbackend cl-reg-abi (:extends callfoo-reg-abi)
         (operands (ind call-ind))
         (ops ,@(%cl-lang-ops))))

(defmachine (cl-up (:extends cv-up)))

(definstruction cl-up ldi (modes call-ri)
  (encoding (opcode 1) (operand dst :width 1) (operand value :width 1))
  (semantics (set! (r dst) value)))
(definstruction cl-up movv (modes call-rr)
  (encoding (opcode 12) (operand dst :width 1) (operand src :width 1))
  (semantics (set! (r dst) (r src))))
(definstruction cl-up sts (modes call-sr)
  (encoding (opcode 17) (operand offset :width 1) (operand src :width 1))
  (semantics (set! (mref machine 'ram (wrap-value (+ sp offset) 16)) (r src))))
(definstruction cl-up jmp (modes absolute)
  (encoding (opcode 28) (operand :mode))
  (semantics (set! pc operand)))
(definstruction cl-up jz (modes call-rt)
  (encoding (opcode 29) (operand src :width 1) (operand target :width 1))
  (semantics (when (zerop (r src)) (set! pc target))))
(definstruction cl-up subr (modes call-rr)
  (encoding (opcode 31) (operand dst :width 1) (operand src :width 1))
  (semantics (set! (r dst) (wrap-value (- (r dst) (r src)) 16))))
(definstruction cl-up mulr (modes call-rr)
  (encoding (opcode 32) (operand dst :width 1) (operand src :width 1))
  (semantics (set! (r dst) (wrap-value (* (r dst) (r src)) 16))))
(definstruction cl-up slt (modes call-rr)
  (encoding (opcode 42) (operand dst :width 1) (operand src :width 1))
  (semantics (set! (r dst) (if (< (r dst) (r src)) 1 0))))

(defbackend cl-up-abi (:extends cv-up-abi :machine cl-up)
  (registers :scratch (b))
  (ops (:const (r v) (ldi r (imm v)))
       (:get (r slot) (lds r slot))
       (:set (slot r) (sts slot r))
       (:move (d s) (movv d s))
       (:jump (target) (jmp target))
       (:branch-zero (r target) (jz r target))
       (:halt () (hlt))
       (:sub (d s) (subr d s))
       (:mul (d s) (mulr d s))
       (:lt (d s) (slt d s))))

(defparameter +cl-backends+
  '((callfoo-lang-abi callfoo) (cl-reg-abi callfoo) (callfoo-lang-fp-abi callfoo-fp))
  "Backends, with their machines, that every language test runs on.")

(defun %cl-compile (source backend)
  (compile-program (items-program-items (read-source-from-string source)) :backend backend))

(defun %cl-run (source backend &optional (machine 'callfoo))
  "Compile, assemble and run SOURCE, with the stack at +CV-SP+; returns the machine."
  (let ((m (make-machine machine)))
    (load-program m (assemble-items (%cl-compile source backend) :backend backend))
    (setf (sref m 'sp) +cv-sp+)
    (run m :max-steps 100000)
    m))

(defun %cl-fail (source &optional (backend 'callfoo-lang-abi))
  "The detail of the compile error SOURCE signals, or NIL."
  (handler-case (progn (%cl-compile source backend) nil)
    (program-compile-error (c) (program-compile-error-detail c))))

(defmacro %cl-each-backend ((backend machine) &body body)
  `(loop for (,backend ,machine) in +cl-backends+
         do (progn ,@body)))

;;; Programs

(defparameter +cl-programs+
  '(("(defun fact (n) (if (< n 2) 1 (* n (fact (- n 1))))) (defun main () (fact 5))" . 120)
    ("(defvar total 3)
      (defun main () (let ((i 0)) (while (< i 5) (set total (+ total i)) (set i (+ i 1))) total))" . 13)
    ("(defun add3 (a b c) (+ a (+ b c))) (defun main () (add3 1 2 (add3 3 4 5)))" . 15)
    ("(defun main () (poke 200 7) (poke 201 (+ (peek 200) 1)) (peek 201))" . 8)
    ("(defvar hits 0) (defun bump () (set hits (+ hits 1)) 1)
      (defun main () (and 0 (bump)) (or 1 (bump)) (or 0 (bump)) (and 1 (bump) hits))" . 2)
    ("(defconstant k 40) (defun main () (let ((x 1) (y 2)) (let ((x (+ x k))) (+ x y))))" . 43)
    ("(defun f (a b c d) (- (* a d) (- b c))) (defun main () (f 6 1 2 7))" . 43)
    ("(defun main () (- 5))" . 65531)
    ("(defun main () (not 0))" . 1)
    ("(defun main () (+ (< 3 5) (>= 3 5) (= 4 4) (/= 4 4) (<= 4 4) (> 2 1)))" . 4)
    ("(defun main () (/ 7 2) (mod 7 2) (+ (logand 12 10) (logior 12 10) (logxor 12 10) (shl 1 4) (shr 32 2)))" . 52)
    ("(defun main () (let ((x 9)) (asm (:op :const (reg a) 3) (:op :set (:var x) (reg a))) x))" . 3)
    ("(defun main () (/ 7 0))" . 0)
    ("(defun main () (< (- 1) 2))" . 1)
    ("(defun main () (if (< 1 2) (return 5)) 9)" . 5)
    ("(defun main () (+ 1 (return 5)))" . 5)
    ("(defun main () (poke (return 3) 9))" . 3)
    ("(defun f (n) (while (< n 5) (if (= n 2) (return 99)) (set n (+ n 1))) n)
      (defun main () (f 0))" . 99)
    ("(defun main () (return) 9)" . 0)
    ;; #364: direct operands and reordering, not always the stack.
    ("(defun f (x) (let ((y 3)) (+ (* x y) (- x) (- y 1)))) (defun main () (f 5))" . 12)
    ("(defun main () (let ((x 1)) (+ x (set x 5))))" . 6)
    ("(defvar g 1) (defun bump () (set g 10)) (defun main () (+ g (bump)))" . 11)
    ("(defun main () (let ((x 1)) (+ x (progn (asm (:op :const (reg a) 3) (:op :set (:var x) (reg a))) 0))))" . 1)
    ("(defconstant k 5) (defun main () (- k (+ 1 1)))" . 3)
    ("(defvar g 0) (defun main () (set g 7) (poke 210 g) (+ (peek 210) g))" . 14)
    ("(defun main () (poke 220 (+ 1 2)) (not (peek 220)))" . 0)
    ;; #373: a register, not always the stack, holds a left operand.
    ("(defun g (x) (+ x 1))
      (defun f (n) (+ (g n) (if (= n 0) (return 42) (g n))))
      (defun main () (f 0))" . 42)
    ("(defun g (x) (+ x 1))
      (defun h (a b c) (+ (g a) (+ b c)))
      (defun main () (h 10 20 30))" . 61)
    ;; #376: saved registers inside a loop, and reused outside it.
    ("(defun g (x) (+ x 1))
      (defun f (n) (let ((s 0)) (while (< n 3) (set s (+ s (+ (g n) (g n)))) (set n (+ n 1))) s))
      (defun main () (f 0))" . 12)
    ("(defun g (x) (+ x 1))
      (defun f (n) (while (< n 1) (set n (+ n (+ (g n) (if (= n 0) (return 42) (g n)))))) n)
      (defun main () (f 0))" . 42)
    ("(defun g (x) (+ x 1))
      (defun main () (let ((i 0) (s 0)) (while (< i 2) (set s (+ s (+ (g 1) (g 2)))) (set i (+ i 1)))
                       (+ s (+ (g 3) (g 4)))))" . 19)
    ;; #365: function values and indirect calls.
    ("(defun f (x) (+ x 1)) (defun main () (funcall (function f) 5))" . 6)
    ("(defvar g 0) (defun f (x) (* x 2))
      (defun main () (set g (function f)) (funcall g 21))" . 42)
    ("(defun g () 100) (defun f (x) x) (defun main () (+ (g) (funcall (function f) 1)))" . 101)
    ("(defun fact (n) (if (< n 2) 1 (* n (funcall (function fact) (- n 1)))))
      (defun main () (fact 5))" . 120)
    ;; #366: arrays, strings and byte access.
    ("(defarray buf 4) (defun main () (aset buf 0 10) (aset buf 1 20) (+ (aref buf 0) (aref buf 1)))" . 30)
    ("(defun double (x) (* x 2))
      (defarray fns ((function double) (function double)))
      (defun main () (funcall (aref fns 0) 21))" . 42)
    ("(defstring msg \"hi\") (defun main () (+ (aref msg 0) (aref msg 1)))" . 209)
    ("(defvar w 0) (defun main () (poke-byte w 5) (poke-byte (+ w 1) 9)
      (+ (peek-byte w) (peek-byte (+ w 1))))" . 14)
    ;; #367, #380: macros, evaluated at compile time.
    ("(defmacro inc (v) `(set ,v (+ ,v 1)))
      (defun main () (let ((x 0)) (inc x) x))" . 1)
    ("(defmacro unless (c &rest body) `(if ,c 0 (progn ,@body)))
      (defvar hit 0)
      (defun main () (unless 0 (set hit 1) (set hit (+ hit 10))) hit)" . 11)
    ;; hygiene: the macro's own `tmp` doesn't capture the caller's variable of the same name.
    ("(defmacro swap (a b) `(let ((tmp ,a)) (set ,a ,b) (set ,b tmp)))
      (defun main () (let ((tmp 1) (y 2)) (swap tmp y) (+ (* tmp 100) y)))" . 201)
    ;; a function can use a macro defined later in the file.
    ("(defun main () (inc2 5)) (defmacro inc2 (v) `(+ ,v 1))" . 6)
    ;; a macro's body can call another macro.
    ("(defmacro inc (v) `(set ,v (+ ,v 1)))
      (defmacro inc2 (v) `(progn (inc ,v) (inc ,v)))
      (defun main () (let ((x 0)) (inc2 x) x))" . 2)
    ;; deep nesting, not limited by the expansion budget.
    ("(defmacro inc (v) `(+ ,v 1))
      (defun main () (inc (inc (inc (inc (inc 0))))))" . 5)
    ;; a top-level macro call can expand to a defun.
    ("(defmacro defadd1 (name) `(defun ,name (x) (+ x 1)))
      (defadd1 add1)
      (defun main () (add1 41))" . 42)
    ;; a top-level macro call can expand to a (progn DEF...).
    ("(defmacro both () '(progn (defvar g 5) (defun main () g)))
      (both)" . 5)
    ;; asm in a template substitutes any unquote; register operands pass through.
    ("(defmacro store3 (v) `(asm (:op :const (reg a) 3) (:op :set (:var ,v) (reg a))))
      (defun main () (let ((x 9)) (store3 x) x))" . 3)
    ;; a macro computes its expansion from its (unevaluated) argument.
    ("(defmacro double-or-inc (n) (if (integerp n) `(+ ,n ,n) `(+ ,n 1)))
      (defun main () (+ (double-or-inc 20) (let ((y 4)) (double-or-inc y))))" . 45)
    ;; a defun-for-syntax helper, called with its argument already evaluated.
    ("(defun-for-syntax count-args (xs) (if (null xs) 0 (+ 1 (count-args (cdr xs)))))
      (defmacro argc (&rest xs) (count-args xs))
      (defun main () (argc 1 2 3 4))" . 4)
    ;; ,@ splices a computed list of forms into the template.
    ("(defmacro sum-all (&rest xs) `(+ ,@xs))
      (defun main () (sum-all 1 2 3 4 5))" . 15)
    ;; gensym never collides with a caller's own variable.
    ("(defmacro capture (v) (let ((g (gensym))) `(let ((,g 99)) (+ ,v ,g))))
      (defun main () (let ((g 1)) (capture g)))" . 100)
    ;; a multi-form body runs as an implicit progn; only the last form expands.
    ("(defmacro noisy (v) (list 'quote 'ignored) `(+ ,v 1))
      (defun main () (noisy 4))" . 5)
    ;; #382: a caller's let can't capture a global the template names.
    ("(defvar counter 10) (defun g () counter)
      (defmacro bump () `(set counter (+ counter 1)))
      (defun main () (let ((counter 5)) (bump) (+ (* counter 100) (g))))" . 511)
    ;; ,'NAME reaches the caller's own variable on purpose.
    ("(defmacro bump-mine () `(set ,'n (+ ,'n 1)))
      (defun main () (let ((n 5)) (bump-mine) n))" . 6)
    ;; each expansion's own let is separate from a nested one's, and from the caller's.
    ("(defmacro twice-tmp (v) `(let ((tmp ,v)) (+ tmp tmp)))
      (defun main () (let ((tmp 1)) (+ (twice-tmp (twice-tmp 3)) (twice-tmp tmp))))" . 14)
    ;; a template defun's own parameter, even with a global of the same name.
    ("(defvar x 100) (defmacro defid (name) `(defun ,name (x) x))
      (defid ident) (defun main () (ident 7))" . 7)
    ;; a template's function calls, (function NAME) and asm labels still resolve.
    ("(defun helper (v) (+ v 1))
      (defmacro call-helper (x) `(helper ,x))
      (defmacro call-through (x) `(funcall (function helper) ,x))
      (defun main () (+ (call-helper 4) (call-through 10)))" . 16)
    ("(defmacro skip-set (v)
        `(let ((r 1)) (asm (:op :jump done) (:op :set (:var r) (reg a)) (:label done)) (+ ,v r)))
      (defun main () (skip-set 4))" . 5)
    ;; a template's asm (:var NAME) resolves to the template's own let.
    ("(defmacro seven () `(let ((tmp 0)) (asm (:op :const (reg a) 7) (:op :set (:var tmp) (reg a))) tmp))
      (defun main () (let ((tmp 1)) (+ (* tmp 100) (seven))))" . 107)
    ;; #383: a macro that defines a macro, its inner template's own ,X and ,@X left literal.
    ("(defmacro defadder (name n) `(defmacro ,name (x) `(+ ,x ,',n)))
      (defadder add5 5)
      (defun main () (add5 10))" . 15)
    ("(defmacro def-sum (name) `(defmacro ,name (&rest xs) `(+ ,@xs)))
      (def-sum sum)
      (defun main () (sum 1 2 3))" . 6)
    ;; ,,X and ,@',X reach through the inner template to the outer level.
    ("(defmacro def-const (name v) `(defmacro ,name () `,,v))
      (def-const seven 7)
      (defun main () (+ (seven) (seven)))" . 14)
    ("(defmacro def-const-list (name &rest xs) `(defmacro ,name () `(+ ,@',xs)))
      (def-const-list six 1 2 3)
      (defun main () (six))" . 6))
  "Source and the value main leaves in the accumulator.")

(fiveam:test programs-run-on-every-backend
  (%cl-each-backend (backend machine)
    (loop for (source . expected) in +cl-programs+
          do (let ((m (%cl-run source backend machine)))
               (fiveam:is (= expected (%cv-a m)) "~A on ~A" source backend)
               (fiveam:is (= +cv-sp+ (sref m 'sp)) "the stack is balanced: ~A on ~A" source backend)))))

(fiveam:test a-program-runs-on-a-machine-whose-stack-grows-up
  (let ((m (%cl-run "(defun fact (n) (if (< n 2) 1 (* n (fact (- n 1))))) (defun main () (fact 5))"
                    'cl-up-abi 'cl-up)))
    (fiveam:is (= 120 (%cv-a m)))
    (fiveam:is (= +cv-sp+ (sref m 'sp)))))

(fiveam:test a-global-starts-at-its-initial-value-and-is-shared
  (%cl-each-backend (backend machine)
    (let ((m (%cl-run "(defvar n 7) (defvar zero) (defun bump () (set n (+ n 1)))
                       (defun main () (bump) (bump) (+ n zero))"
                      backend machine)))
      (fiveam:is (= 9 (%cv-a m))))))

(fiveam:test a-function-is-callable-before-its-definition
  (fiveam:is (= 2 (%cv-a (%cl-run "(defun main () (later 1)) (defun later (x) (+ x 1))" 'callfoo-lang-abi)))))

(fiveam:test a-long-run-of-nested-calls-and-lets-keeps-slots-straight
  (%cl-each-backend (backend machine)
    (let ((m (%cl-run "(defun sub (a b) (- a b))
                       (defun main ()
                         (let ((x 10) (y 3))
                           (let ((z (sub x y)))
                             (sub (sub z 1) (sub y (sub 2 1))))))"
                      backend machine)))
      (fiveam:is (= 4 (%cv-a m))))))

(fiveam:test names-are-mangled-so-they-cannot-clash-with-the-machine
  (%cl-each-backend (backend machine)
    (let ((m (%cl-run "(defun add-one (x) (+ x 1)) (defun a (b) (add-one b))
                       (defun sp (c) (a c)) (defun add () (sp 4)) (defvar d 2)
                       (defun main () (+ (add) d))"
                      backend machine)))
      (fiveam:is (= 7 (%cv-a m))))))

(fiveam:test symbols-are-compared-by-name
  (fiveam:is (= 3 (%cv-a (%cl-run "(DEFUN Main () (LET ((X 1)) (+ x 2)))" 'callfoo-lang-abi)))))

;;; Output

(fiveam:test the-program-starts-with-a-stub-that-calls-main-and-halts
  (let ((items (%cl-compile "(defun main () 1)" 'callfoo-lang-abi)))
    (fiveam:is (equal '(:call :op) (mapcar #'first (subseq items 0 2))))
    (fiveam:is (equal '(:op :halt) (list (first (second items)) (second (second items)))))))

;;; #364: direct operands, not always the stack

(defun %cl-push-count (items)
  "How many (:push ...) items ITEMS, compiled output, contains."
  (cond ((and (consp items) (eq :push (first items))) 1)
        ((consp items) (reduce #'+ (mapcar #'%cl-push-count items) :initial-value 0))
        (t 0)))

(fiveam:test simple-operands-do-not-go-through-the-stack
  (%cl-each-backend (backend machine)
    machine
    (dolist (source '("(defun f (n) (+ (* n 2) n)) (defun main () (f 4))"
                       "(defun g (n) (+ n 1)) (defun f (n) (* n (g n))) (defun main () (f 4))"
                       "(defconstant k 5) (defun main () (- k (+ 1 1)))"))
      (fiveam:is (zerop (%cl-push-count (%cl-compile source backend))) "~A on ~A" source backend))))

;;; #374: immediate and slot variants of an operation

(defun %cl-op-names (items)
  "The names of the (:op NAME ...) items in ITEMS, compiled output, in order."
  (cond ((and (consp items) (eq :op (first items))) (list (%designator-name (second items))))
        ((consp items) (mapcan #'%cl-op-names (copy-list items)))
        (t '())))

(fiveam:test a-leaf-right-operand-uses-the-imm-and-slot-variants-a-backend-defines
  (let ((ops (%cl-op-names (%cl-compile "(defconstant k 2)
                                         (defun f (x y) (< (+ (- x k) 1) (+ x y)))
                                         (defun main () (f 5 6))"
                                        'callfoo-lang-abi))))
    (dolist (name '("SUB-IMM" "ADD-IMM" "ADD-SLOT" "LT"))
      (fiveam:is (member name ops :test #'string=) "~A is used" name))
    (fiveam:is (notany (lambda (name) (member name '("SUB" "ADD") :test #'string=)) ops))))

(fiveam:test a-backend-without-variants-loads-the-operand-as-before
  (let ((ops (%cl-op-names (%cl-compile "(defun f (x) (+ x 1)) (defun main () (f 5))" 'callfoo-lang-fp-abi))))
    (fiveam:is (member "ADD" ops :test #'string=))
    (fiveam:is (notany (lambda (name) (search "-IMM" name)) ops))
    (fiveam:is (notany (lambda (name) (search "-SLOT" name)) ops))))

(fiveam:test an-operation-without-a-variant-falls-back-on-a-backend-that-has-others
  (let ((ops (%cl-op-names (%cl-compile "(defun f (x) (* x 3)) (defun main () (f 5))" 'callfoo-lang-abi))))
    (fiveam:is (member "MUL" ops :test #'string=))
    (fiveam:is (member "CONST" ops :test #'string=))))

(fiveam:test a-global-right-operand-does-not-use-a-variant
  (let ((ops (%cl-op-names (%cl-compile "(defvar g 4) (defun f () 1) (defun main () (+ (f) g))" 'callfoo-lang-abi))))
    (fiveam:is (member "ADD" ops :test #'string=))
    (fiveam:is (notany (lambda (name) (search "-IMM" name)) ops))))

(fiveam:test not-compares-with-an-immediate-zero
  (fiveam:is (member "EQ-IMM" (%cl-op-names (%cl-compile "(defun main () (not 3))" 'callfoo-lang-abi))
                     :test #'string=)))

(defparameter +cl-variant-programs+
  '(("(defun f (x) (+ (- x 2) 1)) (defun main () (f 5))" . 4)
    ("(defun f (x y) (= x y)) (defun main () (+ (f 3 3) (f 3 4)))" . 1)
    ("(defun f (x y) (< x y)) (defun main () (+ (f 3 4) (f 4 3) (f 3 3)))" . 1)
    ("(defun main () (+ 1 2 3 4))" . 10)
    ("(defun f (a b) (+ a 1 b 2)) (defun main () (f 10 20))" . 33)
    ("(defun f (a b c d) (- a b c d)) (defun main () (f 20 1 2 3))" . 14)
    ("(defun f (x) (not x)) (defun main () (+ (f 0) (f 5)))" . 1)
    ("(defvar g 4) (defun main () (+ 10 g))" . 14)
    ("(defarray tbl (1 2 3)) (defun main () (- (+ tbl 1) tbl))" . 1)
    ("(defun f () 1) (defun main () (= (function f) (function f)))" . 1)
    ("(defun main () (let ((x 1)) (+ (set x 5) x)))" . 10)
    ("(defun f (a b c d e) (+ (- a b) (* c d) e)) (defun main () (f 9 2 3 4 5))" . 24)
    ("(defun main () (- 0 3))" . 65533))
  "Programs whose values the variants must not change; every backend runs them.")

(fiveam:test variants-and-fallbacks-compute-the-same-values
  (%cl-each-backend (backend machine)
    (loop for (source . expected) in +cl-variant-programs+
          do (fiveam:is (= expected (%cv-a (%cl-run source backend machine))) "~A on ~A" source backend))))

(fiveam:test a-variant-operation-has-its-arity-checked
  (dolist (name '(:add-imm :lt-slot))
    (fiveam:signals backend-definition-error
      (eval `(defbackend cl-variant-arity-abi (:machine callfoo) (ops (,name (a b c) (ldi a b))))))))

;;; #388: a leaf left operand swaps to the right

(fiveam:test a-leaf-left-operand-swaps-when-only-it-has-a-variant
  (flet ((ops (source) (%cl-op-names (%cl-compile source 'callfoo-lang-abi))))
    (let ((ops (ops "(defun g () 5) (defun main () (+ 1 (g)))")))
      (fiveam:is (member "ADD-IMM" ops :test #'string=))
      (fiveam:is (notany (lambda (name) (string= name "ADD")) ops)))
    (let ((ops (ops "(defun g () 5) (defun main () (> 5 (g)))")))
      (fiveam:is (member "LT-IMM" ops :test #'string=) "a comparison flips its direction"))
    (let ((ops (ops "(defun g () 5) (defun main () (= 5 (g)))")))
      (fiveam:is (member "EQ-IMM" ops :test #'string=)))))

(fiveam:test a-left-operand-that-cannot-swap-stays-on-the-left
  (flet ((ops (source) (%cl-op-names (%cl-compile source 'callfoo-lang-abi))))
    (let ((ops (ops "(defun g () 5) (defun main () (- 1 (g)))")))
      (fiveam:is (member "SUB" ops :test #'string=) "- is not commutative")
      (fiveam:is (notany (lambda (name) (string= name "SUB-IMM")) ops)))
    (let ((ops (ops "(defvar v 1) (defun g () 5) (defun main () (+ v (g)))")))
      (fiveam:is (member "ADD" ops :test #'string=) "a global has no variant"))
    (let ((ops (ops "(defun f (x) (+ x 1)) (defun main () (f 1))")))
      (fiveam:is (member "ADD-IMM" ops :test #'string=) "a right operand with a variant is not swapped"))
    (let ((ops (ops "(defun f (x) (+ 1 x)) (defun main () (f 1))")))
      (fiveam:is (member "ADD-SLOT" ops :test #'string=) "both leaves: the right operand's variant is unchanged")
      (fiveam:is (notany (lambda (name) (string= name "ADD-IMM")) ops)))))

(fiveam:test a-swap-does-not-change-what-an-operand-sees
  (%cl-each-backend (backend machine)
    (loop for (source . expected)
            in '(("(defun main () (let ((x 1)) (< x (set x 5))))" . 1)
                 ("(defun main () (let ((x 1)) (+ x (set x 5))))" . 6)
                 ("(defun main () (let ((x 1)) (if (< x (set x 5)) 1 0)))" . 1)
                 ("(defun main () (let ((x 1)) (> (set x 5) x)))" . 0)
                 ("(defvar g 1) (defun bump () (set g 9) 0) (defun main () (+ g (bump)))" . 1)
                 ("(defun g () 5) (defun main () (+ 1 (g)))" . 6)
                 ("(defun g () 5) (defun main () (- 1 (g)))" . 65532)
                 ("(defun g () 5) (defun main () (+ (> 5 (g)) (+ (= 5 (g)) (< 9 (g)))))" . 1))
          do (fiveam:is (= expected (%cv-a (%cl-run source backend machine))) "~A on ~A" source backend))))

;;; #375: a condition branches on a comparison

(defun %cl-branch-names (ops)
  "The fused BRANCH-cmp operation names in OPS, not BRANCH-ZERO."
  (remove-if-not (lambda (name) (and (search "BRANCH-" name) (string/= name "BRANCH-ZERO"))) ops))

(defbackend cl-no-ge-abi (:extends callfoo-lang-abi)
  (without-ops :branch-ge))

(fiveam:test a-comparison-condition-branches-on-the-comparison
  (flet ((ops (source) (%cl-op-names (%cl-compile source 'callfoo-lang-abi))))
    (let ((ops (ops "(defun f (x y) (if (< x y) 1 2)) (defun main () (f 1 2))")))
      (fiveam:is (member "BRANCH-GE-SLOT" ops :test #'string=) "branches to else on the negation")
      (fiveam:is (notany (lambda (name) (member name '("LT" "LT-SLOT" "BRANCH-ZERO") :test #'string=)) ops)))
    (let ((ops (ops "(defun f (x) (while (< x 5) (set x (+ x 1)))) (defun main () (f 1))")))
      (fiveam:is (member "BRANCH-GE-IMM" ops :test #'string=))
      (fiveam:is (notany (lambda (name) (string= name "BRANCH-ZERO")) ops)))
    (let ((ops (ops "(defun g () 1) (defun f () (if (= (g) (g)) 1 2)) (defun main () (f))")))
      (fiveam:is (member "BRANCH-NE" ops :test #'string=) "a right operand with no variant goes through the temp register"))
    (let ((ops (ops "(defun g () 1) (defun f () (if (> 5 (g)) 1 2)) (defun main () (f))")))
      (fiveam:is (member "BRANCH-GE-IMM" ops :test #'string=) "the swapped left operand takes the flipped, negated branch"))))

(fiveam:test and-or-and-not-conditions-jump-without-computing-a-value
  (flet ((ops (source) (%cl-op-names (%cl-compile source 'callfoo-lang-abi))))
    (dolist (source '("(defun f (a b c) (if (and (< a b) (not (= c 0))) 1 2)) (defun main () (f 1 2 3))"
                      "(defun f (a b c) (if (or (< a b) (= c 0)) 1 2)) (defun main () (f 1 2 3))"
                      "(defun f (a b c) (if (not (or (< a b) (and (= c 0) (> a c)))) 1 2)) (defun main () (f 1 2 3))"))
      (let ((ops (ops source)))
        (fiveam:is (%cl-branch-names ops) "~A" source)
        (fiveam:is (notany (lambda (name) (member name '("BRANCH-ZERO" "EQ" "LT" "GT" "EQ-IMM") :test #'string=)) ops) "~A" source)))))

(fiveam:test a-value-and-or-still-computes-its-comparisons
  (let ((ops (%cl-op-names (%cl-compile "(defun f (a b) (set a (and (< a b) 1))) (defun main () (f 1 2))"
                                        'callfoo-lang-abi))))
    (fiveam:is (member "LT-SLOT" ops :test #'string=))
    (fiveam:is (member "BRANCH-ZERO" ops :test #'string=))))

(fiveam:test a-backend-without-branch-operations-compiles-as-before
  (let ((ops (%cl-op-names (%cl-compile "(defun f (x) (if (< x 5) 1 2)) (defun main () (f 1))" 'callfoo-lang-fp-abi))))
    (fiveam:is (member "LT" ops :test #'string=))
    (fiveam:is (member "BRANCH-ZERO" ops :test #'string=))
    (fiveam:is (null (%cl-branch-names ops)))))

(fiveam:test a-comparison-without-its-branch-operation-falls-back
  (let ((ops (%cl-op-names (%cl-compile "(defun f (x) (if (< x 5) 1 2)) (defun main () (f 1))" 'cl-no-ge-abi))))
    (fiveam:is (member "LT-IMM" ops :test #'string=))
    (fiveam:is (member "BRANCH-ZERO" ops :test #'string=))
    (fiveam:is (null (%cl-branch-names ops)))))

(fiveam:test a-malformed-condition-keeps-its-error
  (fiveam:is (search "takes 2 operands" (%cl-fail "(defun main () (if (< 1) 1 0))")))
  (fiveam:is (search "not is malformed" (%cl-fail "(defun main () (if (not) 1 0))")))
  (fiveam:is (search "not is malformed" (%cl-fail "(defun main () (while (not 1 2) 1))"))))

(defun %cl-signed (value)
  (if (>= value 32768) (- value 65536) value))

(defparameter +cl-condition-shapes+
  (list (cons "(if (~A x y) 1 0)" (lambda (c x y) (funcall c x y)))
        (cons "(if (~A x (+ y 0)) 1 0)" (lambda (c x y) (funcall c x y)))
        (cons "(if (~A x 3) 1 0)" (lambda (c x y) (declare (ignore y)) (funcall c x 3)))
        (cons "(if (~A 3 y) 1 0)" (lambda (c x y) (declare (ignore x)) (funcall c 3 y)))
        (cons "(if (not (~A x y)) 1 0)" (lambda (c x y) (not (funcall c x y))))
        (cons "(if (and (~A x y) (~A y 3)) 1 0)" (lambda (c x y) (and (funcall c x y) (funcall c y 3))))
        (cons "(if (or (~A x y) (~A y 3)) 1 0)" (lambda (c x y) (or (funcall c x y) (funcall c y 3))))
        (cons "(if (not (and (~A x y) (not (~A y 3)))) 1 0)"
              (lambda (c x y) (not (and (funcall c x y) (not (funcall c y 3)))))))
  "Condition source, with the value it must give for comparison C on signed X and Y.")

(fiveam:test conditions-give-the-same-values-with-and-without-branch-operations
  (dolist (backend '((callfoo-lang-abi callfoo) (cl-no-ge-abi callfoo) (callfoo-lang-fp-abi callfoo-fp)))
    (loop for (name function) in '(("=" =) ("/=" /=) ("<" <) (">" >) ("<=" <=) (">=" >=))
          do (loop for (control . expected) in +cl-condition-shapes+
                   do (loop for (x y) in '((1 2) (2 1) (3 3) (65535 1) (1 65535) (65535 65535))
                            for source = (format nil "(defun f (x y) ~A) (defun main () (f ~D ~D))"
                                                 (format nil control name name name) x y)
                            for want = (if (funcall expected (lambda (a b) (funcall function a b))
                                                    (%cl-signed x) (%cl-signed y))
                                           1 0)
                            do (fiveam:is (= want (%cv-a (%cl-run source (first backend) (second backend))))
                                          "~A on ~A" source (first backend)))))))

(fiveam:test loops-and-nested-conditions-run-on-every-backend
  (%cl-each-backend (backend machine)
    (loop for (source . expected)
            in '(("(defun main () (let ((i 0)) (while (< i 5) (set i (+ i 1))) i))" . 5)
                 ("(defun main () (let ((i 0)) (while (and (< i 5) (/= i 3)) (set i (+ i 1))) i))" . 3)
                 ("(defun main () (let ((i 0)) (while (or (> i 8) (< i 4)) (set i (+ i 1))) i))" . 4)
                 ("(defun main () (if (and) 1 2))" . 1)
                 ("(defun main () (if (or) 1 2))" . 2)
                 ("(defun main () (if (not (and)) 1 2))" . 2)
                 ("(defun f (a) (if (< a 0) 7 8)) (defun main () (f 65535))" . 7)
                 ("(defun main () (if (< 1 2) (if (> 1 2) 3 4) 5))" . 4))
          do (fiveam:is (= expected (%cv-a (%cl-run source backend machine))) "~A on ~A" source backend))))

(fiveam:test a-branch-operation-has-its-arity-checked
  (dolist (name '(:branch-lt-imm :branch-ge-slot :branch-eq))
    (fiveam:signals backend-definition-error
      (eval `(defbackend cl-branch-arity-abi (:machine callfoo) (ops (,name (a b) (ldi a b))))))))

;;; #373: a register, not always the stack, holds a left operand

(defun %cl-function-options (items name)
  "The (:args ... :locals ... [:save (...)]) options of function NAME in ITEMS."
  (let ((label (%cc-mangle "fn" name)))
    (third (find-if (lambda (item) (and (eq :function (first item)) (%same-name-p label (second item))))
                    items))))

(defparameter +cl-loop-calls+
  "(defun f (n) n)
   (defun main () (let ((i 0)) (while (< i 1) (+ (f 1) (f 2)) (set i 1)) 0))"
  "One call-holding site inside a while.")

(fiveam:test a-call-in-a-loop-holds-the-left-in-a-saved-register
  (let ((items (%cl-compile +cl-loop-calls+ 'callfoo-lang-abi)))
    (fiveam:is (zerop (%cl-push-count items)))
    (fiveam:is (equal '("C") (mapcar #'%designator-name (getf (%cl-function-options items "main") :save))))))

(fiveam:test a-one-off-call-outside-a-loop-uses-the-stack
  (let ((items (%cl-compile "(defun f (n) n) (defun main () (+ (f 1) (f 2)))" 'callfoo-lang-abi)))
    (fiveam:is (= 1 (%cl-push-count items)))
    (fiveam:is (null (getf (%cl-function-options items "main") :save)))))

(fiveam:test a-saved-register-is-reused-outside-the-loop
  (let ((items (%cl-compile "(defun f (n) n)
                             (defun main () (let ((i 0))
                               (while (< i 1) (+ (f 1) (f 2)) (set i 1))
                               (+ (f 3) (f 4))))"
                            'callfoo-lang-abi)))
    (fiveam:is (zerop (%cl-push-count items)))
    (fiveam:is (equal '("C") (mapcar #'%designator-name (getf (%cl-function-options items "main") :save))))))

(fiveam:test a-call-free-right-operand-holds-the-left-in-a-volatile-register
  (let ((items (%cl-compile "(defvar g 300) (defvar h 301)
                             (defun main () (poke g 5) (poke h 6) (+ (peek g) (* (peek h) 2)))"
                            'cl-reg-abi)))
    (fiveam:is (zerop (%cl-push-count items)))
    (fiveam:is (null (getf (%cl-function-options items "main") :save)))))

(fiveam:test running-out-of-registers-still-falls-back-to-the-stack
  (fiveam:is (= 1 (%cl-push-count (%cl-compile "(defun f (n) n)
                                                 (defun main () (let ((i 0)) (while (< i 1)
                                                   (+ (f 1) (+ (f 2) (+ (f 3) (f 4)))) (set i 1)) 0))"
                                               'callfoo-lang-abi)))))

;;; #377: a declared clobber list keeps the register path

(defparameter +cl-asm-source+
  "(defun main () (let ((x 1))
     (+ x (progn (asm ~A (:op :const (reg b) 3) (:op :set (:var x) (reg b))) 7))))"
  "An asm in a right operand, with the declaration ~A; x is 1 unless the asm is wrongly reordered.")

(defun %cl-asm-source (declaration)
  (format nil +cl-asm-source+ declaration))

(fiveam:test asm-in-the-right-operand-without-a-declaration-uses-the-stack
  (dolist (backend '(callfoo-lang-abi cl-reg-abi))
    (fiveam:is (= 1 (%cl-push-count (%cl-compile (%cl-asm-source "") backend))))))

(fiveam:test asm-that-declares-its-clobbers-keeps-the-register-path
  (fiveam:is (zerop (%cl-push-count (%cl-compile (%cl-asm-source "(:clobbers b)") 'cl-reg-abi))))
  (fiveam:is (zerop (%cl-push-count (%cl-compile (%cl-asm-source "(:clobbers)") 'cl-reg-abi))))
  (fiveam:is (= 8 (%cv-a (%cl-run (%cl-asm-source "(:clobbers b)") 'cl-reg-abi)))))

(fiveam:test asm-declaring-the-only-free-register-uses-the-stack
  (fiveam:is (= 1 (%cl-push-count (%cl-compile (%cl-asm-source "(:clobbers C)") 'cl-reg-abi))))
  (fiveam:is (= 8 (%cv-a (%cl-run (%cl-asm-source "(:clobbers c)") 'cl-reg-abi)))))

(fiveam:test a-declared-callee-saved-clobber-is-saved-by-the-function
  (let ((items (%cl-compile "(defun main () (asm (:clobbers d) (:op :const (reg d) 3)) 0)" 'cl-reg-abi)))
    (fiveam:is (equal '("D") (mapcar #'%designator-name (getf (%cl-function-options items "main") :save)))))
  (fiveam:is (null (getf (%cl-function-options (%cl-compile "(defun main () (asm (:clobbers b)) 0)" 'cl-reg-abi) "main") :save)))
  (fiveam:is (null (getf (%cl-function-options (%cl-compile "(defun main () (asm (:op :halt)) 0)" 'cl-reg-abi) "main") :save))))

(fiveam:test a-clobber-declaration-is-not-emitted
  (let ((items (%cl-compile (%cl-asm-source "(:clobbers b)") 'cl-reg-abi)))
    (fiveam:is (null (find-if (lambda (item) (and (consp item) (eq :clobbers (first item)))) (fourth (find :function items :key #'first)))))))

(fiveam:test a-clobber-declaration-names-registers
  (fiveam:is (search "nope" (%cl-fail (%cl-asm-source "(:clobbers nope)"))))
  (fiveam:is (%cl-fail (%cl-asm-source "(:clobbers 3)"))))

(fiveam:test function-items-declare-their-arguments-and-locals
  (let ((function (find-if (lambda (item) (eq :function (first item)))
                           (%cl-compile "(defun f (a b) (let ((x a) (y b)) (+ x y))) (defun main () (f 1 2))"
                                        'callfoo-lang-abi))))
    (fiveam:is (equal '(:args 2 :locals 2) (third function)))))

(fiveam:test compiled-items-write-and-read-back-to-the-same-cells
  (%cl-each-backend (backend machine)
    (let* ((source "(defvar g 5) (defun f (n) (if (< n 2) g (* n (f (- n 1)))))
                    (defun main () (poke 300 (f 4)) (asm (:label spot) (:directive byte 1)) (peek 300))")
           (program (compile-source (read-source-from-string source) :backend backend))
           (text (with-output-to-string (out) (write-items-program program out)))
           (again (read-items-from-string text)))
      (fiveam:is (equalp (assembly-cells (assemble-items (items-program-items program) :backend backend))
                         (assembly-cells (assemble-items (items-program-items again) :backend backend)))
                 "~A" backend))))

(fiveam:test defarray-and-defstring-data-writes-and-reads-back-to-the-same-cells
  (%cl-each-backend (backend machine)
    (let* ((source "(defun f (x) x) (defarray fns ((function f))) (defstring s \"hi\")
                    (defun main () (+ (funcall (aref fns 0) 1) (aref s 0)))")
           (program (compile-source (read-source-from-string source) :backend backend))
           (text (with-output-to-string (out) (write-items-program program out)))
           (again (read-items-from-string text)))
      (fiveam:is (equalp (assembly-cells (assemble-items (items-program-items program) :backend backend))
                         (assembly-cells (assemble-items (items-program-items again) :backend backend)))
                 "~A" backend))))

;;; #368: words wider than one cell. widefoo-lang-abi (examples/cli/widefoo.lisp,
;;; loaded by tests/backend.lisp) has 16-bit registers over 8-bit cells, so
;;; BACKEND-WORD-CELLS is 2; callfoo-lang-abi's is 1.

(fiveam:test backend-word-cells-follows-the-stack-pointers-width
  (fiveam:is (= 1 (backend-word-cells 'callfoo-lang-abi)))
  (fiveam:is (= 1 (backend-word-cells 'callfoo-lang-fp-abi)))
  (fiveam:is (= 2 (backend-word-cells 'widefoo-lang-abi))))

(fiveam:test a-word-wider-than-a-cell-runs-globals-arrays-and-strings
  (dolist (case '(("(defvar x 1000) (defvar y 2000) (defun main () (+ x y))" . 3000)
                  ("(defarray arr (10 20 30)) (defun main () (+ (aref arr 0) (+ (aref arr 1) (aref arr 2))))" . 60)
                  ("(defarray arr (10 20 30)) (defun main () (let ((i 2)) (aset arr i 99) (aref arr 2)))" . 99)
                  ("(defstring s \"hi\") (defun main () (+ (aref s 0) (aref s 1)))" . 209)
                  ("(defun f (x) x) (defarray fns ((function f))) (defun main () (funcall (aref fns 0) 42))" . 42)
                  ("(defun fact (n) (if (< n 2) 1 (* n (fact (- n 1))))) (defun main () (fact 5))" . 120)))
    (destructuring-bind (source . expected) case
      (let ((m (%cl-run source 'widefoo-lang-abi 'widefoo)))
        (fiveam:is (= expected (regref m 'r 0)) "~A" source)
        (fiveam:is (= +cv-sp+ (sref m 'sp)) "the stack is balanced: ~A" source)))))

(fiveam:test a-word-wider-than-a-cell-lays-globals-and-arrays-out-by-it
  (let* ((source "(defvar x 5) (defarray arr (1 2 3)) (defstring s \"hi\") (defun main () 1)")
         (items (%cl-compile source 'widefoo-lang-abi))
         (res (find-if (lambda (i) (and (consp i) (eq (first i) :directive) (%same-name-p (second i) "res"))) items)))
    (fiveam:is (= 2 (third res)) "a global reserves a whole word (.res 2)")
    (fiveam:is (= 2 (count-if (lambda (i) (and (consp i) (eq (first i) :directive) (%same-name-p (second i) "emit"))) items))
               "the initialised defarray and defstring each use .emit")
    (fiveam:is (every (lambda (i) (eql 2 (third i)))
                      (remove-if-not (lambda (i) (and (consp i) (eq (first i) :directive) (%same-name-p (second i) "emit"))) items))
               ".emit's width is the word size")))

(fiveam:test a-three-cell-word-initialises-data-with-emit
  (eval '(defmachine cl-w3-machine
           (register sp :width 24) (register pc :width 16) (register r :width 24 :names (a b))
           (memory ram :width 8 :addr-width 16)
           (stack-pointer sp :memory ram :width 24)))
  (eval '(defmode cl-w3-reg (expr :register r)))
  (eval '(defbackend cl-w3-abi (:machine cl-w3-machine)
          (registers :return (a) :scratch (a b) :stack-pointer sp :operand reg)
          (operands (reg cl-w3-reg))))
  (fiveam:is (= 3 (backend-word-cells 'cl-w3-abi)))
  (fiveam:is (search "needs the operation" (%cl-fail "(defarray arr (1 2)) (defstring s \"a\") (defun main () 1)" 'cl-w3-abi)))
  (fiveam:is (search "needs the operation" (%cl-fail "(defarray arr 2) (defun main () 1)" 'cl-w3-abi)))
  (fiveam:is (equalp #(1 0 0 2 0 0 97 0 0 0 0 0)
                     (assembly-cells (assemble ".emit 3, 1, 2
.emit 3, \"a\", 0" :machine 'cl-w3-machine)))))

(fiveam:test a-source-file-can-name-its-backend
  (let ((program (read-source-from-string "(:program (:backend callfoo-lang-abi :origin 4)) (defun main () 1)")))
    (fiveam:is (%same-name-p 'callfoo-lang-abi (items-program-backend program)))
    (fiveam:is (= 4 (items-program-origin program)))
    (fiveam:is (= 1 (length (items-program-items program))))
    (let ((compiled (compile-source program)))
      (fiveam:is (= 4 (items-program-origin compiled)))
      (fiveam:is (eq :call (first (first (items-program-items compiled))))))
    (fiveam:signals program-compile-error
      (compile-source (read-source-from-string "(defun main () 1)")))))

;;; Errors

(fiveam:test compile-errors-name-the-problem
  (dolist (case '(("(defun main () x)" "unknown variable x")
                  ("(defun main () (nope 1))" "unknown function nope")
                  ("(defun f (a) a) (defun main () (f 1 2))" "f takes 1 argument, got 2")
                  ("(defun f () 1)" "needs (defun main")
                  ("(defun main (x) 1)" "needs (defun main")
                  ("(defun main () 1) (defun main () 2)" "main is defined twice")
                  ("(defun if () 1) (defun main () 1)" "if is a built-in form")
                  ("(defconstant k 1) (defun main () (set k 2))" "k is a constant")
                  ("(defun main () (if 1))" "if is malformed")
                  ("(defun main () (+ 1))" "takes 2 operands")
                  ("(defun main () (< 1 2 3))" "takes 2 operands")
                  ("(defun main () (let ((x)) x))" "not (NAME VALUE)")
                  ("(defun main () (asm (lds (reg a) (:var nope))))" "unknown variable nope")
                  ("(defun f (a a) a) (defun main () 1)" "a is a parameter twice")
                  ("(defun main () :key)" "expected an expression")
                  ("(defun main () (1 2))" "expected (NAME ARG...)")
                  ("(print 1)" "expected (defun")
                  ("(defvar v x) (defun main () 1)" "expected (defvar NAME [INTEGER])")
                  ("(defun a-b () 1) (defun az2dzb () 2) (defun main () 1)" "both make the label")
                  ;; #365: function values and indirect calls.
                  ("(defun main () (funcall (function nope) 1))" "unknown function nope")
                  ("(defun f (a b) a) (defun main () (funcall (function f) 1))" "f takes 2 arguments, got 1")
                  ("(defun funcall (x) x) (defun main () 1)" "funcall is a built-in form")
                  ("(defun function (x) x) (defun main () 1)" "function is a built-in form")
                  ;; #366: arrays, strings and byte access.
                  ("(defarray buf 4) (defun main () (set buf 1))" "buf is an array or string")
                  ("(defarray buf foo) (defun main () 1)" "expected (defarray NAME size)")
                  ("(defvar buf 0) (defarray buf 4) (defun main () 1)" "buf is defined twice")
                  ("(defstring s 5) (defun main () 1)" "expected (defstring NAME \"text\")")
                  ("(defarray a (1 nope)) (defun main () 1)" "not a constant, a function or an array/string")
                  ;; #367, #380: macros.
                  ("(defmacro inc (v) `(set ,v (+ ,v 1))) (defun main () (inc 1 2))" "inc takes exactly 1 argument, got 2")
                  ("(defmacro spread (v &rest r) v) (defun main () (spread))"
                   "spread takes at least 1 argument, got 0")
                  ("(defmacro if (a) a) (defun main () 0)" "if is a built-in form")
                  ("(defmacro + (a) a) (defun main () 0)" "+ is a built-in form")
                  ("(defmacro defun (a) a) (defun main () 0)" "defun is a built-in form")
                  ("(defmacro quote (a) a) (defun main () 0)" "quote is a built-in form")
                  ("(defun f () 1) (defmacro f (a) a) (defun main () 1)" "f is defined twice")
                  ("(defmacro f (a) a) (defun f () 1) (defun main () 1)" "f is defined twice")
                  ("(defmacro f (a) a) (defmacro f (a) a) (defun main () 1)" "f is defined twice")
                  ("(defmacro f (a) a) (defun-for-syntax f (a) a) (defun main () 1)" "f is defined twice")
                  ("(defmacro bad (v . w) v) (defun main () 1)" "parameter list is malformed")
                  ("(defmacro bad (v v) v) (defun main () (bad 1 2))" "v is a parameter twice")
                  ("(defmacro bad (v)) (defun main () 1)" "expected (defmacro NAME")
                  ("(defmacro loopy () `(loopy)) (defun main () (loopy))" "expands too many times")
                  ("(defmacro leaky (&rest r) r) (defun main () (leaky 1 2))" "expected (NAME ARG...)")
                  ;; #380: the compile-time evaluator.
                  ("(defmacro bad (v) (set v 1)) (defun main () (bad 1))" "set is not a compile-time operator")
                  ("(defmacro bad () (car 5)) (defun main () (bad))" "car needs a list")
                  ("(defmacro bad () (error \"boom\")) (defun main () (bad))" "boom")
                  ("(defmacro bad (v) `(+ ,v ,@1)) (defun main () (bad 1))" ",@ must splice a list")
                  ;; #382: a free name a template writes must be a global.
                  ("(defmacro peek-n () `(+ n 1)) (defun main () (let ((n 5)) (peek-n)))" "unknown variable n")
                  ("(defun-for-syntax loopy (n) (loopy n)) (defmacro bad () (loopy 1)) (defun main () (bad))"
                   "recursed too deeply")
                  ("(defun-for-syntax f (a) a) (defmacro f (a) a) (defun main () 1)" "f is defined twice")
                  ("(defun-for-syntax bad (v . w) v) (defun main () 1)" "parameter list is malformed")))
    (destructuring-bind (source expected) case
      (let ((detail (%cl-fail source)))
        (fiveam:is (and detail (search expected detail)) "~A: ~A" source detail)))))

(fiveam:test an-error-names-the-form-and-the-function
  (handler-case (%cl-compile "(defun helper (x) (+ x y)) (defun main () (helper 1))" 'callfoo-lang-abi)
    (program-compile-error (c)
      (fiveam:is (equal "helper" (program-compile-error-function c)))
      (fiveam:is (equal 3 (length (program-compile-error-form c))))
      (let ((text (princ-to-string c)))
        (fiveam:is (search "unknown variable y" text))
        (fiveam:is (search "(+ x y)" text))
        (fiveam:is (search "helper" text))))))

(fiveam:test a-missing-backend-operation-names-the-operation-and-the-form
  (let ((detail (%cl-fail "(defun main () (- 1 2))" 'callfoo-abi)))
    (fiveam:is (and detail (search "needs the operation :halt" detail)))))

(fiveam:test a-missing-peek-byte-op-names-the-form
  (let ((detail (%cl-fail "(defun main () (peek-byte 5))" 'cl-up-abi)))
    (fiveam:is (and detail (search "needs the operation :peek-byte" detail)))))

(fiveam:test a-backend-without-a-temporary-register-is-rejected
  (eval '(defbackend cl-bare-abi (:machine callfoo)
          (registers :return (a) :stack-pointer sp :operand reg)
          (operands (reg call-reg))))
  (fiveam:is (search "scratch or :caller-saved" (%cl-fail "(defun main () 1)" 'cl-bare-abi)))
  (eval '(defbackend cl-no-operand-abi (:machine callfoo)
          (registers :return (a) :scratch (b) :stack-pointer sp)))
  (fiveam:is (search ":operand" (%cl-fail "(defun main () 1)" 'cl-no-operand-abi))))

(fiveam:test a-compile-needs-a-backend
  (fiveam:signals program-compile-error (compile-program '((defun main () 1)))))

;;; #363: (return [E])

(fiveam:test return-is-a-built-in-form
  (fiveam:is (search "return is a built-in form" (%cl-fail "(defun return () 1) (defun main () 1)"))))

(fiveam:test return-is-malformed-with-two-values
  (fiveam:is (search "return is malformed" (%cl-fail "(defun main () (return 1 2))"))))

(fiveam:test return-leaves-a-recursive-call-balanced
  (let ((m (%cl-run "(defun fact (n) (if (< n 2) (return 1)) (* n (fact (- n 1))))
                      (defun main () (fact 5))"
                    'callfoo-lang-abi)))
    (fiveam:is (= 120 (%cv-a m)))
    (fiveam:is (= +cv-sp+ (sref m 'sp)))))

;;; #367, #380: macros

(fiveam:test a-macro-hiding-a-set-a-call-or-asm-is-a-hazard-like-the-literal-form
  (dolist (case '(("(defmacro setter (v n) `(set ,v ,n))
                    (defun main () (let ((x 1)) (+ x (progn (setter x 5) 0))))"
                   . "(defun main () (let ((x 1)) (+ x (progn (set x 5) 0))))")
                  ("(defun g (x) (+ x 1))
                    (defmacro callm (n) `(g ,n))
                    (defun f (n) (+ (callm n) (if (= n 0) (return 42) (callm n))))
                    (defun main () (f 0))"
                   . "(defun g (x) (+ x 1))
                      (defun f (n) (+ (g n) (if (= n 0) (return 42) (g n))))
                      (defun main () (f 0))")))
    (destructuring-bind (with-macro . without) case
      (fiveam:is (= (%cv-a (%cl-run with-macro 'callfoo-lang-abi)) (%cv-a (%cl-run without 'callfoo-lang-abi)))
                 "~A" with-macro))))

(fiveam:test a-macro-call-can-precede-its-defmacro-at-top-level-only-inside-a-function
  (fiveam:is (search "expected (defun" (%cl-fail "(inc x) (defmacro inc (v) `(+ ,v 1)) (defvar x 0) (defun main () x)"))))

(fiveam:test an-error-inside-a-macro-reports-where-its-own-code-is-written
  ;; NOPE is written in the defmacro's own body, on line 1: the error reports
  ;; that line, not the call's (#380 -- a computed macro's own code can be
  ;; wrong independently of any particular call).
  (let ((text (format nil "(defmacro bad (v) (+ v nope))~%(defun main () (bad 1))~%")))
    (handler-case (compile-source (read-source-from-string
                                   (concatenate 'string "(:program (:backend callfoo-lang-abi)) " text)))
      (program-compile-error (c)
        (fiveam:is (= 1 (lasm-syntax-error-line c)))
        (fiveam:is (search "unbound compile-time variable nope" (diagnostic-text c)))))))

(fiveam:test a-quasiquote-templates-own-conses-are-attributed-to-the-call
  ;; %CC-QQ-LIST rebuilds a fresh cons for every quasiquote list, even one
  ;; with no unquote in it, so an error about the list itself (not one of its
  ;; leaves) reports the call's line, not the template's own (#367, #362).
  (let ((text (format nil "(defmacro bad () `(1 2))~%(defun main () (bad))~%")))
    (handler-case (compile-source (read-source-from-string
                                   (concatenate 'string "(:program (:backend callfoo-lang-abi)) " text)))
      (program-compile-error (c)
        (fiveam:is (= 2 (lasm-syntax-error-line c)))
        (fiveam:is (search "expected (NAME ARG...)" (diagnostic-text c)))))))

;;; #362: positioned compile errors

(fiveam:test a-compile-error-reports-its-line-and-column
  (let ((text (format nil "(defun helper (x y)~%  (+ x nope))~%~%(defun main () (helper 1 2))~%")))
    (handler-case (compile-source (read-source-from-string
                                   (concatenate 'string "(:program (:backend callfoo-lang-abi)) " text)))
      (program-compile-error (c)
        (fiveam:is (= 2 (lasm-syntax-error-line c)))
        (fiveam:is (search "unknown variable nope" (diagnostic-text c)))
        (fiveam:is (search "(+ x nope)" (diagnostic-text c)))
        (fiveam:is (search "nope))" (diagnostic-text c)) "shows the source line")
        (fiveam:is (search "^" (diagnostic-text c)) "shows a caret")))))

(fiveam:test compile-program-on-raw-forms-has-no-position
  (handler-case (compile-program '((defun main () (nope))) :backend 'callfoo-lang-abi)
    (program-compile-error (c)
      (fiveam:is (null (lasm-syntax-error-line c))))))

(fiveam:test a-backend-checks-the-arity-of-the-language-operations
  (fiveam:signals backend-definition-error
    (eval '(defbackend cl-arity-abi (:machine callfoo) (ops (:const (a b c) (ldi a b)))))))

(fiveam:test source-is-read-without-evaluation-or-interning
  (dolist (text '("(defun main () #.(+ 1 2))" "(defun main () 1" "(defun main () 1))"
                  "(defun main () foo:bar)"))
    (fiveam:signals program-compile-error (read-source-from-string text)))
  (let ((*read-eval* t))
    (fiveam:is (null (find-symbol "SOME-UNSEEN-SOURCE-NAME" '#:lasm)))
    (read-source-from-string "(defun some-unseen-source-name () 1)")
    (fiveam:is (null (find-symbol "SOME-UNSEEN-SOURCE-NAME" '#:lasm)))))

(fiveam:test source-reads-quote-and-quasiquote-but-items-and-snapshots-dont
  (let ((body (fourth (first (items-program-items (read-source-from-string "(defun main () '(1 2))"))))))
    (fiveam:is (equal "QUOTE" (%designator-name (first body))))
    (fiveam:is (equal '(1 2) (second body))))
  (dolist (text '("(a 'b)" "(a `b)" "(a ,b)"))
    (fiveam:signals error
      (read-restricted-forms (make-string-input-stream text)
                              (lambda (control &rest args) (error "~?" control args)) "items" :bare :uninterned))))

(fiveam:test read-restricted-forms-reads-every-form
  (with-input-from-string (in "(a 1) b 2")
    (let ((forms (read-restricted-forms in (lambda (control &rest args) (error "~?" control args)) "text"
                                        :bare :uninterned)))
      (fiveam:is (= 3 (length forms)))
      (fiveam:is (eql 2 (third forms))))))

;;; Files

(defun %cl-source-file (text)
  (uiop:with-temporary-file (:stream out :pathname path :type "lsp" :keep t)
    (write-string text out)
    path))

(defun %cl-delete (path)
  (uiop:delete-file-if-exists path))

(fiveam:test a-compile-error-from-a-file-names-the-file
  (let ((path (%cl-source-file (format nil "(:program (:backend callfoo-lang-abi))~%(defun main () (nope))~%"))))
    (unwind-protect
        (handler-case (compile-source-file path)
          (program-compile-error (c)
            (fiveam:is (search (namestring path) (diagnostic-text c)))
            (fiveam:is (= 2 (lasm-syntax-error-line c)))))
      (%cl-delete path))))

(fiveam:test assemble-source-file-runs-a-program
  (let ((path (%cl-source-file "(:program (:backend callfoo-lang-abi)) (defun main () (* 6 7))")))
    (unwind-protect
         (let ((m (make-machine 'callfoo)))
           (load-program m (assemble-source-file path))
           (setf (sref m 'sp) +cv-sp+)
           (run m)
           (fiveam:is (= 42 (%cv-a m))))
      (%cl-delete path))))

;;; Command line

(defun %cl-cli (command path &rest more)
  (%run-cli (list* command (namestring path) "-m" (%cli-path "examples/cli/callfoo.lisp") more)))

(fiveam:test cli-run-executes-a-source-program
  (multiple-value-bind (status out) (%cl-cli "run" (%cli-path "examples/cli/fact.lsp"))
    (fiveam:is (= 0 status))
    (fiveam:is (search "stopped" out))))

;; #365, #366: function values, arrays and strings, run through the CLI.
(fiveam:test cli-run-executes-table-lsp
  (multiple-value-bind (status out) (%cl-cli "run" (%cli-path "examples/cli/table.lsp"))
    (fiveam:is (= 0 status))
    (fiveam:is (search "stopped" out))))

(fiveam:test cli-run-executes-the-macros-example
  (let ((m (%cl-run (%slurp-file (%cli-path "examples/cli/macros.lsp")) 'callfoo-lang-abi)))
    (fiveam:is (= 226 (%cv-a m)))))

(fiveam:test cli-compile-writes-an-items-program-that-run-accepts
  (uiop:with-temporary-file (:pathname path :type "lasm")
    (multiple-value-bind (status out)
        (%cl-cli "compile" (%cli-path "examples/cli/fact.lsp") "-o" (namestring path))
      (fiveam:is (= 0 status))
      (fiveam:is (search "wrote" out)))
    (let ((program (read-items path)))
      (fiveam:is (%same-name-p 'callfoo-lang-abi (items-program-backend program))))
    (multiple-value-bind (status out) (%cl-cli "run" path)
      (fiveam:is (= 0 status))
      (fiveam:is (string= out (nth-value 1 (%cl-cli "run" (%cli-path "examples/cli/fact.lsp"))))))))

(fiveam:test cli-compile-reports-a-compile-error
  (let ((path (%cl-source-file "(defun main () x)")))
    (unwind-protect
         (multiple-value-bind (status out err) (%cl-cli "compile" path "--backend" "callfoo-lang-abi")
           (declare (ignore out))
           (fiveam:is (= 1 status))
           (fiveam:is (search "unknown variable x" err)))
      (%cl-delete path))))

(fiveam:test cli-compile-needs-a-backend
  (let ((path (%cl-source-file "(defun main () 1)")))
    (unwind-protect
         (multiple-value-bind (status out err) (%cl-cli "compile" path)
           (declare (ignore out))
           (fiveam:is (= 1 status))
           (fiveam:is (search "no backend" err)))
      (%cl-delete path))))

;;; #389: a jump on a true value without a comparison

(defbackend cl-no-ne-imm-abi (:extends callfoo-lang-abi)
  (without-ops :branch-ne-imm))

(fiveam:test a-true-jump-on-a-value-uses-branch-ne-imm
  (flet ((ops (source backend) (%cl-op-names (%cl-compile source backend))))
    (dolist (source '("(defun f (x) (while (not x) (set x 1))) (defun main () (f 0))"
                      "(defun f (x y) (if (or x y) 1 2)) (defun main () (f 0 1))"))
      (let ((ops (ops source 'callfoo-lang-abi)))
        (fiveam:is (member "BRANCH-NE-IMM" ops :test #'string=) "~A" source))
      (let ((ops (ops source 'cl-no-ne-imm-abi)))
        (fiveam:is (notany (lambda (name) (string= name "BRANCH-NE-IMM")) ops) "~A" source)
        (fiveam:is (member "BRANCH-ZERO" ops :test #'string=) "~A" source)
        (fiveam:is (member "JUMP" ops :test #'string=) "~A" source)))))

;;; #390: a value-context and/or fuses its comparisons when that is shorter

(defun %cl-value-source (operator count)
  (format nil "(defun f (x y) (set x (~A~{ ~A~}))) (defun main () (f 1 2))" operator
          (loop for i below count collect (format nil "(< x ~D)" (+ i 3)))))

(fiveam:test a-value-and-fuses-only-when-it-is-shorter
  (flet ((fused-p (count)
           (%cl-branch-names (%cl-op-names (%cl-compile (%cl-value-source "and" count) 'callfoo-lang-abi)))))
    (fiveam:is (null (fused-p 2)))
    (fiveam:is (null (fused-p 3)))
    (fiveam:is (fused-p 4))))

(fiveam:test a-value-or-fuses-only-when-it-is-shorter
  (flet ((fused-p (count backend)
           (remove "BRANCH-NE-IMM"
                   (%cl-branch-names (%cl-op-names (%cl-compile (%cl-value-source "or" count) backend)))
                   :test #'string=)))
    (fiveam:is (null (fused-p 2 'callfoo-lang-abi)))
    (fiveam:is (null (fused-p 3 'callfoo-lang-abi)))
    (fiveam:is (fused-p 4 'cl-no-ne-imm-abi) "two saved per comparison without :branch-ne-imm")
    (fiveam:is (fused-p 4 'callfoo-lang-abi))
    (fiveam:is (fused-p 3 'cl-no-ne-imm-abi))
    (fiveam:is (null (fused-p 2 'cl-no-ne-imm-abi)))))

(fiveam:test a-value-or-branches-on-a-nonzero-operand
  (let ((ops (%cl-op-names (%cl-compile "(defun f (x y) (set x (or x y 5))) (defun main () (f 0 1))"
                                        'callfoo-lang-abi))))
    (fiveam:is (member "BRANCH-NE-IMM" ops :test #'string=))
    (fiveam:is (notany (lambda (name) (string= name "BRANCH-ZERO")) ops))))

(defun %cl-not (value)
  (if value nil 1))

(defparameter +cl-value-shapes+
  (list (cons "(and (~A x y) (~A y 3) (~A x 7) (~A 2 y))"
              (lambda (c x y) (and (funcall c x y) (funcall c y 3) (funcall c x 7) (funcall c 2 y))))
        (cons "(or (~A x y) (~A y 3) (~A x 7) (~A 2 y))"
              (lambda (c x y) (or (funcall c x y) (funcall c y 3) (funcall c x 7) (funcall c 2 y))))
        (cons "(or (~A x y) (~A y 3) (~A x 7) 9)"
              (lambda (c x y) (or (funcall c x y) (funcall c y 3) (funcall c x 7) 9)))
        (cons "(and (~A x y) (~A y 3) (~A x 7) 9)"
              (lambda (c x y) (and (funcall c x y) (funcall c y 3) (funcall c x 7) 9)))
        (cons "(or (~A x y) x (~A y 3) (~A x 7) 4)"
              (lambda (c x y) (or (funcall c x y) (if (/= x 0) x nil) (funcall c y 3) (funcall c x 7) 4)))
        (cons "(and (~A x y) x (~A y 3) (~A x 7) 4)"
              (lambda (c x y) (and (funcall c x y) (if (/= x 0) x nil) (funcall c y 3) (funcall c x 7) 4)))
        ;; #391: nested not/and/or operands
        (cons "(and (not (~A x y)) (not (~A y 3)) (not (~A x 7)) (~A 2 y))"
              (lambda (c x y) (and (%cl-not (funcall c x y)) (%cl-not (funcall c y 3))
                                   (%cl-not (funcall c x 7)) (funcall c 2 y))))
        (cons "(or (not (~A x y)) (not (~A y 3)) (not (~A x 7)) 9)"
              (lambda (c x y) (or (%cl-not (funcall c x y)) (%cl-not (funcall c y 3))
                                  (%cl-not (funcall c x 7)) 9)))
        (cons "(and (and (~A x y) (~A y 3)) (and (~A x 7) (~A 2 y)) 6)"
              (lambda (c x y) (and (and (funcall c x y) (funcall c y 3))
                                   (and (funcall c x 7) (funcall c 2 y)) 6)))
        (cons "(or (or (~A x y) (~A y 3)) (or (~A x 7) (~A 2 y)) 9)"
              (lambda (c x y) (or (or (funcall c x y) (funcall c y 3))
                                  (or (funcall c x 7) (funcall c 2 y)) 9)))
        (cons "(or (and (~A x y) x) (and (~A y 3) y) (and (~A x 7) 5) 4)"
              (lambda (c x y) (or (and (funcall c x y) (if (/= x 0) x nil))
                                  (and (funcall c y 3) (if (/= y 0) y nil))
                                  (and (funcall c x 7) 5) 4)))
        (cons "(and (or (~A x y) (~A y 3)) (or (~A x 7) (~A 2 y)) 8)"
              (lambda (c x y) (and (or (funcall c x y) (funcall c y 3))
                                   (or (funcall c x 7) (funcall c 2 y)) 8))))
  "Value-context source, with the value it must give for comparison C on signed X and Y.")

(fiveam:test value-and-or-give-the-same-values-with-and-without-fusing
  (dolist (backend '((callfoo-lang-abi callfoo) (cl-no-ne-imm-abi callfoo) (cl-no-ge-abi callfoo)
                     (callfoo-lang-fp-abi callfoo-fp)))
    (loop for (name function) in '(("=" =) ("/=" /=) ("<" <) (">" >) ("<=" <=) (">=" >=))
          do (loop for (control . expected) in +cl-value-shapes+
                   do (loop for (x y) in '((1 2) (2 1) (3 3) (0 3) (65535 1) (1 65535) (8 65535))
                            for source = (format nil "(defun f (x y) ~A) (defun main () (f ~D ~D))"
                                                 (format nil control name name name name) x y)
                            for want = (let ((value (funcall expected
                                                             (lambda (a b) (and (funcall function a b) 1))
                                                             (%cl-signed x) (%cl-signed y))))
                                         (mod (or value 0) 65536))
                            do (fiveam:is (= want (%cv-a (%cl-run source (first backend) (second backend))))
                                          "~A on ~A" source (first backend)))))))

;;; #391: a value-context and/or fuses nested not/and/or operands by their saving

(defun %cl-value-ops (body backend)
  (%cl-op-names (%cl-compile (format nil "(defun f (x y z w) (set x ~A)) (defun main () (f 1 2 3 4))" body)
                             backend)))

(fiveam:test a-value-and-fuses-a-nested-not-when-it-saves
  (let ((body "(and (not (< x 3)) (not (< y 4)) (not (< z 5)) w)"))
    (fiveam:is (%cl-branch-names (%cl-value-ops body 'callfoo-lang-abi)))
    (fiveam:is (member "JUMP" (%cl-value-ops body 'callfoo-lang-abi) :test #'string=))))

(fiveam:test a-not-of-a-value-fuses-only-with-branch-ne-imm
  (let ((body "(and (not x) (not y) (not z) w)"))
    (fiveam:is (member "BRANCH-NE-IMM" (%cl-value-ops body 'callfoo-lang-abi) :test #'string=))
    (let ((ops (%cl-value-ops body 'cl-no-ne-imm-abi)))
      (fiveam:is (notany (lambda (name) (string= name "JUMP")) ops)))))

(fiveam:test a-value-and-fuses-nested-and-and-or-operands
  (dolist (body '("(and (and (< x 3) (< y 4)) (and (< z 5) (< w 6)) 9)"
                  "(and (or (< x 3) (< y 4)) (or (< z 5) (< w 6)) 9)"
                  "(or (or (< x 3) (< y 4)) (or (< z 5) (< w 6)) 9)"))
    (fiveam:is (member "JUMP" (%cl-value-ops body 'callfoo-lang-abi) :test #'string=) "~A" body)))

(fiveam:test a-value-or-does-not-fuse-an-operand-whose-value-is-not-0-or-1
  (let ((ops (%cl-value-ops "(or (and (< x 3) y) (and (< y 4) z) (and (< z 5) w) 9)" 'callfoo-lang-abi)))
    (fiveam:is (notany (lambda (name) (string= name "JUMP")) ops))))
