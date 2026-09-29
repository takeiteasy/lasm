;;;; tests/compiler.lisp
;;;; #319: the source language compiled to items through a backend.

(in-package #:lasm)

(fiveam:def-suite compiler :in lasm)
(fiveam:in-suite compiler)

;;; Fixtures: callfoo-lang-abi (tests/fixtures/cli/callfoo.lisp, loaded by
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

(defbackend cl-up-abi (:extends cv-up-abi :isa cl-up)
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

(defun %cl-compile (source backend &optional (optimize :size))
  (compile-program (items-program-items (read-source-from-string source)) :backend backend :optimize optimize))

(defun %cl-run (source backend &optional (machine 'callfoo) (optimize :size))
  "Compile, assemble and run SOURCE, with the stack at +CV-SP+; returns the machine."
  (let ((m (make-machine machine)))
    (load-program m (assemble-items (%cl-compile source backend optimize) :backend backend))
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
    ("(defarray tbl ((function f) (function g))) (defun f (x) (+ x 1)) (defun g (x) (* x 2)) (defun main () (+ (funcall (aref tbl 0) 5) (funcall (aref tbl 1) 5)))" . 16)
    ("(defvar fp 0) (defun f (a b) (- a b)) (defun main () (set fp (function f)) (funcall fp 9 4))" . 5)
    ("(defun f (a b) (- a b)) (defun main () (funcall (if 1 (function f) (function f)) 9 4))" . 5)
    ("(defarray tbl ((function f))) (defun f (x) (+ x 1)) (defun g (x) (* x 10)) (defun main () (funcall (aref tbl 0) (g (funcall (aref tbl 0) 1))))" . 21)
    ("(defarray tbl ((function f))) (defun f (a b) (- a b)) (defun g (x) x) (defun main () (funcall (aref tbl 0) (g 9) (g 4)))" . 5)
    ("(defarray tbl ((function f) (function g))) (defun f () 3) (defun g () 4) (defun main () (funcall (aref tbl 1)))" . 4)
    ("(defarray tbl ((function f))) (defun f (x) (let ((y (+ x 1))) y)) (defun main () (let ((k 7)) (+ k (funcall (aref tbl 0) k))))" . 15)
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
    ;; #395: (unmark 'NAME) reaches the caller's own variable on purpose.
    ("(defmacro bump-mine () `(set ,(unmark 'n) (+ ,(unmark 'n) 1)))
      (defun main () (let ((n 5)) (bump-mine) n))" . 6)
    ;; #395: a quoted name is marked too, so a caller's let can't capture it.
    ("(defvar n 10) (defun g () n)
      (defmacro bump-quoted () (list 'set 'n (list '+ 'n 1)))
      (defun main () (let ((n 5)) (bump-quoted) (+ (* n 100) (g))))" . 511)
    ("(defmacro bump-form () (unmark '(set n (+ n 1))))
      (defun main () (let ((n 5)) (bump-form) n))" . 6)
    ;; unmark reaches the calling template's own variable, not the caller's.
    ("(defmacro bump-mine () `(set ,(unmark 'n) (+ ,(unmark 'n) 1)))
      (defmacro with-n () `(let ((n 1)) (bump-mine) n))
      (defun main () (let ((n 100)) (+ (with-n) n)))" . 102)
    ("(defmacro def-bump (name var) `(defmacro ,name () `(set ,(unmark ',var) (+ ,(unmark ',var) 1))))
      (def-bump bump-x x)
      (defun main () (let ((x 5)) (bump-x) x))" . 6)
    ;; #396: an inner template's names get its own mark in place of the outer's,
    ;; so the outer template's binding is reached through unmark.
    ("(defmacro outer () `(progn (defmacro inner () `,(unmark 'x))
                                (defun f () (let ((x 7)) (inner)))))
      (outer)
      (defun main () (f))" . 7)
    ;; #384: function values.
    ("(defmacro sum-scaled (k &rest xs) `(+ ,@(mapcar (lambda (x) `(* ,k ,x)) xs)))
      (defun main () (sum-scaled 10 1 2 3))" . 60)
    ("(defun-for-syntax double (x) (* x 2))
      (defmacro doubled (&rest xs) `(+ ,@(mapcar 'double xs) ,@(mapcar (function double) xs)))
      (defun main () (doubled 1 2 3))" . 24)
    ("(defmacro apply-sum (&rest xs) (+ (apply '+ xs) (apply '+ 100 xs) (funcall (lambda (a b) (+ a b)) 3 4)))
      (defun main () (apply-sum 1 2 3))" . 119)
    ("(defmacro zero-all (&rest vars) `(progn ,@(mapcar (lambda (v) `(set ,v 0)) vars)))
      (defun main () (let ((a 1) (b 2)) (zero-all a b) (+ a b)))" . 0)
    ;; #384: control forms.
    ("(defmacro pick (x) (cond ((eq x 'a) 1) ((eq x 'b) 2) (t 3)))
      (defun main () (+ (pick a) (* 10 (pick b)) (* 100 (pick c))))" . 321)
    ("(defmacro clamp (x) (or (and (integerp x) (when (> x 10) 10)) x))
      (defmacro known (x) (or (unless (integerp x) 7) x))
      (defun main () (+ (clamp 50) (clamp 3) (known 4) (let ((y 0)) (known y))))" . 24)
    ;; #384: list, string, symbol and integer operators.
    ("(defmacro lists ()
        (+ (car (reverse '(1 2 3))) (nth 1 '(5 6 7)) (car (nthcdr 2 '(1 2 3))) (second '(9 8))
           (third '(1 2 4)) (car (last '(1 2 3))) (if (member 2 '(1 2 3)) 100 0)
           (cdr (assoc 'b '((a . 1) (b . 2)))) (if (not (member 9 '(1 2))) 1000 0)))
      (defun main () (lists))" . 1129)
    ("(defmacro strings ()
        (if (and (string= (concat \"ab\" \"c\") \"abc\") (stringp \"x\") (not (stringp 'x))
                 (string= (symbol-name 'foo) \"foo\") (string= (number-to-string 12) \"12\"))
            42 0))
      (defun main () (strings))" . 42)
    ("(defmacro ints ()
        (+ (/ 17 5) (mod 17 5) (min 4 2 9) (max 4 2 9) (logand 12 10) (logior 12 10) (ash 1 4) (ash 16 -2)
           (if (and (<= 1 1) (>= 2 1) (/= 1 2)) 1000 0)))
      (defun main () (ints))" . 1058)
    ("(defmacro defgetter (n v) `(defun ,(intern (concat \"get-\" (symbol-name n))) () ,v))
      (defgetter x 7)
      (defun main () (get-x))" . 7)
    ;; a quoted nil is (), as in Common Lisp.
    ("(defmacro nils () (if (or 'nil (car '(nil)) (cdr '(1))) 0 (if (null 'nil) 7 9)))
      (defun main () (nils))" . 7)
    ;; a macro can be named for a compile-time operator.
    ("(defmacro cond (c a b) `(if ,c ,a ,b))
      (defun main () (cond 1 5 6))" . 5)
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
      (eval `(defbackend cl-variant-arity-abi (:isa callfoo) (ops (,name (a b c) (ldi a b))))))))

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
      (eval `(defbackend cl-branch-arity-abi (:isa callfoo) (ops (,name (a b) (ldi a b))))))))

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

;;; #394: :optimize :speed shares a preserved register between one-off sites

(defparameter +cl-two-sites+
  "(defun f (n) n)
   (defun main () (let ((a (+ (f 1) (f 2))) (b (+ (f 3) (f 4)))) (+ (* a 10) b)))"
  "Two call-holding sites outside any loop; main is 37.")

(defun %cl-saved (items)
  (mapcar #'%designator-name (getf (%cl-function-options items "main") :save)))

(fiveam:test speed-shares-a-saved-register-between-one-off-sites
  (let ((size (%cl-compile +cl-two-sites+ 'callfoo-lang-abi))
        (speed (%cl-compile +cl-two-sites+ 'callfoo-lang-abi :speed)))
    (fiveam:is (= 2 (%cl-push-count size)))
    (fiveam:is (null (%cl-saved size)))
    (fiveam:is (zerop (%cl-push-count speed)))
    (fiveam:is (equal '("C") (%cl-saved speed)))))

(fiveam:test speed-keeps-a-single-one-off-site-on-the-stack
  (let ((items (%cl-compile "(defun f (n) n) (defun main () (+ (f 1) (f 2)))" 'callfoo-lang-abi :speed)))
    (fiveam:is (= 1 (%cl-push-count items)))
    (fiveam:is (null (%cl-saved items)))))

(fiveam:test speed-shares-a-register-with-a-later-loop
  (let ((source "(defun f (n) n)
                 (defun main () (+ (f 3) (f 4))
                   (let ((i 0)) (while (< i 1) (+ (f 1) (f 2)) (set i 1)) 0))"))
    (fiveam:is (= 1 (%cl-push-count (%cl-compile source 'callfoo-lang-abi))))
    (let ((items (%cl-compile source 'callfoo-lang-abi :speed)))
      (fiveam:is (zerop (%cl-push-count items)))
      (fiveam:is (equal '("C") (%cl-saved items))))))

(fiveam:test speed-leaves-a-loop-and-a-call-free-program-as-size-has-them
  (dolist (source (list +cl-loop-calls+ "(defun main () (+ (* 2 3) (- 9 4)))"))
    (fiveam:is (equal (%cl-op-names (%cl-compile source 'callfoo-lang-abi))
                      (%cl-op-names (%cl-compile source 'callfoo-lang-abi :speed)))
               "~A" source)))

(fiveam:test speed-runs-to-the-same-values
  (dolist (backend +cl-backends+)
    (destructuring-bind (name machine) backend
      (fiveam:is (= 37 (%cv-a (%cl-run +cl-two-sites+ name machine :speed))) "~A" name)
      (fiveam:is (= 37 (%cv-a (%cl-run +cl-two-sites+ name machine :size))) "~A" name))))

;;; #400: a site's weight halves in an if arm and in a later and/or operand

(defparameter +cl-arm-sites+
  '(("(defvar g 1) (defun f (n) n)
      (defun main () (if g (+ (f 1) (f 2)) (+ (f 3) (f 4))))" 2 nil 3)
    ("(defvar g 1) (defun f (n) n)
      (defun main () (+ (f 1) (f 2)) (if g (+ (f 3) (f 4)) 0))" 0 ("C") 7)
    ("(defvar g 1) (defun f (n) n)
      (defun main () (and g (+ (f 1) (f 2)) (+ (f 3) (f 4))))" 2 nil 7)
    ("(defvar g 1) (defun f (n) n)
      (defun main ()
        (if g (+ (f 3) (f 4)) 0)
        (if g (if g (if g (let ((i 0)) (while (< i 1) (+ (f 1) (f 2)) (set i 1)) 0) 0) 0) 0)
        7)" 1 ("C") 7))
  "Source, the pushes :speed leaves, the registers it saves, and main's value.")

(fiveam:test speed-weights-sites-by-how-often-they-run
  (loop for (source pushes saved) in +cl-arm-sites+
        do (let ((items (%cl-compile source 'callfoo-lang-abi :speed)))
             (fiveam:is (= pushes (%cl-push-count items)) "~A" source)
             (fiveam:is (equal saved (%cl-saved items)) "~A" source))))

(fiveam:test weighted-sites-run-to-the-same-values
  (dolist (backend +cl-backends+)
    (destructuring-bind (name machine) backend
      (loop for (source nil nil value) in +cl-arm-sites+
            do (dolist (optimize '(:size :speed))
                 (fiveam:is (= value (%cv-a (%cl-run source name machine optimize))) "~A ~A" name optimize))))))

(fiveam:test optimize-is-size-or-speed
  (fiveam:is (search ":optimize" (or (handler-case (%cl-compile "(defun main () 1)" 'callfoo-lang-abi :fast)
                                       (program-compile-error (c) (program-compile-error-detail c)))
                                     ""))))

(fiveam:test cli-optimize-compiles-and-runs-a-source-program
  (let ((path (%cl-source-file +cl-two-sites+)))
    (unwind-protect
         (uiop:with-temporary-file (:pathname out :type "lasm")
           (fiveam:is (= 0 (%cl-cli "compile" path "--backend" "callfoo-lang-abi" "--optimize" "speed" "-o" (namestring out))))
           (fiveam:is (zerop (%cl-push-count (items-program-items (read-items out)))))
           (fiveam:is (= 0 (%cl-cli "compile" path "--backend" "callfoo-lang-abi" "-o" (namestring out))))
           (fiveam:is (= 2 (%cl-push-count (items-program-items (read-items out)))))
           (multiple-value-bind (status out err)
               (%cl-cli "compile" path "--backend" "callfoo-lang-abi" "--optimize" "fast")
             (declare (ignore out))
             (fiveam:is (/= 0 status))
             (fiveam:is (search "--optimize must be size or speed" err))))
      (%cl-delete path))
    (multiple-value-bind (status out) (%cl-cli "run" (%cli-path "tests/fixtures/cli/fact.lsp") "--optimize" "speed")
      (fiveam:is (= 0 status))
      (fiveam:is (search "stopped" out)))))

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

(fiveam:test a-packed-string-writes-as-text-and-reads-back-to-the-same-cells
  (%cl-each-backend (backend machine)
    (let* ((program (compile-source (read-source-from-string
                                     "(defstring s \"hello\" :packed) (defun main () (aref-byte s 1))")
                                    :backend backend))
           (text (with-output-to-string (out) (write-items-program program out)))
           (again (read-items-from-string text)))
      (fiveam:is (search "hello" text) "~A: the .lasm file holds the string" backend)
      (fiveam:is (equalp (assembly-cells (assemble-items (items-program-items program) :backend backend))
                         (assembly-cells (assemble-items (items-program-items again) :backend backend)))
                 "~A" backend))))

;;; #368: words wider than one cell. widefoo-lang-abi (tests/fixtures/cli/widefoo.lisp,
;;; loaded by tests/backend.lisp) has 16-bit registers over 8-bit cells, so
;;; BACKEND-WORD-CELLS is 2; callfoo-lang-abi's is 1.

(fiveam:test backend-word-cells-follows-the-stack-pointers-width
  (fiveam:is (= 1 (backend-word-cells 'callfoo-lang-abi)))
  (fiveam:is (= 1 (backend-word-cells 'callfoo-lang-fp-abi)))
  (fiveam:is (= 2 (backend-word-cells 'widefoo-lang-abi))))

(fiveam:test funcall-through-a-variable-checks-its-arity-against-the-function-values
  (fiveam:is (null (%cl-fail "(defun add (a b) (+ a b)) (defun one (x) x) (defvar g 0)
                              (defun main () (set g (function add)) (+ (funcall g 1 2) (funcall (function one) 3)))")))
  (fiveam:is (null (%cl-fail "(defun f (x) x) (defun main () (funcall (function f) 1))")))
  (fiveam:is (null (%cl-fail "(defconstant rom 5) (defun main () (funcall rom 1) (funcall 7 1 2))"))
             "an integer or constant target is a raw address, not checked")
  (let ((detail (%cl-fail "(defun later (h) (funcall h 1 2)) (defun f (x) x)
                           (defun main () (later (function f)))")))
    (fiveam:is (and detail (search "funcall through h passes 2 arguments, but it holds only function values taking 1" detail))
               "a parameter holds what its callers pass")))

;;; #397: a variable's own function values narrow the check.

(defparameter +cl-two-arities+
  "(defun one (x) x) (defun two (a b) (+ a b)) (defarray taken ((function one) (function two))) "
  "Function values of 1 and 2 arguments are both taken, so only a variable's own holdings can reject a call.")

(fiveam:test funcall-through-a-variable-checks-the-function-values-put-in-it
  (flet ((fail (body) (%cl-fail (concatenate 'string +cl-two-arities+ body))))
    (fiveam:is (search "funcall through g passes 2 arguments, but it holds only function values taking 1"
                       (fail "(defun main () (let ((g (function one))) (funcall g 1 2)))"))
               "a let variable holding a 1-argument function")
    (fiveam:is (search "taking 1"
                       (fail "(defvar g 0) (defun main () (set g (function one)) (funcall g 1 2))"))
               "a global")
    (fiveam:is (null (fail "(defvar g 0)
                            (defun main () (set g (function one)) (set g (function two)) (funcall g 1 2) (funcall g 1))"))
               "every function set into a variable counts")
    (fiveam:is (null (fail "(defun main () (let ((g (function one))) (set g (function two)) (funcall g 1 2)))"))
               "a set after a let counts")
    (fiveam:is (search "taking 1"
                       (fail "(defun later () (funcall g 1 2)) (defvar g 0) (defun main () (set g (function one)) (later))"))
               "a set in another function counts, whatever the order")
    (fiveam:is (null (fail "(defun main () (let ((a 1) (g (function one))) (let ((g (function two))) (funcall g 1 2)) (funcall g 1)))"))
               "a shadowing let has its own holdings")))

(fiveam:test funcall-through-a-variable-falls-back-when-what-it-holds-is-unknown
  (flet ((fail (body) (%cl-fail (concatenate 'string +cl-two-arities+ body))))
    (fiveam:is (null (fail "(defun main () (let ((g (aref taken 0))) (funcall g 1 2) (peek taken)))"))
               "a value from an escaped array is checked program-wide")
    (fiveam:is (null (fail "(defun main () (let ((g (function one))) (set g (+ 1 2)) (funcall g 1 2)))"))
               "a computed value makes the variable unknown")
    (fiveam:is (null (fail "(defun call (h) (funcall h 1 2)) (defun main () (call (function one)) (call (function two)))"))
               "a parameter is unknown")
    (fiveam:is (null (fail "(defun call (h) (set h (function one)) (funcall h 1 2)) (defun main () (call (function two)))"))
               "a set on a parameter adds to what its callers pass")
    (fiveam:is (null (fail "(defvar g 4000) (defun main () (set g (function one)) (funcall g 1 2))"))
               "a global that starts non-zero may hold a raw address")
    (fiveam:is (null (fail "(defvar g 0) (defun main () (set g (function one)) (asm (:var g)) (funcall g 1 2))"))
               "an (asm) naming the variable may write it")))

;;; #402: a parameter, an array element and an if/progn/let value narrow it too.

(fiveam:test funcall-through-a-parameter-checks-what-its-callers-pass
  (flet ((fail (body) (%cl-fail (concatenate 'string +cl-two-arities+ body))))
    (fiveam:is (search "funcall through h passes 2 arguments, but it holds only function values taking 1"
                       (fail "(defun call (h) (funcall h 1 2)) (defun main () (call (function one)))"))
               "one caller")
    (fiveam:is (search "taking 1"
                       (fail "(defun inner (h) (funcall h 1 2)) (defun outer (g) (inner g)) (defun main () (outer (function one)))"))
               "a parameter passed on")
    (fiveam:is (search "taking 1"
                       (fail "(defun rec (h n) (if n (rec h (- n 1)) (funcall h 1 2))) (defun main () (rec (function one) 3))"))
               "a recursive call adds nothing")
    (fiveam:is (null (fail "(defun call (h) (funcall h 1 2)) (defun main () (call (function one)) (call (function two)))"))
               "each caller's function counts")
    (fiveam:is (null (fail "(defun call (h) (funcall h 1 2)) (defarray fns ((function call))) (defun main () (call (function one)))"))
               "a function taken as a value has callers not seen")
    (fiveam:is (null (fail "(defun call (h) (funcall h 1 2)) (defun main () 0)"))
               "a function nothing calls")
    (fiveam:is (null (fail "(defun call (h) (funcall h 1 2)) (defun main () (call (function one)) (asm (:call fncall)))"))
               "a function whose label an (asm) spells")
    (fiveam:is (null (fail "(defun call (h) (funcall h 1 2)) (defun main () (call (+ 1 2)))"))
               "a computed argument")))

(fiveam:test funcall-through-an-array-element-checks-what-is-put-in-it
  (flet ((fail (body) (%cl-fail (concatenate 'string +cl-two-arities+ "(defarray slots 2) " body))))
    (fiveam:is (search "funcall passes 2 arguments, but its target holds only function values taking 1"
                       (fail "(defun main () (funcall (aref taken 0) 1 2))"))
               "an initial value")
    (fiveam:is (null (fail "(defun main () (funcall (aref taken 1) 1 2))")))
    (fiveam:is (search "taking 1"
                       (fail "(defun main () (aset slots 0 (function one)) (aset slots 1 (function two)) (funcall (aref slots 0) 1 2))"))
               "an aset at a constant index")
    (fiveam:is (null (fail "(defun main () (aset slots 0 (function one)) (aset slots 1 (function two)) (funcall (aref slots 1) 1 2))")))
    (fiveam:is (search "taking 1"
                       (fail "(defconstant first 0) (defun main () (aset slots first (function one)) (funcall (aref slots first) 1 2))"))
               "a defconstant index")
    (fiveam:is (search "taking 1, 2"
                       (fail "(defun main () (let ((i 1)) (funcall (aref taken i) 1 2 3)))"))
               "a computed index reads every element")
    (fiveam:is (null (fail "(defun main () (aset slots 0 (function one)) (let ((i 1)) (aset slots i (function two))) (funcall (aref slots 0) 1 2))"))
               "a computed index writes every element")
    (fiveam:is (null (fail "(defun main () (aset slots 0 (+ 1 2)) (funcall (aref slots 0) 1 2))"))
               "a computed value")
    (fiveam:is (null (fail "(defun main () (aset slots 0 (function one)) (funcall (aref slots 5) 1 2))"))
               "an index past the end")
    (fiveam:is (null (fail "(defun main () (aset slots 0 (function one)) (poke slots 0) (funcall (aref slots 0) 1 2))"))
               "an array used other than by aref and aset")
    (fiveam:is (null (fail "(defun get (a) (aref a 0)) (defun main () (aset slots 0 (function one)) (get slots) (funcall (aref slots 0) 1 2))"))
               "an array passed to a function")
    (fiveam:is (null (fail "(defarray other (slots)) (defun main () (aset slots 0 (function one)) (funcall (aref slots 0) 1 2))"))
               "an array's address stored in another")
    (fiveam:is (null (fail "(defun main () (aset slots 0 (function one)) (let ((slots 0)) slots) (funcall (aref slots 0) 1 2))"))
               "a local of the same name makes the array escaped")))

(fiveam:test funcall-through-an-if-progn-or-let-value-checks-what-it-yields
  (flet ((fail (body) (%cl-fail (concatenate 'string +cl-two-arities+ body))))
    (fiveam:is (search "funcall passes 2 arguments, but its target holds only function values taking 1"
                       (fail "(defun main () (funcall (if 1 (function one) (function one)) 1 2))")))
    (fiveam:is (null (fail "(defun main () (funcall (if 1 (function one) (function two)) 1 2))"))
               "each branch counts")
    (fiveam:is (null (fail "(defun main () (funcall (if 1 (function one)) 1 2))"))
               "an if with no else")
    (fiveam:is (search "taking 1" (fail "(defun main () (funcall (progn 1 (function one)) 1 2))")))
    (fiveam:is (search "taking 1" (fail "(defun main () (funcall (let ((x 1)) (function one)) 1 2))")))
    (fiveam:is (search "funcall through g passes"
                       (fail "(defun main () (let ((g (let ((x 1)) (if x (function one) (function one))))) (funcall g 1 2)))"))
               "a value nested in a binding")
    (fiveam:is (search "taking 1"
                       (fail "(defun main () (let ((f (function one))) (funcall (let ((x 1)) f) 1 2)))"))
               "a let's value names a variable of the outer scope")
    (fiveam:is (search "funcall through g passes"
                       (fail "(defvar g 0) (defun main () (set g (function one)) (let ((g g)) (funcall g 1 2)))"))
               "a binding's value is looked up before the binding exists")))

(fiveam:test funcall-arity-narrowing-is-the-same-when-optimizing-for-speed
  (flet ((fail (body optimize)
           (handler-case (progn (%cl-compile (concatenate 'string +cl-two-arities+ body) 'callfoo-lang-abi optimize) nil)
             (program-compile-error (c) (program-compile-error-detail c)))))
    (dolist (optimize '(:size :speed))
      (fiveam:is (search "taking 1" (fail "(defun main () (let ((g (function one))) (while 0 (funcall g 1)) (funcall g 1 2)))" optimize)))
      (fiveam:is (null (fail "(defun main () (let ((g (function one))) (set g (function two)) (funcall g 1 2)))" optimize)))
      (fiveam:is (search "taking 1" (fail "(defun call (h) (funcall h 1 2)) (defun main () (call (function one)))" optimize)))
      (fiveam:is (search "taking 1" (fail "(defun main () (funcall (aref taken 0) 1 2))" optimize))))))

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
  (eval '(defbackend cl-w3-abi (:isa cl-w3-machine)
          (registers :return (a) :scratch (a b) :stack-pointer sp :operand reg)
          (operands (reg cl-w3-reg))))
  (fiveam:is (= 3 (backend-word-cells 'cl-w3-abi)))
  (fiveam:is (search "needs the operation" (%cl-fail "(defarray arr (1 2)) (defstring s \"a\") (defun main () 1)" 'cl-w3-abi)))
  (fiveam:is (search "needs the operation" (%cl-fail "(defarray arr 2) (defun main () 1)" 'cl-w3-abi)))
  (fiveam:is (equalp #(1 0 0 2 0 0 97 0 0 0 0 0)
                     (assembly-cells (assemble ".emit 3, 1, 2
.emit 3, \"a\", 0" :cpu 'cl-w3-machine)))))

;;; #379: packed strings and (aref-byte S I)/(aset-byte S I V). callfoo's cells are
;;; 16 bits (two characters), widefoo's 8 (one, over a two-cell word).

(defparameter +cl-packed-programs+
  '(("(defstring s \"abc\" :packed) (defun main () (+ (aref-byte s 0) (+ (aref-byte s 1) (aref-byte s 2))))" . 294)
    ("(defstring s \"abc\" :packed) (defun main () (aref-byte s 3))" . 0)
    ("(defstring s \"ab\" :packed) (defun main () (+ (aref-byte s 2) (aref-byte s 3)))" . 0)
    ("(defstring s \"hello\" :packed)
      (defun main () (let ((i 0)) (while (aref-byte s i) (set i (+ i 1))) i))" . 5)
    ("(defstring s \"ab\" :packed)
      (defun main () (aset-byte s 1 65) (+ (aref-byte s 0) (* 256 (aref-byte s 1))))" . 16737)
    ("(defstring s \"ab\" :packed) (defstring t2 \"cd\" :packed)
      (defun main () (+ (aref-byte s 1) (aref-byte t2 0)))" . 197)))

(fiveam:test a-packed-string-reads-and-writes-a-character-a-byte
  (dolist (case +cl-packed-programs+)
    (destructuring-bind (source . expected) case
      (%cl-each-backend (backend machine)
        (fiveam:is (= expected (%cv-a (%cl-run source backend machine))) "~A: ~A" backend source))
      (let ((m (%cl-run source 'widefoo-lang-abi 'widefoo)))
        (fiveam:is (= expected (regref m 'r 0)) "widefoo: ~A" source)
        (fiveam:is (= +cv-sp+ (sref m 'sp)))))))

(fiveam:test byte-access-reaches-the-bytes-of-a-defarray
  (let ((source "(defarray a 2)
                 (defun main () (aset-byte a 3 200) (aset-byte a 0 5) (+ (aref-byte a 3) (aref a 0)))"))
    (%cl-each-backend (backend machine)
      (fiveam:is (= 205 (%cv-a (%cl-run source backend machine))) "~A" backend))))

(fiveam:test a-packed-string-stores-several-characters-a-cell
  (fiveam:is (= 25185 (%cv-a (%cl-run "(defstring s \"ab\" :packed) (defun main () (aref s 0))" 'callfoo-lang-abi))))
  (fiveam:is (= 2 (count-if (lambda (i) (and (consp i) (eq (first i) :directive) (%same-name-p (second i) "packz")))
                            (%cl-compile "(defstring s \"a\" :packed) (defstring t2 \"bc\" :packed) (defun main () 1)"
                                         'callfoo-lang-abi)))
             "each packed string is one .packz directive"))

(fiveam:test a-packed-string-lays-out-by-the-memorys-endianness
  (eval '(defmachine cl-be-machine (register pc :width 16) (register r :width 16 :names (a b))
           (memory ram :width 16 :addr-width 16 :endian :big)))
  (eval '(defmachine cl-b8-machine (register pc :width 16) (register r :width 8 :names (a b))
           (memory ram :width 8 :addr-width 16)))
  (eval '(defmode cl-lay-reg (expr :register r)))
  (dolist (name '(cl-be cl-b8))
    (eval `(defbackend ,(intern (format nil "~A-ABI" name)) (:isa ,(intern (format nil "~A-MACHINE" name)))
             (registers :return (a) :scratch (a b) :operand reg)
             (operands (reg cl-lay-reg)))))
  (flet ((cells (machine text)
           (coerce (assembly-cells (assemble (format nil ".packz ~S" text) :cpu machine)) 'list)))
    (fiveam:is (equal '(#x6162 #x6300) (cells 'cl-be-machine "abc")) "big-endian: the first character in the high bits")
    (fiveam:is (equal '(#x6261 #x0063) (cells 'callfoo "abc")) "little-endian: the first in the low bits")
    (fiveam:is (equal '(97 98 99 0) (cells 'cl-b8-machine "abc")) "an 8-bit cell holds one character"))
  (fiveam:is (= 1 (backend-cell-bytes 'cl-b8-abi)))
  (fiveam:is (= 2 (backend-cell-bytes 'callfoo-lang-abi)))
  (fiveam:is (eq :big (nth-value 1 (backend-cell-bytes 'cl-be-abi))))
  (let ((*cc-backend* (find-backend 'cl-be-abi)))
    (fiveam:is (equal '(shl (- 1 (logand 0 1)) 3) (%cc-character-shift 0)) "big-endian: characters count from the high end"))
  (let ((*cc-backend* (find-backend 'callfoo-lang-abi)))
    (fiveam:is (equal '(shl (logand 0 1) 3) (%cc-character-shift 0)) "little-endian: from the low end"))
  (let ((*cc-backend* (find-backend 'cl-b8-abi)) (*cc-word-cells* 1))
    (fiveam:is (%cc-direct-character-p) "one character a cell and one cell a word: a character is a word"))
  (let ((*cc-backend* (find-backend 'widefoo-lang-abi)) (*cc-word-cells* 2))
    (fiveam:is (not (%cc-direct-character-p)))))

(defbackend cl-byte-address-abi (:extends callfoo-lang-abi)
  (ops (:byte-address (d) (add d d))))

(fiveam:test a-backends-byte-address-operation-replaces-the-default-scaling
  (let ((source "(defstring s \"ab\" :packed) (defun main () (aref-byte s 1))"))
    (fiveam:is (member "BYTE-ADDRESS" (%cl-op-names (%cl-compile source 'cl-byte-address-abi)) :test #'string=))
    (fiveam:is (not (member "BYTE-ADDRESS" (%cl-op-names (%cl-compile source 'callfoo-lang-abi)) :test #'string=)))
    (fiveam:is (= 98 (%cv-a (%cl-run source 'cl-byte-address-abi))))))

(defbackend cl-no-peek-byte-abi (:extends callfoo-lang-abi)
  (without-ops :peek-byte :poke-byte))

(defbackend cl-no-shr-abi (:extends cl-no-peek-byte-abi)
  (without-ops :shr))

(defbackend cl-no-poke-byte-abi (:extends callfoo-lang-abi)
  (without-ops :poke-byte))

(fiveam:test a-packed-string-without-byte-operations-goes-through-its-cells
  (dolist (case +cl-packed-programs+)
    (destructuring-bind (source . expected) case
      (fiveam:is (= expected (%cv-a (%cl-run source 'cl-no-peek-byte-abi))) "~A" source)
      (fiveam:is (not (intersection '("PEEK-BYTE" "POKE-BYTE") (%cl-op-names (%cl-compile source 'cl-no-peek-byte-abi))
                                    :test #'string=)))))
  (fiveam:is (= 205 (%cv-a (%cl-run "(defarray a 2)
                 (defun main () (aset-byte a 3 200) (aset-byte a 0 5) (+ (aref-byte a 3) (aref a 0)))"
                                    'cl-no-peek-byte-abi))))
  (fiveam:is (= 66 (%cv-a (%cl-run "(defstring s \"ab\" :packed) (defun main () (aset-byte s 1 66))" 'cl-no-peek-byte-abi)))
             "an aset-byte is its value")
  (fiveam:is (= 16737 (%cv-a (%cl-run "(defstring s \"ab\" :packed)
      (defun main () (aset-byte s 1 65) (+ (aref-byte s 0) (* 256 (aref-byte s 1))))" 'cl-no-poke-byte-abi)))
             "each direction falls back on its own operation"))

(fiveam:test word-lowered-byte-access-names-the-operation-it-lacks
  (fiveam:is (search "needs the operation :shr"
                     (%cl-fail "(defstring s \"a\" :packed) (defun main () (aref-byte s 0))" 'cl-no-shr-abi))))

(fiveam:test a-packed-string-needs-8-bit-characters-and-a-declared-form
  (fiveam:is (search "8-bit characters"
                     (%cl-fail (format nil "(defstring s \"~C\" :packed) (defun main () 1)" (code-char 300)))))
  (fiveam:is (search "expected (defstring"
                     (%cl-fail "(defstring s \"a\" :bogus) (defun main () 1)")))
  (fiveam:is (search "expected (defstring"
                     (%cl-fail "(defstring s \"a\" :packed :packed) (defun main () 1)")))
  (fiveam:is (search "aref-byte is malformed"
                     (%cl-fail "(defstring s \"a\" :packed) (defun main () (aref-byte s))"))))

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

(defparameter +cl-header-speed+
  "(:program (:backend callfoo-lang-abi :optimize speed))
   (defun f (n) n)
   (defun main () (let ((a (+ (f 1) (f 2))) (b (+ (f 3) (f 4)))) (+ (* a 10) b)))")

(fiveam:test a-source-header-can-name-its-optimize
  (let ((program (read-source-from-string +cl-header-speed+)))
    (fiveam:is (eq :speed (items-program-optimize program)))
    (fiveam:is (zerop (%cl-push-count (items-program-items (compile-source program)))))
    (fiveam:is (= 2 (%cl-push-count (items-program-items (compile-source program :optimize :size)))))
    (fiveam:is (zerop (%cl-push-count (items-program-items (compile-source program :optimize :speed))))))
  (fiveam:is (eq :speed (items-program-optimize
                         (read-source-from-string "(:program (:optimize :speed)) (defun main () 1)"))))
  (fiveam:is (null (items-program-optimize (read-source-from-string "(defun main () 1)")))))

(fiveam:test a-header-optimize-must-be-size-or-speed-in-a-source-file
  (fiveam:signals items-malformed
    (read-source-from-string "(:program (:optimize fast)) (defun main () 1)"))
  (fiveam:signals items-malformed
    (read-source-from-string "(:program (:optimize 3)) (defun main () 1)"))
  (fiveam:signals items-malformed
    (read-items-from-string "(:program (:optimize speed))")))

(fiveam:test cli-optimize-overrides-the-header
  (let ((path (%cl-source-file +cl-header-speed+)))
    (unwind-protect
         (uiop:with-temporary-file (:pathname out :type "lasm")
           (fiveam:is (= 0 (%cl-cli "compile" path "-o" (namestring out))))
           (fiveam:is (zerop (%cl-push-count (items-program-items (read-items out)))))
           (fiveam:is (= 0 (%cl-cli "compile" path "--optimize" "size" "-o" (namestring out))))
           (fiveam:is (= 2 (%cl-push-count (items-program-items (read-items out))))))
      (%cl-delete path))))

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
                  ;; #378: a computed target's argument count must be one some function value takes.
                  ("(defvar g 0) (defun f (x) x) (defun main () (set g (function f)) (funcall g 1 2))"
                   "funcall through g passes 2 arguments, but it holds only function values taking 1")
                  ("(defun f (x) x) (defarray fns ((function f))) (defun main () (funcall (aref fns 0)))"
                   "funcall passes 0 arguments, but its target holds only function values taking 1")
                  ("(defvar g 0) (defun main () (funcall g 1))" "no function value takes 1 argument")
                  ("(defun funcall (x) x) (defun main () 1)" "funcall is a built-in form")
                  ("(defun function (x) x) (defun main () 1)" "function is a built-in form")
                  ;; #366: arrays, strings and byte access.
                  ("(defarray buf 4) (defun main () (set buf 1))" "buf is an array or string")
                  ("(defarray buf foo) (defun main () 1)" "expected (defarray NAME size)")
                  ("(defvar buf 0) (defarray buf 4) (defun main () 1)" "buf is defined twice")
                  ("(defstring s 5) (defun main () 1)" "expected (defstring NAME \"text\" [:packed])")
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
                  ;; #396: the same holds through a macro that defines a macro.
                  ("(defmacro outer () `(progn (defmacro inner () `x) (defun f () (let ((x 7)) (inner)))))
                    (outer) (defun main () (f))"
                   "unknown variable x")
                  ("(defun-for-syntax loopy (n) (loopy n)) (defmacro bad () (loopy 1)) (defun main () (bad))"
                   "recursed too deeply")
                  ("(defun-for-syntax f (a) a) (defmacro f (a) a) (defun main () 1)" "f is defined twice")
                  ("(defun-for-syntax bad (v . w) v) (defun main () 1)" "parameter list is malformed")
                  ;; #384: the evaluator's operators check their arguments.
                  ("(defmacro bad () (/ 1 0)) (defun main () (bad))" "division by zero")
                  ("(defmacro bad () (mod 1 0)) (defun main () (bad))" "division by zero")
                  ("(defmacro bad () (nth -1 '(1))) (defun main () (bad))" "non-negative index")
                  ("(defmacro bad () (concat \"a\" 1)) (defun main () (bad))" "expected a string")
                  ("(defmacro bad () (assoc 1 '(2))) (defun main () (bad))" "assoc needs a list of pairs")
                  ("(defmacro bad () (ash 1 100)) (defun main () (bad))" "ash shifts by at most 64")
                  ("(defmacro bad () (intern \"a b\")) (defun main () (bad))" "cannot intern")
                  ("(defmacro bad () (intern \"12\")) (defun main () (bad))" "cannot intern")
                  ("(defmacro bad () (intern \"\")) (defun main () (bad))" "cannot intern")
                  ("(defmacro bad () (mapcar (function nope) '(1))) (defun main () (bad))" "nope is not a compile-time function")
                  ("(defmacro bad () (funcall 'if 1)) (defun main () (bad))" "if is not a compile-time function")
                  ("(defmacro bad () (funcall (lambda (x) x) 1 2)) (defun main () (bad))" "lambda takes exactly 1 argument, got 2")
                  ("(defmacro bad () (lambda (x) x)) (defun main () (bad))" "#<compile-time lambda>")
                  ("(defun-for-syntax when (a) a) (defun main () 0)" "when is a built-in form")
                  ("(defmacro unquote (a) a) (defun main () 0)" "unquote is a built-in form")))
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
  (eval '(defbackend cl-bare-abi (:isa callfoo)
          (registers :return (a) :stack-pointer sp :operand reg)
          (operands (reg call-reg))))
  (fiveam:is (search "scratch or :caller-saved" (%cl-fail "(defun main () 1)" 'cl-bare-abi)))
  (eval '(defbackend cl-no-operand-abi (:isa callfoo)
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
    (eval '(defbackend cl-arity-abi (:isa callfoo) (ops (:const (a b c) (ldi a b)))))))

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

(fiveam:test source-reads-radix-integers-and-block-comments
  (flet ((body (text) (fourth (first (items-program-items (read-source-from-string text))))))
    (fiveam:is (equal '(16 255 3 7 -8) (rest (body "(defun main () (+ #x10 #XfF #b11 #o7 #x-8))"))))
    (fiveam:is (eql 1 (body "#| a #| nested |# b |# (defun main () #| in |# 1)"))))
  (dolist (text '("(defun main () #x)" "(defun main () #xZZ)" "(defun main () #b12)" "(defun main () #r10)"
                  "(defun main () #'main)" "(defun main () #+sbcl 1)" "(defun main () #| open)"))
    (fiveam:signals program-compile-error (read-source-from-string text)))
  (fiveam:signals program-compile-error
    (read-source-from-string (format nil "(defun main () #x~A)"
                                     (make-string (1+ +reader-max-number-chars+) :initial-element #\F)))))

(fiveam:test source-radix-integers-compile-and-run
  (fiveam:is (= 26 (%cv-a (%cl-run "(defun main () (+ #x10 #b11 #o7))" 'callfoo-lang-abi)))))

(fiveam:test source-error-after-a-block-comment-reports-its-position
  (handler-case (compile-source (read-source-from-string "(:program (:backend callfoo-lang-abi))
#| one
two |#
(defun main () (+ 1 y))"))
    (program-compile-error (c) (fiveam:is (eql 4 (lasm-syntax-error-line c))))))

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
  (%run-cli (list* command (namestring path) "-m" (%cli-path "tests/fixtures/cli/callfoo.lisp") more)))

(fiveam:test cli-run-executes-a-source-program
  (multiple-value-bind (status out) (%cl-cli "run" (%cli-path "tests/fixtures/cli/fact.lsp"))
    (fiveam:is (= 0 status))
    (fiveam:is (search "stopped" out))))

;; #365, #366: function values, arrays and strings, run through the CLI.
(fiveam:test cli-run-executes-table-lsp
  (multiple-value-bind (status out) (%cl-cli "run" (%cli-path "tests/fixtures/cli/table.lsp"))
    (fiveam:is (= 0 status))
    (fiveam:is (search "stopped" out))))

(fiveam:test cli-run-executes-the-macros-example
  (let ((m (%cl-run (%slurp-file (%cli-path "tests/fixtures/cli/macros.lsp")) 'callfoo-lang-abi)))
    (fiveam:is (= 287 (%cv-a m)))))

(fiveam:test cli-compile-writes-an-items-program-that-run-accepts
  (uiop:with-temporary-file (:pathname path :type "lasm")
    (multiple-value-bind (status out)
        (%cl-cli "compile" (%cli-path "tests/fixtures/cli/fact.lsp") "-o" (namestring path))
      (fiveam:is (= 0 status))
      (fiveam:is (search "wrote" out)))
    (let ((program (read-items path)))
      (fiveam:is (%same-name-p 'callfoo-lang-abi (items-program-backend program))))
    (multiple-value-bind (status out) (%cl-cli "run" path)
      (fiveam:is (= 0 status))
      (fiveam:is (string= out (nth-value 1 (%cl-cli "run" (%cli-path "tests/fixtures/cli/fact.lsp"))))))))

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
                     (cl-no-ge-variants-abi callfoo) (cl-no-eq-variants-abi callfoo)
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

;;; #392: the fuse estimate counts the variant each operation uses

(defbackend cl-no-ge-variants-abi (:extends callfoo-lang-abi)
  (without-ops :branch-ge-imm :branch-ge-slot :branch-le-slot))

(defbackend cl-no-eq-variants-abi (:extends callfoo-lang-abi)
  (without-ops :eq-imm :eq-slot))

(defun %cl-fuses-p (body backend)
  (and (member "JUMP" (%cl-value-ops body backend) :test #'string=) t))

(fiveam:test a-comparison-without-a-value-variant-saves-more-when-fused
  (fiveam:is (not (%cl-fuses-p "(and (< x 3) (< y 4) w)" 'callfoo-lang-abi)))
  (fiveam:is (%cl-fuses-p "(and (>= x 3) (>= y 4) w)" 'callfoo-lang-abi) ":ge has no -imm variant"))

(fiveam:test a-comparison-without-a-branch-variant-does-not-fuse
  (let ((body "(and (< x 3) (< y 4) (< z 5) w)"))
    (fiveam:is (%cl-fuses-p body 'callfoo-lang-abi))
    (fiveam:is (not (%cl-fuses-p body 'cl-no-ge-variants-abi)) "fusing is 12 instructions, not fusing 10")))

(fiveam:test a-not-without-eq-variants-fuses-sooner
  (let ((body "(and (not x) (not y) w)"))
    (fiveam:is (not (%cl-fuses-p body 'callfoo-lang-abi)))
    (fiveam:is (%cl-fuses-p body 'cl-no-eq-variants-abi))))

;;; #415: static frames

(eval `(defbackend cl-static-abi (:extends callfoo-lang-abi)
         (frame :static t)
         (without-ops :get :set :alloc :free :push :pop
                      ,@(loop for (name nil kinds) in (backend-descriptor-ops (find-backend 'callfoo-lang-abi))
                              when (and (search "-SLOT" name) (not (some #'identity kinds)))
                                collect (intern name :keyword)))))

(defun %cl-static-items (source &optional (backend 'cl-static-abi))
  (%cl-compile source backend))

(defun %cl-static-fail (source)
  (%cl-fail source 'cl-static-abi))

(defparameter +cl-static-programs+
  '(("(defun g (x) (+ x 1)) (defun main () (let ((a 5)) (g 1) a))" . 5)
    ("(defun g (x) (+ x 1)) (defun f (n) (let ((k n)) (g 1) (+ k n))) (defun main () (f 4))" . 8)
    ("(defun g (x) (* x 10)) (defun f (a b) (- a b)) (defun main () (f (g 1) (g 2)))" . 65526)
    ("(defun f (a b) (- a b)) (defun g (x) (+ x 10)) (defun main () (f 100 (g 2)))" . 88)
    ("(defun f (a b c) (- a (- b c))) (defun g (x) x) (defun main () (f (g 9) 4 (g 1)))" . 6)
    ("(defun f (x) (+ x 1)) (defun main () (funcall (function f) 5))" . 6)
    ("(defarray tbl ((function g))) (defvar fp 0) (defun g (x) (+ x 1)) (defun f (x) (funcall (aref tbl 0) x)) (defun main () (set fp (function f)) (funcall fp 4))" . 5)
    ("(defarray tbl ((function g))) (defvar fp 0) (defun g (x) (+ x 1)) (defun h (x y) (+ x y)) (defun f (x) (funcall (aref tbl 0) x)) (defun main () (set fp (function f)) (funcall fp 4))" . 5)
    ("(defun f (x) (+ x 1)) (defun g (x) (f (f x))) (defun main () (+ (g 1) (g 10)))" . 15)
    ("(defvar g 1) (defun f (x) (set g (+ g x))) (defun main () (f 2) (f 3) g)" . 6)
    ("(defun main () (let ((x 9)) (asm (:clobbers a b) (:op :const (reg a) 3) (:op :const (reg b) (:var x)) (:op :poke b a)) x))" . 3))
  "Programs that run the same on a static-frame backend, and the value main leaves in the accumulator.")

(fiveam:test static-frames-run-programs
  (loop for (source . expected) in +cl-static-programs+
        do (let ((m (%cl-run source 'cl-static-abi)))
             (fiveam:is (= expected (%cv-a m)) "~A" source)
             (fiveam:is (= +cv-sp+ (sref m 'sp)) "~A" source))))

(fiveam:test static-frames-run-every-stack-program-that-does-not-recurse
  (loop for (source . expected) in +cl-programs+
        unless (search ":op :set" source)
          do (let ((detail (%cl-static-fail source)))
               (if detail
                   (fiveam:is (or (search "recurs" detail) (search "calls itself" detail))
                              "~A: ~A" source detail)
                   (fiveam:is (= expected (%cv-a (%cl-run source 'cl-static-abi))) "~A" source)))))

(fiveam:test static-frames-need-no-stack-instructions
  (let ((names (loop for item in (%cl-static-items "(defun f (a b) (let ((c (+ a b))) (* c (f2 c)))) (defun f2 (x) (+ x 1)) (defun main () (f 1 2))")
                     when (and (consp item) (eq (first item) :op)) collect (string (second item)))))
    (fiveam:is (notany (lambda (name) (member name '("GET" "SET" "PUSH" "POP" "ALLOC" "FREE") :test #'string-equal)) names))))

(fiveam:test static-frames-share-the-addresses-of-functions-that-never-run-together
  (flet ((reserved (source)
           (count-if (lambda (item) (and (consp item) (eq (first item) :directive) (string-equal (second item) "res")))
                     (%cl-static-items source))))
    (fiveam:is (= 2 (reserved "(defun f (x) x) (defun g (y) y) (defun main () (+ (f 1) (g 2)))"))
               "main's temporary, then one slot f and g share")
    (fiveam:is (= 2 (reserved "(defun f (x) x) (defun g (y) (f y)) (defun main () (g 1))"))
               "g calls f, so their slots are apart")))

(fiveam:test static-frames-report-recursion-at-the-call
  (dolist (case '(("(defun f (n) (if n (f (- n 1)) 0)) (defun main () (f 3))" "f calls itself")
                  ("(defun a (n) (b n)) (defun b (n) (a n)) (defun main () (a 1))" "a calls b calls a is recursive")
                  ("(defun g () (asm (call fnf))) (defun f () (g)) (defun main () (f))" "g calls f calls g is recursive")))
    (let ((detail (%cl-static-fail (first case))))
      (fiveam:is (and detail (search (second case) detail)) "~A: ~A" (first case) detail))))

(fiveam:test static-frames-report-the-line-of-a-recursive-call
  (let ((c (handler-case (compile-source (read-source-from-string (format nil "(defun f (n)~%  (f n))~%(defun main () (f 1))"))
                                         :backend 'cl-static-abi)
             (program-compile-error (c) c))))
    (fiveam:is (typep c 'program-compile-error))
    (fiveam:is (eql 2 (lasm-syntax-error-line c)))))

(fiveam:test static-frames-report-recursion-through-a-computed-funcall
  (dolist (case '(("(defarray tbl ((function f))) (defun f (n) (if n (funcall (aref tbl 0) (- n 1)) 0)) (defun main () (f 3))" "f calls itself")
                  ("(defarray tbl ((function b))) (defun a (n) (funcall (aref tbl 0) n)) (defun b (n) (a n)) (defun main () (a 1))" "is recursive")))
    (let ((detail (%cl-static-fail (first case))))
      (fiveam:is (and detail (search (second case) detail)) "~A: ~A" (first case) detail))))

(fiveam:test static-frames-report-recursion-through-an-unknown-computed-target
  (let ((detail (%cl-static-fail "(defun f (p n) (if n (funcall p p (- n 1)) 0)) (defun main () (f (function f) 3))")))
    (fiveam:is (and detail (search "f calls itself" detail)) "~A" detail)))

(fiveam:test static-frames-place-an-entry-thunk-before-its-function
  (let* ((items (%cl-static-items "(defvar fp 0) (defun f (x) x) (defun main () (set fp (function f)) (funcall fp 1))"))
         (thunk (position-if (lambda (item) (and (consp item) (eq (first item) :label) (string-equal (second item) "sffe"))) items)))
    (fiveam:is (not (null thunk)))
    (when thunk
      (let ((after (find-if-not (lambda (item) (and (consp item) (eq (first item) :op))) items :start (1+ thunk))))
        (fiveam:is (and (consp after) (eq (first after) :function)))))
    (fiveam:is (notany (lambda (item) (and (consp item) (eq (first item) :op) (string-equal (second item) "jump"))) items))))

(fiveam:test static-frames-keep-a-computed-callee-apart-from-its-caller
  (flet ((reserved (source)
           (count-if (lambda (item) (and (consp item) (eq (first item) :directive) (string-equal (second item) "res")))
                     (%cl-static-items source))))
    (fiveam:is (= 4 (reserved "(defarray tbl ((function f))) (defun f (x) x) (defun g (y) (funcall (aref tbl y) y)) (defun main () (g 0))"))
               "g's slot, f's slot beyond it, main's temporary, and the argument block")))

(fiveam:test static-frames-still-check-a-computed-funcall-arity
  (fiveam:is (search "holds only function values" (%cl-static-fail "(defvar fp 0) (defun f (x) x) (defun main () (set fp (function f)) (funcall fp 1 2))"))))

(fiveam:test the-program-header-and-the-key-choose-the-frames
  (flet ((items (header backend &optional frames)
           (items-program-items
            (compile-source (read-source-from-string (format nil "~A (defun f (x) (let ((y x)) y)) (defun main () (f 1))" header))
                            :backend backend :frames frames))))
    (fiveam:is (find-if (lambda (item) (and (consp item) (eq (first item) :function) (member :frame (third item))))
                        (items "(:program (:frames static))" 'callfoo-lang-abi)))
    (fiveam:is (null (find-if (lambda (item) (and (consp item) (eq (first item) :function) (member :frame (third item))))
                              (items "(:program (:frames stack))" 'callfoo-lang-abi))))
    (fiveam:is (null (find-if (lambda (item) (and (consp item) (eq (first item) :function) (member :frame (third item))))
                              (items "(:program (:frames static))" 'callfoo-lang-abi :stack))))
    (fiveam:signals program-compile-error (items "(:program (:frames stack))" 'cl-static-abi))
    (fiveam:signals program-compile-error (items "" 'callfoo-lang-abi :heap))))

(fiveam:test a-static-program-header-runs-on-a-stack-backend
  (let* ((program (compile-source (read-source-from-string "(:program (:frames static)) (defun sq (n) (* n n)) (defun main () (+ (sq 3) (sq 4)))")
                                  :backend 'callfoo-lang-abi))
         (m (make-machine 'callfoo)))
    (load-program m (assemble-items (items-program-items program) :backend 'callfoo-lang-abi))
    (setf (sref m 'sp) +cv-sp+)
    (run m :max-steps 100000)
    (fiveam:is (= 25 (%cv-a m)))))

(fiveam:test defbackend-checks-the-static-frame-option
  (fiveam:is (typep (%backend-error-of '(defbackend bk-static-bad (:isa callfoo) (frame :static 1)))
                    'backend-definition-error))
  (fiveam:is (getf (backend-descriptor-frame (find-backend 'cl-static-abi)) :static))
  (fiveam:is (not (getf (backend-descriptor-frame (find-backend 'callfoo-lang-abi)) :static))))

(fiveam:test cli-frames-chooses-static-or-stack
  (multiple-value-bind (status out) (%cl-cli "run" (%cli-path "tests/fixtures/cli/static.lsp"))
    (fiveam:is (= 0 status))
    (fiveam:is (search "stopped" out)))
  (multiple-value-bind (status out) (%cl-cli "run" (%cli-path "tests/fixtures/cli/fact.lsp") "--frames" "stack")
    (fiveam:is (= 0 status))
    (fiveam:is (search "stopped" out)))
  (multiple-value-bind (status out) (%cl-cli "run" (%cli-path "tests/fixtures/cli/fact.lsp") "--frames" "static")
    (fiveam:is (= 0 status))
    (fiveam:is (search "stopped" out)))
  (multiple-value-bind (status out err) (%cl-cli "run" (%cli-path "tests/fixtures/cli/fact.lsp") "--frames" "heap")
    (declare (ignore out))
    (fiveam:is (/= 0 status))
    (fiveam:is (search "--frames must be static or stack" err))))

;;; #421: a callee-saved register saved in a static frame

(defparameter +cl-static-saved-programs+
  '(("(defun g (x) (+ x 1))
     (defun f (n) (let ((i 0) (a 0)) (while (< i n) (set a (+ (* a 2) (g i))) (set i (+ i 1))) a))
     (defun main () (let ((i 0) (r 0)) (while (< i 2) (set r (+ (* r 3) (f 2))) (set i (+ i 1))) r))" . 16)
    ("(defun g (x) (+ x 1))
     (defun f (n) (let ((i 0) (a 0)) (while (< i n) (set a (+ (* a 2) (g i))) (if (= i 1) (return a)) (set i (+ i 1))) 0))
     (defun main () (let ((i 0) (r 0)) (while (< i 2) (set r (+ (* r 3) (f 3))) (set i (+ i 1))) r))" . 16)
    ("(defun f () (asm (:clobbers c) (:op :const (reg c) 99)) 1)
     (defun main () (let ((i 0) (r 0)) (while (< i 2) (set r (+ (* r 3) (f))) (set i (+ i 1))) r))" . 4))
  "Programs whose callers and callees both hold a value in a callee-saved register, and the value main leaves in the accumulator.")

(defun %cl-function-ops (items name)
  "The (OP ARG...) of each operation in function NAME's items, as downcased strings."
  (let ((label (%cc-mangle "fn" name)))
    (loop for item in (cdddr (find-if (lambda (item) (and (eq :function (first item)) (%same-name-p label (second item))))
                                      items))
          when (and (consp item) (eq (first item) :op))
            collect (mapcar (lambda (x) (string-downcase (princ-to-string x))) (rest item)))))

(fiveam:test static-frames-save-a-callee-saved-register-in-the-frame
  (dolist (optimize '(:size :speed))
    (let ((ops (%cl-function-ops (%cl-compile (first (first +cl-static-saved-programs+)) 'cl-static-abi optimize) "f")))
      (fiveam:is (= 1 (count-if (lambda (op) (and (equal "poke" (first op)) (equal "c" (car (last op))))) ops)) "~A" optimize)
      (fiveam:is (= 1 (count-if (lambda (op) (and (equal "peek" (first op)) (equal "c" (second op)))) ops)) "~A" optimize))))

(fiveam:test static-frames-restore-a-saved-register-before-each-return
  (let ((ops (%cl-function-ops (%cl-compile (first (second +cl-static-saved-programs+)) 'cl-static-abi) "f")))
    (fiveam:is (= 2 (count-if (lambda (op) (and (equal "peek" (first op)) (equal "c" (second op)))) ops)))))

(fiveam:test static-frames-hold-values-across-calls-in-callee-saved-registers
  (dolist (optimize '(:size :speed))
    (loop for (source . expected) in +cl-static-saved-programs+
          do (let ((m (%cl-run source 'cl-static-abi 'callfoo optimize)))
               (fiveam:is (= expected (%cv-a m)) "~A ~A" optimize source)
               (fiveam:is (= +cv-sp+ (sref m 'sp)) "~A ~A" optimize source)))))

(fiveam:test static-frames-save-a-callee-saved-asm-clobber
  (let ((ops (%cl-function-ops (%cl-compile (first (third +cl-static-saved-programs+)) 'cl-static-abi) "f")))
    (fiveam:is (find-if (lambda (op) (and (equal "poke" (first op)) (equal "c" (car (last op))))) ops))))

;;; #420: a recursive function keeps a stack frame

(defparameter +cl-static-recursive-programs+
  '(("(defun fact (n) (if (< n 2) 1 (* n (fact (- n 1))))) (defun main () (fact 5))" . 120)
    ("(defun even (n) (if (= n 0) 1 (odd (- n 1)))) (defun odd (n) (if (= n 0) 0 (even (- n 1)))) (defun main () (+ (even 10) (* 2 (odd 7))))" . 3)
    ("(defun sum (n) (if (< n 1) 0 (+ n (sum (- n 1))))) (defun twice (x) (+ x x)) (defun main () (twice (sum 4)))" . 20)
    ("(defun fib (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (defun main () (fib 10))" . 55)
    ("(defun f (a b) (if (< a 1) b (f (- a 1) (+ b a)))) (defun main () (f 4 0))" . 10)
    ("(defun h (x) (* x 3)) (defun f (n) (if (< n 1) 0 (+ (h n) (f (- n 1))))) (defun main () (f 3))" . 18)
    ("(defun f (n) (if (< n 1) 0 (+ 1 (f (- n 1))))) (defun g (x) (f x)) (defun main () (+ (g 3) (g 5)))" . 8)
    ("(defvar fp 0) (defun f (n) (if (< n 1) 0 (+ n (funcall fp (- n 1))))) (defun main () (set fp (function f)) (funcall fp 4))" . 10)
    ("(defarray tbl ((function f))) (defun f (n) (if (< n 1) 0 (+ n (funcall (aref tbl 0) (- n 1))))) (defun main () (f 4))" . 10)
    ("(defun f (n) (if (< n 1) 0 (+ (* n 2) (f (- n 1))))) (defun main () (let ((i 0) (r 0)) (while (< i 2) (set r (+ (* r 3) (f 2))) (set i (+ i 1))) r))" . 24))
  "Recursive programs, and the value main leaves in the accumulator.")

(fiveam:test static-frames-run-recursive-programs-on-a-stack-backend
  (dolist (optimize '(:size :speed))
    (loop for (source . expected) in +cl-static-recursive-programs+
          do (let ((m (let ((backend (find-backend 'callfoo-lang-abi))
                            (machine (make-machine 'callfoo)))
                        (declare (ignore backend))
                        (load-program machine
                                      (assemble-items (compile-program (items-program-items (read-source-from-string source))
                                                                       :backend 'callfoo-lang-abi :frames :static :optimize optimize)
                                                      :backend 'callfoo-lang-abi))
                        (setf (sref machine 'sp) +cv-sp+)
                        (run machine :max-steps 100000)
                        machine)))
               (fiveam:is (= expected (%cv-a m)) "~A ~A" optimize source)
               (fiveam:is (= +cv-sp+ (sref m 'sp)) "~A ~A" optimize source)))))

(defun %cl-static-frames-items (source)
  (compile-program (items-program-items (read-source-from-string source))
                   :backend 'callfoo-lang-abi :frames :static))

(fiveam:test static-frames-run-every-stack-program-on-a-stack-backend
  (loop for (source . expected) in +cl-programs+
        unless (search ":op :set" source)
          do (let ((m (make-machine 'callfoo)))
               (load-program m (assemble-items (%cl-static-frames-items source) :backend 'callfoo-lang-abi))
               (setf (sref m 'sp) +cv-sp+)
               (run m :max-steps 100000)
               (fiveam:is (= expected (%cv-a m)) "~A" source))))

(fiveam:test only-a-function-in-a-cycle-keeps-a-stack-frame
  (let ((items (%cl-static-frames-items "(defun h (x) (* x 3)) (defun f (n) (if (< n 1) 0 (+ (h n) (f (- n 1))))) (defun main () (f 3))")))
    (fiveam:is (member :frame (%cl-function-options items "h")))
    (fiveam:is (not (member :frame (%cl-function-options items "f"))))
    (fiveam:is (member :frame (%cl-function-options items "main")))
    (fiveam:is (zerop (getf (%cl-function-options items "h") :locals)))
    (fiveam:is (plusp (getf (%cl-function-options items "f") :locals)))))

(fiveam:test a-stack-function-copies-its-parameters-from-the-static-words
  (let* ((ops (%cl-function-ops (%cl-static-frames-items "(defun f (a b) (if (< a 1) b (f (- a 1) (+ b a)))) (defun main () (f 4 0))") "f"))
         (sets (loop for op in ops for index from 0 when (string= "set" (first op)) collect index)))
    (fiveam:is (< (second sets) (position "get" ops :key #'first :test #'string=))
               "a set for each parameter, before the body reads a slot")))

(fiveam:test recursive-and-static-functions-do-not-share-addresses
  (flet ((reserved (source)
           (count-if (lambda (item) (and (consp item) (eq (first item) :directive) (string-equal (second item) "res")))
                     (%cl-static-frames-items source))))
    (fiveam:is (= 2 (reserved "(defun f (n) (if (< n 1) 0 (f (- n 1)))) (defun main () (let ((x 1)) (+ x (f 2))))"))
               "main's word, then f's, which lies after it")
    (fiveam:is (= 1 (reserved "(defun a (n) (if (< n 1) 0 (b (- n 1)))) (defun b (n) (if (< n 1) 0 (a (- n 1)))) (defun main () (a 3))"))
               "the one word a and b share")))

(fiveam:test static-frames-still-report-recursion-without-stack-operations
  (dolist (source '("(defun f (n) (if n (f (- n 1)) 0)) (defun main () (f 3))"
                    "(defun a (n) (b n)) (defun b (n) (a n)) (defun main () (a 1))"))
    (fiveam:is (search "needs a stack frame" (%cl-static-fail source)) "~A" source)))

(fiveam:test a-recursive-function-with-macros-compiles-twice
  (let ((m (make-machine 'callfoo)))
    (load-program m (assemble-items (%cl-static-frames-items "(defmacro dec (x) `(- ,x 1)) (defun fact (n) (if (< n 2) 1 (* n (fact (dec n))))) (defun main () (fact 5))")
                                    :backend 'callfoo-lang-abi))
    (setf (sref m 'sp) +cv-sp+)
    (run m :max-steps 100000)
    (fiveam:is (= 120 (%cv-a m)))))

;;; #429: a load and store by label

(eval '(defbackend cl-label-abi (:extends cl-static-abi)
         (ops (:peek-label (d label) (ldm d label))
              (:poke-label (label s) (stm s label)))))

(defun %cl-label-op-counts (source backend)
  (let ((names (%cl-op-names (%cl-compile source backend))))
    (flet ((count-of (name) (count name names :test #'string-equal)))
      (list (count-of "PEEK-LABEL") (count-of "POKE-LABEL") (count-of "PEEK") (count-of "POKE")))))

(fiveam:test label-ops-run-the-static-programs
  (loop for (source . expected) in +cl-static-programs+
        do (let ((m (%cl-run source 'cl-label-abi)))
             (fiveam:is (= expected (%cv-a m)) "~A" source))))

(fiveam:test label-ops-replace-const-and-peek-poke-for-globals-and-static-slots
  (let ((source "(defvar g 1) (defun f (x) (let ((y (+ x g))) (set g y) y)) (defun main () (f 2))"))
    (destructuring-bind (peek-label poke-label peek poke) (%cl-label-op-counts source 'cl-label-abi)
      (fiveam:is (plusp peek-label))
      (fiveam:is (plusp poke-label))
      (fiveam:is (= 0 peek))
      (fiveam:is (= 0 poke)))
    (destructuring-bind (peek-label poke-label peek poke) (%cl-label-op-counts source 'cl-static-abi)
      (fiveam:is (= 0 peek-label))
      (fiveam:is (= 0 poke-label))
      (fiveam:is (plusp peek))
      (fiveam:is (plusp poke)))
    (fiveam:is (= 3 (%cv-a (%cl-run source 'cl-label-abi))))))

(fiveam:test label-ops-leave-computed-addresses-to-peek-and-poke
  (destructuring-bind (peek-label poke-label peek poke)
      (%cl-label-op-counts "(defarray a 2) (defun main () (aset a (+ 0 1) 5) (aref a (+ 0 1)))" 'cl-label-abi)
    (fiveam:is (= 0 peek-label))
    (fiveam:is (= 0 poke-label))
    (fiveam:is (plusp peek))
    (fiveam:is (plusp poke))))

;;; #431: a constant-index aref and aset by label

(fiveam:test constant-index-aref-and-aset-use-label-ops
  (dolist (source '("(defarray a 3) (defun main () (aset a 2 7) (aref a 2))"
                    "(defconstant k 2) (defarray a 3) (defun main () (aset a k 7) (aref a k))"))
    (destructuring-bind (peek-label poke-label peek poke) (%cl-label-op-counts source 'cl-label-abi)
      (fiveam:is (plusp peek-label) "~A" source)
      (fiveam:is (plusp poke-label) "~A" source)
      (fiveam:is (= 0 peek))
      (fiveam:is (= 0 poke)))
    (fiveam:is (= 7 (%cv-a (%cl-run source 'cl-label-abi))) "~A" source)))

(fiveam:test constant-index-aref-skips-the-address-add
  (fiveam:is (= 0 (%cl-count-op "ADD-IMM" "(defarray a 3) (defun main () (aref a 2))" 'cl-static-abi)))
  (fiveam:is (= 0 (%cl-count-op "ADD-IMM" "(defarray a 3) (defun main () (aref a 2))" 'cl-label-abi))))

(fiveam:test constant-index-aref-reuses-the-pointer-register
  (fiveam:is (= 1 (%cl-count-op "POINT-LABEL" "(defarray a 3) (defun main () (aset a 1 9) (aref a 1))" 'cl-pointer-label-abi)))
  (fiveam:is (= 2 (%cl-count-op "POINT-LABEL" "(defarray a 3) (defun main () (aref a 0) (aref a 1))" 'cl-pointer-label-abi)))
  (fiveam:is (= 10 (%cv-a (%cl-run "(defarray a (1 2 3)) (defun main () (aset a 1 9) (+ (aref a 0) (aref a 1)))" 'cl-pointer-label-abi)))))

(fiveam:test label-ops-initialise-a-nonzero-global-in-the-stub
  (let ((m (%cl-run "(defvar g 7) (defun main () g)" 'cl-label-abi)))
    (fiveam:is (= 7 (%cv-a m)))))

(fiveam:test label-ops-must-take-two-parameters
  (fiveam:is (typep (%backend-error-of '(defbackend cl-label-arity-abi (:isa callfoo) (ops (:peek-label (a b c) (ldi a b)))))
                    'backend-definition-error))
  (fiveam:is (typep (%backend-error-of '(defbackend cl-label-arity-abi (:isa callfoo) (ops (:poke-label (a) (ldi a 1)))))
                    'backend-definition-error)))

;;; #417: a dedicated pointer register

(eval `(defbackend cl-pointer-abi (:extends cl-static-abi)
         (registers :address d)
         (without-ops :peek :poke)
         (ops (:point (r) (movv (reg d) (reg r)))
              (:peek-pointer (dst) (ldx (reg dst) (ind d)))
              (:poke-pointer (src) (stx (ind d) (reg src))))))

(eval '(defbackend cl-pointer-label-abi (:extends cl-pointer-abi)
         (ops (:point-label (label) (ldi (reg d) (imm label))))))

(defparameter +cl-pointer-programs+
  (append (remove-if (lambda (program) (search ":op :poke" (car program))) +cl-static-programs+)
          '(("(defarray a (1 2 3)) (defun main () (aset a 1 9) (+ (aref a 0) (aref a 1)))" . 10)
            ("(defvar g 0) (defun main () (set g 5) (poke (+ 1 (function main)) 0) (+ g 1))" . 6)
            ("(defvar g 3) (defun main () (set g (+ g g)) (set g (* g g)) g)" . 36))))

(defun %cl-count-op (name source backend)
  (count name (%cl-op-names (%cl-compile source backend)) :test #'string-equal))

(fiveam:test pointer-register-backends-run-the-programs
  (dolist (backend '(cl-pointer-abi cl-pointer-label-abi))
    (loop for (source . expected) in +cl-pointer-programs+
          do (fiveam:is (= expected (%cv-a (%cl-run source backend))) "~A ~A" backend source))))

(fiveam:test pointer-register-reads-and-writes-globals-and-static-slots-through-it
  (dolist (backend '(cl-pointer-abi cl-pointer-label-abi))
    (let ((source "(defvar g 0) (defun f (x) (let ((y (+ x g))) (set g y) y)) (defun main () (f 2))"))
      (fiveam:is (plusp (%cl-count-op "PEEK-POINTER" source backend)))
      (fiveam:is (plusp (%cl-count-op "POKE-POINTER" source backend)))
      (fiveam:is (= 0 (%cl-count-op "PEEK" source backend)))
      (fiveam:is (= 0 (%cl-count-op "POKE" source backend))))))

(fiveam:test pointer-register-is-reused-while-it-holds-the-address
  (fiveam:is (= 1 (%cl-count-op "POINT-LABEL" "(defvar g 0) (defun main () (set g (+ g 1)) g)" 'cl-pointer-label-abi)))
  (fiveam:is (= 1 (%cl-count-op "POINT" "(defvar g 0) (defun main () (set g (+ g 1)) g)" 'cl-pointer-abi))))

(fiveam:test pointer-register-is-reloaded-after-a-label-a-call-or-an-asm
  (dolist (source '("(defvar g 0) (defun main () g (if g 1 2) g)"
                    "(defvar g 0) (defun f () 1) (defun main () g (f) g)"
                    "(defvar g 0) (defun main () g (asm (:op :const (reg a) 0)) g)"))
    (fiveam:is (= 2 (%cl-count-op "POINT-LABEL" source 'cl-pointer-label-abi)) "~A" source))
  (fiveam:is (= 1 (%cl-count-op "POINT-LABEL" "(defvar g 0) (defun main () g (asm (:clobbers a) (:op :const (reg a) 0)) g)"
                                'cl-pointer-label-abi))))

(fiveam:test pointer-register-is-forgotten-after-a-computed-address
  (fiveam:is (= 2 (%cl-count-op "POINT-LABEL" "(defvar g 0) (defarray a 2) (defun main () g (peek a) g)" 'cl-pointer-label-abi))))

(fiveam:test label-ops-come-before-the-pointer-register
  (eval '(defbackend cl-pointer-both-abi (:extends cl-pointer-abi)
          (ops (:peek-label (d label) (ldm d label))
               (:poke-label (label s) (stm s label)))))
  (let ((source "(defvar g 0) (defun main () (set g (+ g 1)) g)"))
    (fiveam:is (= 0 (%cl-count-op "POINT" source 'cl-pointer-both-abi)))
    (fiveam:is (= 0 (%cl-count-op "PEEK-POINTER" source 'cl-pointer-both-abi)))))

(fiveam:test address-register-needs-its-operations
  (fiveam:is (typep (%backend-error-of '(defbackend cl-address-bad-abi (:extends callfoo-lang-abi) (registers :address d)))
                    'backend-definition-error))
  (fiveam:is (typep (%backend-error-of '(defbackend cl-address-bad-abi (:extends cl-pointer-abi) (registers :address a :return (a))))
                    'backend-definition-error))
  (fiveam:is (null (%backend-error-of '(defbackend cl-address-ok-abi (:extends cl-pointer-abi))))))

(fiveam:test pointer-register-operations-take-their-parameter-counts
  (dolist (form '((:point (a b) (ldi a b)) (:point-label (a b) (ldi a b))
                  (:peek-pointer (a b) (ldi a b)) (:poke-pointer (a b) (ldi a b))
                  (:peek-byte-pointer (a b) (ldi a b)) (:poke-byte-pointer (a b) (ldi a b))))
    (fiveam:is (typep (%backend-error-of `(defbackend cl-pointer-arity-abi (:isa callfoo) (ops ,form)))
                      'backend-definition-error) "~S" form)))

;;; #432: byte access through the pointer register

(eval `(defbackend cl-pointer-byte-abi (:extends cl-pointer-abi)
         (without-ops :peek-byte :poke-byte)
         (ops (:peek-byte-pointer (dst) (ldb (reg dst) (ind d)))
              (:poke-byte-pointer (src) (stb (ind d) (reg src))))))

(fiveam:test byte-access-goes-through-the-pointer-register
  (dolist (source (append (mapcar #'car +cl-packed-programs+)
                          '("(defarray a 2) (defun main () (aset-byte a 3 200) (aset-byte a 0 5) (+ (aref-byte a 3) (aref a 0)))"
                            "(defvar w 0) (defun main () (poke-byte w 5) (poke-byte (+ w 1) 9) (+ (peek-byte w) (peek-byte (+ w 1))))")))
    (let ((expected (%cv-a (%cl-run source 'callfoo-lang-abi))))
      (fiveam:is (= expected (%cv-a (%cl-run source 'cl-pointer-byte-abi))) "~A" source)
      (fiveam:is (= 0 (%cl-count-op "PEEK-BYTE" source 'cl-pointer-byte-abi)))
      (fiveam:is (= 0 (%cl-count-op "POKE-BYTE" source 'cl-pointer-byte-abi)))))
  (fiveam:is (plusp (%cl-count-op "PEEK-BYTE-POINTER" "(defstring s \"ab\" :packed) (defun main () (aref-byte s 1))" 'cl-pointer-byte-abi)))
  (fiveam:is (plusp (%cl-count-op "POKE-BYTE-POINTER" "(defstring s \"ab\" :packed) (defun main () (aset-byte s 1 65))" 'cl-pointer-byte-abi))))

;;; #433: a repeated computed address

(eval `(defbackend cl-pointer-stack-abi (:extends callfoo-lang-abi)
         (registers :address d)
         (without-ops :peek :poke :peek-byte :poke-byte)
         (ops (:point (r) (movv (reg d) (reg r)))
              (:peek-pointer (dst) (ldx (reg dst) (ind d)))
              (:poke-pointer (src) (stx (ind d) (reg src)))
              (:peek-byte-pointer (dst) (ldb (reg dst) (ind d)))
              (:poke-byte-pointer (src) (stb (ind d) (reg src))))))

(eval `(defbackend cl-pointer-poke-label-abi (:extends cl-pointer-byte-abi)
         (ops (:peek-label (d label) (ldm d label))
              (:poke-label (label s) (stm s label)))))

(defparameter +cl-address-programs+
  '(("(defarray a (1 2 3 4)) (defun f (i) (+ (aref a i) (aref a i))) (defun main () (f 2))" 1 6)
    ("(defstring s \"abcd\" :packed) (defun f (i) (+ (aref-byte s i) (aref-byte s i))) (defun main () (f 1))" 1 196)
    ("(defarray a 4) (defun f (i) (aset a i 5) (aref a i)) (defun main () (f 2))" 1 5)
    ("(defarray a (1 2 3 4)) (defun f (i) (+ (aref a i) (aref a (+ i 0)))) (defun main () (f 2))" 2 6)
    ("(defarray a (1 2 3 4)) (defun f (i) (let ((x (aref a i))) (set i 3) (+ x (aref a i)))) (defun main () (f 1))" 2 6)
    ("(defarray a (1 2 3 4)) (defun f (i) (+ (aref a i) (progn (if 0 0 0) (aref a i)))) (defun main () (f 2))" 2 6)
    ("(defarray a (1 2 3 4)) (defun f (i) (+ (aref a i) (aref a (progn (set i 3) i)))) (defun main () (f 1))" 2 6)
    ("(defarray a (1 2 3 4)) (defun f (i) (aset a i (progn (set i 3) 9)) (aref a i)) (defun main () (f 1))" 2 4)
    ("(defarray a (1 2 3 4)) (defun f (i) (let ((i 1)) (+ (aref a i) (let ((i 2)) (aref a i))))) (defun main () (f 0))" 2 5)
    ("(defarray a (1 2 3 4)) (defarray b (5 6 7 8)) (defun f (i) (+ (aref a i) (aref b i))) (defun main () (f 1))" 2 8)
    ("(defarray a (1 2 3 4)) (defun g () 0) (defun f (i) (+ (aref a i) (progn (g) (aref a i)))) (defun main () (f 2))" 2 6)))

(fiveam:test a-repeated-computed-address-points-the-register-once
  (dolist (backend '(cl-pointer-poke-label-abi cl-pointer-stack-abi))
    (loop for (source points nil) in +cl-address-programs+
          do (fiveam:is (= points (%cl-count-op "POINT" source backend)) "~A ~A" backend source)))
  (dolist (backend '(cl-pointer-byte-abi cl-pointer-poke-label-abi cl-pointer-stack-abi))
    (loop for (source nil expected) in +cl-address-programs+
          do (fiveam:is (= expected (%cv-a (%cl-run source backend))) "~A ~A" backend source))))

(fiveam:test an-asm-that-writes-a-variable-of-the-address-forgets-it
  (let ((source "(defarray a (1 2 3 4))
                 (defun f (i) (+ (aref a i) (progn (asm (:clobbers a) (:op :const (reg a) 3) (:op :set (:var i) (reg a))) (aref a i))))
                 (defun main () (f 1))"))
    (fiveam:is (= 2 (%cl-count-op "POINT" source 'cl-pointer-stack-abi)))
    (fiveam:is (= 6 (%cv-a (%cl-run source 'cl-pointer-stack-abi))))))

;;; #436: a store reuses the register its value left

(defparameter +cl-read-modify-write-programs+
  '(("(defarray a (1 2 3 4)) (defun f (i) (aset a i (+ (aref a i) 1)) (aref a i)) (defun main () (f 2))" 1 4)
    ("(defstring s \"abcd\" :packed) (defun f (i) (aset-byte s i (+ (aref-byte s i) 1)) (aref-byte s i)) (defun main () (f 1))" 1 99)
    ("(defarray a (1 2 3 4)) (defun f (i) (aset a i (+ (aref a (+ i 1)) 1)) (aref a i)) (defun main () (f 1))" 2 4)
    ("(defarray a (1 2 3 4)) (defun g () 1) (defun f (i) (aset a (+ i 1) (+ (g) 1)) (aref a 2)) (defun main () (f 1))" nil 2)
    ("(defarray a (1 2 3 4)) (defun f (i) (aset a i (progn (set i 3) 9)) (aref a i)) (defun main () (f 1))" 2 4)
    ("(defarray a (1 2 3 4)) (defun f (p) (poke p (+ (peek p) 1)) (peek p)) (defun main () (f a))" 1 2)
    ("(defarray a (1 2 3 4)) (defun f (p) (poke p (+ (peek (+ p 1)) 1)) (peek p)) (defun main () (f a))" 2 3)
    ("(defarray a (1 2 3 4)) (defun f (i) (+ 1 (aset a i (+ (aref a i) 1)))) (defun main () (f 2))" nil 5)
    ("(defarray a (1 2 3 4)) (defun f (i) (+ 1 (aset a i (+ (aref a (+ i 1)) 1)))) (defun main () (f 1))" nil 5)
    ("(defarray a (1 2 3 4)) (defun f (i) (+ 1 (aset a i 5))) (defun main () (f 2))" nil 6)
    ("(defstring s \"abcd\" :packed) (defun f (i) (+ 1 (aset-byte s i (+ (aref-byte s i) 1)))) (defun main () (f 1))" nil 100)
    ("(defarray a (1 2 3 4)) (defun f (p) (+ 1 (poke p (+ (peek p) 1)))) (defun main () (f a))" nil 3)))

(fiveam:test a-store-reuses-the-register-its-value-left
  (dolist (backend '(cl-pointer-poke-label-abi cl-pointer-stack-abi))
    (loop for (source points nil) in +cl-read-modify-write-programs+
          when points
            do (fiveam:is (= points (%cl-count-op "POINT" source backend)) "~A ~A" backend source)))
  (dolist (backend '(cl-pointer-byte-abi cl-pointer-poke-label-abi cl-pointer-stack-abi))
    (loop for (source nil expected) in +cl-read-modify-write-programs+
          do (fiveam:is (= expected (%cv-a (%cl-run source backend))) "~A ~A" backend source))))

;;; #430: -label variants

(eval `(defbackend cl-label-variant-abi (:extends cl-label-abi)
         (ops (:add-label (d label) (addrm d label))
              (:sub-label (d label) (subrm d label))
              (:eq-label (d label) (seqrm d label))
              (:lt-label (d label) (sltrm d label))
              ,@(loop for comparison in '("EQ" "NE" "LT" "GT" "LE" "GE")
                      for mnemonic in '(beqrm bnerm bltrm bgtrm blerm bgerm)
                      collect `(,(intern (format nil "BRANCH-~A-LABEL" comparison) :keyword)
                                (a label target) (,mnemonic a label target))))))

(defun %cl-label-variant-names (source)
  (%cl-op-names (%cl-compile source 'cl-label-variant-abi)))

(fiveam:test label-variants-replace-the-load-of-a-global-right-operand
  (let ((names (%cl-label-variant-names "(defvar g 4) (defun f (x) (+ x g)) (defun main () (f 3))")))
    (fiveam:is (member "ADD-LABEL" names :test #'string-equal))
    (fiveam:is (= 1 (count "PEEK-LABEL" names :test #'string-equal)) "only x is loaded")
    (fiveam:is (= 7 (%cv-a (%cl-run "(defvar g 4) (defun f (x) (+ x g)) (defun main () (f 3))"
                                   'cl-label-variant-abi))))))

(fiveam:test label-variants-serve-a-static-slot
  (let ((source "(defun f (a b) (- a b)) (defun main () (f 9 4))"))
    (fiveam:is (member "SUB-LABEL" (%cl-label-variant-names source) :test #'string-equal))
    (fiveam:is (= 5 (%cv-a (%cl-run source 'cl-label-variant-abi))))))

(fiveam:test label-variants-branch-on-a-global
  (let ((source "(defvar g 5) (defun f (x) (if (< x g) 1 2)) (defun main () (+ (* 10 (f 3)) (f 7)))"))
    (fiveam:is (member "BRANCH-GE-LABEL" (%cl-label-variant-names source) :test #'string-equal))
    (fiveam:is (= 12 (%cv-a (%cl-run source 'cl-label-variant-abi))))))

(fiveam:test label-variants-swap-a-global-left-operand-past-a-leaf
  (let ((source "(defvar g 5) (defun f (x) (> g x)) (defun main () (+ (* 10 (f 3)) (f 7)))"))
    (fiveam:is (member "LT-LABEL" (%cl-label-variant-names source) :test #'string-equal))
    (fiveam:is (= 10 (%cv-a (%cl-run source 'cl-label-variant-abi))))))

(fiveam:test label-variants-do-not-swap-a-global-past-a-call
  (let ((source "(defvar g 5) (defun bump () (set g 9) 1) (defun main () (- g (bump)))"))
    (fiveam:is (not (member "SUB-LABEL" (%cl-label-variant-names source) :test #'string-equal)))
    (fiveam:is (= 4 (%cv-a (%cl-run source 'cl-label-variant-abi))))))

(fiveam:test label-variants-must-take-the-parameters-of-their-operation
  (fiveam:is (typep (%backend-error-of '(defbackend cl-label-variant-arity-abi (:isa callfoo) (ops (:add-label (a) (ldi a 1)))))
                    'backend-definition-error))
  (fiveam:is (typep (%backend-error-of '(defbackend cl-label-variant-arity-abi (:isa callfoo) (ops (:branch-lt-label (a b) (ldi a b)))))
                    'backend-definition-error)))

;;; #434: a constant-index aref as an operand leaf

(defun %cl-op-args (name source backend)
  "The arguments after the destination of each (:op NAME ...) item in the compiled SOURCE."
  (labels ((walk (items)
             (cond ((and (consp items) (eq :op (first items)) (string-equal (%designator-name (second items)) name))
                    (list (cdddr items)))
                   ((consp items) (mapcan #'walk (copy-list items))))))
    (walk (%cl-compile source backend))))

(fiveam:test aref-leaf-right-operand-uses-the-label-variant
  (let ((source "(defarray a (1 2 3)) (defun f (x) (+ x (aref a 2))) (defun main () (f 4))"))
    (let ((names (%cl-label-variant-names source)))
      (fiveam:is (member "ADD-LABEL" names :test #'string-equal))
      (fiveam:is (= 1 (count "PEEK-LABEL" names :test #'string-equal)) "only x is loaded"))
    (fiveam:is (= 7 (%cv-a (%cl-run source 'cl-label-variant-abi))))))

(fiveam:test aref-leaf-index-zero-passes-the-bare-label
  (let ((args (%cl-op-args "ADD-LABEL" "(defarray a (5 2)) (defun f (x) (+ x (aref a 0))) (defun main () (f 4))" 'cl-label-variant-abi)))
    (fiveam:is (= 1 (length args)))
    (fiveam:is (symbolp (first (first args))))))

(fiveam:test aref-leaf-takes-a-defconstant-index
  (let ((source "(defconstant k 1) (defarray a (5 2)) (defun f (x) (+ x (aref a k))) (defun main () (f 4))"))
    (fiveam:is (member "ADD-LABEL" (%cl-label-variant-names source) :test #'string-equal))
    (fiveam:is (= 6 (%cv-a (%cl-run source 'cl-label-variant-abi))))))

(fiveam:test aref-leaf-branches-on-an-element
  (let ((source "(defarray a (5 2)) (defun f (x) (if (< x (aref a 0)) 1 2)) (defun main () (+ (* 10 (f 3)) (f 7)))"))
    (fiveam:is (member "BRANCH-GE-LABEL" (%cl-label-variant-names source) :test #'string-equal))
    (fiveam:is (= 12 (%cv-a (%cl-run source 'cl-label-variant-abi))))))

(fiveam:test aref-leaf-left-operand-swaps-past-a-leaf
  (let ((source "(defarray a (5 2)) (defun f (x) (> (aref a 0) x)) (defun main () (+ (* 10 (f 3)) (f 7)))"))
    (fiveam:is (member "LT-LABEL" (%cl-label-variant-names source) :test #'string-equal))
    (fiveam:is (= 10 (%cv-a (%cl-run source 'cl-label-variant-abi))))))

(fiveam:test aref-leaf-left-operand-does-not-swap-past-a-call
  (let ((source "(defarray a (5 2)) (defun bump () (aset a 0 9) 1) (defun main () (- (aref a 0) (bump)))"))
    (fiveam:is (not (member "SUB-LABEL" (%cl-label-variant-names source) :test #'string-equal)))
    (fiveam:is (= 4 (%cv-a (%cl-run source 'cl-label-variant-abi))))))

(fiveam:test aref-leaf-loads-straight-into-the-temp-register
  (let ((source "(defarray a (1 2 3)) (defun f (x) (* x (aref a 2))) (defun main () (f 4))"))
    (fiveam:is (= 12 (%cv-a (%cl-run source 'cl-label-abi))))
    (fiveam:is (= 0 (%cl-count-op "MOVE" source 'cl-label-abi)))))

(fiveam:test aref-callee-is-computed-before-the-arguments
  (let ((source "(defarray ops (0 0))
                 (defun one (n) 1) (defun two (n) 2)
                 (defun pick (n) (aset ops 0 (function two)) n)
                 (defun main () (aset ops 0 (function one)) (funcall (aref ops 0) (pick 0)))"))
    (fiveam:is (= 1 (%cv-a (%cl-run source 'cl-label-variant-abi))))))

(fiveam:test aref-leaf-runs-on-the-pointer-register-backends
  (dolist (backend '(cl-pointer-abi cl-pointer-label-abi))
    (fiveam:is (= 9 (%cv-a (%cl-run "(defarray a (1 2 3)) (defun f (x) (+ x (aref a 1))) (defun main () (f 7))" backend))))))

(fiveam:test aref-leaf-operand-errors-are-the-arefs-own
  (fiveam:is (search "unknown variable" (%cl-fail "(defun main () (+ 1 (aref nope 1)))" 'cl-label-variant-abi)))
  (fiveam:is (%cl-fail "(defarray a 2) (defun main () (+ 1 (aref a 1 2)))" 'cl-label-variant-abi)))

;;; #435: funcall reads its callee before the arguments

(defparameter +cl-callee-order-globals+
  "(defvar g 0) (defun one (n) 1) (defun two (n) 2)
   (defun pick (n) (set g (function two)) n)
   (defun main () (set g (function one)) (funcall g (pick 0)))")

(fiveam:test global-callee-is-read-before-an-argument-sets-it
  (dolist (backend '(callfoo-lang-abi cl-static-abi cl-label-variant-abi))
    (fiveam:is (= 1 (%cv-a (%cl-run +cl-callee-order-globals+ backend))))))

(fiveam:test local-callee-is-read-before-an-argument-sets-it
  (let ((source "(defun one (n) 1) (defun two (n) 2)
                 (defun main () (let ((f (function one))) (funcall f (progn (set f (function two)) 0))))"))
    (dolist (backend '(callfoo-lang-abi cl-static-abi))
      (fiveam:is (= 1 (%cv-a (%cl-run source backend)))))))

(fiveam:test leaf-callee-still-loads-after-leaf-arguments
  (let ((source "(defvar g 0) (defun add (a b) (+ a b))
                 (defun main () (set g (function add)) (funcall g 3 4))"))
    (dolist (backend '(callfoo-lang-abi cl-static-abi))
      (fiveam:is (= 7 (%cv-a (%cl-run source backend)))))))

(fiveam:test aref-callee-loads-after-leaf-arguments
  (let ((source "(defarray ops ((function add))) (defun add (a b) (+ a b))
                 (defun main () (funcall (aref ops 0) 3 4))"))
    (fiveam:is (= 7 (%cv-a (%cl-run source 'cl-label-variant-abi))))))
