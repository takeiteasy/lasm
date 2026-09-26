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
  (loop for (name params . forms) in (backend-descriptor-ops (find-backend 'callfoo-lang-abi))
        unless (member name '("LOAD" "PUSH" "POP" "MOVE" "ALLOC" "FREE" "CALL" "RETURN") :test #'string=)
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
    ("(defun main () (< (- 1) 2))" . 1))
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
                  ("(defun a-b () 1) (defun az2dzb () 2) (defun main () 1)" "both make the label")))
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

(fiveam:test a-backend-checks-the-arity-of-the-language-operations
  (fiveam:signals backend-definition-error
    (eval '(defbackend cl-arity-abi (:machine callfoo) (ops (:const (a b c) (ldi a b)))))))

(fiveam:test source-is-read-without-evaluation-or-interning
  (dolist (text '("(defun main () '1)" "(defun main () #.(+ 1 2))" "(defun main () 1" "(defun main () 1))"
                  "(defun main () foo:bar)"))
    (fiveam:signals program-compile-error (read-source-from-string text)))
  (let ((*read-eval* t))
    (fiveam:is (null (find-symbol "SOME-UNSEEN-SOURCE-NAME" '#:lasm)))
    (read-source-from-string "(defun some-unseen-source-name () 1)")
    (fiveam:is (null (find-symbol "SOME-UNSEEN-SOURCE-NAME" '#:lasm)))))

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
