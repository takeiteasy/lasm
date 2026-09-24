;;;; tests/definition-error.lisp
;;;; fiveam tests for the DEFINITION-ERROR family (diagnostic.lisp).

(in-package #:lasm)

(fiveam:def-suite definition-error :in lasm)
(fiveam:in-suite definition-error)

(defun %definition-error-of (form)
  (handler-case (progn (eval form) nil)
    (definition-error (c) c)))

(fiveam:test each-definer-signals-its-own-subtype-and-names-the-definition
  (loop for (form type name) in
        '(((defmachine bad-def-machine (bogus-clause)) machine-definition-error bad-def-machine)
          ((defmode bad-def-mode (one-of just-one)) mode-definition-error bad-def-mode)
          ((deflexer bad-def-lexer (bogus-clause)) lexer-definition-error bad-def-lexer)
          ((defdirective ".baddef" (a b c) (bogus a)) directive-definition-error ".baddef")
          ((definstruction instr-test-machine baddef (bogus-clause))
           instruction-definition-error baddef))
        do (let ((c (%definition-error-of form)))
             (fiveam:is (typep c type))
             (fiveam:is (typep c 'definition-error))
             (fiveam:is (typep c 'lasm-error))
             (fiveam:is (equalp (string name) (string (definition-error-name c))))
             (fiveam:is (stringp (definition-error-message c)))
             (fiveam:is (string= (definition-error-message c) (princ-to-string c))))))

(fiveam:test definition-error-is-not-a-syntax-error
  (let ((c (%definition-error-of '(defmode bad-def-mode-2 (one-of just-one)))))
    (fiveam:is (not (typep c 'lasm-syntax-error)))))

(fiveam:test opcode-conflict-is-an-instruction-definition-error
  (fiveam:is (subtypep 'opcode-conflict 'instruction-definition-error)))

(defmacro %bad-definition-macro ()
  (%defmode-error "raised while expanding"))

(fiveam:test compile-definition-resignals-the-typed-error-sbcl-defers
  (let ((c (handler-case (%compile-definition '(lambda () (%bad-definition-macro)) 'probe)
             (definition-error (c) c))))
    (fiveam:is (typep c 'mode-definition-error))
    (fiveam:is (eq 'probe (definition-error-name c)))
    (fiveam:is (string= "raised while expanding" (definition-error-message c)))))

(fiveam:test compile-definition-returns-the-function-when-nothing-is-raised
  (fiveam:is (= 3 (funcall (%compile-definition '(lambda () 3) 'probe)))))

(fiveam:test semantics-macrolet-misuse-is-an-instruction-definition-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction instr-test-machine baddef-sem
             (encoding (opcode #xFE))
             (semantics (interrupt-return))))))
