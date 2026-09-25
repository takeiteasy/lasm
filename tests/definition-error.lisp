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

(fiveam:test lambda-list-mismatch-in-a-definer-is-a-definition-error
  (let ((c (%definition-error-of
            '(definstruction word-test-machine bogus-keys
              (modes wc-two)
              (encoding (opcode 5)
                        (operand value :field src
                          (variant (range 0 7) inline :alias t)))
              (semantics nil)))))
    (fiveam:is (typep c 'instruction-definition-error))
    (fiveam:is (search "Malformed" (definition-error-message c)))))

(fiveam:test definition-bind-outside-a-definer-stays-a-lisp-error
  (fiveam:signals error (%definition-bind (a &key b) '(1 :c 2) (list a b))))

(fiveam:test definition-bind-lets-body-errors-through
  (let ((*definition-type* 'mode-definition-error))
    (fiveam:signals type-error
      (%definition-bind (a) '(1) (+ a :not-a-number)))))

(fiveam:test with-definition-errors-returns-body-values
  (fiveam:is (equal '(1 2) (multiple-value-list (with-definition-errors (values 1 2))))))

(fiveam:test with-definition-errors-passes-a-definition-error-through
  (fiveam:signals mode-definition-error
    (with-definition-errors (eval '(defmode bad-def-mode-3 (one-of just-one))))))

(fiveam:test with-definition-errors-replaces-an-escaping-error
  (let ((c (handler-case
               (with-definition-errors
                 (ignore-errors (eval '(defmode bad-def-mode-4 (one-of just-one))))
                 (error "unrelated"))
             (definition-error (c) c))))
    (fiveam:is (typep c 'mode-definition-error))
    (fiveam:is (eq 'bad-def-mode-4 (definition-error-name c)))))

(fiveam:test with-definition-errors-resignals-an-error-the-body-handled
  (fiveam:signals mode-definition-error
    (with-definition-errors
      (ignore-errors (eval '(defmode bad-def-mode-5 (one-of just-one)))))))

(fiveam:test with-definition-errors-types-a-compile-file-failure
  (let* ((source (asdf:system-relative-pathname :lasm "tests/fixtures/definition/bad-mode.lisp"))
         (output (uiop:tmpize-pathname (merge-pathnames "bad-mode.fasl" (uiop:temporary-directory))))
         (c (unwind-protect
                 (handler-case
                     (let ((*error-output* (make-broadcast-stream))
                           (*standard-output* (make-broadcast-stream)))
                       (with-definition-errors (compile-file source :output-file output))
                       nil)
                   (definition-error (c) c))
              (uiop:delete-file-if-exists output))))
    (fiveam:is (typep c 'mode-definition-error))
    (fiveam:is (eq 'compile-file-bad-mode (definition-error-name c)))))
