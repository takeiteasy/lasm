;;;; tests/include.lisp
;;;; fiveam tests for .include expansion (include.lisp, #78). Reuses the
;;;; INSTR-TEST-MACHINE fixture; source files live in tests/fixtures/include/.

(in-package #:lasm)

(fiveam:def-suite include :in lasm)
(fiveam:in-suite include)

(defun %assemble-include-fixture (name)
  (assemble-file (asdf:system-relative-pathname :lasm (format nil "tests/fixtures/include/~A" name))
                 :machine 'instr-test-machine))

(fiveam:test included-equ-and-macro-are-visible-to-the-includer
  (fiveam:is (equalp #(#xA2 10) (assembly-cells (%assemble-include-fixture "main.asm")))))

(fiveam:test nested-include-resolves-against-its-own-includer
  (fiveam:is (equalp #(#xEA #xEA #xEA) (assembly-cells (%assemble-include-fixture "nested.asm")))))

(fiveam:test include-same-file-twice-processes-it-twice
  (fiveam:is (equalp #(#xEA #xEA #xEA) (assembly-cells (%assemble-include-fixture "twice.asm")))))

(fiveam:test label-on-include-line-binds-to-first-included-address
  (fiveam:is (= 1 (gethash "foo" (assembly-symbols (%assemble-include-fixture "label.asm"))))))

(fiveam:test include-inside-macro-body-is-spliced-into-the-body
  (fiveam:is (equalp #(#xEA #xEA) (assembly-cells (%assemble-include-fixture "in-macro.asm")))))

(fiveam:test include-from-string-assemble-resolves-against-cwd-binding
  (let ((*include-directory* (asdf:system-relative-pathname :lasm "tests/fixtures/include/")))
    (fiveam:is (equalp #(#xEA #xEA)
                       (assembly-cells (assemble "nop
.include \"sub/c.asm\"" :machine 'instr-test-machine))))))

(fiveam:test include-is-case-insensitive
  (let ((*include-directory* (asdf:system-relative-pathname :lasm "tests/fixtures/include/")))
    (fiveam:is (equalp #(#xEA)
                       (assembly-cells (assemble ".INCLUDE \"sub/c.asm\"" :machine 'instr-test-machine))))))

;;; Errors

(fiveam:test circular-include-signals-and-names-the-chain
  (handler-case (%assemble-include-fixture "cycle-a.asm")
    (include-error (e)
      (let ((message (lasm-syntax-error-message e)))
        (fiveam:is (search "Circular" message))
        (fiveam:is (search "cycle-a.asm" message))
        (fiveam:is (search "cycle-b.asm" message))))
    (:no-error (&rest _) (declare (ignore _)) (fiveam:fail "expected include-error"))))

(fiveam:test missing-include-target-signals-with-the-include-line
  (handler-case (%assemble-include-fixture "missing.asm")
    (include-error (e)
      (fiveam:is (= 2 (lasm-syntax-error-line e)))
      (fiveam:is (search "nope.asm" (lasm-syntax-error-message e))))
    (:no-error (&rest _) (declare (ignore _)) (fiveam:fail "expected include-error"))))

(fiveam:test non-string-include-operand-signals
  (fiveam:signals include-error (%assemble-include-fixture "bad-operand.asm")))

(fiveam:test include-mode-suffix-signals
  (fiveam:signals include-error
    (expand-includes (parse ".include.w \"x.asm\""))))

(fiveam:test include-with-no-operand-signals
  (fiveam:signals include-error (expand-includes (parse ".include"))))

(fiveam:test unexpanded-include-reaching-layout-signals
  (fiveam:signals include-error
    (assemble-statements (parse ".include \"sub/c.asm\"") :machine 'instr-test-machine)))

(fiveam:test include-is-not-a-registered-directive
  (fiveam:is (null (find-directive-descriptor ".include"))))

(fiveam:test included-instruction-error-has-own-file-and-excerpt
  (handler-case (%assemble-include-fixture "undefined.asm")
    (unresolved-label (c)
      (fiveam:is (search "undefined.asm" (lasm-syntax-error-file c)))
      (fiveam:is (= 2 (lasm-syntax-error-line c)))
      (fiveam:is (= 5 (lasm-syntax-error-column c)))
      (fiveam:is (search "bne missing" (diagnostic-text c))))))

(fiveam:test nested-included-data-error-has-own-file-and-excerpt
  (handler-case (%assemble-include-fixture "nested-undefined.asm")
    (unresolved-label (c)
      (fiveam:is (search "sub/undefined.asm" (lasm-syntax-error-file c)))
      (fiveam:is (= 2 (lasm-syntax-error-line c)))
      (fiveam:is (search ".byte missing + 1" (diagnostic-text c))))))

(fiveam:test included-directive-error-has-file
  (handler-case (%assemble-include-fixture "bad-operand.asm")
    (include-error (c)
      (fiveam:is (search "bad-operand.asm" (lasm-syntax-error-file c))))))

(fiveam:test parse-error-in-included-file-has-own-file
  (handler-case (%assemble-include-fixture "load-bad-syntax.asm")
    (parse-failure (c)
      (fiveam:is (search "bad-syntax.asm" (lasm-syntax-error-file c)))
      (fiveam:is (search "ldx 1,,2" (diagnostic-text c))))))

(fiveam:test missing-include-reports-containing-file
  (handler-case (%assemble-include-fixture "missing.asm")
    (include-error (c)
      (fiveam:is (search "missing.asm" (lasm-syntax-error-file c)))
      (fiveam:is (search ".include \"nope.asm\"" (diagnostic-text c))))))

(fiveam:test macro-defined-in-include-reports-call-and-body-files
  (handler-case (%assemble-include-fixture "macro-call.asm")
    (unresolved-label (c)
      (fiveam:is (search "macro-call.asm" (lasm-syntax-error-file c)))
      (fiveam:is (search "macro-def.asm" (lasm-syntax-error-definition-file c)))
      (fiveam:is (= 2 (lasm-syntax-error-line c)))
      (fiveam:is (= 2 (lasm-syntax-error-definition-line c)))
      (fiveam:is (search "ldx #missing" (diagnostic-text c))))))
