(in-package #:lasm)

(fiveam:def-suite assertion :in lasm)
(fiveam:in-suite assertion)

(defun %assert-assemble (source)
  (assemble source :machine 'instr-test-machine))

(fiveam:test passing-assert-emits-nothing
  (let ((a (%assert-assemble "nop
.assert 1")))
    (fiveam:is (equalp #(#xEA) (assembly-cells a)))
    (fiveam:is (= 1 (length (assembly-listing a))))))

(fiveam:test failing-assert-signals-with-its-message
  (handler-case (%assert-assemble ".assert 0, \"boom\"")
    (assertion-error (e)
      (fiveam:is (search "boom" (lasm-syntax-error-message e)))
      (fiveam:is (= 1 (lasm-syntax-error-line e))))
    (:no-error (&rest _) (declare (ignore _)) (fiveam:fail "expected assertion-error"))))

(fiveam:test failing-assert-without-a-message-has-a-default
  (handler-case (%assert-assemble ".assert 0")
    (assertion-error (e) (fiveam:is (search "assertion failed" (lasm-syntax-error-message e))))
    (:no-error (&rest _) (declare (ignore _)) (fiveam:fail "expected assertion-error"))))

(fiveam:test assert-is-an-assembly-error
  (fiveam:signals assembly-error (%assert-assemble ".assert 0")))

(fiveam:test assert-checks-the-location-counter
  (fiveam:finishes (%assert-assemble ".org $10
.assert * <= $10"))
  (fiveam:signals assertion-error (%assert-assemble ".org $10
nop
.assert * <= $10")))

(fiveam:test assert-sees-a-label-defined-later
  (fiveam:finishes (%assert-assemble ".assert end == 1
nop
end:"))
  (fiveam:signals assertion-error (%assert-assemble ".assert end == 5
nop
end:")))

(fiveam:test assert-sees-a-local-label
  (fiveam:finishes (%assert-assemble "main: nop
.x: nop
.assert .x == 1")))

(fiveam:test assert-takes-the-set-value-at-its-line
  (fiveam:finishes (%assert-assemble ".set a, 1
.assert a == 1
.set a, 2
.assert a == 2")))

(fiveam:test assert-can-follow-a-label-on-its-line
  (fiveam:is (= 1 (gethash "here" (assembly-symbols (%assert-assemble "nop
here: .assert 1"))))))

(fiveam:test assert-inside-a-macro-substitutes-arguments
  (fiveam:finishes (%assert-assemble ".macro check v
.assert v == 1
.endm
    check 1"))
  (handler-case (%assert-assemble ".macro check v
.assert v == 1
.endm
    check 2")
    (assertion-error (e) (fiveam:is (= 4 (lasm-syntax-error-line e))))
    (:no-error (&rest _) (declare (ignore _)) (fiveam:fail "expected assertion-error"))))

(fiveam:test assert-in-a-skipped-branch-is-not-checked
  (fiveam:finishes (%assert-assemble ".if 0
.assert 0
.endif")))

(fiveam:test assert-defined-tests-the-final-symbol-table
  (fiveam:finishes (%assert-assemble ".assert defined(later)
later:"))
  (fiveam:signals assertion-error (%assert-assemble ".assert defined(nowhere)")))

(fiveam:test malformed-assert-signals
  (fiveam:signals assembly-error (%assert-assemble ".assert"))
  (fiveam:signals assembly-error (%assert-assemble ".assert 1, 2"))
  (fiveam:signals assembly-error (%assert-assemble ".assert 1, \"a\", \"b\""))
  (fiveam:signals assembly-error (%assert-assemble ".assert.w 1")))

(fiveam:test error-signals-when-reached
  (handler-case (%assert-assemble "nop
.error \"unsupported\"")
    (assertion-error (e)
      (fiveam:is (search "unsupported" (lasm-syntax-error-message e)))
      (fiveam:is (= 2 (lasm-syntax-error-line e))))
    (:no-error (&rest _) (declare (ignore _)) (fiveam:fail "expected assertion-error"))))

(fiveam:test error-in-a-skipped-branch-is-ignored
  (fiveam:is (equalp #(#xEA) (assembly-cells (%assert-assemble ".if 0
.error \"no\"
.endif
nop")))))

(fiveam:test error-takes-precedence-over-a-later-layout-error
  (fiveam:signals assertion-error (%assert-assemble ".error \"first\"
bogus_instruction")))

(fiveam:test error-in-a-taken-else-branch-signals
  (fiveam:signals assertion-error (%assert-assemble ".ifdef CONFIG
nop
.else
.error \"CONFIG is required\"
.endif")))

(fiveam:test malformed-error-signals
  (fiveam:signals assembly-error (%assert-assemble ".error"))
  (fiveam:signals assembly-error (%assert-assemble ".error 1"))
  (fiveam:signals assembly-error (%assert-assemble ".error \"a\", \"b\""))
  (fiveam:signals assembly-error (%assert-assemble ".error.w \"a\"")))

(fiveam:test assert-and-error-names-are-reserved-from-macros
  (fiveam:signals macro-error (%assert-assemble ".macro .assert
.endm"))
  (fiveam:signals macro-error (%assert-assemble ".macro .error
.endm")))
