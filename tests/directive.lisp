;;;; tests/directive.lisp
;;;; fiveam tests for DEFDIRECTIVE and the directive registry (directive.lisp,
;;;; #14) -- registration/lookup and macroexpansion-time validation only.
;;;; Statement-level dispatch (layout size, .ORG's origin/address effect,
;;;; .BYTE/.WORD/.RES encoding) is exercised in tests/assembler.lisp against
;;;; the shared INSTR-TEST-MACHINE fixture.

(in-package #:lasm)

(fiveam:def-suite directive :in lasm)
(fiveam:in-suite directive)

(fiveam:test builtin-directives-are-registered
  (fiveam:is (eq :set-origin (directive-descriptor-action (find-directive-descriptor ".org"))))
  (fiveam:is (eq :emit (directive-descriptor-action (find-directive-descriptor ".byte"))))
  (fiveam:is (eq :emit (directive-descriptor-action (find-directive-descriptor ".word"))))
  (fiveam:is (eq :reserve (directive-descriptor-action (find-directive-descriptor ".res")))))

(fiveam:test directive-lookup-is-case-insensitive
  (fiveam:is (eq (find-directive-descriptor ".org") (find-directive-descriptor ".ORG"))))

(fiveam:test unknown-directive-lookup-returns-nil
  (fiveam:is (null (find-directive-descriptor ".nope"))))

(fiveam:test org-directive-has-fixed-arity-one
  (fiveam:is (equal '(:fixed 1) (directive-descriptor-arity (find-directive-descriptor ".org")))))

(fiveam:test byte-directive-is-variadic-with-width-one
  (fiveam:is (eq :variadic (directive-descriptor-arity (find-directive-descriptor ".byte"))))
  (fiveam:is (= 1 (directive-descriptor-width (find-directive-descriptor ".byte")))))

(fiveam:test word-directive-width-is-two
  (fiveam:is (= 2 (directive-descriptor-width (find-directive-descriptor ".word")))))

(fiveam:test res-directive-has-fixed-arity-one
  (fiveam:is (equal '(:fixed 1) (directive-descriptor-arity (find-directive-descriptor ".res")))))

;; DEFDIRECTIVE's body-shape check (exactly one action form) runs at
;; macroexpansion time, so MACROEXPAND alone triggers it; its param-list and
;; action-head/reference checks run inside BUILD-DIRECTIVE-DESCRIPTOR, called
;; from the expansion's body -- those need EVAL, not just MACROEXPAND.

(fiveam:test defdirective-rejects-multi-form-body
  (fiveam:signals error
    (macroexpand '(defdirective ".bad" (x) (set-origin! x) (reserve x)))))

(fiveam:test defdirective-rejects-unknown-action-head
  (fiveam:signals error
    (eval '(defdirective ".bad" (x) (frobnicate x)))))

(fiveam:test defdirective-rejects-action-referencing-wrong-parameter
  (fiveam:signals error
    (eval '(defdirective ".bad" (x) (set-origin! y)))))

(fiveam:test defdirective-rejects-malformed-params
  (fiveam:signals error
    (eval '(defdirective ".bad" (x y) (set-origin! x)))))

(fiveam:test defdirective-registers-a-new-directive
  (defdirective ".test-marker" (n) (reserve n))
  (fiveam:is (eq :reserve (directive-descriptor-action (find-directive-descriptor ".test-marker")))))
