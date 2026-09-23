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
  (fiveam:is (eq :emit (directive-descriptor-action (find-directive-descriptor ".cell"))))
  (fiveam:is (eq :emit (directive-descriptor-action (find-directive-descriptor ".dat"))))
  (fiveam:is (eq :reserve (directive-descriptor-action (find-directive-descriptor ".res"))))
  (fiveam:is (eq :assign (directive-descriptor-action (find-directive-descriptor ".equ"))))
  (fiveam:is (eq :reassign (directive-descriptor-action (find-directive-descriptor ".set")))))

(fiveam:test equ-directive-has-fixed-arity-two
  (fiveam:is (equal '(:fixed 2) (directive-descriptor-arity (find-directive-descriptor ".equ")))))

(fiveam:test set-directive-has-fixed-arity-two
  (fiveam:is (equal '(:fixed 2) (directive-descriptor-arity (find-directive-descriptor ".set")))))

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

;; #65 -- .CELL/.DAT are plain .BYTE aliases: same :EMIT action, :VARIADIC
;; arity, width 1. Equivalence of emitted cells is exercised against
;; INSTR-TEST-MACHINE/WORDADDR-TEST-MACHINE in tests/assembler.lisp.
(fiveam:test cell-directive-is-variadic-with-width-one
  (fiveam:is (eq :variadic (directive-descriptor-arity (find-directive-descriptor ".cell"))))
  (fiveam:is (= 1 (directive-descriptor-width (find-directive-descriptor ".cell")))))

(fiveam:test dat-directive-is-variadic-with-width-one
  (fiveam:is (eq :variadic (directive-descriptor-arity (find-directive-descriptor ".dat"))))
  (fiveam:is (= 1 (directive-descriptor-width (find-directive-descriptor ".dat")))))

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
    (eval '(defdirective ".bad" (x y z) (set-origin! x)))))

(fiveam:test defdirective-rejects-malformed-rest-params
  ;; (&rest x y) -- more than one name after &REST -- must still be rejected
  ;; even though it's the same length as .EQU's legal (x y). Param-list
  ;; checks run inside BUILD-DIRECTIVE-DESCRIPTOR (called from the
  ;; expansion's body), so this needs EVAL, not just MACROEXPAND.
  (fiveam:signals error
    (eval '(defdirective ".bad" (&rest x y) (emit 1 x)))))

(fiveam:test defdirective-accepts-two-fixed-params-for-assign
  ;; (x y) is exactly .EQU's own param list (#35) -- legal now that ASSIGN
  ;; exists, unlike the three-parameter case above.
  (defdirective ".test-assign" (x y) (assign x y))
  (fiveam:is (eq :assign (directive-descriptor-action (find-directive-descriptor ".test-assign"))))
  (fiveam:is (equal '(:fixed 2) (directive-descriptor-arity (find-directive-descriptor ".test-assign")))))

(fiveam:test defdirective-rejects-assign-referencing-wrong-parameters
  (fiveam:signals error
    (eval '(defdirective ".bad" (x y) (assign y x)))))

(fiveam:test defdirective-registers-a-new-directive
  (defdirective ".test-marker" (n) (reserve n))
  (fiveam:is (eq :reserve (directive-descriptor-action (find-directive-descriptor ".test-marker")))))
