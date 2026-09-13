;;;; tests/macro.lisp
;;;; fiveam tests for .macro/.endm expansion (macro.lisp, #33). Reuses the
;;;; INSTR-TEST-MACHINE fixture and its LDX/NOP/BNE/MOVI instructions from
;;;; tests/instruction.lisp, the same way tests/assembler.lisp does.

(in-package #:lasm)

(fiveam:def-suite macro :in lasm)
(fiveam:in-suite macro)

;;; Expansion produces the same bytes as the hand-written equivalent

(fiveam:test macro-expansion-matches-hand-written-equivalent
  (let ((expanded (assemble ".macro loadx n
    ldx #n
.endm
    loadx 10" :machine 'instr-test-machine))
        (hand-written (assemble "ldx #10" :machine 'instr-test-machine)))
    (fiveam:is (equalp (assembly-bytes hand-written) (assembly-bytes expanded)))))

(fiveam:test macro-can-be-invoked-more-than-once
  (let ((a (assemble ".macro loadx n
    ldx #n
.endm
    loadx 10
    loadx 20" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#xA2 10 #xA2 20) (assembly-bytes a)))))

(fiveam:test macro-can-be-invoked-before-its-own-definition
  (let ((a (assemble "    loadx 10
.macro loadx n
    ldx #n
.endm" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#xA2 10) (assembly-bytes a)))))

;;; Substitution into a multi-hole addressing mode (MOVI, two-hole-test-mode)

(fiveam:test macro-substitutes-into-multi-hole-mode
  (let ((a (assemble ".macro store dst, src
    movi dst, src
.endm
    store 1, 2" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#xF8 1 2 0) (assembly-bytes a)))))

;;; Zero-parameter and empty-body macros

(fiveam:test zero-parameter-macro-expands
  (let ((a (assemble ".macro noop
    nop
.endm
    noop" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#xEA) (assembly-bytes a)))))

(fiveam:test empty-body-macro-with-label-binds-and-emits-nothing
  (let ((a (assemble ".macro nothing
.endm
here: nothing
    nop" :machine 'instr-test-machine)))
    (fiveam:is (= 0 (gethash "here" (assembly-symbols a))))
    (fiveam:is (equalp #(#xEA) (assembly-bytes a)))))

;;; A label on the invocation line

(fiveam:test label-on-invocation-binds-to-macro-body-start
  (let ((a (assemble ".macro loadx n
    ldx #n
.endm
start: loadx 10
    bne start" :machine 'instr-test-machine)))
    (fiveam:is (= 0 (gethash "start" (assembly-symbols a))))
    (fiveam:is (equalp #(#xA2 10 #xD0 0 0) (assembly-bytes a)))))

;;; Nested invocation (a macro invoking another macro)

(fiveam:test nested-macro-invocation-expands
  (let ((a (assemble ".macro inner n
    ldx #n
.endm
.macro outer n
    inner n
.endm
    outer 10" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#xA2 10) (assembly-bytes a)))))

;;; assemble-statements sees expansion too, not just assemble

(fiveam:test assemble-statements-also-expands-macros
  (let* ((statements (parse ".macro loadx n
    ldx #n
.endm
    loadx 10"))
         (a (assemble-statements statements :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#xA2 10) (assembly-bytes a)))))

;;; expand-macros is directly callable and idempotent on macro-free input

(fiveam:test expand-macros-on-macro-free-input-returns-equivalent-statements
  (let* ((statements (parse "nop"))
         (expanded (expand-macros statements)))
    (fiveam:is (= 1 (length expanded)))
    (fiveam:is (string= "nop" (statement-mnemonic (first expanded))))))

;;; Error conditions

(fiveam:test unterminated-macro-signals-macro-error
  (fiveam:signals macro-error
    (assemble ".macro loadx n
    ldx #n" :machine 'instr-test-machine)))

(fiveam:test orphan-endm-signals-macro-error
  (fiveam:signals macro-error
    (assemble "nop
.endm" :machine 'instr-test-machine)))

(fiveam:test nested-macro-definition-signals-macro-error
  (fiveam:signals macro-error
    (assemble ".macro outer n
.macro inner n
    ldx #n
.endm
.endm" :machine 'instr-test-machine)))

(fiveam:test duplicate-macro-name-signals-macro-error
  (fiveam:signals macro-error
    (assemble ".macro loadx n
    ldx #n
.endm
.macro loadx n
    ldx #n
.endm" :machine 'instr-test-machine)))

(fiveam:test macro-name-colliding-with-directive-signals-macro-error
  (fiveam:signals macro-error
    (assemble ".macro .byte n
    ldx #n
.endm" :machine 'instr-test-machine)))

(fiveam:test macro-invocation-wrong-argument-count-signals-macro-error
  (fiveam:signals macro-error
    (assemble ".macro loadx n
    ldx #n
.endm
    loadx 10, 20" :machine 'instr-test-machine)))

(fiveam:test recursive-macro-signals-macro-error
  (fiveam:signals macro-error
    (assemble ".macro recur n
    recur n
.endm
    recur 10" :machine 'instr-test-machine)))

(fiveam:test macro-header-with-non-identifier-parameter-signals-macro-error
  (fiveam:signals macro-error
    (assemble ".macro loadx 1
    nop
.endm" :machine 'instr-test-machine)))

(fiveam:test macro-name-starting-with-local-label-prefix-signals-macro-error
  (fiveam:signals macro-error
    (assemble ".macro .loadx n
    ldx #n
.endm" :machine 'instr-test-machine)))

(fiveam:test macro-parameter-starting-with-local-label-prefix-signals-macro-error
  (fiveam:signals macro-error
    (assemble ".macro loadx .n
    ldx #.n
.endm" :machine 'instr-test-machine)))

;;; Existing duplicate-label behavior, unaffected by expansion but documented
;;; here since the plan notes no label hygiene: a body label collides when
;;; the same macro is invoked twice under one enclosing scope.

(fiveam:test macro-body-label-collides-on-second-invocation-without-hygiene
  (fiveam:signals assembly-error
    (assemble ".macro tagged
tag: nop
.endm
    tagged
    tagged" :machine 'instr-test-machine)))

;;; Forced addressing-mode suffix (#40) and macros

(fiveam:test mode-suffix-on-macro-invocation-signals-macro-error
  (fiveam:signals macro-error
    (assemble ".macro loadx n
    ldx #n
.endm
    loadx.w 10" :machine 'instr-test-machine)))

(fiveam:test mode-suffix-in-macro-body-survives-expansion
  ;; "lda.w n" inside the body still forces ABSOLUTE (opcode #x12) even
  ;; though the substituted argument would otherwise fit zero-page.
  (let ((a (assemble ".macro loada n
    lda.w n
.endm
    loada 5" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#x12 5 0) (assembly-bytes a)))))
