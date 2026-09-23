;;;; tests/macro.lisp
;;;; fiveam tests for .macro/.endm expansion. Reuses the
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
    (fiveam:is (equalp (assembly-cells hand-written) (assembly-cells expanded)))))

(fiveam:test macro-can-be-invoked-more-than-once
  (let ((a (assemble ".macro loadx n
    ldx #n
.endm
    loadx 10
    loadx 20" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#xA2 10 #xA2 20) (assembly-cells a)))))

(fiveam:test macro-can-be-invoked-before-its-own-definition
  (let ((a (assemble "    loadx 10
.macro loadx n
    ldx #n
.endm" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#xA2 10) (assembly-cells a)))))

;;; Substitution into a multi-hole addressing mode (MOVI, two-hole-test-mode)

(fiveam:test macro-substitutes-into-multi-hole-mode
  (let ((a (assemble ".macro store dst, src
    movi dst, src
.endm
    store 1, 2" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#xF8 1 2 0) (assembly-cells a)))))

;;; Zero-parameter and empty-body macros

(fiveam:test zero-parameter-macro-expands
  (let ((a (assemble ".macro noop
    nop
.endm
    noop" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#xEA) (assembly-cells a)))))

(fiveam:test empty-body-macro-with-label-binds-and-emits-nothing
  (let ((a (assemble ".macro nothing
.endm
here: nothing
    nop" :machine 'instr-test-machine)))
    (fiveam:is (= 0 (gethash "here" (assembly-symbols a))))
    (fiveam:is (equalp #(#xEA) (assembly-cells a)))))

;;; A label on the invocation line

(fiveam:test label-on-invocation-binds-to-macro-body-start
  (let ((a (assemble ".macro loadx n
    ldx #n
.endm
start: loadx 10
    bne start" :machine 'instr-test-machine)))
    (fiveam:is (= 0 (gethash "start" (assembly-symbols a))))
    (fiveam:is (equalp #(#xA2 10 #xD0 0 0) (assembly-cells a)))))

;;; Nested invocation (a macro invoking another macro)

(fiveam:test nested-macro-invocation-expands
  (let ((a (assemble ".macro inner n
    ldx #n
.endm
.macro outer n
    inner n
.endm
    outer 10" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#xA2 10) (assembly-cells a)))))

;;; assemble-statements sees expansion too, not just assemble

(fiveam:test assemble-statements-also-expands-macros
  (let* ((statements (parse ".macro loadx n
    ldx #n
.endm
    loadx 10"))
         (a (assemble-statements statements :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#xA2 10) (assembly-cells a)))))

;;; expand-macros is directly callable and idempotent on macro-free input

(fiveam:test expand-macros-on-macro-free-input-returns-equivalent-statements
  (let* ((statements (parse "nop"))
         (expanded (expand-macros statements 'instr-test-machine)))
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

(fiveam:test macro-body-global-labels-are-unique
  (let ((a (assemble ".macro tagged
tag: nop
    bne tag
.endm
    tagged
    tagged" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#xEA #xD0 0 0 #xEA #xD0 4 0) (assembly-cells a)))
    (fiveam:is (null (gethash "tag" (assembly-symbols a))))))

(fiveam:test macro-body-symbols-are-private
  (fiveam:signals unresolved-label
    (assemble ".macro tagged
tag: nop
.endm
    tagged
    bne tag" :machine 'instr-test-machine)))

(fiveam:test caller-argument-is-not-captured-by-body-symbol
  (let ((a (assemble "target: nop
.macro jump addr
target: nop
    bne addr
.endm
    jump target" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#xEA #xEA #xD0 0 0) (assembly-cells a)))))

(fiveam:test generated-macro-names-avoid-source-names
  (let ((a (assemble ".macro tagged
tag: nop
.endm
tag__LASM_1: nop
    tagged" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#xEA #xEA) (assembly-cells a)))))

(fiveam:test macro-body-local-labels-are-unique-in-one-scope
  (let ((a (assemble ".macro spin
.loop: nop
    bne .loop
.endm
top: nop
    spin
    spin" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#xEA #xEA #xD0 1 0 #xEA #xD0 5 0)
                       (assembly-cells a)))))

(fiveam:test macro-body-equ-names-are-unique
  (let ((a (assemble ".macro constant
value = 7
    ldx #value
.endm
    constant
    constant" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#xA2 7 #xA2 7) (assembly-cells a)))
    (fiveam:is (null (gethash "value" (assembly-symbols a))))))

(fiveam:test macro-body-local-equ-names-are-unique
  (let ((a (assemble ".macro constant
.equ .value, 7
    ldx #.value
.endm
top: nop
    constant
    constant" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#xEA #xA2 7 #xA2 7) (assembly-cells a)))))

(fiveam:test nested-macro-body-labels-are-unique
  (let ((a (assemble ".macro inner
again: nop
    bne again
.endm
.macro outer
    inner
.endm
    outer
    outer" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#xEA #xD0 0 0 #xEA #xD0 4 0) (assembly-cells a)))))

(fiveam:test macro-defaults-are-trailing-token-runs
  (let ((a (assemble ".macro load dst, k=1 + 2
    movi dst, k
.endm
    load 3
    load 4, 5" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#xF8 3 3 0 #xF8 4 5 0) (assembly-cells a)))))

(fiveam:test macro-defaults-are-literal
  (let ((a (assemble ".macro copy dst, src=dst
    bne src
.endm
dst: nop
    copy 4" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#xEA #xD0 0 0) (assembly-cells a)))))

(fiveam:test macro-default-rejects-missing-and-extra-arguments
  (fiveam:signals macro-error
    (assemble ".macro load dst, k=1
    ldx #k
.endm
    load" :machine 'instr-test-machine))
  (fiveam:signals macro-error
    (assemble ".macro load dst, k=1
    ldx #k
.endm
    load 1, 2, 3" :machine 'instr-test-machine)))

(fiveam:test macro-default-header-validation
  (dolist (header '(".macro load dst, k="
                    ".macro load dst, k=1, extra"
                    ".macro load dst k=1"
                    ".macro load dst, dst=1"))
    (fiveam:signals macro-error
      (assemble (format nil "~A~%    nop~%.endm" header)
                :machine 'instr-test-machine))))

(fiveam:test macro-parameter-cannot-define-a-body-symbol
  (fiveam:signals macro-error
    (assemble ".macro tagged tag
tag: nop
.endm" :machine 'instr-test-machine)))

(fiveam:test macro-name-colliding-with-instruction-signals-macro-error
  (fiveam:signals macro-error
    (assemble ".macro NoP
.endm" :machine 'instr-test-machine)))

(fiveam:test macro-name-check-uses-target-machine
  (let ((expanded (expand-macros (parse ".macro nop
.endm
    nop") 'test-machine)))
    (fiveam:is (null expanded))))

;;; Forced addressing-mode suffix and macros

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
    (fiveam:is (equalp #(#x12 5 0) (assembly-cells a)))))
