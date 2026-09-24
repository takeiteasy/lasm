;;;; tests/conditional.lisp
;;;; fiveam tests for .if/.elseif/.else/.endif (conditional.lisp). Reuses the
;;;; INSTR-TEST-MACHINE fixture.

(in-package #:lasm)

(fiveam:def-suite conditional :in lasm)
(fiveam:in-suite conditional)

(defun %cells (source)
  (coerce (assembly-cells (assemble source :machine 'instr-test-machine)) 'list))

(fiveam:test true-branch-is-kept-and-false-branch-dropped
  (fiveam:is (equal '(#xEA) (%cells ".if 1
    nop
.else
    ldx #1
.endif")))
  (fiveam:is (equal '(#xA2 1) (%cells ".if 0
    nop
.else
    ldx #1
.endif")))
  (fiveam:is (equal '(#xEA) (%cells ".if 1
    nop
.endif"))))

(fiveam:test elseif-chain-selects-the-first-true-branch
  (let ((source ".equ x, ~D
.if x == 1
    ldx #1
.elseif x == 2
    ldx #2
.elseif x >= 2
    ldx #3
.else
    ldx #4
.endif"))
    (fiveam:is (equal '(#xA2 1) (%cells (format nil source 1))))
    (fiveam:is (equal '(#xA2 2) (%cells (format nil source 2))))
    (fiveam:is (equal '(#xA2 3) (%cells (format nil source 3))))
    (fiveam:is (equal '(#xA2 4) (%cells (format nil source 0))))))

(fiveam:test conditional-directives-are-case-insensitive
  (fiveam:is (equal '(#xEA) (%cells ".IF 1
    nop
.ELSE
.ENDIF"))))

(fiveam:test nested-blocks
  (fiveam:is (equal '(#xA2 2) (%cells ".if 1
.if 0
    ldx #1
.else
    ldx #2
.endif
.else
    ldx #3
.endif")))
  (fiveam:is (equal '(#xA2 3) (%cells ".if 0
.if 1
    ldx #1
.else
    ldx #2
.endif
.else
    ldx #3
.endif"))))

(fiveam:test skipped-blocks-are-not-evaluated
  (fiveam:is (equal '(#xEA) (%cells ".if 0
.if undefined_name
    nop
.elseif also_undefined
    nop
.endif
.else
    nop
.endif")))
  (fiveam:is (equal '(#xEA) (%cells ".if 1
    nop
.elseif undefined_name
    ldx #1
.endif"))))

(fiveam:test conditions-see-earlier-constants
  (fiveam:is (equal '(#xEA) (%cells "debug = 1
.equ size, 4
.if debug && size > 2
    nop
.endif"))))

(fiveam:test set-values-are-read-in-source-order
  (fiveam:is (equal '(#xA2 1 #xEA) (%cells ".set n, 1
.if n
    ldx #1
.endif
.set n, 0
.if n
    ldx #2
.else
    nop
.endif"))))

(fiveam:test local-constants-resolve-within-their-scope
  (fiveam:is (equal '(#xEA) (%cells "start:
.on = 1
.if .on
    nop
.endif"))))

(fiveam:test logical-operators-in-conditions
  (fiveam:is (equal '(#xEA) (%cells ".if !0 && (1 || undefined_name)
    nop
.endif")))
  (fiveam:is (equal '() (%cells ".if 0 && undefined_name
    nop
.endif"))))

(fiveam:test label-on-a-conditional-line-binds
  (let ((a (assemble "    nop
here: .if 1
    nop
.endif
there:" :machine 'instr-test-machine)))
    (fiveam:is (= 1 (gethash "here" (assembly-symbols a))))
    (fiveam:is (= 2 (gethash "there" (assembly-symbols a))))))

(fiveam:test same-label-in-both-branches-does-not-collide
  (fiveam:is (equal '(#xEA) (%cells ".if 0
tag: nop
.else
tag: nop
.endif"))))

(fiveam:test conditional-inside-macro-sees-substituted-argument
  (let ((source ".macro pick n
.if n
    ldx #1
.else
    ldx #2
.endif
.endm
    pick ~D"))
    (fiveam:is (equal '(#xA2 1) (%cells (format nil source 5))))
    (fiveam:is (equal '(#xA2 2) (%cells (format nil source 0))))))

(fiveam:test macro-invocation-in-skipped-branch-emits-nothing
  (fiveam:is (equal '(#xEA) (%cells ".macro loadx n
    ldx #n
.endm
.if 0
    loadx 1
.else
    nop
.endif"))))

(fiveam:test skipped-lines-leave-no-listing-entries
  (let ((a (assemble ".if 0
    ldx #1
.else
    nop
.endif" :machine 'instr-test-machine)))
    (fiveam:is (= 1 (length (assembly-listing a))))))

;;; Conditions that are not constants

(fiveam:test label-in-condition-signals
  (fiveam:signals conditional-error
    (assemble "start: nop
.if start
    nop
.endif" :machine 'instr-test-machine))
  (fiveam:signals conditional-error
    (assemble ".if later
    nop
.endif
later: nop" :machine 'instr-test-machine)))

(fiveam:test location-counter-and-bank-in-condition-signal
  (fiveam:signals conditional-error
    (assemble ".if * == 0
    nop
.endif" :machine 'instr-test-machine))
  (fiveam:signals conditional-error
    (assemble "start: nop
.if bank(start)
    nop
.endif" :machine 'instr-test-machine)))

(fiveam:test layout-dependent-equ-in-condition-signals
  (fiveam:signals conditional-error
    (assemble "start: nop
.equ size, * - start
.if size
    nop
.endif" :machine 'instr-test-machine)))

(fiveam:test undefined-name-in-condition-signals
  (fiveam:signals conditional-error
    (assemble ".if nope
    nop
.endif" :machine 'instr-test-machine)))

;;; Malformed and unbalanced blocks

(fiveam:test unbalanced-blocks-signal
  (dolist (source '(".endif" ".else" ".elseif 1"
                    ".if 1
    nop"
                    ".if 1
.else
.else
.endif"
                    ".if 1
.else
.elseif 1
.endif"))
    (fiveam:signals conditional-error (assemble source :machine 'instr-test-machine))))

(fiveam:test unterminated-if-reports-its-own-line
  (handler-case (assemble "nop
.if 1
    nop" :machine 'instr-test-machine)
    (conditional-error (c) (fiveam:is (= 2 (lasm-syntax-error-line c))))))

(fiveam:test malformed-conditional-lines-signal
  (dolist (source '(".if
.endif" ".if 1, 2
.endif" ".if.w 1
.endif" ".if 1
.else 1
.endif" ".if 1
.endif 1"))
    (fiveam:signals lasm-syntax-error (assemble source :machine 'instr-test-machine))))

;;; Interaction with macros and includes

(fiveam:test macro-defined-in-a-taken-branch-is-usable
  (fiveam:is (equalp #(#xEA)
                     (assembly-cells (assemble ".if 1
.macro m
    nop
.endm
.endif
    m" :machine 'instr-test-machine)))))

(fiveam:test macro-defined-in-a-skipped-branch-is-not-defined
  (fiveam:signals unknown-instruction
    (assemble ".if 0
.macro m
    nop
.endm
.endif
    m" :machine 'instr-test-machine)))

(fiveam:test skipped-macro-body-is-not-interpreted
  (fiveam:is (equalp #(#xEA)
                     (assembly-cells (assemble ".if 0
.macro m a, a
.if
.endm
.endif
    nop" :machine 'instr-test-machine)))))

(fiveam:test same-macro-name-can-be-defined-in-each-branch
  (flet ((run (flag)
           (assembly-cells (assemble (format nil ".equ flag, ~D
.if flag
.macro m
    nop
.endm
.else
.macro m
    ldx #1
.endm
.endif
    m" flag) :machine 'instr-test-machine))))
    (fiveam:is (equalp #(#xEA) (run 1)))
    (fiveam:is (equalp #(#xA2 1) (run 0)))))

(fiveam:test invocation-in-a-skipped-branch-is-not-checked
  (fiveam:is (equalp #(#xEA)
                     (assembly-cells (assemble ".macro m a
    nop
.endm
.if 0
    m 1, 2, 3
.endif
    nop" :machine 'instr-test-machine)))))

(fiveam:test recursive-macro-terminates-on-an-if-base-case
  (fiveam:is (equalp #(#xEA #xEA #xEA)
                     (assembly-cells (assemble ".macro rep n
.if n > 0
    nop
    rep n-1
.endif
.endm
    rep 3" :machine 'instr-test-machine)))))

(fiveam:test unbounded-macro-recursion-signals-macro-error
  (fiveam:signals macro-error
    (assemble ".macro r
    r
.endm
    r" :machine 'instr-test-machine)))

(fiveam:test macro-body-must-balance-its-conditionals
  (fiveam:signals macro-error
    (assemble ".macro open
.if 1
.endm
    open
.endif" :machine 'instr-test-machine))
  (fiveam:signals macro-error
    (assemble ".macro close
.endif
.endm" :machine 'instr-test-machine)))

(fiveam:test macro-cannot-take-a-conditional-name
  (fiveam:signals macro-error
    (assemble ".macro .if
    nop
.endm" :machine 'instr-test-machine)))

(fiveam:test include-inside-false-if-contributes-nothing
  (let ((*include-directory* (asdf:system-relative-pathname :lasm "tests/fixtures/include/")))
    (fiveam:is (equal '(#xEA) (%cells ".if 0
.include \"sub/c.asm\"
.endif
    nop")))))

(fiveam:test include-defining-a-macro-inside-if-defines-it
  (let ((*include-directory* (asdf:system-relative-pathname :lasm "tests/fixtures/include/")))
    (fiveam:is (equalp #(#xEA)
                       (assembly-cells (assemble ".if 1
.include \"macro-def.asm\"
.endif
    nop" :machine 'instr-test-machine))))))

(fiveam:test include-in-a-skipped-branch-is-not-read
  (fiveam:is (equalp #(#xEA)
                     (assembly-cells (assemble ".if 0
.include \"does-not-exist.asm\"
.endif
    nop" :machine 'instr-test-machine)))))

(fiveam:test include-in-a-taken-branch-is-read
  (fiveam:signals include-error
    (assemble ".if 1
.include \"does-not-exist.asm\"
.endif" :machine 'instr-test-machine)))

(fiveam:test include-in-a-macro-body-resolves-against-the-macro-file
  (let ((*include-directory* (asdf:system-relative-pathname :lasm "tests/fixtures/include/")))
    (fiveam:is (equalp #(#xEA)
                       (assembly-cells (assemble ".include \"sub/lib.asm\"
    nopc" :machine 'instr-test-machine))))))

(fiveam:test included-file-cannot-leave-an-if-open
  (let ((*include-directory* (asdf:system-relative-pathname :lasm "tests/fixtures/include/")))
    (fiveam:signals conditional-error
      (assemble ".include \"open-if.asm\"
.endif" :machine 'instr-test-machine))))

;;; .ifdef, .ifndef and defined()

(defun %ifdef-cells (source)
  (assembly-cells (assemble source :machine 'instr-test-machine)))

(fiveam:test ifdef-tests-a-constant-defined-above
  (fiveam:is (equalp #(#xEA) (%ifdef-cells ".equ x, 1
.ifdef x
    nop
.endif")))
  (fiveam:is (equalp #() (%ifdef-cells ".ifdef x
    nop
.endif"))))

(fiveam:test ifndef-inverts-ifdef
  (fiveam:is (equalp #(#xEA) (%ifdef-cells ".ifndef x
    nop
.endif")))
  (fiveam:is (equalp #() (%ifdef-cells ".equ x, 1
.ifndef x
    nop
.endif"))))

(fiveam:test ifdef-supports-else-and-elseif
  (fiveam:is (equalp #(#xEA) (%ifdef-cells ".ifdef x
    ldx #1
.elseif 1
    nop
.else
    ldx #2
.endif"))))

(fiveam:test ifdef-sees-a-label-defined-above
  (fiveam:is (equalp #(#xEA #xEA) (%ifdef-cells "start: nop
.ifdef start
    nop
.endif"))))

(fiveam:test ifdef-does-not-see-a-name-defined-below
  (fiveam:is (equalp #(#xEA) (%ifdef-cells ".ifdef later
    ldx #1
.endif
later: nop"))))

(fiveam:test ifdef-qualifies-local-labels
  (fiveam:is (equalp #(#xEA #xEA) (%ifdef-cells "main: nop
.loop:
.ifdef .loop
    nop
.endif"))))

(fiveam:test ifdef-sees-set-and-assignment-names
  (fiveam:is (equalp #(#xEA #xEA) (%ifdef-cells ".set a, 1
b = 2
.ifdef a
    nop
.endif
.ifdef b
    nop
.endif"))))

(fiveam:test ifdef-requires-a-single-name
  (fiveam:signals conditional-error (%ifdef-cells ".ifdef
.endif"))
  (fiveam:signals conditional-error (%ifdef-cells ".ifdef 1
.endif"))
  (fiveam:signals conditional-error (%ifdef-cells ".ifdef a, b
.endif")))

(fiveam:test ifdef-block-balances-in-a-macro-body
  (fiveam:signals macro-error
    (%ifdef-cells ".macro open
.ifdef x
.endm")))

(fiveam:test defined-operator-in-a-condition
  (fiveam:is (equalp #(#xEA) (%ifdef-cells ".equ x, 3
.if defined(x) && x > 2
    nop
.endif
.if defined(y) && y > 2
    ldx #1
.endif"))))

(fiveam:test defined-guards-an-undefined-name-in-a-condition
  (fiveam:is (equalp #(#xEA) (%ifdef-cells ".if defined(y) && y
    ldx #1
.else
    nop
.endif"))))

(fiveam:test macro-name-cannot-be-a-conditional-keyword
  (fiveam:signals macro-error (%ifdef-cells ".macro .ifdef
.endm")))

(fiveam:test ifndef-include-guard-includes-a-file-once
  (let ((*include-directory* (asdf:system-relative-pathname :lasm "tests/fixtures/include/")))
    (fiveam:is (equalp #(#xEA)
                       (assembly-cells (assemble ".include \"guarded.asm\"
.include \"guarded.asm\"" :machine 'instr-test-machine))))))
