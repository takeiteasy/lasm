;;;; tests/instruction.lisp
;;;; fiveam tests for DEFINSTRUCTION and the addressing-mode/encoding
;;;; pipeline (instruction.lisp, mode.lisp).

(in-package #:lasm)

(fiveam:def-suite instruction :in lasm)
(fiveam:in-suite instruction)

;; A dedicated fixture (rather than reusing TEST-MACHINE from suites.lisp):
;; one memory element, so %DEFAULT-ADDRESS-WIDTH can resolve without
;; ambiguity, and a PC register per the "PC is a plain register" convention.
(defmachine instr-test-machine
  (register a :width 8)
  (register x :width 8)
  (register pc :width 16)
  (register v :width 8 :count 4)  ; #13: banked, for the operand-shadowing test below
  (memory ram :width 8 :addr-width 16)
  (flags z n c))

(definstruction instr-test-machine ldx
  (modes immediate)
  (encoding (opcode #xA2) (operand :mode))
  (semantics (set! x operand)))

(definstruction instr-test-machine adc
  (modes absolute)
  (encoding (opcode #x6D) (operand :mode))
  (semantics
    (let ((r (+ a (mref machine 'ram operand) c)))
      (set! a (wrap-value r 8))
      (set-flags! (c (> r 255)) (z (zero? a)) (n (bit-set? a 7))))))

(definstruction instr-test-machine bne
  (modes absolute)
  (encoding (opcode #xD0) (operand :mode))
  (semantics
    (when (zerop z)
      (set! pc operand))))

;; RELATIVE mode (#23): kept separate from BNE (which stays ABSOLUTE) so the
;; existing ABSOLUTE-mode BNE tests/byte expectations elsewhere in this file
;; and tests/assembler.lisp are undisturbed.
(definstruction instr-test-machine bra
  (modes relative)
  (encoding (opcode #x90) (operand :mode))
  (semantics (set! pc (+ pc operand))))

;; RELATIVE alongside a wider mode sharing the same bare-expr syntax -- the
;; case #31 needs relaxation's provisional address/symbol table for: a near
;; target should narrow to the 2-byte relative encoding, a far one should
;; widen to the 3-byte absolute encoding instead of always winning by default.
(definstruction instr-test-machine brx
  (modes
    (relative (opcode #x91) (semantics (set! pc (+ pc operand))))
    (absolute (opcode #x92) (semantics (set! pc operand)))))

(definstruction instr-test-machine nop
  (encoding (opcode #xEA))
  (semantics nil))

(definstruction instr-test-machine jmpfar
  (modes absolute)
  (encoding (opcode #x4C) (operand :width 3))
  (semantics (set! pc operand)))

;; Multi-mode: each mode gets its own opcode (deliberately not 6502's real
;; LDA opcodes, to avoid colliding with the other opcodes this fixture and
;; its tests use, including LDX's temporary #xA9 in
;; DEFINSTRUCTION-REDEFINITION-REPLACES below), and immediate/zero-page each
;; override the shared default semantics since only ABSOLUTE addresses
;; memory -- exercising the per-mode semantics override this ticket adds.
(definstruction instr-test-machine lda
  (modes
    (immediate (opcode #x10) (semantics (set! a operand)))
    (zero-page (opcode #x11))
    (absolute  (opcode #x12)))
  (semantics (set! a (mref machine 'ram operand))))

;; SIGNED, non-RELATIVE (#30) alongside a wider unsigned mode sharing the
;; same "#" expr syntax -- like BRX above but for %CHOOSE-VARIANT's signed
;; fit test instead of the RELATIVE one: a value in the unsigned-only range
;; (e.g. 200) must not fit the signed byte and should widen, while a negative
;; value should fit it.
(defmode instr-signed-imm-test-mode "#" expr :width 1 :signed t)
(defmode instr-wide-imm-test-mode "#" expr :width 2)

(definstruction instr-test-machine ldsi
  (modes
    (instr-signed-imm-test-mode (opcode #x93) (semantics (set! a operand)))
    (instr-wide-imm-test-mode (opcode #x94) (semantics (set! a operand)))))

;; A second fixture with two memory elements, so (operand :mode) on an
;; ABSOLUTE instruction is genuinely ambiguous -- exercises the "more than
;; one memory element" branch of %DEFAULT-ABSOLUTE-WIDTH. An explicit
;; :width sidesteps the ambiguity, as STA-RAM below demonstrates.
(defmachine multi-memory-machine
  (register a :width 8)
  (memory ram :width 8 :addr-width 16)
  (memory io :width 8 :addr-width 8))

(definstruction multi-memory-machine sta-ram
  (modes absolute)
  (encoding (opcode #x8D) (operand :width 2))
  (semantics (setf (mref machine 'ram operand) a)))

;;; DEFINSTRUCTION registration

(fiveam:test definstruction-registers-by-mnemonic-and-opcode
  (let ((ldx (find-instruction 'instr-test-machine 'ldx)))
    (fiveam:is (string= "LDX" (instruction-descriptor-name ldx)))
    (fiveam:is (eq 'immediate (mode-descriptor-name (instruction-descriptor-mode ldx))))
    (fiveam:is (= #xA2 (instruction-descriptor-opcode ldx)))
    (fiveam:is (eq ldx (find-instruction-by-opcode 'instr-test-machine #xA2)))))

(fiveam:test definstruction-redefinition-replaces
  (definstruction instr-test-machine ldx
    (modes immediate)
    (encoding (opcode #xA9) (operand :mode))
    (semantics (set! x operand)))
  (let ((ldx (find-instruction 'instr-test-machine 'ldx)))
    (fiveam:is (= #xA9 (instruction-descriptor-opcode ldx)))
    (fiveam:is (eq ldx (find-instruction-by-opcode 'instr-test-machine #xA9)))
    ;; the old opcode (#xA2) must no longer resolve to anything -- it was
    ;; superseded by this redefinition, not left dangling in the opcode table
    (fiveam:signals unknown-instruction (find-instruction-by-opcode 'instr-test-machine #xA2)))
  ;; restore for other tests in this file
  (definstruction instr-test-machine ldx
    (modes immediate)
    (encoding (opcode #xA2) (operand :mode))
    (semantics (set! x operand))))

;; #26: a different mnemonic claiming an already-registered opcode must
;; error rather than silently clobbering the earlier mnemonic's table entry.
(fiveam:test definstruction-opcode-conflict-with-other-mnemonic-signals-error
  (fiveam:signals opcode-conflict
    (definstruction instr-test-machine ldx-conflict
      (modes immediate)
      (encoding (opcode #xA2) (operand :mode))
      (semantics (set! x operand))))
  ;; the conflicting registration must not have gone through -- LDX (#xA2)
  ;; still resolves to itself, not to the rejected LDX-CONFLICT descriptor
  (let ((ldx (find-instruction 'instr-test-machine 'ldx)))
    (fiveam:is (string= "LDX" (instruction-descriptor-name ldx)))
    (fiveam:is (eq ldx (find-instruction-by-opcode 'instr-test-machine #xA2)))))

(fiveam:test no-operand-instruction
  (let ((nop (find-instruction 'instr-test-machine 'nop)))
    (fiveam:is (null (instruction-descriptor-mode nop)))
    (fiveam:is (null (instruction-descriptor-operand-widths nop)))))

;;; Multi-mode registration (mode.lisp, #18)

(fiveam:test multi-mode-registers-one-descriptor-per-mode
  (let ((variants (find-instruction-variants 'instr-test-machine 'lda)))
    (fiveam:is (= 3 (length variants)))
    (fiveam:is (equal '(#x10 #x11 #x12) (mapcar #'instruction-descriptor-opcode variants)))
    (fiveam:is (equal '(immediate zero-page absolute)
                       (mapcar (lambda (d) (mode-descriptor-name (instruction-descriptor-mode d)))
                               variants)))))

(fiveam:test multi-mode-find-instruction-by-mode
  (let ((imm (find-instruction 'instr-test-machine 'lda :mode 'immediate))
        (abs (find-instruction 'instr-test-machine 'lda :mode 'absolute)))
    (fiveam:is (= #x10 (instruction-descriptor-opcode imm)))
    (fiveam:is (= #x12 (instruction-descriptor-opcode abs)))))

(fiveam:test multi-mode-each-opcode-decodes-independently
  (fiveam:is (eq (find-instruction 'instr-test-machine 'lda :mode 'immediate)
                  (find-instruction-by-opcode 'instr-test-machine #x10)))
  (fiveam:is (eq (find-instruction 'instr-test-machine 'lda :mode 'absolute)
                  (find-instruction-by-opcode 'instr-test-machine #x12))))

(fiveam:test multi-mode-per-mode-semantics-override
  (let ((m (make-machine 'instr-test-machine))
        (imm (find-instruction 'instr-test-machine 'lda :mode 'immediate)))
    (execute-instruction imm m (list 42))
    (fiveam:is (= 42 (sref m 'a)))))

(fiveam:test multi-mode-shared-default-semantics
  (let ((m (make-machine 'instr-test-machine))
        (abs (find-instruction 'instr-test-machine 'lda :mode 'absolute)))
    (setf (mref m 'ram #x1000) 7)
    (execute-instruction abs m (list #x1000))
    (fiveam:is (= 7 (sref m 'a)))))

(fiveam:test multi-mode-redefinition-retires-dropped-opcode
  (definstruction instr-test-machine redef-multi
    (modes
      (immediate (opcode #xF0) (semantics nil))
      (absolute (opcode #xF1) (semantics nil)))
    (semantics nil))
  (fiveam:is (= 2 (length (find-instruction-variants 'instr-test-machine 'redef-multi))))
  ;; drop the ABSOLUTE variant on redefinition -- its old opcode (#xF1) must
  ;; no longer resolve
  (definstruction instr-test-machine redef-multi
    (modes immediate)
    (encoding (opcode #xF0) (operand :mode))
    (semantics nil))
  (fiveam:is (= 1 (length (find-instruction-variants 'instr-test-machine 'redef-multi))))
  (fiveam:signals unknown-instruction (find-instruction-by-opcode 'instr-test-machine #xF1)))

;;; Clause errors

(fiveam:test missing-encoding-clause-signals-error
  (fiveam:signals error
    (eval '(definstruction instr-test-machine bogus
             (modes immediate)
             (semantics (set! x operand))))))

(fiveam:test missing-semantics-clause-signals-error
  (fiveam:signals error
    (eval '(definstruction instr-test-machine bogus
             (modes immediate)
             (encoding (opcode #xFF) (operand :mode))))))

(fiveam:test multiple-bare-mode-symbols-signals-error
  ;; more than one bare mode symbol requires the multi-mode list form
  (fiveam:signals error
    (eval '(definstruction instr-test-machine bogus
             (modes immediate absolute)
             (encoding (opcode #xFF) (operand :mode))
             (semantics (set! x operand))))))

(fiveam:test multi-mode-with-top-level-encoding-signals-error
  (fiveam:signals error
    (eval '(definstruction instr-test-machine bogus
             (modes (immediate (opcode #xF0)) (absolute (opcode #xF1)))
             (encoding (opcode #xFF))
             (semantics nil)))))

(fiveam:test multi-mode-single-variant-signals-error
  (fiveam:signals error
    (eval '(definstruction instr-test-machine bogus
             (modes (immediate (opcode #xF0)))
             (semantics nil)))))

(fiveam:test multi-mode-variant-without-opcode-signals-error
  (fiveam:signals error
    (eval '(definstruction instr-test-machine bogus
             (modes (immediate (semantics nil)) (absolute (opcode #xF1)))
             (semantics nil)))))

(fiveam:test multi-mode-variant-without-semantics-or-default-signals-error
  (fiveam:signals error
    (eval '(definstruction instr-test-machine bogus
             (modes (immediate (opcode #xF0)) (absolute (opcode #xF1)))))))

(defmode two-hole-test-mode expr "," expr :width 1)

;;; Multi-operand instructions: a two-hole mode wires up one operand
;;; encoding field per hole, named or not.

(definstruction instr-test-machine movi
  (modes two-hole-test-mode)
  (encoding (opcode #xF8) (operand dst :width 1) (operand src :width 2))
  (semantics (setf (mref machine 'ram dst) src)))

(fiveam:test multi-operand-instruction-registers-one-width-per-hole
  (let ((movi (find-instruction 'instr-test-machine 'movi)))
    (fiveam:is (equal '(1 2) (instruction-descriptor-operand-widths movi)))
    (fiveam:is (equal '(dst src) (instruction-descriptor-operand-names movi)))
    (fiveam:is (= 3 (instruction-descriptor-total-operand-width movi)))))

(fiveam:test multi-operand-instruction-too-few-operand-subclauses-signals-error
  (fiveam:signals error
    (eval '(definstruction instr-test-machine bogus
             (modes two-hole-test-mode)
             (encoding (opcode #xFF) (operand :width 1))
             (semantics nil)))))

(fiveam:test multi-operand-instruction-too-many-operand-subclauses-signals-error
  (fiveam:signals error
    (eval '(definstruction instr-test-machine bogus
             (modes immediate)
             (encoding (opcode #xFF) (operand :width 1) (operand :width 1))
             (semantics nil)))))

(fiveam:test multi-operand-instruction-with-no-operand-subclause-in-multi-mode-form-signals-error
  ;; the multi-mode form's operand-subclause default only applies to a
  ;; single-hole mode -- a two-hole mode has no single width to default to
  (fiveam:signals error
    (eval '(definstruction instr-test-machine bogus
             (modes (two-hole-test-mode (opcode #xFF)) (absolute (opcode #xFE)))
             (semantics nil)))))

(fiveam:test multi-operand-instruction-encodes-each-field-little-endian
  (let ((movi (find-instruction 'instr-test-machine 'movi)))
    ;; distinct bytes at every position pin per-field width and ordering,
    ;; not just total size
    (fiveam:is (equal (list #xF8 #x11 #x22 #x33) (encode-instruction movi (list #x11 #x3322))))))

(fiveam:test multi-operand-instruction-binds-named-slots-in-semantics
  (let ((m (make-machine 'instr-test-machine))
        (movi (find-instruction 'instr-test-machine 'movi)))
    (execute-instruction movi m (list #x10 #xAB))
    (fiveam:is (= #xAB (mref m 'ram #x10)))))

(fiveam:test multi-operand-instruction-duplicate-operand-name-signals-error
  (fiveam:signals error
    (eval '(definstruction instr-test-machine bogus
             (modes two-hole-test-mode)
             (encoding (opcode #xFF) (operand n :width 1) (operand n :width 1))
             (semantics nil)))))

(fiveam:test multi-operand-instruction-operand-name-shadowing-register-signals-error
  ;; INSTR-TEST-MACHINE declares a scalar register named A -- naming an
  ;; operand field the same would leave (semantics ...) unable to see one of
  ;; them, so this is rejected at DEFINSTRUCTION time rather than silently
  ;; shadowing the register.
  (fiveam:signals error
    (eval '(definstruction instr-test-machine bogus
             (modes two-hole-test-mode)
             (encoding (opcode #xFF) (operand a :width 1) (operand :width 1))
             (semantics nil)))))

(fiveam:test multi-operand-instruction-operand-name-shadowing-banked-register-signals-error
  ;; #13: INSTR-TEST-MACHINE's V is a banked (:count 4) register, bound by
  ;; WITH-MACHINE-BINDINGS as a MACROLET rather than a symbol-macro -- an
  ;; operand named V would shadow it exactly as silently as a scalar
  ;; register would, so %SCALAR-BINDABLE-NAMES must reject it too.
  (fiveam:signals error
    (eval '(definstruction instr-test-machine bogus
             (modes two-hole-test-mode)
             (encoding (opcode #xFF) (operand v :width 1) (operand :width 1))
             (semantics nil)))))

(fiveam:test multi-operand-instruction-operand-name-shadowing-flag-signals-error
  (fiveam:signals error
    (eval '(definstruction instr-test-machine bogus
             (modes two-hole-test-mode)
             (encoding (opcode #xFF) (operand z :width 1) (operand :width 1))
             (semantics nil)))))

;;; ONE-OF (#103): a mode's hole count still comes from the pattern, whether
;;; a hole is a plain EXPR or a ONE-OF alternation -- DEFINSTRUCTION requires
;;; one (operand ...) subclause per hole exactly as for any other mode.

(defmode oo-instr-reg expr)
(defmode oo-instr-ind "[" expr "]")
(defmode oo-instr-two (one-of oo-instr-reg oo-instr-ind) "," (one-of oo-instr-reg oo-instr-ind))

(definstruction instr-test-machine moo
  (modes oo-instr-two)
  (encoding (opcode #xF7) (operand dst :width 1) (operand src :width 1))
  (semantics (setf (mref machine 'ram dst) src)))

(fiveam:test one-of-mode-registers-one-width-per-hole
  (let ((moo (find-instruction 'instr-test-machine 'moo)))
    (fiveam:is (equal '(1 1) (instruction-descriptor-operand-widths moo)))
    (fiveam:is (equal '(dst src) (instruction-descriptor-operand-names moo)))))

(fiveam:test one-of-mode-too-few-operand-subclauses-signals-error
  (fiveam:signals error
    (eval '(definstruction instr-test-machine bogus
             (modes oo-instr-two)
             (encoding (opcode #xFF) (operand :width 1))
             (semantics nil)))))

(fiveam:test multi-operand-instruction-named-operand-not-shadowed-by-register
  ;; The reverse of the two error tests above, but with a name that does NOT
  ;; collide (SRC is not a storage element on INSTR-TEST-MACHINE) -- proves
  ;; %SEMANTICS-FN-FORM's LET is nested inside WITH-MACHINE-BINDINGS so a
  ;; named operand field actually wins visibility, not just that the
  ;; colliding case is rejected.
  (let ((m (make-machine 'instr-test-machine))
        (movi (find-instruction 'instr-test-machine 'movi)))
    (execute-instruction movi m (list #x22 #x77))
    (fiveam:is (= #x77 (mref m 'ram #x22)))))

;; FLEX: one mnemonic with variants of *differing* hole counts -- exercises
;; %CHOOSE-VARIANT (assembler.lisp) picking correctly among candidates whose
;; OPERAND-WIDTHS lists aren't even the same length.
(definstruction instr-test-machine flex
  (modes
    (immediate (opcode #x20) (semantics (set! a operand)))
    (two-hole-test-mode (opcode #x21) (operand :width 1) (operand :width 1)
                         (semantics nil))))

(fiveam:test multi-operand-variant-alongside-single-operand-variant
  (let ((variants (find-instruction-variants 'instr-test-machine 'flex)))
    (fiveam:is (equal '((1) (1 1)) (mapcar #'instruction-descriptor-operand-widths variants)))))

(defmode relative-two-hole-test-mode expr "," expr :width 1 :relative t)

(fiveam:test relative-mode-with-more-than-one-hole-signals-error
  (fiveam:signals error
    (eval '(definstruction instr-test-machine bogus
             (modes relative-two-hole-test-mode)
             (encoding (opcode #xFF) (operand :width 1) (operand :width 1))
             (semantics nil)))))

(fiveam:test unknown-clause-head-signals-error
  (fiveam:signals error
    (eval '(definstruction instr-test-machine bogus
             (bogus-clause 1)
             (encoding (opcode #xFF))
             (semantics nil)))))

(fiveam:test mode-without-operand-subclause-signals-error
  (fiveam:signals error
    (eval '(definstruction instr-test-machine bogus
             (modes immediate)
             (encoding (opcode #xFF))
             (semantics nil)))))

(fiveam:test operand-subclause-without-mode-signals-error
  (fiveam:signals error
    (eval '(definstruction instr-test-machine bogus
             (encoding (opcode #xFF) (operand :mode))
             (semantics nil)))))

(definstruction instr-test-machine bogus-ref
  (modes immediate)
  (encoding (opcode #xFE) (operand :mode))
  (semantics (mref machine 'nonexistent-ram operand)))

(fiveam:test unknown-storage-reference-in-semantics-signals-at-runtime
  (let ((m (make-machine 'instr-test-machine))
        (bogus-ref (find-instruction 'instr-test-machine 'bogus-ref)))
    (fiveam:signals unknown-storage (execute-instruction bogus-ref m (list 10)))))

;;; match-operand-mode is now mode.lisp's territory -- see tests/mode.lisp
;;; for pattern-matching coverage (immediate/absolute/indexed-x/indirect-y,
;;; case-insensitive literals, trailing-token rejection, TRY-MATCH-OPERAND-
;;; MODE). This file keeps just enough of it to build ASTs for the
;;; EVAL-EXPR-CONSTANT tests below.

(defun %single-operand (string)
  (statement-operand-tokens (first (parse (format nil "nop ~A" string)))))

;;; eval-expr-constant

(fiveam:test eval-expr-constant-arithmetic
  (fiveam:is (= 7 (eval-expr-constant (match-operand-mode (%single-operand "#(3+4)") 'immediate)))))

(fiveam:test eval-expr-constant-lo-hi
  (fiveam:is (= #x34 (eval-expr-constant (match-operand-mode (%single-operand "#<$1234") 'immediate))))
  (fiveam:is (= #x12 (eval-expr-constant (match-operand-mode (%single-operand "#>$1234") 'immediate)))))

(fiveam:test eval-expr-constant-unresolved-label-signals
  (fiveam:signals unresolved-label
    (eval-expr-constant (match-operand-mode (%single-operand "loop") 'absolute))))

;;; Location counter ("*", #15)

(fiveam:test eval-expr-location-resolves-against-pc
  (fiveam:is (= 16 (eval-expr-constant (match-operand-mode (%single-operand "*") 'absolute) :pc 16))))

(fiveam:test eval-expr-location-plus-offset
  (fiveam:is (= 18 (eval-expr-constant (match-operand-mode (%single-operand "*+2") 'absolute) :pc 16))))

(fiveam:test eval-expr-location-with-no-pc-signals-unresolved-location
  (fiveam:signals unresolved-location
    (eval-expr-constant (match-operand-mode (%single-operand "*") 'absolute))))

;;; encode-instruction

(fiveam:test encode-immediate-instruction
  (let ((ldx (find-instruction 'instr-test-machine 'ldx)))
    (fiveam:is (equal (list #xA2 10) (encode-instruction ldx (list 10))))))

(fiveam:test encode-absolute-instruction-little-endian
  (let ((adc (find-instruction 'instr-test-machine 'adc)))
    (fiveam:is (equal (list #x6D #x00 #x10) (encode-instruction adc (list #x1000))))))

(fiveam:test encode-instruction-masks-overwide-value
  (let ((ldx (find-instruction 'instr-test-machine 'ldx)))
    (fiveam:is (equal (list #xA2 #x2C) (encode-instruction ldx (list 300))))))

(fiveam:test encode-no-operand-instruction
  (let ((nop (find-instruction 'instr-test-machine 'nop)))
    (fiveam:is (equal (list #xEA) (encode-instruction nop nil)))))

(fiveam:test explicit-operand-width-overrides-mode-default
  (let ((jmpfar (find-instruction 'instr-test-machine 'jmpfar)))
    (fiveam:is (equal '(3) (instruction-descriptor-operand-widths jmpfar)))
    ;; distinct bytes in every position pin little-endian ordering, not just width
    (fiveam:is (equal (list #x4C #x56 #x34 #x12) (encode-instruction jmpfar (list #x123456))))))

(fiveam:test ambiguous-memory-element-requires-explicit-width
  (fiveam:signals error
    (eval '(definstruction multi-memory-machine bogus
             (modes absolute)
             (encoding (opcode #xFF) (operand :mode))
             (semantics (set! a operand)))))
  (fiveam:is (equal '(2) (instruction-descriptor-operand-widths
                          (find-instruction 'multi-memory-machine 'sta-ram)))))

;;; execute-instruction

(fiveam:test execute-immediate-sets-register
  (let ((m (make-machine 'instr-test-machine))
        (ldx (find-instruction 'instr-test-machine 'ldx)))
    (execute-instruction ldx m (list 10))
    (fiveam:is (= 10 (sref m 'x)))))

(fiveam:test execute-absolute-reads-memory-and-sets-flags
  (let ((m (make-machine 'instr-test-machine))
        (adc (find-instruction 'instr-test-machine 'adc)))
    (setf (sref m 'a) 200)
    (setf (mref m 'ram #x1000) 100)
    (execute-instruction adc m (list #x1000))
    (fiveam:is (= 44 (sref m 'a)))          ; 200 + 100 wraps mod 256
    (fiveam:is (= 1 (flag m 'c)))
    (fiveam:is (= 0 (flag m 'z)))))

(fiveam:test execute-branch-sets-pc-conditionally
  (let ((m (make-machine 'instr-test-machine))
        (bne (find-instruction 'instr-test-machine 'bne)))
    (setf (flag m 'z) t)
    (execute-instruction bne m (list #x2000))
    (fiveam:is (= 0 (sref m 'pc)))          ; branch not taken
    (setf (flag m 'z) nil)
    (execute-instruction bne m (list #x2000))
    (fiveam:is (= #x2000 (sref m 'pc)))))   ; branch taken

;;; Word-encoded instructions (#20, M4) -- bitfield/variant operand encoding
;;; on a machine declaring an (instruction-word ...) layout, DCPU-16-shaped.
;;; 16-bit word: a 4-bit OPCODE field, a 2-bit DST field, and a 10-bit SRC
;;; field an operand can pack into inline (a small biased range) or escape
;;; out of into its own following word.

(defmachine word-test-machine
  (register pc :width 16)
  (register a :width 16)
  (register b :width 16)
  (memory ram :width 8 :addr-width 16)
  (instruction-word :width 16
    (field opcode 4)
    (field dst 2)
    (field src 10)))

(defmode word-imm "#" expr)
(defmode word-abs expr)

(definstruction word-test-machine set
  (modes word-imm)
  (encoding
    (opcode 1)
    (operand value :field src
      (variant (range -1 30) inline :bias 1)
      (variant :else (extra-word :escape #x3ff))))
  (semantics (set! a operand)))

(definstruction word-test-machine hlt
  (encoding (opcode 2))
  (semantics (trap :halt)))

;;; instruction-word layout parsing (machine.lisp)

(fiveam:test instruction-word-clause-requires-width
  (fiveam:signals error
    (eval '(defmachine bogus-word-machine
             (instruction-word (field opcode 4))))))

(fiveam:test instruction-word-clause-requires-whole-cell-width
  ;; A MEMORY element is required here so the check this exercises --
  ;; %FINISH-INSTRUCTION-WORD-LAYOUT's (mod width cell-width) test
  ;; (machine.lisp) -- is actually reached; a machine with no memory element
  ;; fails earlier with a different error ("no memory element declared",
  ;; %MACHINE-CELL-WIDTH), which FIVEAM:SIGNALS ERROR can't tell apart from
  ;; the intended failure.
  (fiveam:signals error
    (eval '(defmachine bogus-word-machine
             (memory ram :width 8 :addr-width 8)
             (instruction-word :width 12 (field opcode 12))))))

(fiveam:test instruction-word-clause-requires-whole-cell-width-non-8-bit-cell
  ;; Same check, on a machine whose cell width isn't 8 -- :width 24 doesn't
  ;; divide evenly by a 16-bit cell (24 mod 16 = 8), so this must still
  ;; signal rather than only ever checking against a hardcoded 8.
  (fiveam:signals error
    (eval '(defmachine bogus-word-machine-16
             (memory ram :width 16 :addr-width 8 :cell-width 16)
             (instruction-word :width 24 (field opcode 24))))))

(fiveam:test instruction-word-clause-requires-opcode-field
  (fiveam:signals error
    (eval '(defmachine bogus-word-machine
             (instruction-word :width 16 (field a 16))))))

(fiveam:test instruction-word-clause-rejects-duplicate-field-names
  (fiveam:signals error
    (eval '(defmachine bogus-word-machine
             (instruction-word :width 16 (field opcode 8) (field opcode 8))))))

(fiveam:test instruction-word-clause-field-widths-must-sum-to-word-width
  (fiveam:signals error
    (eval '(defmachine bogus-word-machine
             (instruction-word :width 16 (field opcode 4) (field a 4))))))

(fiveam:test instruction-word-clause-fields-are-msb-first
  (let ((layout (machine-descriptor-instruction-word (find-machine-descriptor 'word-test-machine))))
    (fiveam:is (= 16 (instruction-word-layout-width layout)))
    (fiveam:is (= 2 (instruction-word-layout-width-cells layout)))
    (fiveam:is (equal '(opcode 4 12) (instruction-word-field layout 'opcode)))
    (fiveam:is (equal '(dst 2 10) (instruction-word-field layout 'dst)))
    (fiveam:is (equal '(src 10 0) (instruction-word-field layout 'src)))))

;;; Variant expansion / registration

(fiveam:test word-instruction-expands-into-one-descriptor-per-variant
  (let ((variants (find-instruction-variants 'word-test-machine "SET")))
    (fiveam:is (= 2 (length variants)))
    (fiveam:is (equal '(0 1) (mapcar #'instruction-descriptor-extra-words variants)))
    (fiveam:is (every (lambda (d) (= 1 (instruction-descriptor-opcode d))) variants))))

(fiveam:test word-instruction-no-operand-descriptor
  (let ((hlt (find-instruction 'word-test-machine 'hlt)))
    (fiveam:is (null (instruction-descriptor-word-fields hlt)))
    (fiveam:is (= 0 (instruction-descriptor-extra-words hlt)))))

;;; Decodability checks (#20's own ambiguity, %CHECK-WORD-VARIANTS)

(fiveam:test word-variant-inline-range-overflowing-field-signals-error
  (fiveam:signals error
    (eval '(definstruction word-test-machine bogus
             (modes word-imm)
             (encoding (opcode 3)
                       (operand value :field src
                         (variant (range 0 2000) inline)
                         (variant :else (extra-word :escape #x3ff))))
             (semantics nil)))))

(fiveam:test word-variant-escape-overflowing-field-signals-error
  (fiveam:signals error
    (eval '(definstruction word-test-machine bogus
             (modes word-imm)
             (encoding (opcode 3)
                       (operand value :field src
                         (variant (range 0 10) inline)
                         (variant :else (extra-word :escape 2000))))
             (semantics nil)))))

(fiveam:test word-variant-escape-colliding-with-inline-range-signals-error
  (fiveam:signals error
    (eval '(definstruction word-test-machine bogus
             (modes word-imm)
             (encoding (opcode 3)
                       (operand value :field src
                         (variant (range 0 30) inline)
                         (variant :else (extra-word :escape 30))))
             (semantics nil)))))

;;; CHOICE-selected word fields (#104) -- syntax-, not value-, selected
;;; encoding and unconditional extra words, keyed by which ONE-OF
;;; alternative an operand hole actually matched (mode.lisp, #103's CHOICES,
;;; hole-aligned by #104).

(defmode wc-reg expr)
(defmode wc-ind "[" expr "]")
(defmode wc-mem "(" expr ")")
(defmode wc-two (one-of wc-reg wc-ind))
;; #118: a third alternative, used only by the multiple-unclaimed-alternative
;; error test below (choice-and-value-selected-mix-multiple-unclaimed-signals-error).
(defmode wc-three (one-of wc-reg wc-ind wc-mem))

(definstruction word-test-machine wcx
  (modes wc-two)
  (encoding
    (opcode 4)
    (operand value :field src
      (variant (choice wc-reg) inline :range (0 7) :bias #x00)
      (variant (choice wc-ind) inline :range (0 7) :bias #x08)))
  (semantics (set! a operand)))

(fiveam:test choice-selected-field-parses-into-word-field-choice
  ;; Two CHOICE-selected variants on one field -> two sibling descriptors,
  ;; one per combo, each carrying its own variant's :CHOICE (instruction.lisp)
  ;; on its own WORD-FIELDS entry.
  (let* ((variants (find-instruction-variants 'word-test-machine "WCX"))
         (choices (sort (mapcar (lambda (d) (word-field-choice-choice (first (instruction-descriptor-word-fields d))))
                                 variants)
                         #'string< :key #'symbol-name)))
    (fiveam:is (= 2 (length variants)))
    (fiveam:is (equal '(wc-ind wc-reg) choices))))

(fiveam:test choice-inline-without-range-signals-error
  (fiveam:signals error
    (eval '(definstruction word-test-machine bogus
             (modes wc-two)
             (encoding (opcode 5)
                       (operand value :field src
                         (variant (choice wc-reg) inline :bias 3)
                         (variant (choice wc-ind) inline :range (0 7))))
             (semantics nil)))))

(fiveam:test choice-naming-unregistered-mode-signals-error
  (fiveam:signals error
    (eval '(definstruction word-test-machine bogus
             (modes wc-two)
             (encoding (opcode 5)
                       (operand value :field src
                         (variant (choice no-such-mode-at-all) inline :range (0 7))
                         (variant (choice wc-ind) inline :range (8 15))))
             (semantics nil)))))

(fiveam:test choice-on-non-one-of-hole-signals-error
  ;; WORD-IMM's single hole is a plain EXPR, not a ONE-OF -- CHOICE only
  ;; selects between ONE-OF alternatives.
  (fiveam:signals error
    (eval '(definstruction word-test-machine bogus
             (modes word-imm)
             (encoding (opcode 5)
                       (operand value :field src
                         (variant (choice wc-reg) inline :range (0 7))
                         (variant :else (extra-word :escape #x3ff))))
             (semantics nil)))))

(fiveam:test choice-not-among-hole-alternatives-signals-error
  ;; WORD-ABS is a registered mode, so FIND-MODE-DESCRIPTOR alone wouldn't
  ;; catch this -- it just isn't one of WC-TWO's own ONE-OF alternatives.
  (fiveam:signals error
    (eval '(definstruction word-test-machine bogus
             (modes wc-two)
             (encoding (opcode 5)
                       (operand value :field src
                         (variant (choice word-abs) inline :range (0 7))
                         (variant (choice wc-ind) inline :range (8 15))))
             (semantics nil)))))

(fiveam:test choice-and-value-selected-variants-may-mix
  ;; #118: WC-TWO's only unclaimed alternative (WC-REG is claimed by the
  ;; CHOICE-selected variant) is WC-IND -- the value-selected (RANGE 8 15)
  ;; variant is stamped with it, rather than signalling the old no-mixing
  ;; error.
  (eval '(definstruction word-test-machine bogus
           (modes wc-two)
           (encoding (opcode 5)
                     (operand value :field src
                       (variant (choice wc-reg) inline :range (0 7))
                       (variant (range 8 15) inline)))
           (semantics nil)))
  (let* ((variants (find-instruction-variants 'word-test-machine "BOGUS"))
         (choices (sort (mapcar (lambda (d) (word-field-choice-choice (first (instruction-descriptor-word-fields d))))
                                 variants)
                         #'string< :key #'symbol-name)))
    (fiveam:is (= 2 (length variants)))
    (fiveam:is (equal '(wc-ind wc-reg) choices))))

(fiveam:test choice-and-value-selected-mix-every-alternative-claimed-signals-error
  ;; WC-TWO has only two alternatives (WC-REG, WC-IND); claiming both with
  ;; CHOICE-selected variants leaves no unclaimed alternative for the
  ;; value-selected (:ELSE) one to be stamped with.
  (fiveam:signals error
    (eval '(definstruction word-test-machine bogus
             (modes wc-two)
             (encoding (opcode 5)
                       (operand value :field src
                         (variant (choice wc-reg) inline :range (0 7))
                         (variant (choice wc-ind) inline :range (8 15))
                         (variant :else (extra-word :escape #x3ff))))
             (semantics nil)))))

(fiveam:test choice-and-value-selected-mix-multiple-unclaimed-signals-error
  ;; WC-THREE (below) has three alternatives; claiming only one with a
  ;; CHOICE-selected variant leaves two unclaimed for the value-selected
  ;; variants -- nothing tells decode which of the two they belong to.
  (fiveam:signals error
    (eval '(definstruction word-test-machine bogus
             (modes wc-three)
             (encoding (opcode 5)
                       (operand value :field src
                         (variant (choice wc-reg) inline :range (0 7))
                         (variant (range 8 15) inline)
                         (variant :else (extra-word :escape #x3ff))))
             (semantics nil)))))

(fiveam:test choice-overlapping-inline-ranges-signal-error
  (fiveam:signals error
    (eval '(definstruction word-test-machine bogus
             (modes wc-two)
             (encoding (opcode 5)
                       (operand value :field src
                         (variant (choice wc-reg) inline :range (0 7))
                         (variant (choice wc-ind) inline :range (4 11))))
             (semantics nil)))))

;; WCXS (#104): a forced-suffix (#40) single-mode instruction whose one mode
;; contains a ONE-OF -- %EXPAND-WORD-COMBOS (instruction.lisp) still expands
;; it into several sibling descriptors (one per field variant) sharing this
;; one mode's name, exactly the shape %CHOOSE-FORCED-VARIANT's own
;; same-mode-name FIND has to pick correctly among by CHOICE eligibility,
;; not just grab the first. See tests/assembler.lisp's own use of this.
(defmode wc-two-forced (one-of wc-reg wc-ind) :suffix "c")

(definstruction word-test-machine wcxs
  (modes wc-two-forced)
  (encoding
    (opcode 7)
    (operand value :field src
      (variant (choice wc-reg) inline :range (0 7) :bias #x00)
      (variant (choice wc-ind) inline :range (0 7) :bias #x08)))
  (semantics (set! a value)))

;; WCXW (#104): a CHOICE-selected field whose second alternative is an
;; *unconditional* extra word -- WC-IND syntax always spills its value into
;; its own following word, regardless of what that value is (unlike an
;; :ELSE fallback, which only escapes a value-selected field's inline range
;; when the value doesn't fit). Used by tests/emulator.lisp's round-trip.
(definstruction word-test-machine wcxw
  (modes wc-two)
  (encoding
    (opcode 6)
    (operand value :field src
      (variant (choice wc-reg) inline :range (0 7) :bias #x00)
      (variant (choice wc-ind) (extra-word :escape #x3ff))))
  (semantics (set! b operand)))

(fiveam:test choice-duplicate-escapes-signal-error
  (fiveam:signals error
    (eval '(definstruction word-test-machine bogus
             (modes wc-two)
             (encoding (opcode 5)
                       (operand value :field src
                         (variant (choice wc-reg) (extra-word :escape #x3ff))
                         (variant (choice wc-ind) (extra-word :escape #x3ff))))
             (semantics nil)))))

;;; CHOICE-CASE semantics dispatch (#73) -- reading back which ONE-OF
;;; alternative a hole actually matched from inside (semantics ...), rather
;;; than every CHOICE-selected sibling sharing one runtime effect.

;; WCC's single operand dispatches to a different register depending on
;; whether WC-REG or WC-IND matched, despite both packing the identical
;; value into disjoint halves of one field -- the differentiated-execution
;; case #104 alone could not express (see tests/emulator.lisp's own
;; STEP-MACHINE-CHOICE-CASE-... test for this run end to end).
(definstruction word-test-machine wcc
  (modes wc-two)
  (encoding
    (opcode 8)
    (operand value :field src
      (variant (choice wc-reg) inline :range (0 7) :bias #x00)
      (variant (choice wc-ind) inline :range (0 7) :bias #x08)))
  (semantics
    (choice-case value
      (wc-reg (set! a value))
      (wc-ind (set! b value)))))

;; WCCM: a second operand (DST, a plain value-selected field with no variant
;; forms of its own) alongside SRC's CHOICE-selected one -- pins down that
;; DECODE-INSTRUCTION-AT's per-hole CHOICES stays positionally aligned with
;; its per-hole VALUES: DST's own hole always decodes a NIL choice, SRC's
;; names whichever alternative was actually written.
(defmode wccm-mode expr "," (one-of wc-reg wc-ind))

(definstruction word-test-machine wccm
  (modes wccm-mode)
  (encoding
    (opcode 9)
    (operand dst :field dst)
    (operand src :field src
      (variant (choice wc-reg) inline :range (0 7) :bias #x00)
      (variant (choice wc-ind) inline :range (0 7) :bias #x08)))
  (semantics (set! a dst)))

;; WCM (#118): a *mixed* field -- WC-REG is CHOICE-selected, but WC-TWO's
;; other alternative (WC-IND) has no (choice ...) variant of its own at all;
;; %CHECK-WORD-VARIANT-CHOICES! stamps the value-selected (RANGE 8 100)
;; variant with WC-IND, the one ONE-OF alternative no CHOICE-selected variant
;; here claims. WC-REG's own range (0 7) still packs by CHOICE (syntax
;; alone); WC-IND's range (8 100) now packs by the *value* actually written,
;; scoped to WC-IND's own syntax the same way a CHOICE-selected variant
;; would be. CHOICE-CASE dispatches on both names, exactly as if WC-IND's
;; row were CHOICE-selected too.
;;
;; A dedicated tiny machine, not WORD-TEST-MACHINE, for the same reason
;; MIXED-KIND-TEST-MACHINE (below) is: WORD-TEST-MACHINE's own 4-bit (0-15)
;; opcode space has opcodes 1-14 already claimed and 15 reserved unregistered
;; (tests/emulator.lisp's STEP-MACHINE-WORD-ENCODED-DECODE-FAILURE-ON-
;; UNREGISTERED-OPCODE), leaving no room.
(defmachine mixed-field-test-machine
  (register pc :width 16)
  (register a :width 16)
  (register b :width 16)
  (memory ram :width 8 :addr-width 16)
  (instruction-word :width 16
    (field opcode 4)
    (field dst 2)
    (field src 10)))

(definstruction mixed-field-test-machine wcm
  (modes wc-two)
  (encoding
    (opcode 1)
    (operand value :field src
      (variant (choice wc-reg) inline :range (0 7))
      (variant (range 8 100) inline)))
  (semantics
    (choice-case value
      (wc-reg (set! a value))
      (wc-ind (set! b value)))))

(definstruction mixed-field-test-machine hlt
  (encoding (opcode 2))
  (semantics (trap :halt)))

;; WCC's two sibling descriptors (one per matched CHOICE) share one identical
;; semantics-fn (%SEMANTICS-FN-FORM builds it once per DEFINSTRUCTION variant,
;; from the same SEMANTICS-FORMS/OPERAND-NAMES/HOLE-ALTERNATIVES-LIST) -- so
;; which branch CHOICE-CASE actually takes is entirely a function of the
;; CHOICES argument EXECUTE-INSTRUCTION is handed, not which sibling
;; descriptor happens to be at hand. Every test below picks FIRST arbitrarily
;; for exactly this reason.
(fiveam:test choice-case-dispatches-on-matched-alternative
  (let ((descriptor (first (find-instruction-variants 'word-test-machine "WCC")))
        (m (make-machine 'word-test-machine)))
    (execute-instruction descriptor m (list 5) (list (make-word-field-choice :width 10 :shift 0 :kind :inline :choice 'wc-reg)))
    (fiveam:is (= 5 (sref m 'a)))
    (execute-instruction descriptor m (list 7) (list (make-word-field-choice :width 10 :shift 0 :kind :inline :choice 'wc-ind)))
    (fiveam:is (= 7 (sref m 'b)))))

(fiveam:test choice-case-list-of-keys-matches-either
  (eval '(definstruction word-test-machine wcc-shared
           (modes wc-two)
           (encoding (opcode 10)
                     (operand value :field src
                       (variant (choice wc-reg) inline :range (0 7) :bias #x00)
                       (variant (choice wc-ind) inline :range (0 7) :bias #x08)))
           (semantics (choice-case value ((wc-reg wc-ind) (set! a value))))))
  (let ((descriptor (first (find-instruction-variants 'word-test-machine "WCC-SHARED")))
        (m (make-machine 'word-test-machine)))
    (dolist (choice-name '(wc-reg wc-ind))
      (execute-instruction descriptor m (list 3) (list (make-word-field-choice :width 10 :shift 0 :kind :inline :choice choice-name)))
      (fiveam:is (= 3 (sref m 'a))))))

(fiveam:test choice-case-no-choice-and-no-otherwise-signals-no-matching-choice
  (let ((descriptor (first (find-instruction-variants 'word-test-machine "WCC")))
        (m (make-machine 'word-test-machine)))
    (fiveam:signals no-matching-choice
      (execute-instruction descriptor m (list 5) nil))
    (fiveam:signals no-matching-choice
      (execute-instruction descriptor m (list 5) (list nil)))))

(fiveam:test choice-case-with-otherwise-falls-back-on-no-choice
  (eval '(definstruction word-test-machine wcc-otherwise
           (modes wc-two)
           (encoding (opcode 11)
                     (operand value :field src
                       (variant (choice wc-reg) inline :range (0 7) :bias #x00)
                       (variant (choice wc-ind) inline :range (0 7) :bias #x08)))
           (semantics (choice-case value
                        (wc-reg (set! a value))
                        (otherwise (set! a -1))))))
  (let ((descriptor (first (find-instruction-variants 'word-test-machine "WCC-OTHERWISE")))
        (m (make-machine 'word-test-machine)))
    (execute-instruction descriptor m (list 9) nil)
    (fiveam:is (= (wrap-value -1 16) (sref m 'a)))))

(fiveam:test choice-case-unknown-operand-name-signals-error
  (fiveam:signals error
    (eval '(definstruction word-test-machine bogus
             (modes wc-two)
             (encoding (opcode 12)
                       (operand value :field src
                         (variant (choice wc-reg) inline :range (0 7) :bias #x00)
                         (variant (choice wc-ind) inline :range (0 7) :bias #x08)))
             (semantics (choice-case no-such-operand (wc-reg 1)))))))

(fiveam:test choice-case-key-not-among-hole-alternatives-signals-error
  (fiveam:signals error
    (eval '(definstruction word-test-machine bogus
             (modes wc-two)
             (encoding (opcode 12)
                       (operand value :field src
                         (variant (choice wc-reg) inline :range (0 7) :bias #x00)
                         (variant (choice wc-ind) inline :range (0 7) :bias #x08)))
             (semantics (choice-case value (word-abs 1) (wc-ind 2)))))))

(fiveam:test choice-case-normalizes-mode-descriptor-entry
  ;; MATCH-OPERAND-MODE's own assemble-time CHOICES arrive as MODE-DESCRIPTORs,
  ;; not WORD-FIELD-CHOICEs -- %MATCHED-CHOICE-NAME must normalize both.
  (let ((descriptor (first (find-instruction-variants 'word-test-machine "WCC")))
        (m (make-machine 'word-test-machine)))
    (execute-instruction descriptor m (list 5) (list (find-mode-descriptor 'wc-ind)))
    (fiveam:is (= 5 (sref m 'b)))))

(fiveam:test decode-instruction-at-choices-hole-aligned-with-values
  ;; #73: CHOICE-CASE's whole mechanism depends on this staying true --
  ;; DST's hole (a plain value-selected field, no CHOICE of its own) decodes
  ;; a NIL choice; SRC's hole (CHOICE-selected) names the real alternative,
  ;; in the same hole order as VALUES.
  (let* ((a (assemble "wccm 2, [5]" :machine 'word-test-machine))
         (reader (vector-cell-reader (assembly-cells a))))
    (multiple-value-bind (descriptor values size choices)
        (decode-instruction-at reader 0 'word-test-machine)
      (declare (ignore size))
      (fiveam:is (string= "WCCM" (instruction-descriptor-name descriptor)))
      (fiveam:is (equal '(2 5) values))
      (fiveam:is (null (%matched-choice-name choices 0)))
      (fiveam:is (eq 'wc-ind (%matched-choice-name choices 1))))))

(fiveam:test word-opcode-overflowing-opcode-field-signals-error
  (fiveam:signals error
    (eval '(definstruction word-test-machine bogus
             (encoding (opcode 16))
             (semantics nil)))))

(fiveam:test word-relative-mode-signals-error
  (fiveam:signals error
    (eval '(definstruction word-test-machine bogus
             (modes relative)
             (encoding (opcode 3) (operand value :field src))
             (semantics nil)))))

(fiveam:test word-multi-mode-single-hole-without-operand-subclause-signals-error
  ;; unlike the byte-encoded multi-mode form, a word-encoded single-hole mode
  ;; still requires an explicit (operand ...) subclause -- there is no
  ;; default field to fall back to
  (fiveam:signals error
    (eval '(definstruction word-test-machine bogus
             (modes (word-imm (opcode 3)) (word-abs (opcode 4) (operand value :field src)))
             (semantics nil)))))

;;; encode-instruction (word path)

(fiveam:test encode-word-instruction-inline-variant
  (let ((inline-d (first (find-instruction-variants 'word-test-machine "SET"))))
    ;; value 5, biased by 1 -> field value 6; word = (1 << 12) | 6 = #x1006,
    ;; little-endian: low byte #x06, high byte #x10
    (fiveam:is (equal (list #x06 #x10) (encode-instruction inline-d (list 5))))))

(fiveam:test encode-word-instruction-extra-word-variant
  (let ((extra-d (second (find-instruction-variants 'word-test-machine "SET"))))
    ;; escape #x3ff in SRC -> word = (1 << 12) | #x3ff = #x13ff, followed by
    ;; the value's own little-endian word
    (fiveam:is (equal (list #xff #x13 #xe8 #x03) (encode-instruction extra-d (list 1000))))))

(fiveam:test encode-word-instruction-no-operand
  (let ((hlt (find-instruction 'word-test-machine 'hlt)))
    ;; word = (2 << 12) = #x2000
    (fiveam:is (equal (list #x00 #x20) (encode-instruction hlt nil)))))

;;; Cell-width-typed encoding (#53) -- a machine whose memory is
;;; word-addressed (:CELL-WIDTH 16) rather than byte-addressed, but with an
;;; ordinary opcode-plus-operand-cells encoding (not INSTRUCTION-WORD/#20's
;;; bitfield scheme -- that's a separate axis: #20 is about packing several
;;; operands into one fixed-width word, this ticket is about what a "byte" of
;;; encoded output actually is). ADDR-WIDTH 12 exercises %DEFAULT-ADDRESS-WIDTH
;;; rounding up to a whole CELL rather than a whole 8-bit byte.
(defmachine wordaddr-test-machine
  (register pc :width 16)
  (register a :width 16)
  (memory ram :width 16 :addr-width 12 :cell-width 16))

(definstruction wordaddr-test-machine nop
  (encoding (opcode 0))
  (semantics nil))

(definstruction wordaddr-test-machine lda
  (modes immediate)
  (encoding (opcode 1) (operand :mode))
  (semantics (set! a operand)))

(definstruction wordaddr-test-machine jmp
  (modes absolute)
  (encoding (opcode 2) (operand :mode))
  (semantics (set! pc operand)))

(definstruction wordaddr-test-machine hlt
  (encoding (opcode 3))
  (semantics (trap :halt)))

(fiveam:test encode-value-cells-splits-into-16-bit-cells
  ;; #x00011234 split into two 16-bit cells, little-endian: low cell #x1234,
  ;; high cell #x0001 -- the cell-width-typed counterpart of the byte-encoded
  ;; suite's ENCODE-ABSOLUTE-INSTRUCTION-LITTLE-ENDIAN test.
  (fiveam:is (equal (list #x1234 #x0001) (%encode-value-cells #x00011234 2 16))))

(fiveam:test encode-value-cells-wraps-overwide-value
  (fiveam:is (equal (list (wrap-value #x1FFFF 16)) (%encode-value-cells #x1FFFF 1 16))))

(fiveam:test wordaddr-default-address-width-is-one-cell
  ;; ADDR-WIDTH 12 rounds up to one 16-bit cell, not two 8-bit bytes --
  ;; (ceiling 12 16) = 1.
  (let ((jmp (find-instruction 'wordaddr-test-machine 'jmp)))
    (fiveam:is (equal '(1) (instruction-descriptor-operand-widths jmp)))))

(fiveam:test encode-instruction-returns-cells-not-byte-pairs
  ;; LDA #x1234 encodes as two 16-bit CELLS -- (opcode value), not four bytes
  ;; (opcode value-lo value-hi).
  (let ((lda (find-instruction 'wordaddr-test-machine 'lda)))
    (fiveam:is (equal (list 1 #x1234) (encode-instruction lda (list #x1234))))))

(fiveam:test encode-instruction-opcode-masked-to-cell-width-not-8-bits
  ;; Confirms the opcode mask in ENCODE-INSTRUCTION's cell-encoded path is
  ;; CELL-WIDTH, not a hardcoded 8 -- a value that would overflow an 8-bit
  ;; mask but fits 16 bits must round-trip unchanged.
  (let ((nop (find-instruction 'wordaddr-test-machine 'nop)))
    (fiveam:is (equal (list 0) (encode-instruction nop nil)))))

;;; Non-uniform register widths + banked registers (#54, M4) -- CHIP8FOO
;;; mirrors examples/chip8.lisp: an 8-bit banked V register (#13's REGREF)
;;; and a 12-bit I register on the same machine, ordinary opcode-plus-
;;; operand-cells encoding (not #20's INSTRUCTION-WORD).

(defmachine chip8-test-machine
  (register pc :width 12)
  (register v :width 8 :count 16)
  (register i :width 12)
  (memory ram :width 8 :addr-width 12))

(defmode chip8-v-imm "V" expr "," "#" expr)
(defmode chip8-v-only "V" expr)

(definstruction chip8-test-machine ldv
  (modes chip8-v-imm)
  (encoding (opcode 1) (operand x :width 1) (operand nn :width 1))
  (semantics (set! (v x) nn)))

(definstruction chip8-test-machine addv
  (modes chip8-v-imm)
  (encoding (opcode 2) (operand x :width 1) (operand nn :width 1))
  (semantics (set! (v x) (wrap-value (+ (v x) nn) 8))))

(definstruction chip8-test-machine ldi
  (modes immediate)
  (encoding (opcode 3) (operand :width 2))
  (semantics (set! i operand)))

(definstruction chip8-test-machine addi
  (modes chip8-v-only)
  (encoding (opcode 4) (operand x :width 1))
  (semantics (set! i (wrap-value (+ i (v x)) 12))))

(definstruction chip8-test-machine jp
  (modes absolute)
  (encoding (opcode 5) (operand :mode))
  (semantics (set! pc operand)))

(definstruction chip8-test-machine hlt
  (encoding (opcode 6))
  (semantics (trap :halt)))

(fiveam:test chip8-ldv-writes-independent-bank-cells
  (let ((m (make-machine 'chip8-test-machine))
        (ldv (find-instruction 'chip8-test-machine 'ldv)))
    (execute-instruction ldv m (list 0 10))
    (execute-instruction ldv m (list 1 20))
    (fiveam:is (= 10 (regref m 'v 0)))
    (fiveam:is (= 20 (regref m 'v 1)))))

(fiveam:test chip8-addv-wraps-at-v-own-8-bit-width
  (let ((m (make-machine 'chip8-test-machine))
        (ldv (find-instruction 'chip8-test-machine 'ldv))
        (addv (find-instruction 'chip8-test-machine 'addv)))
    (execute-instruction ldv m (list 0 250))
    (execute-instruction addv m (list 0 10))
    (fiveam:is (= 4 (regref m 'v 0)))))     ; 260 mod 256

(fiveam:test chip8-addi-wraps-at-i-own-12-bit-width-not-v-8-bit
  ;; The discriminating case: an 8-bit V source added into a 12-bit I
  ;; destination must wrap at 12 bits, not 8.
  (let ((m (make-machine 'chip8-test-machine))
        (ldv (find-instruction 'chip8-test-machine 'ldv))
        (ldi (find-instruction 'chip8-test-machine 'ldi))
        (addi (find-instruction 'chip8-test-machine 'addi)))
    (execute-instruction ldv m (list 1 5))
    (execute-instruction ldi m (list 4094))
    (execute-instruction addi m (list 1))
    (fiveam:is (= 3 (sref m 'i)))))         ; 4099 mod 4096

(fiveam:test chip8-machine-end-to-end
  ;; Mirrors examples/chip8.lisp's *SOURCE* verbatim.
  (let ((a (assemble "ldv V 0, #$fa
ldv V 1, #5
addv V 0, #10
ldi #$ffe
addi V 1
jp skip
ldv V 2, #99
skip: hlt" :machine 'chip8-test-machine)))
    (fiveam:is (= 21 (length (assembly-cells a))))
    (let ((m (make-machine 'chip8-test-machine)))
      (load-program m a)
      (multiple-value-bind (reason steps) (run m)
        (fiveam:is (eq :trap reason))
        (fiveam:is (= 7 steps))
        (fiveam:is (= 4 (regref m 'v 0)))
        (fiveam:is (= 5 (regref m 'v 1)))
        (fiveam:is (= 0 (regref m 'v 2)))
        (fiveam:is (= 3 (sref m 'i)))))))

;;; Word-addressed memory + bitfield/variant encoding combined (#55, M4) --
;;; DCPU16FOO mirrors examples/dcpu16.lisp: DCPU-16's real instruction-word
;;; layout (6-bit A, 5-bit B, 5-bit OPCODE fields) over :CELL-WIDTH 16
;;; memory, and a banked (#13) 16-bit REG register standing in for DCPU-16's
;;; eight named registers, addressed here by index.

(defmachine dcpu16-test-machine
  (register pc :width 16)
  (register reg :width 16 :count 8)
  (memory ram :width 16 :addr-width 16 :cell-width 16)
  (instruction-word :width 16
    (field a 6)
    (field b 5)
    (field opcode 5)))

(defmode dcpu16-rr expr "," expr)

(definstruction dcpu16-test-machine set
  (modes dcpu16-rr)
  (encoding
    (opcode 1)
    (operand dst :field b)
    (operand src :field a
      (variant (range -1 30) inline :bias 33)
      (variant :else (extra-word :escape #x1f))))
  (semantics (set! (reg dst) src)))

(definstruction dcpu16-test-machine add
  (modes dcpu16-rr)
  (encoding
    (opcode 2)
    (operand dst :field b)
    (operand src :field a
      (variant (range -1 30) inline :bias 33)
      (variant :else (extra-word :escape #x1f))))
  (semantics (set! (reg dst) (wrap-value (+ (reg dst) src) 16))))

(definstruction dcpu16-test-machine addr
  (modes dcpu16-rr)
  (encoding
    (opcode 3)
    (operand dst :field b)
    (operand srcreg :field a))
  (semantics (set! (reg dst) (wrap-value (+ (reg dst) (reg srcreg)) 16))))

(definstruction dcpu16-test-machine sto
  (modes dcpu16-rr)
  (encoding
    (opcode 4)
    (operand addr :field a
      (variant (range -1 30) inline :bias 33)
      (variant :else (extra-word :escape #x1f)))
    (operand dst :field b))
  (semantics (setf (mref machine 'ram addr) (reg dst))))

(definstruction dcpu16-test-machine hlt
  (encoding (opcode 5))
  (semantics (trap :halt)))

(fiveam:test dcpu16-set-small-value-packs-inline-one-cell
  (let ((a (assemble "set 0, 5" :machine 'dcpu16-test-machine)))
    (fiveam:is (= 1 (length (assembly-cells a))))))

(fiveam:test dcpu16-set-large-value-escapes-to-extra-cell
  (let ((a (assemble "set 1, 1000" :machine 'dcpu16-test-machine)))
    (fiveam:is (= 2 (length (assembly-cells a))))
    (fiveam:is (= 1000 (aref (assembly-cells a) 1)))))

(fiveam:test dcpu16-addr-registers-both-plain-inline-one-cell
  (let ((a (assemble "addr 0, 1" :machine 'dcpu16-test-machine)))
    (fiveam:is (= 1 (length (assembly-cells a))))))

(fiveam:test dcpu16-machine-end-to-end
  ;; Mirrors examples/dcpu16.lisp's *SOURCE* verbatim.
  (let ((a (assemble "set 0, 5
set 1, 1000
addr 0, 1
sto result, 0
hlt
result: .byte 0" :machine 'dcpu16-test-machine)))
    (fiveam:is (= 16 (assembly-cell-width a)))
    (fiveam:is (= 7 (length (assembly-cells a))))
    (fiveam:is (equal '(unsigned-byte 16) (array-element-type (assembly-cells a))))
    (let ((m (make-machine 'dcpu16-test-machine)))
      (load-program m a)
      (multiple-value-bind (reason steps) (run m)
        (fiveam:is (eq :trap reason))
        (fiveam:is (= 5 steps))
        (fiveam:is (= 1005 (regref m 'reg 0)))
        (fiveam:is (= 1005 (mref m 'ram (gethash "result" (assembly-symbols a)))))))))

;;; Shared opcodes across mode-distinguished variants (#105) -- several
;;; decode-distinguishable descriptors, one mnemonic's own several MODES
;;; clauses or several distinct mnemonics, sharing one opcode on a
;;; word-encoded machine. WORD-TEST-MACHINE's src field is 10 bits, so a
;;; small bias offset per variant is plenty of room to keep raw field ranges
;;; disjoint.

(definstruction word-test-machine wcy
  (modes (wc-two (opcode 13)
           (operand v :field src
             (variant (choice wc-reg) inline :range (0 7) :bias 0)
             (variant (choice wc-ind) inline :range (0 7) :bias 8))
           (semantics (set! a v)))
         (word-imm (opcode 13)
           (operand v :field src (variant (range 0 15) inline :bias 800))
           (semantics (set! a v)))))

(fiveam:test shared-opcode-one-mnemonic-two-mode-clauses-registers
  (let ((variants (find-instruction-variants 'word-test-machine "WCY")))
    (fiveam:is (= 3 (length variants))) ; WC-TWO expands into 2 sibling combos + WORD-IMM's 1
    (fiveam:is (every (lambda (d) (= 13 (instruction-descriptor-opcode d))) variants))
    (fiveam:is (= 3 (length (find-instruction-descriptors-by-opcode 'word-test-machine 13))))))

(fiveam:test shared-opcode-one-mnemonic-two-mode-clauses-each-decodes-to-its-own-mode
  ;; The #105 reproduction: before this ticket, one of these two forms
  ;; returned :DECODE-FAILURE and the other silently reported the wrong
  ;; descriptor -- the opcode table held exactly one, last-write-wins.
  ;; MODE is the whole (MODES ...) clause matched (WC-TWO for both the
  ;; bare-register and bracketed-indirect forms, since both are ONE-OF
  ;; alternatives of that one mode); CHOICE is that hole's own matched
  ;; ONE-OF alternative (NIL for WORD-IMM, which has none), read off the
  ;; fourth CHOICES return value the same way the disassembler does.
  (dolist (case '(("wcy 5" wc-two wc-reg) ("wcy [5]" wc-two wc-ind) ("wcy #5" word-imm nil)))
    (destructuring-bind (source expected-mode expected-choice) case
      (let* ((cells (assembly-cells (assemble source :machine 'word-test-machine))))
        (multiple-value-bind (descriptor values size choices)
            (decode-instruction-at (vector-cell-reader cells) 0 'word-test-machine)
          (declare (ignore size))
          (fiveam:is (not (eq :decode-failure descriptor)))
          (fiveam:is (string= "WCY" (instruction-descriptor-name descriptor)))
          (fiveam:is (eq expected-mode (mode-descriptor-name (instruction-descriptor-mode descriptor))))
          (fiveam:is (eq expected-choice (word-field-choice-choice (first choices))))
          (fiveam:is (= 5 (first values))))))))

(definstruction word-test-machine wcz1
  (modes wc-two)
  (encoding
    (opcode 14)
    (operand v :field src
      (variant (choice wc-reg) inline :range (0 7) :bias 0)
      (variant (choice wc-ind) inline :range (0 7) :bias 8)))
  (semantics (set! a v)))

(definstruction word-test-machine wcz2
  (modes word-imm)
  (encoding (opcode 14) (operand v :field src (variant (range 0 15) inline :bias 800)))
  (semantics (set! b v)))

(fiveam:test shared-opcode-two-mnemonics-registers-and-decodes-both
  ;; WCZ1's WC-TWO field is CHOICE-selected (2 variants -> 2 sibling combos);
  ;; WCZ2's WORD-IMM field is a single plain variant -> 1 combo. 3 total.
  (fiveam:is (= 3 (length (find-instruction-descriptors-by-opcode 'word-test-machine 14))))
  (let ((ld (assembly-cells (assemble "wcz1 5" :machine 'word-test-machine)))
        (st (assembly-cells (assemble "wcz2 #5" :machine 'word-test-machine))))
    (fiveam:is (string= "WCZ1" (instruction-descriptor-name
                                 (decode-instruction-at (vector-cell-reader ld) 0 'word-test-machine))))
    (fiveam:is (string= "WCZ2" (instruction-descriptor-name
                                 (decode-instruction-at (vector-cell-reader st) 0 'word-test-machine))))))

(fiveam:test shared-opcode-indistinguishable-pair-signals-opcode-conflict
  ;; WCZ3 declares the identical field encoding WCZ1 already claims at
  ;; opcode 14 -- no fetched raw value could ever tell the two mnemonics
  ;; apart at decode time.
  (fiveam:signals opcode-conflict
    (eval '(definstruction word-test-machine wcz3
             (modes wc-two)
             (encoding
               (opcode 14)
               (operand v :field src
                 (variant (choice wc-reg) inline :range (0 7) :bias 0)
                 (variant (choice wc-ind) inline :range (0 7) :bias 8)))
             (semantics (set! a v)))))
  (handler-case
      (eval '(definstruction word-test-machine wcz3
               (modes wc-two)
               (encoding
                 (opcode 14)
                 (operand v :field src
                   (variant (choice wc-reg) inline :range (0 7) :bias 0)
                   (variant (choice wc-ind) inline :range (0 7) :bias 8)))
               (semantics (set! a v))))
    (opcode-conflict (c) (fiveam:is (eq :indistinguishable (opcode-conflict-reason c))))))

;; The mixed-kind case %HOLE-DISJOINT-P must also catch: one candidate's
;; field is :EXTRA-WORD (a single escape value), the other's is :INLINE (a
;; whole biased range) -- disjointness must hold in the direction where the
;; *new* descriptor being registered is the :EXTRA-WORD one and the
;; *already-registered* co-tenant is the :INLINE one (%CHECK-OPCODE-
;; DECODABLE! enumerates the new descriptor's own field values and tests
;; them against the existing one's WORD-FIELD-CHOICEs), the reverse of
;; SHARED-OPCODE-INDISTINGUISHABLE-PAIR-SIGNALS-OPCODE-CONFLICT above (both
;; :INLINE) and the LD/WCZ tests elsewhere (both :EXTRA-WORD via :ELSE, or
;; neither). A dedicated tiny word machine, rather than reusing WORD-TEST-
;; MACHINE's own 4-bit (0-15) opcode space -- opcodes 1-14 there are already
;; claimed by tests above and 15 is reserved unregistered
;; (tests/emulator.lisp's STEP-MACHINE-WORD-ENCODED-DECODE-FAILURE-ON-
;; UNREGISTERED-OPCODE), leaving no room.

(defmachine mixed-kind-test-machine
  (register pc :width 16)
  (register a :width 16)
  (register b :width 16)
  (memory ram :width 8 :addr-width 16)
  (instruction-word :width 16 (field opcode 4) (field src 12)))

(defmode mk-imm "#" expr)

(definstruction mixed-kind-test-machine mk1
  (modes mk-imm)
  (encoding (opcode 1) (operand v :field src (variant (range 900 920) inline)))
  (semantics (set! a v)))

(fiveam:test shared-opcode-new-extra-word-escape-inside-existing-inline-range-signals-error
  (fiveam:signals opcode-conflict
    (eval '(definstruction mixed-kind-test-machine mk2
             (modes mk-imm)
             (encoding (opcode 1)
                       (operand v :field src (variant :else (extra-word :escape 910))))
             (semantics (set! b v))))))

(fiveam:test shared-opcode-redefining-one-co-tenant-leaves-the-other-intact
  (definstruction word-test-machine wcz1
    (modes wc-two)
    (encoding
      (opcode 14)
      (operand v :field src
        (variant (choice wc-reg) inline :range (0 7) :bias 0)
        (variant (choice wc-ind) inline :range (0 7) :bias 8)))
    (semantics (set! a (+ v 1)))) ; redefined body, same opcode/mode
  (fiveam:is (= 3 (length (find-instruction-descriptors-by-opcode 'word-test-machine 14))))
  (fiveam:is (find "WCZ2" (find-instruction-descriptors-by-opcode 'word-test-machine 14)
                    :key #'instruction-descriptor-name :test #'string=)))

;; Byte-encoded machines have no per-field discriminator to decode two modes
;; of one mnemonic apart -- what #105's reproduction found registering
;; silently and then mis-decoding (one DEFINSTRUCTION's own (MODES ...)
;; clause declaring two modes at one opcode, not a later redefinition
;; replacing the mnemonic wholesale -- REGISTER-INSTRUCTION-VARIANTS!'s
;; cleanup phase drops a redefined mnemonic's own old entries first, so
;; redefining BYTE-SH under a new mode at the same opcode is not this case
;; at all) is now an unconditional DEFINSTRUCTION-time error there, same
;; severity as the pre-existing cross-mnemonic case.
(fiveam:test shared-opcode-byte-machine-same-mnemonic-different-mode-signals-error
  (fiveam:signals opcode-conflict
    (eval '(definstruction instr-test-machine byte-sh
             (modes (immediate (opcode #x77) (operand :mode) (semantics (set! a operand)))
                    (absolute (opcode #x77) (operand :mode) (semantics (set! a operand)))))))
  (handler-case
      (eval '(definstruction instr-test-machine byte-sh
               (modes (immediate (opcode #x77) (operand :mode) (semantics (set! a operand)))
                      (absolute (opcode #x77) (operand :mode) (semantics (set! a operand))))))
    (opcode-conflict (c) (fiveam:is (eq :undecodable-byte-machine (opcode-conflict-reason c))))))
