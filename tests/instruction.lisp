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

(fiveam:test multi-operand-instruction-operand-name-shadowing-flag-signals-error
  (fiveam:signals error
    (eval '(definstruction instr-test-machine bogus
             (modes two-hole-test-mode)
             (encoding (opcode #xFF) (operand z :width 1) (operand :width 1))
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
