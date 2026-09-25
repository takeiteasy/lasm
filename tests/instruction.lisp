;;;; tests/instruction.lisp
;;;; fiveam tests for DEFINSTRUCTION and the addressing-mode/encoding
;;;; pipeline (instruction.lisp, mode.lisp).

(in-package #:lasm)

(fiveam:def-suite instruction :in lasm)
(fiveam:in-suite instruction)

(definstruction sole-memory-semantics-test-machine inc-memory-cell
  (encoding (opcode #x01))
  (semantics (setf (mref machine 3) (1+ (mref machine 3)))))

(fiveam:test mref-shorthand-in-instruction-semantics
  (let ((machine (make-machine 'sole-memory-semantics-test-machine)))
    (setf (mref machine 'ram 3) 41)
    (execute-instruction
     (find-instruction 'sole-memory-semantics-test-machine 'inc-memory-cell)
     machine nil)
    (fiveam:is (= 42 (mref machine 'ram 3)))))

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

(fiveam:test instruction-rejects-duplicate-top-level-clauses
  (loop for (head form) in
        '((modes (definstruction instr-test-machine bogus
                   (modes immediate) (modes absolute)
                   (encoding (opcode #xFF) (operand :mode))
                   (semantics nil)))
          (encoding (definstruction instr-test-machine bogus
                      (encoding (opcode #xFF)) (encoding (opcode #xFE))
                      (semantics nil)))
          (semantics (definstruction instr-test-machine bogus
                       (encoding (opcode #xFF))
                       (semantics nil) (semantics nil)))
          (cycles (definstruction instr-test-machine bogus
                    (encoding (opcode #xFF)) (semantics nil)
                    (cycles 1) (cycles 2))))
        for message = (handler-case (progn (macroexpand-1 form) nil)
                        (error (condition) (princ-to-string condition)))
        do (fiveam:is (not (null message)))
           (fiveam:is (and message (search "BOGUS" message)))
           (fiveam:is (and message (search (string-upcase (symbol-name head)) message)))))

(fiveam:test missing-encoding-clause-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction instr-test-machine bogus
             (modes immediate)
             (semantics (set! x operand))))))

(fiveam:test missing-semantics-clause-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction instr-test-machine bogus
             (modes immediate)
             (encoding (opcode #xFF) (operand :mode))))))

(fiveam:test multiple-bare-mode-symbols-signals-error
  ;; more than one bare mode symbol requires the multi-mode list form
  (fiveam:signals instruction-definition-error
    (eval '(definstruction instr-test-machine bogus
             (modes immediate absolute)
             (encoding (opcode #xFF) (operand :mode))
             (semantics (set! x operand))))))

(fiveam:test multi-mode-with-top-level-encoding-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction instr-test-machine bogus
             (modes (immediate (opcode #xF0)) (absolute (opcode #xF1)))
             (encoding (opcode #xFF))
             (semantics nil)))))

(fiveam:test multi-mode-single-variant-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction instr-test-machine bogus
             (modes (immediate (opcode #xF0)))
             (semantics nil)))))

(fiveam:test multi-mode-variant-without-opcode-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction instr-test-machine bogus
             (modes (immediate (semantics nil)) (absolute (opcode #xF1)))
             (semantics nil)))))

(fiveam:test multi-mode-variant-without-semantics-or-default-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction instr-test-machine bogus
             (modes (immediate (opcode #xF0)) (absolute (opcode #xF1)))))))

(fiveam:test multi-mode-rejects-unknown-and-malformed-subclauses
  (fiveam:signals instruction-definition-error
    (macroexpand-1 '(definstruction instr-test-machine bogus
                     (modes (immediate (opcode #xF0))
                            (absolute (opcode #xF1) (cycle 2)))
                     (semantics nil))))
  (fiveam:signals instruction-definition-error
    (macroexpand-1 '(definstruction instr-test-machine bogus
                     (modes (immediate (opcode #xF0) stray)
                            (absolute (opcode #xF1)))
                     (semantics nil)))))

(fiveam:test encoding-rejects-unknown-subclauses-in-both-shapes
  (fiveam:signals instruction-definition-error
    (macroexpand-1 '(definstruction instr-test-machine bogus
                     (encoding (opcode #xF0) (opcod #xF1))
                     (semantics nil))))
  (fiveam:signals instruction-definition-error
    (macroexpand-1 '(definstruction instr-test-machine bogus
                     (modes immediate)
                     (encoding (opcode #xF0) (operand :mode) (cycle 2))
                     (semantics nil)))))

(fiveam:test instruction-rejects-duplicate-singular-subclauses
  (fiveam:signals instruction-definition-error
    (macroexpand-1 '(definstruction instr-test-machine bogus
                     (modes (immediate (opcode #xF0) (cycles 2) (cycles 3))
                            (absolute (opcode #xF1)))
                     (semantics nil))))
  (fiveam:signals instruction-definition-error
    (macroexpand-1 '(definstruction instr-test-machine bogus
                     (encoding (opcode #xF0) (opcode #xF1))
                     (semantics nil))))
  (fiveam:signals instruction-definition-error
    (macroexpand-1 '(definstruction instr-test-machine bogus
                     (modes immediate)
                     (encoding (opcode #xF0) (opcode #xF1) (operand :mode))
                     (semantics nil)))))

(fiveam:test word-mode-rejects-unknown-subclause
  (fiveam:signals instruction-definition-error
    (macroexpand-1 '(definstruction word-test-machine bogus
                     (modes (immediate (opcode 1) (cycle 2))
                            (absolute (opcode 2)))
                     (semantics nil)))))

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
  (fiveam:signals instruction-definition-error
    (eval '(definstruction instr-test-machine bogus
             (modes two-hole-test-mode)
             (encoding (opcode #xFF) (operand :width 1))
             (semantics nil)))))

(fiveam:test multi-operand-instruction-too-many-operand-subclauses-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction instr-test-machine bogus
             (modes immediate)
             (encoding (opcode #xFF) (operand :width 1) (operand :width 1))
             (semantics nil)))))

(fiveam:test multi-operand-instruction-with-no-operand-subclause-in-multi-mode-form-signals-error
  ;; the multi-mode form's operand-subclause default only applies to a
  ;; single-hole mode -- a two-hole mode has no single width to default to
  (fiveam:signals instruction-definition-error
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
  (fiveam:signals instruction-definition-error
    (eval '(definstruction instr-test-machine bogus
             (modes two-hole-test-mode)
             (encoding (opcode #xFF) (operand n :width 1) (operand n :width 1))
             (semantics nil)))))

(fiveam:test multi-operand-instruction-operand-name-shadowing-register-signals-error
  ;; INSTR-TEST-MACHINE declares a scalar register named A -- naming an
  ;; operand field the same would leave (semantics ...) unable to see one of
  ;; them, so this is rejected at DEFINSTRUCTION time rather than silently
  ;; shadowing the register.
  (fiveam:signals instruction-definition-error
    (eval '(definstruction instr-test-machine bogus
             (modes two-hole-test-mode)
             (encoding (opcode #xFF) (operand a :width 1) (operand :width 1))
             (semantics nil)))))

(fiveam:test multi-operand-instruction-operand-name-shadowing-banked-register-signals-error
  ;; #13: INSTR-TEST-MACHINE's V is a banked (:count 4) register, bound by
  ;; WITH-MACHINE-BINDINGS as a MACROLET rather than a symbol-macro -- an
  ;; operand named V would shadow it exactly as silently as a scalar
  ;; register would, so %SCALAR-BINDABLE-NAMES must reject it too.
  (fiveam:signals instruction-definition-error
    (eval '(definstruction instr-test-machine bogus
             (modes two-hole-test-mode)
             (encoding (opcode #xFF) (operand v :width 1) (operand :width 1))
             (semantics nil)))))

(fiveam:test multi-operand-instruction-operand-name-shadowing-flag-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction instr-test-machine bogus
             (modes two-hole-test-mode)
             (encoding (opcode #xFF) (operand z :width 1) (operand :width 1))
             (semantics nil)))))

;; #72: a dedicated byte-encoded fixture with an aliased banked register --
;; ALIAS-TEST-MACHINE's V carries :names (v0 v1), so an operand named V0
;; would shadow that alias's symbol-macro exactly as silently as a scalar
;; register or a banked register's own name would.
(defmachine alias-test-machine
  (register v :width 8 :names (v0 v1))
  (register pc :width 16)
  (memory ram :width 8 :addr-width 16))

(fiveam:test multi-operand-instruction-operand-name-shadowing-register-alias-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction alias-test-machine bogus
             (modes two-hole-test-mode)
             (encoding (opcode #xFF) (operand v0 :width 1) (operand :width 1))
             (semantics nil)))))

;;; #143: (operand ... :register ELEM) -- validated at DEFINSTRUCTION time,
;;; reusing ALIAS-TEST-MACHINE above (its V bank carries :NAMES (v0 v1)).

(fiveam:test operand-register-naming-unknown-element-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction alias-test-machine bogus
             (modes two-hole-test-mode)
             (encoding (opcode #xFE) (operand x :width 1 :register nosuch) (operand :width 1))
             (semantics nil)))))

(fiveam:test operand-register-naming-a-non-register-element-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction alias-test-machine bogus
             (modes two-hole-test-mode)
             (encoding (opcode #xFE) (operand x :width 1 :register ram) (operand :width 1))
             (semantics nil)))))

(fiveam:test operand-register-naming-an-unnamed-register-signals-error
  ;; ALIAS-TEST-MACHINE's PC is a scalar register declaring no #72 :NAMES --
  ;; :REGISTER only makes sense against a bank the disassembler can alias.
  (fiveam:signals instruction-definition-error
    (eval '(definstruction alias-test-machine bogus
             (modes two-hole-test-mode)
             (encoding (opcode #xFE) (operand x :width 1 :register pc) (operand :width 1))
             (semantics nil)))))

(fiveam:test operand-register-on-a-relative-hole-signals-error
  ;; A relative hole's value is adjusted to an absolute target at render
  ;; time (disassembler.lisp's %OPERAND-RENDER-VALUES) -- combined with
  ;; :REGISTER, that would corrupt a bank index rather than merely
  ;; mis-render one.
  (fiveam:signals instruction-definition-error
    (eval '(definstruction alias-test-machine bogus
             (modes relative)
             (encoding (opcode #xFE) (operand :mode :register v))
             (semantics nil)))))

(fiveam:test operand-register-on-a-signed-hole-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction alias-test-machine bogus
             (modes instr-signed-imm-test-mode)
             (encoding (opcode #xFE) (operand :mode :register v))
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
  (fiveam:signals instruction-definition-error
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

(definstruction instr-test-machine relpair
  (modes relative-two-hole-test-mode)
  (encoding (opcode #xFF) (operand :width 1) (operand :width 1))
  (semantics nil))

(fiveam:test relative-mode-with-more-than-one-hole
  (fiveam:is (equal '(t t) (instruction-descriptor-relative-holes
                           (find-instruction 'instr-test-machine 'relpair))))
  (fiveam:is (equalp #(#xFF #xFD #xFD)
                     (assembly-cells (assemble "relpair *, *" :machine 'instr-test-machine)))))

(fiveam:test relative-holes-use-independent-targets
  (fiveam:is (equalp #(#xFF 0 1 #xEA #xEA)
                     (assembly-cells (assemble "relpair first, second
first: nop
second: nop" :machine 'instr-test-machine))))
  (fiveam:signals assembly-error
    (assemble "relpair *, $1000" :machine 'instr-test-machine)))

(defmode hole-attribute-mix (expr :relative t) "," (expr :signed t) "," expr :width 1)
(definstruction instr-test-machine attrmix
  (modes hole-attribute-mix)
  (encoding (opcode #xFA) (operand target :mode) (operand delta :mode) (operand raw :mode))
  (semantics nil))

(defmode hole-attribute-override (expr :relative nil :signed nil) "," expr
  :width 1 :relative t)
(definstruction instr-test-machine attrover
  (modes hole-attribute-override)
  (encoding (opcode #xFB) (operand raw :mode) (operand target :mode))
  (semantics nil))

(fiveam:test direct-hole-attributes-encode-independently
  (let ((a (assemble "attrmix *, -5, 200" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#xFA #xFC #xFB 200) (assembly-cells a)))
    (multiple-value-bind (descriptor values)
        (decode-instruction-at (vector-cell-reader (assembly-cells a)) 0 'instr-test-machine)
      (fiveam:is (equal '(t nil nil) (instruction-descriptor-relative-holes descriptor)))
      (fiveam:is (equal '(t t nil) (instruction-descriptor-operand-signedness descriptor)))
      (fiveam:is (equal '(-4 -5 200) values)))))

(fiveam:test hole-attributes-override-mode-defaults
  (let ((a (assemble "attrover 200, *" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#xFB 200 #xFD) (assembly-cells a)))
    (let ((descriptor (find-instruction 'instr-test-machine 'attrover)))
      (fiveam:is (equal '(nil t) (instruction-descriptor-relative-holes descriptor)))
      (fiveam:is (equal '(nil t) (instruction-descriptor-operand-signedness descriptor))))))

(fiveam:test direct-relative-hole-overflow-signals-error
  (fiveam:signals assembly-error
    (assemble "attrmix $1000, 0, 0" :machine 'instr-test-machine)))

(fiveam:test direct-hole-attributes-disassemble-and-reassemble
  (let* ((a (assemble "attrmix *, -5, 200" :machine 'instr-test-machine))
         (line (first (disassemble-assembly a :machine 'instr-test-machine :labels nil))))
    (fiveam:is (equalp (assembly-cells a)
                       (assembly-cells (assemble (disassembly-line-text line)
                                                 :machine 'instr-test-machine))))))

(defmode hole-attribute-alt-plain "[" expr "," expr "]" :width 1)
(defmode hole-attribute-alt-relative "#" expr "," (expr :relative t) :width 1)
(defmode hole-attribute-alternatives (one-of hole-attribute-alt-plain
                                            hole-attribute-alt-relative))
(definstruction instr-test-machine attrchoice
  (modes hole-attribute-alternatives)
  (encoding (opcode #xFC)
            (operand first-value :mode)
            (operand second-value :mode)
            (sub-opcode
              (variant (choice hole-attribute-alt-plain hole-attribute-alt-plain) (sub 0))
              (variant (choice hole-attribute-alt-plain hole-attribute-alt-relative) (sub 1))
              (variant (choice hole-attribute-alt-relative hole-attribute-alt-plain) (sub 2))
              (variant (choice hole-attribute-alt-relative hole-attribute-alt-relative) (sub 3))))
  (semantics nil))

(fiveam:test multi-hole-one-of-alternative-keeps-its-own-hole-attributes
  (let* ((a (assemble "attrchoice #5, *" :machine 'instr-test-machine))
         (line (first (disassemble-assembly a :machine 'instr-test-machine :labels nil))))
    (fiveam:is (equalp #(#xFC 3 5 #xFC) (assembly-cells a)))
    (fiveam:is (equalp (assembly-cells a)
                       (assembly-cells (assemble (disassembly-line-text line)
                                                 :machine 'instr-test-machine))))))

;; ONE-OF alternatives supply their own attributes, so the enclosing mode
;; cannot also assign a relative default to those holes.
(defmode relative-one-of-hole-test-a expr)
(defmode relative-one-of-hole-test-b "[" expr "]")
(defmode relative-one-of-hole-test-mode
    (one-of relative-one-of-hole-test-a relative-one-of-hole-test-b) :relative t)

(fiveam:test relative-mode-whose-single-hole-is-a-one-of-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction instr-test-machine bogus
             (modes relative-one-of-hole-test-mode)
             (encoding (opcode #xFE) (operand :width 1))
             (semantics nil)))))

;; #132: the same hazard as above, one hole earlier in the pipeline, for a
;; plain whole-mode :SIGNED T (not :RELATIVE). %BYTE-OPERAND-SIGNEDNESS
;; resolves a ONE-OF hole's signedness from its own matched alternative
;; first, never falling back to MODE's own SIGNEDP -- so a mode's own
;; :SIGNED T would be silently dropped in favor of whichever alternative
;; matched (agreeing or not) rather than erroring where the contradiction is
;; written. %CHECK-MODE-HOLE-ATTRIBUTES now rejects this outright.
(defmode signed-one-of-hole-test-a expr)
(defmode signed-one-of-hole-test-b "[" expr "]")
(defmode signed-one-of-hole-test-mode
    (one-of signed-one-of-hole-test-a signed-one-of-hole-test-b) :signed t)

(fiveam:test signed-mode-whose-single-hole-is-a-one-of-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction instr-test-machine bogus
             (modes signed-one-of-hole-test-mode)
             (encoding (opcode #xFD) (operand :width 1))
             (semantics nil)))))

(fiveam:test unknown-clause-head-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction instr-test-machine bogus
             (bogus-clause 1)
             (encoding (opcode #xFF))
             (semantics nil)))))

(fiveam:test mode-without-operand-subclause-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction instr-test-machine bogus
             (modes immediate)
             (encoding (opcode #xFF))
             (semantics nil)))))

(fiveam:test operand-subclause-without-mode-signals-error
  (fiveam:signals instruction-definition-error
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

(fiveam:test eval-expr-constant-logical-operators
  (flet ((value (source)
           (eval-expr-constant (parse-expression (tokenize source)))))
    (fiveam:is (equal '(1 0 0 0) (list (value "2 && 3") (value "2 && 0")
                                       (value "0 && 3") (value "0 && 0"))))
    (fiveam:is (equal '(1 1 1 0) (list (value "2 || 3") (value "2 || 0")
                                       (value "0 || 3") (value "0 || 0"))))
    (fiveam:is (equal '(0 1 1) (list (value "!5") (value "!0") (value "!!5"))))
    (fiveam:is (= 1 (value "1 < 2 && 3 >= 3 || 0")))
    (fiveam:is (= 1 (value "0 && missing || 1")))))

(fiveam:test logical-operators-short-circuit
  (flet ((value (source)
           (eval-expr-constant (parse-expression (tokenize source)))))
    (fiveam:is (= 0 (value "0 && missing")))
    (fiveam:is (= 1 (value "1 || missing")))
    (fiveam:signals unresolved-label (value "1 && missing"))
    (fiveam:signals unresolved-label (value "0 || missing"))))

(fiveam:test eval-expr-constant-modulo
  (flet ((value (source)
           (eval-expr-constant (parse-expression (tokenize source)))))
    (fiveam:is (= 3 (value "13 % 5")))
    (fiveam:is (= -2 (value "-5 % 3")))
    (fiveam:is (= 2 (value "5 % -3")))
    (fiveam:signals division-by-zero (value "5 % 0"))))

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
  (fiveam:signals instruction-definition-error
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

(defmachine lazy-semantics-test-machine
  (register pc :width 16)
  (register a :width 16)
  (memory ram :width 8 :addr-width 16)
  (instruction-word :width 16 (field opcode 4) (field value 12)))

(fiveam:test word-semantics-compile-on-first-use-and-redefinition
  (eval '(definstruction lazy-semantics-test-machine lazyset
           (modes word-imm)
           (encoding (opcode 1)
                     (operand value :field value
                       (variant (range 0 7) inline)
                       (variant :else (extra-word :escape #xfff))))
           (semantics (set! a value))))
  (let* ((variants (find-instruction-variants 'lazy-semantics-test-machine "LAZYSET"))
         (proxy (instruction-descriptor-semantics-fn (first variants)))
         (machine (make-machine 'lazy-semantics-test-machine)))
    (fiveam:is (= 2 (length variants)))
    (fiveam:is (eq proxy (instruction-descriptor-semantics-fn (second variants))))
    (execute-instruction (first variants) machine (list 3))
    (fiveam:is (= 3 (sref machine 'a)))
    (fiveam:is (not (eq proxy (instruction-descriptor-semantics-fn (first variants)))))
    (fiveam:is (eq (instruction-descriptor-semantics-fn (first variants))
                   (instruction-descriptor-semantics-fn (second variants))))
    (execute-instruction (second variants) machine (list 9))
    (fiveam:is (= 9 (sref machine 'a)))
    (eval '(definstruction lazy-semantics-test-machine lazyset
             (modes word-imm)
             (encoding (opcode 1)
                       (operand value :field value
                         (variant (range 0 7) inline)
                         (variant :else (extra-word :escape #xfff))))
             (semantics (set! a (+ value 1)))))
    (let ((replacement (first (find-instruction-variants 'lazy-semantics-test-machine "LAZYSET"))))
      (fiveam:is (not (eq (instruction-descriptor-semantics-fn replacement)
                          (instruction-descriptor-semantics-fn (first variants)))))
      (execute-instruction replacement machine (list 3))
      (fiveam:is (= 4 (sref machine 'a))))))

(fiveam:test word-semantics-promote-after-enough-calls
  (eval '(definstruction lazy-semantics-test-machine lazypromote
           (modes word-imm)
           (encoding (opcode 2)
                     (operand value :field value
                       (variant (range 0 7) inline)
                       (variant :else (extra-word :escape #xfff))))
           (semantics (set! a value))))
  (let* ((*semantics-promotion-calls* 3)
         (descriptor (first (find-instruction-variants 'lazy-semantics-test-machine "LAZYPROMOTE")))
         (machine (make-machine 'lazy-semantics-test-machine)))
    (execute-instruction descriptor machine (list 1))
    (let ((fast (instruction-descriptor-semantics-fn descriptor)))
      (execute-instruction descriptor machine (list 2))
      (fiveam:is (eq fast (instruction-descriptor-semantics-fn descriptor)))
      (execute-instruction descriptor machine (list 3))
      (fiveam:is (not (eq fast (instruction-descriptor-semantics-fn descriptor))))
      (let ((final (instruction-descriptor-semantics-fn descriptor)))
        (execute-instruction descriptor machine (list 5))
        (fiveam:is (= 5 (sref machine 'a)))
        (fiveam:is (eq final (instruction-descriptor-semantics-fn descriptor)))))))

;;; instruction-word layout parsing (machine.lisp)

(fiveam:test instruction-word-clause-requires-width
  (fiveam:signals machine-definition-error
    (eval '(defmachine bogus-word-machine
             (instruction-word (field opcode 4))))))

(fiveam:test instruction-word-clause-requires-whole-cell-width
  ;; A MEMORY element is required here so the check this exercises --
  ;; %FINISH-INSTRUCTION-WORD-LAYOUT's (mod width cell-width) test
  ;; (machine.lisp) -- is actually reached; a machine with no memory element
  ;; fails earlier with a different error ("no memory element declared",
  ;; %MACHINE-CELL-WIDTH), which FIVEAM:SIGNALS ERROR can't tell apart from
  ;; the intended failure.
  (fiveam:signals machine-definition-error
    (eval '(defmachine bogus-word-machine
             (memory ram :width 8 :addr-width 8)
             (instruction-word :width 12 (field opcode 12))))))

(fiveam:test instruction-word-clause-requires-whole-cell-width-non-8-bit-cell
  ;; Same check, on a machine whose cell width isn't 8 -- :width 24 doesn't
  ;; divide evenly by a 16-bit cell (24 mod 16 = 8), so this must still
  ;; signal rather than only ever checking against a hardcoded 8.
  (fiveam:signals machine-definition-error
    (eval '(defmachine bogus-word-machine-16
             (memory ram :width 16 :addr-width 8 :cell-width 16)
             (instruction-word :width 24 (field opcode 24))))))

(fiveam:test instruction-word-clause-requires-opcode-field
  (fiveam:signals machine-definition-error
    (eval '(defmachine bogus-word-machine
             (instruction-word :width 16 (field a 16))))))

(fiveam:test instruction-word-clause-rejects-duplicate-field-names
  (fiveam:signals machine-definition-error
    (eval '(defmachine bogus-word-machine
             (instruction-word :width 16 (field opcode 8) (field opcode 8))))))

(fiveam:test instruction-word-clause-field-widths-must-sum-to-word-width
  (fiveam:signals machine-definition-error
    (eval '(defmachine bogus-word-machine
             (instruction-word :width 16 (field opcode 4) (field a 4))))))

(fiveam:test instruction-word-clause-fields-are-msb-first
  (let ((layout (machine-descriptor-instruction-word (find-machine-descriptor 'word-test-machine))))
    (fiveam:is (= 16 (instruction-word-layout-width layout)))
    (fiveam:is (= 2 (instruction-word-layout-width-cells layout)))
    (fiveam:is (equal '(opcode 4 12) (instruction-word-field layout 'opcode)))
    (fiveam:is (equal '(dst 2 10) (instruction-word-field layout 'dst)))
    (fiveam:is (equal '(src 10 0) (instruction-word-field layout 'src)))))

;;; #66: :ENDIAN on a word-encoded machine -- BIGENDIAN-WORD-TEST-MACHINE is
;;; WORD-TEST-MACHINE's exact shape, its sole memory element :ENDIAN :BIG
;;; instead of the implicit :LITTLE default.

(defmachine bigendian-word-test-machine
  (register pc :width 16)
  (register a :width 16)
  (memory ram :width 8 :addr-width 16 :endian :big)
  (instruction-word :width 16
    (field opcode 4)
    (field dst 2)
    (field src 10)))

(definstruction bigendian-word-test-machine set
  (modes word-imm)
  (encoding
    (opcode 1)
    (operand value :field src
      (variant (range -1 30) inline :bias 1)
      (variant :else (extra-word :escape #x3ff))))
  (semantics (set! a operand)))

(definstruction bigendian-word-test-machine hlt
  (encoding (opcode 2))
  (semantics (trap :halt)))

(fiveam:test word-encoded-instruction-word-big-endian-round-trip
  ;; SET #5 packs inline (bias 1 -> field value 6) -- opcode 1, dst 0, src 6
  ;; is word #x1006, whose two cells swap order under :BIG relative to
  ;; STEP-MACHINE-WORD-ENCODED-INLINE-ROUND-TRIP's little-endian equivalent
  ;; (tests/emulator.lisp). ASSEMBLE (not a direct FIND-INSTRUCTION +
  ;; ENCODE-INSTRUCTION call), since %CHOOSE-VARIANT is what picks the
  ;; inline sibling descriptor over the extra-word one for this value.
  (let ((a (assemble "set #5" :machine 'bigendian-word-test-machine)))
    (fiveam:is (equalp #(#x10 #x06) (assembly-cells a)))
    (multiple-value-bind (descriptor values)
        (decode-instruction-at (vector-cell-reader (assembly-cells a)) 0 'bigendian-word-test-machine :memory 'ram)
      (fiveam:is (string= "SET" (instruction-descriptor-name descriptor)))
      (fiveam:is (equal '(5) values)))))

(fiveam:test word-encoded-extra-word-big-endian-round-trip
  ;; SET #1000 escapes SRC (#x3FF) and carries 1000 (#x03E8) in its own
  ;; extra word -- proves %ENCODE-WORD-INSTRUCTION's second
  ;; %ENCODE-VALUE-CELLS call (the extra word) and %TRY-DECODE-WORD-
  ;; CANDIDATE's %FETCH-CELLS call both honour LAYOUT's own ENDIAN, not
  ;; just the instruction word's own first %ENCODE-VALUE-CELLS call.
  (let ((a (assemble "set #1000" :machine 'bigendian-word-test-machine)))
    (fiveam:is (equalp #(#x13 #xFF #x03 #xE8) (assembly-cells a)))
    (multiple-value-bind (descriptor values)
        (decode-instruction-at (vector-cell-reader (assembly-cells a)) 0 'bigendian-word-test-machine :memory 'ram)
      (fiveam:is (string= "SET" (instruction-descriptor-name descriptor)))
      (fiveam:is (equal '(1000) values)))))

;;; #64: per-instruction, non-uniform instruction-word layouts. WORD-LAYOUTS-
;;; TEST-MACHINE's field X is deliberately *not* nested across its three
;;; layouts -- width 4/shift 8 in the default, width 12/shift 0 in WIDE,
;;; width 2/shift 10 in NARROW -- so a bug that resolved :FIELD against the
;;; wrong layout would encode wrong bits, not merely fail to find a field or
;;; reproduce the default's own bits by coincidence.
;;;
;;;   default (4/4/8):  (field opcode 4) (field x 4) (field y 8)
;;;   wide    (4/12):   (field opcode 4) (field x 12)
;;;   narrow  (4/2/2/8): (field opcode 4) (field x 2) (field y 2) (field z 8)
;;;
;;; Every layout shares the OPCODE field (width 4, shift 12).

(defmachine word-layouts-test-machine
  (register pc :width 16)
  (register a :width 16)
  (register b :width 16)
  (memory ram :width 8 :addr-width 16)
  (instruction-word :width 16
    (field opcode 4) (field x 4) (field y 8)
    (layout wide (field opcode 4) (field x 12))
    (layout narrow (field opcode 4) (field x 2) (field y 2) (field z 8))))

(defmode word-layouts-xy expr "," expr)
(defmode word-layouts-x expr)
(defmode word-layouts-xyz expr "," expr "," expr)

(definstruction word-layouts-test-machine setx
  (modes word-layouts-xy)
  (encoding (opcode 1)
    (operand x :field x)
    (operand y :field y))
  (semantics (set! a x) (set! b y)))

(definstruction word-layouts-test-machine setwide
  (modes word-layouts-x)
  (encoding (opcode 2) (layout wide)
    (operand x :field x))
  (semantics (set! a x)))

(definstruction word-layouts-test-machine setnarrow
  (modes word-layouts-xyz)
  (encoding (opcode 3) (layout narrow)
    (operand x :field x)
    (operand y :field y)
    (operand z :field z))
  (semantics (set! a x) (set! b (+ y z))))

(definstruction word-layouts-test-machine hlt
  (encoding (opcode 4))
  (semantics (trap :halt)))

;; #137 fixtures: DISJX alone at opcode 10 (a conflicting DISJY is added only
;; via EVAL, by the test that expects it to be rejected). DISJY1/DISJY2 share
;; opcode 11, both on field Y with disjoint ranges -- genuinely
;; distinguishable. ORDERAB/ORDERBA share opcode 12 and both use fields X and
;; Y, but declare their (operand ...) subclauses in opposite order -- only
;; field Y's ranges (0..2 vs. 3..255) actually disagree; field X's ranges
;; fully overlap (0..15 both sides).

(definstruction word-layouts-test-machine disjx
  (modes word-layouts-x)
  (encoding (opcode 10) (operand v :field x (variant (range 0 3) inline)))
  (semantics (set! a v)))

(definstruction word-layouts-test-machine disjy1
  (modes word-layouts-x)
  (encoding (opcode 11) (operand v :field y (variant (range 0 100) inline)))
  (semantics (set! a v)))

(definstruction word-layouts-test-machine disjy2
  (modes word-layouts-x)
  (encoding (opcode 11) (operand v :field y (variant (range 200 255) inline)))
  (semantics (set! a v)))

(definstruction word-layouts-test-machine orderab
  (modes word-layouts-xy)
  (encoding (opcode 12)
    (operand x :field x)
    (operand y :field y (variant (range 0 2) inline)))
  (semantics (set! a x) (set! b y)))

(definstruction word-layouts-test-machine orderba
  (modes word-layouts-xy)
  (encoding (opcode 12)
    (operand y :field y (variant (range 3 255) inline))
    (operand x :field x))
  (semantics (set! a x) (set! b y)))

;; #140 fixtures: cross-layout co-tenancy, once the same-layout gate no
;; longer applies. WIDEVALLO (default layout, field Y, 8@0) and
;; NARROWVALHI (NARROW layout, field Z, also 8@0) share opcode 13 -- exact
;; same bits, different layout *and* different field name, disjoint ranges.
;; PINYA/PINZB share opcode 14, told apart purely by a (field-value ...) pin
;; on the same 8@0 bits under different layouts/field names -- their own X
;; holes (4@8 default vs. 2@10 narrow) overlap fully and never distinguish
;; them, so this is a true test of cross-layout pin comparison. PARTIALLO/
;; PARTIALHI share opcode 15 with only a *partial* bit overlap -- default's X
;; (4@8, range 0-3) and narrow's X (2@10, range 1-3) share only bits 10-11,
;; where PARTIALLO's range 0-3 always projects to 0 and PARTIALHI's range 1-3
;; projects to 1-3 -- disjoint despite neither hole being disjoint outright.

(definstruction word-layouts-test-machine widevallo
  (modes word-layouts-x)
  (encoding (opcode 13) (operand v :field y (variant (range 0 10) inline)))
  (semantics (set! a v)))

(definstruction word-layouts-test-machine narrowvalhi
  (modes word-layouts-x)
  (encoding (opcode 13) (layout narrow) (operand v :field z (variant (range 11 255) inline)))
  (semantics (set! a v)))

(definstruction word-layouts-test-machine pinya
  (modes word-layouts-x)
  (encoding (opcode 14) (field-value y 0)
    (operand x :field x))
  (semantics (set! a x)))

(definstruction word-layouts-test-machine pinzb
  (modes word-layouts-x)
  (encoding (opcode 14) (layout narrow) (field-value z 1)
    (operand x :field x))
  (semantics (set! a x)))

(definstruction word-layouts-test-machine partiallo
  (modes word-layouts-x)
  (encoding (opcode 15) (operand v :field x (variant (range 0 3) inline)))
  (semantics (set! a v)))

(definstruction word-layouts-test-machine partialhi
  (modes word-layouts-x)
  (encoding (opcode 15) (layout narrow) (operand v :field x (variant (range 1 3) inline)))
  (semantics (set! a v)))

(fiveam:test instruction-word-layout-alternates-share-width-and-opcode-field
  (let ((layout (machine-descriptor-instruction-word (find-machine-descriptor 'word-layouts-test-machine))))
    (fiveam:is (null (instruction-word-layout-name layout)))
    (fiveam:is (equal '(opcode 4 12) (instruction-word-field layout 'opcode)))
    (fiveam:is (equal '(x 4 8) (instruction-word-field layout 'x)))
    (fiveam:is (equal '(y 8 0) (instruction-word-field layout 'y)))
    (let ((wide (instruction-word-layout-named layout 'wide)))
      (fiveam:is (eq 'wide (instruction-word-layout-name wide)))
      (fiveam:is (= 2 (instruction-word-layout-width-cells wide)))
      (fiveam:is (equal '(opcode 4 12) (instruction-word-field wide 'opcode)))
      (fiveam:is (equal '(x 12 0) (instruction-word-field wide 'x))))
    (let ((narrow (instruction-word-layout-named layout 'narrow)))
      (fiveam:is (equal '(opcode 4 12) (instruction-word-field narrow 'opcode)))
      (fiveam:is (equal '(x 2 10) (instruction-word-field narrow 'x)))
      (fiveam:is (equal '(y 2 8) (instruction-word-field narrow 'y)))
      (fiveam:is (equal '(z 8 0) (instruction-word-field narrow 'z))))
    (fiveam:is (null (instruction-word-layout-named layout 'no-such-layout)))))

(fiveam:test instruction-word-layout-rejects-duplicate-layout-name
  (fiveam:signals machine-definition-error
    (eval '(defmachine bogus-word-layouts-machine
             (memory ram :width 8 :addr-width 8)
             (instruction-word :width 16
               (field opcode 4) (field x 12)
               (layout dup (field opcode 4) (field x 12))
               (layout dup (field opcode 4) (field x 12)))))))

(fiveam:test instruction-word-layout-widths-must-match-default
  (fiveam:signals machine-definition-error
    (eval '(defmachine bogus-word-layouts-machine
             (memory ram :width 8 :addr-width 8)
             (instruction-word :width 16
               (field opcode 4) (field x 12)
               (layout narrower (field opcode 4) (field x 4)))))))

(fiveam:test instruction-word-layout-opcode-field-width-must-match-default
  (fiveam:signals machine-definition-error
    (eval '(defmachine bogus-word-layouts-machine
             (memory ram :width 8 :addr-width 8)
             (instruction-word :width 16
               (field opcode 4) (field x 12)
               (layout wide-opcode (field opcode 8) (field x 8)))))))

(fiveam:test instruction-word-layout-opcode-field-shift-must-match-default
  (fiveam:signals machine-definition-error
    (eval '(defmachine bogus-word-layouts-machine
             (memory ram :width 8 :addr-width 8)
             (instruction-word :width 16
               (field opcode 4) (field x 12)
               (layout shifted-opcode (field x 12) (field opcode 4)))))))

(fiveam:test definstruction-layout-subclause-rejects-unknown-layout-name
  (fiveam:signals instruction-definition-error
    (eval '(definstruction word-layouts-test-machine bogus
             (modes word-layouts-x)
             (encoding (opcode 5) (layout no-such-layout)
               (operand x :field x))
             (semantics (set! a x))))))

(fiveam:test definstruction-layout-subclause-rejects-field-not-in-selected-layout
  (fiveam:signals instruction-definition-error
    (eval '(definstruction word-layouts-test-machine bogus
             (modes word-layouts-x)
             (encoding (opcode 5) (layout wide)
               (operand z :field z)) ; Z only exists in NARROW, not WIDE
             (semantics (set! a z))))))

(fiveam:test definstruction-layout-subclause-rejected-on-byte-machine
  (fiveam:signals instruction-definition-error
    (eval '(definstruction test-machine bogus
             (modes immediate)
             (encoding (opcode 200) (layout anything) (operand :mode))
             (semantics)))))

(fiveam:test definstruction-layout-subclause-rejected-on-no-operand-instruction
  (fiveam:signals instruction-definition-error
    (eval '(definstruction word-layouts-test-machine bogus
             (modes)
             (encoding (opcode 6) (layout wide))
             (semantics)))))

(fiveam:test definstruction-co-tenants-in-different-layouts-sharing-no-bits-are-rejected
  ;; #140: SETX (opcode 1, default layout) and a bogus second descriptor at
  ;; the same opcode naming WIDE instead -- mixing layouts is no longer
  ;; rejected outright, but this pair is still genuinely indistinguishable:
  ;; WIDE's X (12@0) overlaps both of SETX's fields (X 4@8, Y 8@0), and its
  ;; full 0-4095 range projects onto every shared bit range as the full set
  ;; either side ever produces, so no pair of fields disagrees.
  (handler-case
      (eval '(definstruction word-layouts-test-machine bogus
               (modes word-layouts-x)
               (encoding (opcode 1) (layout wide)
                 (operand x :field x))
               (semantics (set! a x))))
    (opcode-conflict (c)
      (fiveam:is (eq :indistinguishable (opcode-conflict-reason c))))
    (:no-error () (fiveam:fail "expected OPCODE-CONFLICT"))))

(fiveam:test encode-instruction-selects-fields-from-the-named-layout
  ;; SETX (default 4/4/8), SETWIDE (WIDE, 4/12), SETNARROW (NARROW, 4/2/2/8)
  ;; each pack their operands into a *different* bit position for the same
  ;; field name X -- the one test that actually pins layout-aware field
  ;; resolution, per the non-nested WORD-LAYOUTS-TEST-MACHINE fixture above.
  (let ((setx (first (find-instruction-variants 'word-layouts-test-machine "setx")))
        (setwide (first (find-instruction-variants 'word-layouts-test-machine "setwide")))
        (setnarrow (first (find-instruction-variants 'word-layouts-test-machine "setnarrow"))))
    (fiveam:is (null (instruction-descriptor-word-layout-name setx)))
    (fiveam:is (eq 'wide (instruction-descriptor-word-layout-name setwide)))
    (fiveam:is (eq 'narrow (instruction-descriptor-word-layout-name setnarrow)))
    ;; SETX 5, 200 -> opcode 1 << 12 | 5 << 8 | 200 = #x15C8
    (fiveam:is (equalp #(#xc8 #x15) (coerce (encode-instruction setx '(5 200)) 'vector)))
    ;; SETWIDE 4000 -> opcode 2 << 12 | 4000 = #x2FA0
    (fiveam:is (equalp #(#xa0 #x2f) (coerce (encode-instruction setwide '(4000)) 'vector)))
    ;; SETNARROW 3, 2, 100 -> opcode 3 << 12 | 3 << 10 | 2 << 8 | 100 = #x3E64
    (fiveam:is (equalp #(#x64 #x3e) (coerce (encode-instruction setnarrow '(3 2 100)) 'vector)))))

;;; Word-encoded constant discriminator fields (#136) -- (field-value FIELD-
;;; NAME n) pins a field to a literal with no operand hole at all, letting
;;; several descriptors share one opcode when nothing else tells them apart.
;;; FIELD-VALUE-TEST-MACHINE mirrors chip8word.lisp's own opcode-8 ALU shape:
;;; one 4/4/4/4 default layout, X an ordinary hole, N pinned per mnemonic --
;;; INCN/DECN both carry an X hole and differ only by their own pinned N;
;;; ZEROALL, at the same opcode, carries no operand at all and is
;;; discriminated purely by its own pinned N, mirroring CLS/RET/HLT's own
;;; opcode-0 shape in the extended example.

(defmachine field-value-test-machine
  (register pc :width 16)
  (register a :width 8 :count 4)
  (memory ram :width 8 :addr-width 16)
  (instruction-word :width 16
    (field opcode 4) (field x 4) (field y 4) (field n 4)))

(defmode field-value-x expr)

(definstruction field-value-test-machine incn
  (modes field-value-x)
  (encoding (opcode 1) (field-value n 1)
    (operand x :field x))
  (semantics (set! (a x) (wrap-value (1+ (a x)) 8))))

(definstruction field-value-test-machine decn
  (modes field-value-x)
  (encoding (opcode 1) (field-value n 2)
    (operand x :field x))
  (semantics (set! (a x) (wrap-value (1- (a x)) 8))))

(definstruction field-value-test-machine zeroall
  (encoding (opcode 1) (field-value n 0))
  (semantics (set! (a 0) 0) (set! (a 1) 0) (set! (a 2) 0) (set! (a 3) 0)))

(definstruction field-value-test-machine hlt
  (encoding (opcode 2))
  (semantics (trap :halt)))

(fiveam:test field-value-constant-recorded-on-the-descriptor
  (let ((incn (first (find-instruction-variants 'field-value-test-machine "incn")))
        (zeroall (first (find-instruction-variants 'field-value-test-machine "zeroall"))))
    (fiveam:is (equalp (list (make-word-constant :name 'n :width 4 :shift 0 :value 1))
                        (instruction-descriptor-word-constants incn)))
    (fiveam:is (equalp (list (make-word-constant :name 'n :width 4 :shift 0 :value 0))
                        (instruction-descriptor-word-constants zeroall)))
    (fiveam:is (null (instruction-descriptor-word-fields zeroall)))))

(fiveam:test field-value-constant-packs-into-declared-bits
  ;; INCN A2 -> opcode 1 << 12 | x=2 << 8 | y=0 << 4 | n=1 = #x1201
  (let ((incn (first (find-instruction-variants 'field-value-test-machine "incn"))))
    (fiveam:is (equalp #(#x01 #x12) (coerce (encode-instruction incn '(2)) 'vector)))))

(fiveam:test field-value-co-tenants-decode-and-execute-distinctly
  ;; INCN/DECN (each carrying an X hole) and ZEROALL (no operand at all)
  ;; all share opcode 1, told apart purely by their own pinned N (#136) --
  ;; no hole disagreement is needed or present between INCN and ZEROALL.
  (let ((a (assemble "  incn 0
  incn 0
  decn 0
  zeroall
  incn 1
  hlt" :machine 'field-value-test-machine)))
    (let ((m (make-machine 'field-value-test-machine)))
      (load-program m a)
      (multiple-value-bind (reason steps) (run m)
        (fiveam:is (eq :trap reason))
        (fiveam:is (= 6 steps))
        ;; A0: +1 +1 -1 then ZEROALL clears it back to 0; A1: ZEROALL clears
        ;; it, then +1 -> 1.
        (fiveam:is (= 0 (regref m 'a 0)))
        (fiveam:is (= 1 (regref m 'a 1)))))))

(fiveam:test field-value-disassembles-each-co-tenant-back-to-its-own-mnemonic
  (let ((a (assemble "  incn 0
  decn 0
  zeroall
  hlt" :machine 'field-value-test-machine)))
    (let ((lines (disassemble-cells (assembly-cells a) :machine 'field-value-test-machine)))
      (fiveam:is (equal '("INCN" "DECN" "ZEROALL" "HLT")
                         (mapcar (lambda (l) (instruction-descriptor-name (disassembly-line-descriptor l)))
                                 lines))))))

(fiveam:test field-value-rejected-on-byte-machine
  (fiveam:signals instruction-definition-error
    (eval '(definstruction test-machine bogus
             (modes immediate)
             (encoding (opcode 201) (field-value n 1) (operand :mode))
             (semantics)))))

(fiveam:test field-value-rejects-unknown-field
  (fiveam:signals instruction-definition-error
    (eval '(definstruction field-value-test-machine bogus
             (modes field-value-x)
             (encoding (opcode 3) (field-value zzz 1)
               (operand x :field x))
             (semantics (set! (a x) 0))))))

(fiveam:test field-value-rejects-opcode-as-target-field
  (fiveam:signals instruction-definition-error
    (eval '(definstruction field-value-test-machine bogus
             (modes field-value-x)
             (encoding (opcode 3) (field-value opcode 1)
               (operand x :field x))
             (semantics (set! (a x) 0))))))

(fiveam:test field-value-rejects-out-of-range-value
  (fiveam:signals instruction-definition-error
    (eval '(definstruction field-value-test-machine bogus
             (modes field-value-x)
             (encoding (opcode 3) (field-value n 16)
               (operand x :field x))
             (semantics (set! (a x) 0))))))

(fiveam:test field-value-rejects-duplicate-pin
  (fiveam:signals instruction-definition-error
    (eval '(definstruction field-value-test-machine bogus
             (modes field-value-x)
             (encoding (opcode 3) (field-value n 1) (field-value n 2)
               (operand x :field x))
             (semantics (set! (a x) 0))))))

(fiveam:test field-value-rejects-collision-with-operand-field
  (fiveam:signals instruction-definition-error
    (eval '(definstruction field-value-test-machine bogus
             (modes field-value-x)
             (encoding (opcode 3) (field-value x 1)
               (operand x :field x))
             (semantics (set! (a x) 0))))))

(fiveam:test field-value-indistinguishable-co-tenants-signal-error
  ;; A second descriptor at opcode 1 pinning N to a value ZEROALL/INCN/DECN
  ;; already claim -- no field tells them apart at decode time.
  (handler-case
      (eval '(definstruction field-value-test-machine bogus
               (encoding (opcode 1) (field-value n 0))
               (semantics nil)))
    (opcode-conflict (c)
      (fiveam:is (eq :indistinguishable (opcode-conflict-reason c))))
    (:no-error () (fiveam:fail "expected OPCODE-CONFLICT"))))

;;; #137: %HOLE-DISJOINT-P must pair two co-tenants' holes by the bits they
;;; occupy, not by index -- reusing WORD-LAYOUTS-TEST-MACHINE (default layout
;;; opcode 4/x 4 shift 8/y 8 shift 0) since its two non-opcode fields differ
;;; in both width and shift, the exact shape the positional comparison got
;;; wrong.

(fiveam:test definstruction-co-tenants-disjoint-in-different-fields-are-rejected
  ;; DISJX's only hole is field X (raw 0-3); a second descriptor at the same
  ;; opcode whose only hole is field Y (raw 4-255) -- comparing those two raw
  ;; value sets positionally looks disjoint, but the sets were never
  ;; comparable: X and Y occupy different bits, so a fetched word with X=0
  ;; and Y=4 matches both. No field ties them apart, so this must be
  ;; rejected as :INDISTINGUISHABLE.
  (handler-case
      (eval '(definstruction word-layouts-test-machine disjy
              (modes word-layouts-x)
              (encoding (opcode 10) (operand v :field y (variant (range 4 255) inline)))
              (semantics (set! a v))))
    (opcode-conflict (c)
      (fiveam:is (eq :indistinguishable (opcode-conflict-reason c))))
    (:no-error () (fiveam:fail "expected OPCODE-CONFLICT"))))

(fiveam:test definstruction-co-tenants-disjoint-in-the-same-field-are-accepted
  ;; DISJY1 and DISJY2 are genuinely distinguishable (field Y, disjoint
  ;; ranges) -- each must round-trip to its own mnemonic at decode time, not
  ;; merely have registered without error.
  (let ((v1 (assembly-cells (assemble "disjy1 50" :machine 'word-layouts-test-machine)))
        (v2 (assembly-cells (assemble "disjy2 210" :machine 'word-layouts-test-machine))))
    (fiveam:is (string= "DISJY1" (instruction-descriptor-name
                                   (decode-instruction-at (vector-cell-reader v1) 0 'word-layouts-test-machine))))
    (fiveam:is (string= "DISJY2" (instruction-descriptor-name
                                   (decode-instruction-at (vector-cell-reader v2) 0 'word-layouts-test-machine))))))

(fiveam:test definstruction-co-tenants-with-holes-in-different-order-pair-by-bits
  ;; ORDERAB declares its X hole before its Y hole; ORDERBA declares them the
  ;; other way round. Positionally, hole 0 of each (X's full 0-15 range vs.
  ;; Y's 3-255) and hole 1 of each (Y's 0-2 vs. X's full 0-15) both overlap,
  ;; so the old index-paired comparison would find no disjoint hole and
  ;; falsely reject an actually-distinguishable pair (field Y: {0,1,2} vs.
  ;; {3..255}, genuinely disjoint). Pairing by bits must still find it, and
  ;; each must round-trip to its own mnemonic.
  (let ((ab (assembly-cells (assemble "orderab 9, 1" :machine 'word-layouts-test-machine)))
        (ba (assembly-cells (assemble "orderba 200, 9" :machine 'word-layouts-test-machine))))
    (fiveam:is (string= "ORDERAB" (instruction-descriptor-name
                                    (decode-instruction-at (vector-cell-reader ab) 0 'word-layouts-test-machine))))
    (fiveam:is (string= "ORDERBA" (instruction-descriptor-name
                                    (decode-instruction-at (vector-cell-reader ba) 0 'word-layouts-test-machine))))))

;;; #140: two co-tenants may now name *different* instruction-word layouts,
;;; once #137's bit-based pairing makes the same-layout gate unnecessary --
;;; each pair below must round-trip through assemble/decode to its own
;;; mnemonic, not merely register without error.

(fiveam:test definstruction-co-tenants-in-different-layouts-same-bits-are-accepted
  ;; WIDEVALLO (default layout, field Y, 8@0) and NARROWVALHI (NARROW
  ;; layout, field Z, also 8@0) -- exact same bits, different layout and
  ;; different field name, disjoint ranges (0-10 vs. 11-255).
  (let ((lo (assembly-cells (assemble "widevallo 5" :machine 'word-layouts-test-machine)))
        (hi (assembly-cells (assemble "narrowvalhi 200" :machine 'word-layouts-test-machine))))
    (fiveam:is (string= "WIDEVALLO" (instruction-descriptor-name
                                       (decode-instruction-at (vector-cell-reader lo) 0 'word-layouts-test-machine))))
    (fiveam:is (string= "NARROWVALHI" (instruction-descriptor-name
                                         (decode-instruction-at (vector-cell-reader hi) 0 'word-layouts-test-machine))))))

(fiveam:test definstruction-co-tenants-in-different-layouts-distinguished-by-pin-are-accepted
  ;; PINYA (default layout, field-value Y=0) and PINZB (NARROW layout,
  ;; field-value Z=1) -- both pins land on the same 8@0 bits. Each also
  ;; carries an X hole (4@8 default, 2@10 narrow) that overlaps the other's
  ;; fully and never distinguishes them on its own -- only the pin does.
  (let ((a (assembly-cells (assemble "pinya 5" :machine 'word-layouts-test-machine)))
        (b (assembly-cells (assemble "pinzb 2" :machine 'word-layouts-test-machine))))
    (fiveam:is (string= "PINYA" (instruction-descriptor-name
                                  (decode-instruction-at (vector-cell-reader a) 0 'word-layouts-test-machine))))
    (fiveam:is (string= "PINZB" (instruction-descriptor-name
                                  (decode-instruction-at (vector-cell-reader b) 0 'word-layouts-test-machine))))))

(fiveam:test definstruction-co-tenants-in-different-layouts-without-a-pin-are-rejected
  ;; Same shape as PINYA/PINZB but with the pin removed from the second
  ;; descriptor -- pins the previous test's own claim that the X holes alone
  ;; never distinguish PINYA from PINZB, so PINYA + a PINZB-shaped no-pin
  ;; bogus descriptor must be :INDISTINGUISHABLE.
  (handler-case
      (eval '(definstruction word-layouts-test-machine bogus
               (modes word-layouts-x)
               (encoding (opcode 14) (layout narrow)
                 (operand x :field x))
               (semantics (set! a x))))
    (opcode-conflict (c)
      (fiveam:is (eq :indistinguishable (opcode-conflict-reason c))))
    (:no-error () (fiveam:fail "expected OPCODE-CONFLICT"))))

(fiveam:test definstruction-co-tenants-with-partial-bit-overlap-are-accepted
  ;; PARTIALLO (default X, 4@8, range 0-3) and PARTIALHI (NARROW X, 2@10,
  ;; range 1-3) share only bits 10-11 of their respective fields -- neither
  ;; hole is disjoint from the other outright (both ranges are subsets of
  ;; 0-15), but PARTIALLO's range always projects to 0 on those two bits
  ;; while PARTIALHI's projects to 1-3, so the fields disagree once narrowed
  ;; to the bits they actually share (#140's generalization of #137).
  (let ((lo (assembly-cells (assemble "partiallo 2" :machine 'word-layouts-test-machine)))
        (hi (assembly-cells (assemble "partialhi 2" :machine 'word-layouts-test-machine))))
    (fiveam:is (string= "PARTIALLO" (instruction-descriptor-name
                                      (decode-instruction-at (vector-cell-reader lo) 0 'word-layouts-test-machine))))
    (fiveam:is (string= "PARTIALHI" (instruction-descriptor-name
                                      (decode-instruction-at (vector-cell-reader hi) 0 'word-layouts-test-machine))))))

(definstruction word-layouts-test-machine jointa
  (modes word-layouts-x)
  (encoding (opcode 7) (layout narrow) (field-value x 1) (field-value y 0)
    (operand v :field z))
  (semantics (set! a v)))

(definstruction word-layouts-test-machine jointb
  (modes word-layouts-xy)
  (encoding (opcode 7)
    (operand x :field x (variant (range 5 8) inline))
    (operand y :field y))
  (semantics (set! a x) (set! b y)))

(fiveam:test definstruction-co-tenants-distinguished-only-jointly-are-accepted
  ;; JOINTA's narrow x=1 and y=0 pins together fix default X to 4. Each pin
  ;; alone overlaps JOINTB's X range 5-8, but no word satisfies both.
  (let ((a (assembly-cells (assemble "jointa 9" :machine 'word-layouts-test-machine)))
        (b (assembly-cells (assemble "jointb 6, 9" :machine 'word-layouts-test-machine))))
    (fiveam:is (string= "JOINTA" (instruction-descriptor-name
                                   (decode-instruction-at (vector-cell-reader a) 0 'word-layouts-test-machine))))
    (fiveam:is (string= "JOINTB" (instruction-descriptor-name
                                   (decode-instruction-at (vector-cell-reader b) 0 'word-layouts-test-machine))))))

(fiveam:test definstruction-co-tenants-overlapping-jointly-are-rejected
  ;; Widening X to 4-8 admits the word JOINTA accepts.
  (handler-case
      (eval '(definstruction word-layouts-test-machine bogus
               (modes word-layouts-xy)
               (encoding (opcode 7)
                 (operand x :field x (variant (range 4 8) inline))
                 (operand y :field y))
               (semantics (set! a x) (set! b y))))
    (opcode-conflict (c)
      (fiveam:is (eq :indistinguishable (opcode-conflict-reason c))))
    (:no-error () (fiveam:fail "expected OPCODE-CONFLICT"))))

(fiveam:test raw-interval-projection-matches-small-value-sets
  (loop for field-width from 1 to 5 do
    (loop for offset below field-width do
      (loop for width from 1 to (- field-width offset) do
        (loop for lo from 0 below (ash 1 field-width) do
          (loop for hi from lo below (ash 1 field-width) do
            (let ((expected (remove-duplicates
                             (loop for value from lo to hi
                                   collect (ldb (byte width offset) value))))
                  (actual (loop for (start . end) in (%project-raw-interval
                                                      (cons lo hi) offset width)
                                append (loop for value from start to end collect value))))
              (fiveam:is (equal (sort expected #'<) (sort actual #'<))))))))))

(fiveam:test bit-constraint-intervals-match-small-value-set-oracle
  (flet ((raw-values (intervals)
           (loop for (lo . hi) in intervals append (loop for value from lo to hi collect value)))
         (project (value shift overlap-shift width)
           (ldb (byte width (- overlap-shift shift)) value)))
    (loop for shift-a from 0 to 2 do
      (loop for shift-b from 0 to 2 do
        (loop for lo-a from 0 to 7 do
          (loop for hi-a from lo-a to 7 do
            (loop for lo-b from 0 to 7 do
              (loop for hi-b from lo-b to 7 do
                (let* ((overlap-shift (max shift-a shift-b))
                       (width (- (min (+ shift-a 3) (+ shift-b 3)) overlap-shift))
                       (values-a (raw-values (list (cons lo-a hi-a))))
                       (values-b (raw-values (list (cons lo-b hi-b))))
                       (expected
                         (notany (lambda (a)
                                   (member (project a shift-a overlap-shift width)
                                           values-b
                                           :key (lambda (b)
                                                  (project b shift-b overlap-shift width))))
                                 values-a)))
                  (fiveam:is (eq expected
                                 (%bit-constraints-disjoint-p
                                  (list 3 shift-a (list (cons lo-a hi-a)))
                                  (list 3 shift-b (list (cons lo-b hi-b)))))))))))))))

(defmachine wide-constraint-test-machine
  (register pc :width 32)
  (memory ram :width 8 :addr-width 16)
  (instruction-word :width 32 (field opcode 8) (field src 24)))

(defmode wide-constraint-imm "#" expr)

(definstruction wide-constraint-test-machine widelow
  (modes wide-constraint-imm)
  (encoding (opcode 1) (operand v :field src (variant (range 0 #xfffffe) inline)))
  (semantics nil))

(definstruction wide-constraint-test-machine widehigh
  (modes wide-constraint-imm)
  (encoding (opcode 1) (operand v :field src (variant (range #xffffff #xffffff) inline)))
  (semantics nil))

(fiveam:test wide-word-field-co-tenants-register-and-decode
  (fiveam:is (= 2 (length (find-instruction-descriptors-by-opcode
                           'wide-constraint-test-machine 1))))
  (dolist (case '(("widelow #42" "WIDELOW")
                  ("widehigh #16777215" "WIDEHIGH")))
    (destructuring-bind (source expected-name) case
      (let ((cells (assembly-cells (assemble source :machine 'wide-constraint-test-machine))))
        (fiveam:is (string= expected-name
                            (instruction-descriptor-name
                             (decode-instruction-at (vector-cell-reader cells) 0
                                                    'wide-constraint-test-machine))))))))

;;; %CONSTRAINTS-SATISFIABLE-P walks bits rather than enumerating fields, so
;;; it is checked against brute force over a small word.

(fiveam:test constraints-satisfiable-matches-brute-force
  (let ((state 141))
    (flet ((rand (n)
             (setf state (mod (+ (* state 1103515245) 12345) (ash 1 31)))
             (mod (ash state -8) n)))
      (dotimes (trial 600)
        (let ((constraints nil) (shift 0))
          (dotimes (i (1+ (rand 3)))
            (let* ((width (1+ (rand 4)))
                   (intervals (loop repeat (rand 3)
                                    collect (let ((lo (rand (ash 1 width))))
                                              (cons lo (+ lo (rand (- (ash 1 width) lo))))))))
              (cl:push (list width (+ shift (rand 3)) intervals) constraints)
              (setf shift (+ shift (rand 4)))))
          (let ((expected (loop for word below (ash 1 12)
                                  thereis (every (lambda (c)
                                                   (destructuring-bind (width shift intervals) c
                                                     (let ((v (ldb (byte width shift) word)))
                                                       (some (lambda (iv) (<= (car iv) v (cdr iv)))
                                                             intervals))))
                                                 constraints))))
            (fiveam:is (eq (and expected t)
                           (%constraints-satisfiable-p constraints)))))))))

(defmachine wide-fallback-test-machine
  (register pc :width 32)
  (memory ram :width 8 :addr-width 16)
  (instruction-word :width 32 (field opcode 8) (field src 24)))

(defmode wide-fallback-imm "#" expr)

(definstruction wide-fallback-test-machine widefbgen
  (modes wide-fallback-imm)
  (encoding (opcode 1) (fallback) (operand v :field src))
  (semantics nil))

(definstruction wide-fallback-test-machine widefbpin
  (encoding (opcode 1) (field-value src #x123456))
  (semantics nil))

(fiveam:test fallback-contains-co-tenant-on-a-wide-field
  (flet ((decoded (source)
           (instruction-descriptor-name
            (decode-instruction-at
             (vector-cell-reader (assembly-cells (assemble source :machine 'wide-fallback-test-machine)))
             0 'wide-fallback-test-machine))))
    (fiveam:is (string= "WIDEFBGEN" (decoded "widefbgen #5")))
    (fiveam:signals assembly-error
      (assemble "widefbgen #1193046" :machine 'wide-fallback-test-machine))))

(fiveam:test wide-field-co-tenants-are-checked-exactly
  ;; A pin just outside a 24-bit range is accepted; inside it, rejected.
  (eval '(definstruction wide-constraint-test-machine widepin
           (encoding (opcode 2) (field-value src #x123456))
           (semantics nil)))
  (eval '(definstruction wide-constraint-test-machine widerange
           (modes wide-constraint-imm)
           (encoding (opcode 2) (operand v :field src (variant (range 0 #x123455) inline)))
           (semantics nil)))
  (handler-case
      (eval '(definstruction wide-constraint-test-machine widebad
               (modes wide-constraint-imm)
               (encoding (opcode 2) (operand v :field src (variant (range 0 #x123456) inline)))
               (semantics nil)))
    (opcode-conflict (c) (fiveam:is (eq :indistinguishable (opcode-conflict-reason c))))
    (:no-error () (fiveam:fail "expected OPCODE-CONFLICT"))))

(defmachine wide-joint-test-machine
  (register pc :width 32)
  (memory ram :width 8 :addr-width 16)
  (instruction-word :width 32
    (field opcode 8) (field src 24)
    (layout split (field opcode 8) (field hi 12) (field lo 12))))

(defmode wide-joint-imm "#" expr)

(definstruction wide-joint-test-machine widejointa
  (encoding (opcode 1) (layout split) (field-value hi #x123) (field-value lo #x456))
  (semantics nil))

(definstruction wide-joint-test-machine widejointb
  (modes wide-joint-imm)
  (encoding (opcode 1) (operand v :field src (variant (range 0 #x123455) inline)))
  (semantics nil))

(fiveam:test wide-co-tenants-distinguished-only-jointly-are-accepted
  ;; WIDEJOINTA pins both halves of the 24-bit value #x123456. Each pin alone
  ;; overlaps WIDEJOINTB's range, but the combined value lies just above it.
  (flet ((decoded (source)
           (instruction-descriptor-name
            (decode-instruction-at
             (vector-cell-reader (assembly-cells (assemble source :machine 'wide-joint-test-machine)))
             0 'wide-joint-test-machine))))
    (fiveam:is (string= "WIDEJOINTA" (decoded "widejointa")))
    (fiveam:is (string= "WIDEJOINTB" (decoded "widejointb #5")))))

(fiveam:test wide-co-tenants-overlapping-jointly-are-rejected
  (handler-case
      (eval '(definstruction wide-joint-test-machine widejointbad
               (modes wide-joint-imm)
               (encoding (opcode 1) (operand v :field src (variant (range 0 #x123456) inline)))
               (semantics nil)))
    (opcode-conflict (c) (fiveam:is (eq :indistinguishable (opcode-conflict-reason c))))
    (:no-error () (fiveam:fail "expected OPCODE-CONFLICT"))))

;;; #138: an (operand ... :field opcode) hole would OR its own bits into the
;;; already-placed opcode field at encode time (%ENCODE-WORD-INSTRUCTION) --
;;; rejected the same way (field-value opcode ...) already is (#136, above).
;;; Two operand subclauses naming the same field carry the identical hazard.

(fiveam:test word-operand-rejects-opcode-as-target-field
  (fiveam:signals instruction-definition-error
    (eval '(definstruction word-layouts-test-machine bogus
             (modes word-layouts-x)
             (encoding (opcode 13) (operand v :field opcode))
             (semantics (set! a v))))))

(fiveam:test word-operand-rejects-duplicate-target-field
  (fiveam:signals instruction-definition-error
    (eval '(definstruction word-layouts-test-machine bogus
             (modes word-layouts-xy)
             (encoding (opcode 13)
               (operand x :field x)
               (operand y :field x))
             (semantics (set! a x) (set! b y))))))

;;; Variant expansion / registration

(fiveam:test word-instruction-expands-into-one-descriptor-per-variant
  ;; SET's :else fallback declares no :CELLS, so it defaults to the layout's
  ;; own WIDTH-CELLS -- 2, for this 16-bit word / 8-bit cell machine (#135).
  (let ((variants (find-instruction-variants 'word-test-machine "SET")))
    (fiveam:is (= 2 (length variants)))
    (fiveam:is (equal '(0 2) (mapcar #'instruction-descriptor-extra-cells variants)))
    (fiveam:is (every (lambda (d) (= 1 (instruction-descriptor-opcode d))) variants))))

(fiveam:test word-instruction-no-operand-descriptor
  (let ((hlt (find-instruction 'word-test-machine 'hlt)))
    (fiveam:is (null (instruction-descriptor-word-fields hlt)))
    (fiveam:is (= 0 (instruction-descriptor-extra-cells hlt)))))

;;; Decodability checks (#20's own ambiguity, %CHECK-WORD-VARIANTS)

(fiveam:test word-variant-inline-range-overflowing-field-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction word-test-machine bogus
             (modes word-imm)
             (encoding (opcode 3)
                       (operand value :field src
                         (variant (range 0 2000) inline)
                         (variant :else (extra-word :escape #x3ff))))
             (semantics nil)))))

(fiveam:test word-variant-escape-overflowing-field-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction word-test-machine bogus
             (modes word-imm)
             (encoding (opcode 3)
                       (operand value :field src
                         (variant (range 0 10) inline)
                         (variant :else (extra-word :escape 2000))))
             (semantics nil)))))

(fiveam:test word-variant-escape-colliding-with-inline-range-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction word-test-machine bogus
             (modes word-imm)
             (encoding (opcode 3)
                       (operand value :field src
                         (variant (range 0 30) inline)
                         (variant :else (extra-word :escape 30))))
             (semantics nil)))))

;; #63: the value-selected counterpart of CHOICE-OVERLAPPING-INLINE-RANGES-
;; SIGNAL-ERROR below -- two ordinary (RANGE lo hi) variants (no (CHOICE m)
;; selector at all) whose raw footprints overlap purely through their own
;; :BIAS, unreachable before #104 gave a field room for more than one
;; value-selected :INLINE variant. The check itself (%CHECK-WORD-VARIANTS'
;; pairwise raw-chunk overlap loop) already covers this -- it does not
;; distinguish CHOICE-selected from value-selected variants -- this test
;; only closes the ticket-#63 gap of having no coverage for this exact shape.
(fiveam:test word-variant-value-selected-overlapping-inline-ranges-signal-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction word-test-machine bogus
             (modes word-imm)
             (encoding (opcode 3)
                       (operand value :field src
                         (variant (range 0 7) inline :bias 0)
                         (variant (range 0 7) inline :bias 4)))
             (semantics nil)))))

;;; :SIGNED on a value-selected word field (#63) -- before this, a
;;; word-encoded hole's signedness came only from a (CHOICE m) variant's own
;;; mode (#127) or from a :RELATIVE hole (#62); a plain :SIGNED T mode had no
;;; effect on a value-selected variant at all, and its declared negative
;;; range was rejected outright as failing the field's unsigned bound.
;;; SIGNED-WORD-TEST-MACHINE's SRC field is 5 bits (0..31 unsigned,
;;; -16..15 signed) -- wide enough to leave room, inside the same field,
;;; for both an inline range narrower than the full signed span and an
;;; escape marker distinct from it.

(defmachine signed-word-test-machine
  (register pc :width 16)
  (register a :width 16)
  (memory ram :width 8 :addr-width 16)
  (instruction-word :width 16
    (field opcode 11)
    (field src 5)))

(defmode word-signed-imm "#" expr :signed t)

(definstruction signed-word-test-machine signset
  (modes word-signed-imm)
  (encoding
    (opcode 1)
    (operand value :field src
      (variant (range -8 7) inline)
      (variant :else (extra-word :escape 16))))
  (semantics (set! a operand)))

(fiveam:test signed-word-field-value-selected-inline-range-accepted
  ;; Before #63 this DEFINSTRUCTION itself would have signalled an error --
  ;; a negative-LO range validated against the field's *unsigned* bound.
  (let ((variants (find-instruction-variants 'signed-word-test-machine "SIGNSET")))
    (fiveam:is (= 2 (length variants)))
    (fiveam:is (every (lambda (d) (word-field-choice-signedp (first (instruction-descriptor-word-fields d))))
                       variants))))

(fiveam:test signed-word-field-value-selected-implicit-default-uses-signed-bound
  ;; No (variant ...) forms at all on a :SIGNED T hole -- the implicit
  ;; default range must be the field's *signed* bound (-16..15), mirroring
  ;; the existing :RELATIVE behavior (#62), not [0, 31].
  (eval '(definstruction signed-word-test-machine signset2
           (modes word-signed-imm)
           (encoding (opcode 2) (operand value :field src))
           (semantics (set! a value))))
  (let ((d (find-instruction 'signed-word-test-machine 'signset2)))
    (fiveam:is (word-field-choice-signedp (first (instruction-descriptor-word-fields d))))
    (fiveam:is (equal '(-16 . 15) (word-field-choice-range (first (instruction-descriptor-word-fields d)))))))

(fiveam:test signed-word-field-inline-encode-decode-round-trips-a-negative-value
  (let* ((cells (assembly-cells (assemble "signset #-5" :machine 'signed-word-test-machine))))
    (fiveam:is (= 2 (length cells))) ; fits inline, no extra word
    (multiple-value-bind (descriptor values) (decode-instruction-at (vector-cell-reader cells) 0 'signed-word-test-machine)
      (fiveam:is (string= "SIGNSET" (instruction-descriptor-name descriptor)))
      (fiveam:is (equal '(-5) values)))))

(fiveam:test signed-word-field-escapes-to-extra-word-and-decodes-signed
  ;; The other half of ticket #63's item 2: a signed value whose magnitude
  ;; exceeds the inline range's bias must still be representable, via the
  ;; existing :ELSE escape -- and the escaped extra word itself must decode
  ;; back signed (decoder.lisp's %TRY-DECODE-WORD-CANDIDATE), not just the
  ;; inline path.
  (let* ((cells (assembly-cells (assemble "signset #-5000" :machine 'signed-word-test-machine))))
    (fiveam:is (= 4 (length cells))) ; instruction word + one extra word
    (multiple-value-bind (descriptor values) (decode-instruction-at (vector-cell-reader cells) 0 'signed-word-test-machine)
      (fiveam:is (string= "SIGNSET" (instruction-descriptor-name descriptor)))
      (fiveam:is (equal '(-5000) values)))))

(fiveam:test signed-word-field-executes-negative-inline-and-escaped-values
  (dolist (case '(("signset #-5" . -5) ("signset #-5000" . -5000)))
    (destructuring-bind (source . expected) case
      (let ((m (make-machine 'signed-word-test-machine)))
        (load-program m (assemble source :machine 'signed-word-test-machine))
        (multiple-value-bind (reason steps) (run m :max-steps 1)
          (declare (ignore steps))
          (fiveam:is (eq :max-steps reason))
          (fiveam:is (= (wrap-value expected 16) (sref m 'a))))))))

;; #63: disjointness between two co-tenant descriptors' value-selected
;; fields must be checked in raw (two's-complement-wrapped) bit-pattern
;; space, chunk-against-chunk, not naively over declared value-space ranges
;; -- exactly %CHECK-WORD-VARIANTS' own overlap check already does for two
;; variants of *one* field (#127), now exercised across two *different*
;; mnemonics' fields sharing one opcode (%CHECK-OPCODE-DECODABLE!'s
;; raw field intervals). SD-NEG's signed range
;; -5..5 splits into raw chunks (0..5) and (27..31) in a 5-bit field; SD-POS's
;; unsigned 6..26 sits entirely in the gap between them.

(defmachine signed-disjoint-test-machine
  (register pc :width 16)
  (register a :width 16)
  (register b :width 16)
  (memory ram :width 8 :addr-width 16)
  (instruction-word :width 16
    (field opcode 11)
    (field src 5)))

(defmode sd-imm "#" expr)
(defmode sd-signed-imm "#" expr :signed t)

(definstruction signed-disjoint-test-machine sdpos
  (modes sd-imm)
  (encoding (opcode 1) (operand v :field src (variant (range 6 26) inline)))
  (semantics (set! a v)))

(definstruction signed-disjoint-test-machine sdneg
  (modes sd-signed-imm)
  (encoding (opcode 1) (operand v :field src (variant (range -5 5) inline)))
  (semantics (set! b v)))

(fiveam:test signed-word-field-two-chunk-raw-footprint-disjoint-from-sibling-mnemonic
  (fiveam:is (= 2 (length (find-instruction-descriptors-by-opcode 'signed-disjoint-test-machine 1))))
  (let ((pos (assembly-cells (assemble "sdpos #20" :machine 'signed-disjoint-test-machine)))
        (neg (assembly-cells (assemble "sdneg #-3" :machine 'signed-disjoint-test-machine))))
    (fiveam:is (string= "SDPOS" (instruction-descriptor-name
                                  (decode-instruction-at (vector-cell-reader pos) 0 'signed-disjoint-test-machine))))
    (fiveam:is (string= "SDNEG" (instruction-descriptor-name
                                  (decode-instruction-at (vector-cell-reader neg) 0 'signed-disjoint-test-machine))))))

(fiveam:test signed-word-field-two-chunk-raw-footprint-collision-signals-error
  ;; SDBAD's 20..31 range overlaps SDNEG's high (negative-wrapped) chunk
  ;; 27..31 -- only detectable if the two-chunk split is honored rather than
  ;; SDNEG's declared value-space range (-5..5) being compared directly
  ;; against SDBAD's (20..31), which never numerically overlap.
  (fiveam:signals opcode-conflict
    (eval '(definstruction signed-disjoint-test-machine sdbad
             (modes sd-imm)
             (encoding (opcode 1) (operand v :field src (variant (range 20 31) inline)))
             (semantics (set! a v))))))

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

(fiveam:test choice-selected-siblings-share-one-semantics-fn
  ;; #150: WCX's two combos come from one tuple (WC-TWO has no varying ONE-OF
  ;; element), so %WORD-MODE-DESCRIPTOR-FORMS builds their SEMANTICS-FN,
  ;; WORD-ALTERNATIVES, and WORD-CONSTANTS once and shares the same object
  ;; across both -- rather than re-emitting (and so re-compiling) an
  ;; identical semantics lambda and variant-menu builder once per combo, the
  ;; actual code-size blow-up #150 was filed against.
  (let ((variants (find-instruction-variants 'word-test-machine "WCX")))
    (fiveam:is (= 2 (length variants)))
    (fiveam:is (eq (instruction-descriptor-semantics-fn (first variants))
                   (instruction-descriptor-semantics-fn (second variants))))
    (fiveam:is (eq (instruction-descriptor-word-alternatives (first variants))
                   (instruction-descriptor-word-alternatives (second variants))))
    (fiveam:is (eq (instruction-descriptor-word-constants (first variants))
                   (instruction-descriptor-word-constants (second variants))))))

;; #132: the byte-machine hazard above has a word-machine equivalent --
;; %WORD-FIELD-CHOICE-FORM stamps a CHOICE-selected variant's SIGNEDP from
;; the alternative it names, never from MODE's own SIGNEDP -- so a
;; whole-mode :SIGNED T mode whose single hole is a ONE-OF is rejected here
;; too, by the same %CHECK-MODE-HOLE-ATTRIBUTES call every DEFINSTRUCTION
;; path makes before branching on encoding scheme.
(defmode wc-signed-one-of-hole-test-mode (one-of wc-reg wc-ind) :signed t)

(fiveam:test word-signed-mode-whose-single-hole-is-a-one-of-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction word-test-machine bogus
             (modes wc-signed-one-of-hole-test-mode)
             (encoding (opcode 5)
                       (operand value :field src
                         (variant (choice wc-reg) inline :range (0 7))
                         (variant (choice wc-ind) inline :range (8 15))))
             (semantics nil)))))

(fiveam:test choice-inline-without-range-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction word-test-machine bogus
             (modes wc-two)
             (encoding (opcode 5)
                       (operand value :field src
                         (variant (choice wc-reg) inline :bias 3)
                         (variant (choice wc-ind) inline :range (0 7))))
             (semantics nil)))))

(fiveam:test choice-naming-unregistered-mode-signals-error
  (fiveam:signals instruction-definition-error
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
  (fiveam:signals instruction-definition-error
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
  (fiveam:signals instruction-definition-error
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
  (fiveam:signals instruction-definition-error
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
  (fiveam:signals instruction-definition-error
    (eval '(definstruction word-test-machine bogus
             (modes wc-three)
             (encoding (opcode 5)
                       (operand value :field src
                         (variant (choice wc-reg) inline :range (0 7))
                         (variant (range 8 15) inline)
                         (variant :else (extra-word :escape #x3ff))))
             (semantics nil)))))

(fiveam:test choice-overlapping-inline-ranges-signal-error
  (fiveam:signals instruction-definition-error
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
;; one mode's name, exactly the shape %NARROW-TO-FORCED-MODE's own
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
  (fiveam:signals instruction-definition-error
    (eval '(definstruction word-test-machine bogus
             (modes wc-two)
             (encoding (opcode 5)
                       (operand value :field src
                         (variant (choice wc-reg) (extra-word :escape #x3ff))
                         (variant (choice wc-ind) (extra-word :escape #x3ff))))
             (semantics nil)))))

;;; Alias variants (#187) -- a second spelling of one canonical escape.

(defmachine alias-test-machine
  (register pc :width 16)
  (register a :width 16)
  (memory ram :width 8 :addr-width 16)
  (instruction-word :width 16
    (field opcode 4)
    (field dst 2)
    (field src 10)))

(defmode wc-pick "pick" expr)
(defmode wc-alias (one-of wc-pick wc-ind wc-reg))

(definstruction alias-test-machine wcal
  (modes wc-alias)
  (encoding
    (opcode 8)
    (operand value :field src
      (variant (choice wc-reg) inline :range (0 7) :bias #x00)
      (variant (choice wc-ind) (extra-word :escape #x3ff))
      (variant (choice wc-pick) (extra-word :escape #x3ff :alias t))))
  (semantics (set! a value)))

(fiveam:test alias-variant-stamps-word-field-choice
  (let ((flags (mapcar (lambda (d) (word-field-choice-alias (first (instruction-descriptor-word-fields d))))
                       (find-instruction-variants 'alias-test-machine "WCAL"))))
    (fiveam:is (= 3 (length flags)))
    (fiveam:is (= 1 (count t flags)))))

(fiveam:test alias-spellings-assemble-identically
  (fiveam:is (equalp (assembly-cells (assemble "wcal [5]" :machine 'alias-test-machine))
                     (assembly-cells (assemble "wcal pick 5" :machine 'alias-test-machine)))))

(fiveam:test alias-word-decodes-to-canonical-spelling
  (let ((lines (disassemble-assembly (assemble "wcal pick 5" :machine 'alias-test-machine)
                                     :machine 'alias-test-machine :labels nil)))
    (fiveam:is (string= "wcal [$5]" (disassembly-line-text (first lines))))))

(fiveam:test alias-without-canonical-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction alias-test-machine bogus
             (modes wc-alias)
             (encoding (opcode 5)
                       (operand value :field src
                         (variant (choice wc-reg) inline :range (0 7) :bias #x00)
                         (variant (choice wc-pick) (extra-word :escape #x3ff :alias t))))
             (semantics nil)))))

(fiveam:test alias-disagreeing-on-cells-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction alias-test-machine bogus
             (modes wc-two)
             (encoding (opcode 5)
                       (operand value :field src
                         (variant (choice wc-reg) (extra-word :escape #x3ff))
                         (variant (choice wc-ind) (extra-word :escape #x3ff :cells 3 :alias t))))
             (semantics nil)))))

(fiveam:test two-non-alias-escapes-still-signal-with-alias-present
  (fiveam:signals instruction-definition-error
    (eval '(definstruction alias-test-machine bogus
             (modes wc-alias)
             (encoding (opcode 5)
                       (operand value :field src
                         (variant (choice wc-reg) (extra-word :escape #x3ff))
                         (variant (choice wc-ind) (extra-word :escape #x3ff))
                         (variant (choice wc-pick) (extra-word :escape #x3ff :alias t))))
             (semantics nil)))))

(fiveam:test alias-on-inline-or-else-variant-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction alias-test-machine bogus
             (modes wc-two)
             (encoding (opcode 5)
                       (operand value :field src
                         (variant (choice wc-reg) inline :range (0 7) :alias t)
                         (variant (choice wc-ind) (extra-word :escape #x3ff))))
             (semantics nil))))
  (fiveam:signals instruction-definition-error
    (eval '(definstruction alias-test-machine bogus
             (modes wc-two)
             (encoding (opcode 5)
                       (operand value :field src
                         (variant :else (extra-word :escape #x3ff :alias t))))
             (semantics nil)))))

;;; Varying hole counts across ONE-OF alternatives (#120) -- a ONE-OF hole
;;; whose alternatives disagree on hole count, on a word-encoded machine,
;;; gated on the governing field being wholly CHOICE-selected (or #118-mixed
;;; with the unclaimed alternative sharing the base hole count). Expands one
;;; fixed-arity INSTRUCTION-DESCRIPTOR per alternative-tuple
;;; (mode.lisp's %MODE-HOLE-TUPLES) rather than one shared shape.

(defmachine varying-hole-test-machine
  (register pc :width 16)
  (register a :width 16 :count 8)
  (memory ram :width 16 :addr-width 16)
  (instruction-word :width 16
    (field src 6)
    (field dst 5)
    (field opcode 5)))

(defmode vh-reg expr)
(defmode vh-idx "[" expr "," expr "]")
(defmode vh-mode expr "," (one-of vh-reg vh-idx))

(definstruction varying-hole-test-machine ldv
  (modes vh-mode)
  (encoding
    (opcode 1)
    (operand dst :field dst)
    (operand src :field src
      (variant (choice vh-reg) inline :range (0 7) :bias #x00)
      (variant (choice vh-idx) inline :range (0 7) :bias #x10))
    (for-choice vh-idx (operand off :trailing-word)))
  (semantics
    (choice-case src
      (vh-reg (set! (a dst) (a src)))
      (vh-idx (set! (a dst) (mref machine 'ram (+ (a src) off)))))))

(fiveam:test varying-hole-counts-expand-one-descriptor-per-tuple
  (let ((variants (find-instruction-variants 'varying-hole-test-machine 'ldv)))
    (fiveam:is (= 2 (length variants)))
    (let ((one-hole (find 2 variants :key (lambda (d) (length (instruction-descriptor-operand-names d)))))
          (two-hole (find 3 variants :key (lambda (d) (length (instruction-descriptor-operand-names d))))))
      (fiveam:is (equal '(dst src) (instruction-descriptor-operand-names one-hole)))
      (fiveam:is (equal '(dst src off) (instruction-descriptor-operand-names two-hole)))
      (fiveam:is (= 1 (instruction-descriptor-size one-hole)))
      (fiveam:is (= 2 (instruction-descriptor-size two-hole)))
      (fiveam:is (= 0 (instruction-descriptor-extra-cells one-hole)))
      (fiveam:is (= 1 (instruction-descriptor-extra-cells two-hole))))))

(fiveam:test varying-hole-tuples-share-one-normalized-semantics-fn
  ;; Tuples retain separate decode menus, but normalize their varying operand
  ;; lists before invoking the one semantics function for the instruction.
  (let* ((variants (find-instruction-variants 'varying-hole-test-machine 'ldv))
         (one-hole (find 2 variants :key (lambda (d) (length (instruction-descriptor-operand-names d)))))
         (two-hole (find 3 variants :key (lambda (d) (length (instruction-descriptor-operand-names d))))))
    (fiveam:is (eq (instruction-descriptor-semantics-fn one-hole)
                   (instruction-descriptor-semantics-fn two-hole)))
    (fiveam:is (not (eq (instruction-descriptor-word-alternatives one-hole)
                         (instruction-descriptor-word-alternatives two-hole))))))

(fiveam:test varying-hole-counts-governing-field-filtered-per-tuple
  ;; The one-hole tuple's own field-SRC menu must NOT include the two-hole
  ;; alternative's variant -- otherwise a value in VH-IDX's own 0x10-0x17
  ;; range would be judged decodable by the one-hole descriptor too (this is
  ;; the load-bearing per-tuple filtering fix, not merely a shape check).
  (let* ((variants (find-instruction-variants 'varying-hole-test-machine 'ldv))
         (one-hole (find 2 variants :key (lambda (d) (length (instruction-descriptor-operand-names d)))))
         ;; hole order is (dst src), so the SRC field's own variant menu is
         ;; the second entry.
         (src-alternatives (second (instruction-descriptor-word-alternatives one-hole))))
    (fiveam:is (= 1 (length src-alternatives)))
    (fiveam:is (equal '(vh-reg) (mapcar #'word-field-choice-choice src-alternatives)))))

(fiveam:test varying-hole-counts-assemble-and-decode-round-trip
  (let ((one (assembly-cells (assemble "ldv 1, 5" :machine 'varying-hole-test-machine)))
        (two (assembly-cells (assemble "ldv 2, [3, 100]" :machine 'varying-hole-test-machine))))
    (fiveam:is (= 1 (length one)))
    (fiveam:is (= 2 (length two)))
    (multiple-value-bind (descriptor values size choices)
        (decode-instruction-at (lambda (addr) (aref two addr)) 0 'varying-hole-test-machine)
      (declare (ignore choices))
      (fiveam:is (= 2 size))
      (fiveam:is (equal '(2 3 100) values))
      (fiveam:is (= 3 (length (instruction-descriptor-operand-names descriptor)))))))

(defmode vh-ind "[" expr "]")
(defmode vh-plus-idx "[" expr "+" expr "]")
(defmode vh-overlap-mode expr "," (one-of vh-ind vh-plus-idx))

(definstruction varying-hole-test-machine ldo
  (modes vh-overlap-mode)
  (encoding
    (opcode 2)
    (operand dst :field dst)
    (operand src :field src
      (variant (choice vh-ind) inline :range (0 7) :bias #x08)
      (variant (choice vh-plus-idx) inline :range (0 7) :bias #x10))
    (for-choice vh-plus-idx (operand off :trailing-word)))
  (semantics
    (choice-case src
      (vh-ind (set! (a dst) (mref machine 'ram (a src))))
      (vh-plus-idx (set! (a dst) (mref machine 'ram (+ (a src) off)))))))

(fiveam:test indexed-syntax-spends-trailing-offset-when-indirect-is-first
  (let ((indirect (assembly-cells (assemble "ldo 1, [0]" :machine 'varying-hole-test-machine)))
        (indexed (assembly-cells (assemble "ldo 1, [0 + 4]" :machine 'varying-hole-test-machine))))
    (fiveam:is (equalp (vector (logior 2 (ash 1 5) (ash #x08 10))) indirect))
    (fiveam:is (equalp (vector (logior 2 (ash 1 5) (ash #x10 10)) 4) indexed))
    (multiple-value-bind (descriptor values size choices)
        (decode-instruction-at (lambda (addr) (aref indexed addr)) 0 'varying-hole-test-machine)
      (declare (ignore choices))
      (fiveam:is (= 2 size))
      (fiveam:is (equal '(1 0 4) values))
      (fiveam:is (equal '(dst src off) (instruction-descriptor-operand-names descriptor))))
    (let ((machine (make-machine 'varying-hole-test-machine)))
      (setf (regref machine 'a 0) 100
            (mref machine 'ram 104) 444)
      (load-program machine indexed)
      (step-machine machine)
      (fiveam:is (= 444 (regref machine 'a 1))))))

(fiveam:test varying-hole-counts-semantics-dispatch-reads-extra-hole
  (let ((m (make-machine 'varying-hole-test-machine)))
    (setf (regref m 'a 3) 3)
    (setf (mref m 'ram 103) 999)
    (load-program m (assembly-cells (assemble "ldv 2, [3, 100]" :machine 'varying-hole-test-machine)))
    (step-machine m)
    (fiveam:is (= 999 (regref m 'a 2)))))

(fiveam:test varying-hole-counts-disassemble-renders-matched-alternative
  (let* ((cells (assembly-cells (assemble "ldv 2, [3, 100]" :machine 'varying-hole-test-machine)))
         (lines (disassemble-cells cells :machine 'varying-hole-test-machine)))
    (fiveam:is (search "[$3,$64]" (disassembly-line-text (first lines))))))

(fiveam:test for-choice-missing-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction varying-hole-test-machine bogus
             (modes vh-mode)
             (encoding
               (opcode 5)
               (operand dst :field dst)
               (operand src :field src
                 (variant (choice vh-reg) inline :range (0 7) :bias #x00)
                 (variant (choice vh-idx) inline :range (0 7) :bias #x10)))
             (semantics nil)))))

(fiveam:test for-choice-wrong-arity-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction varying-hole-test-machine bogus
             (modes vh-mode)
             (encoding
               (opcode 5)
               (operand dst :field dst)
               (operand src :field src
                 (variant (choice vh-reg) inline :range (0 7) :bias #x00)
                 (variant (choice vh-idx) inline :range (0 7) :bias #x10))
               (for-choice vh-idx (operand off :trailing-word) (operand off2 :trailing-word)))
             (semantics nil)))))

(fiveam:test for-choice-unknown-alternative-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction varying-hole-test-machine bogus
             (modes vh-mode)
             (encoding
               (opcode 5)
               (operand dst :field dst)
               (operand src :field src
                 (variant (choice vh-reg) inline :range (0 7) :bias #x00)
                 (variant (choice vh-idx) inline :range (0 7) :bias #x10))
               (for-choice vh-reg (operand off :trailing-word)))
             (semantics nil)))))

(fiveam:test for-choice-alternative-unclaimed-by-any-choice-variant-signals-error
  ;; VH-IDX contributes a FOR-CHOICE group but no (CHOICE VH-IDX) variant
  ;; claims it on SRC -- %FILTER-TUPLE-GOVERNING-SPECS' own tuple then has
  ;; nothing left in its filtered menu.
  (fiveam:signals instruction-definition-error
    (eval '(definstruction varying-hole-test-machine bogus
             (modes vh-mode)
             (encoding
               (opcode 5)
               (operand dst :field dst)
               (operand src :field src
                 (variant (choice vh-reg) inline :range (0 7) :bias #x00))
               (for-choice vh-idx (operand off :trailing-word)))
             (semantics nil)))))

(fiveam:test for-choice-base-alternative-unclaimed-by-any-choice-variant-signals-error
  ;; The base tuple's own alternative (VH-REG, contributing no extra holes)
  ;; is just as much an unclaimed-alternative case as the over-count one
  ;; above -- SRC only claims VH-IDX, so the base tuple's own filtered menu
  ;; comes up empty too.
  (fiveam:signals instruction-definition-error
    (eval '(definstruction varying-hole-test-machine bogus
             (modes vh-mode)
             (encoding
               (opcode 5)
               (operand dst :field dst)
               (operand src :field src
                 (variant (choice vh-idx) inline :range (0 7) :bias #x10))
               (for-choice vh-idx (operand off :trailing-word)))
             (semantics nil)))))

;; Byte-encoded varying hole counts: a ONE-OF's alternative-tuples are told apart
;; by the sub-opcode cell, and each extra hole comes from a (for-choice ...).
(defmachine varying-hole-byte-test-machine
  (register pc :width 16)
  (register a :width 16)
  (memory ram :width 8 :addr-width 16))

(definstruction varying-hole-byte-test-machine ldv
  (modes vh-mode)
  (encoding
    (opcode 1)
    (operand dst :width 1)
    (operand src :width 1
      (variant (choice vh-reg) (sub 0))
      (variant (choice vh-idx) (sub 1)))
    (for-choice vh-idx (operand off :width 1)))
  (semantics
    (choice-case src
      (vh-reg (set! a (+ dst src)))
      (vh-idx (set! a (+ dst src off))))))

(defun %byte-vh-variant (name holes)
  (find holes (find-instruction-variants 'varying-hole-byte-test-machine name)
        :key (lambda (d) (length (instruction-descriptor-operand-widths d)))))

(fiveam:test byte-varying-hole-counts-expand-one-descriptor-per-table-entry
  (let ((short (%byte-vh-variant 'ldv 2))
        (long (%byte-vh-variant 'ldv 3)))
    (fiveam:is (= 2 (length (find-instruction-variants 'varying-hole-byte-test-machine 'ldv))))
    (fiveam:is (= 0 (instruction-descriptor-sub-opcode short)))
    (fiveam:is (= 1 (instruction-descriptor-sub-opcode long)))
    (fiveam:is (= 4 (instruction-descriptor-size short)))
    (fiveam:is (= 5 (instruction-descriptor-size long)))
    (fiveam:is (equal '(nil vh-reg) (instruction-descriptor-sub-choices short)))
    (fiveam:is (equal '(nil vh-idx vh-idx) (instruction-descriptor-sub-choices long)))
    (fiveam:is (equal '(dst src) (instruction-descriptor-operand-names short)))
    (fiveam:is (equal '(dst src off) (instruction-descriptor-operand-names long)))))

(fiveam:test byte-varying-hole-counts-assemble-and-decode-round-trip
  (let ((short (assembly-cells (assemble "ldv 2, 5" :machine 'varying-hole-byte-test-machine)))
        (long (assembly-cells (assemble "ldv 2, [3, 4]" :machine 'varying-hole-byte-test-machine))))
    (fiveam:is (equalp #(1 0 2 5) short))
    (fiveam:is (equalp #(1 1 2 3 4) long))
    (multiple-value-bind (descriptor values size choices)
        (decode-instruction-at (vector-cell-reader short) 0 'varying-hole-byte-test-machine)
      (fiveam:is (string= "LDV" (instruction-descriptor-name descriptor)))
      (fiveam:is (equal '(2 5) values))
      (fiveam:is (= 4 size))
      (fiveam:is (equal '(nil vh-reg) choices)))
    (multiple-value-bind (descriptor values size choices)
        (decode-instruction-at (vector-cell-reader long) 0 'varying-hole-byte-test-machine)
      (declare (ignore descriptor))
      (fiveam:is (equal '(2 3 4) values))
      (fiveam:is (= 5 size))
      (fiveam:is (equal '(nil vh-idx vh-idx) choices)))))

(fiveam:test byte-varying-hole-counts-semantics-dispatch-reads-extra-hole
  (let ((m (make-machine 'varying-hole-byte-test-machine)))
    (load-program m (assembly-cells (assemble "ldv 2, [3, 4]" :machine 'varying-hole-byte-test-machine)))
    (step-machine m)
    (fiveam:is (= 9 (sref m 'a)))))

(defmode vh-pair (one-of vh-reg vh-idx) "," (one-of vh-reg vh-idx))

(definstruction varying-hole-byte-test-machine ldp
  (modes vh-pair)
  (encoding
    (opcode 2)
    (operand p :width 1)
    (operand q :width 1)
    (for-choice (p vh-idx) (operand p-off :width 1))
    (for-choice (q vh-idx) (operand q-off :width 1))
    (sub-opcode
      (variant (choice vh-reg vh-reg) (sub 0))
      (variant (choice vh-reg vh-idx) (sub 1))
      (variant (choice vh-idx vh-reg) (sub 2))
      (variant (choice vh-idx vh-idx) (sub 3))))
  (semantics (set! a 0)))

(fiveam:test byte-varying-hole-counts-two-independent-elements
  (dolist (case '(("ldp 1, 2" #(2 0 1 2))
                  ("ldp 1, [2, 3]" #(2 1 1 2 3))
                  ("ldp [1, 2], 3" #(2 2 1 2 3))
                  ("ldp [1, 2], [3, 4]" #(2 3 1 2 3 4))))
    (destructuring-bind (source cells) case
      (let ((assembled (assembly-cells (assemble source :machine 'varying-hole-byte-test-machine))))
        (fiveam:is (equalp cells assembled))
        (multiple-value-bind (descriptor values size)
            (decode-instruction-at (vector-cell-reader assembled) 0 'varying-hole-byte-test-machine)
          (fiveam:is (string= "LDP" (instruction-descriptor-name descriptor)))
          (fiveam:is (= (length cells) size))
          (fiveam:is (equal (coerce (subseq cells 2) 'list) values)))))))

(fiveam:test byte-varying-hole-counts-stamp-every-hole-of-the-element
  (let ((both (find 4 (find-instruction-variants 'varying-hole-byte-test-machine 'ldp)
                    :key (lambda (d) (length (instruction-descriptor-operand-widths d))))))
    (fiveam:is (equal '(vh-idx vh-idx vh-idx vh-idx) (instruction-descriptor-sub-choices both)))
    (fiveam:is (equal '(p p-off q q-off) (instruction-descriptor-operand-names both)))))

;; A varying element spanning several base holes: only its first hole selects.
(defmode vh-p2 "[" expr "," expr "]")
(defmode vh-p3 "[" expr "," expr "," expr "]")
(defmode vh-multi (one-of vh-p2 vh-p3))

(definstruction varying-hole-byte-test-machine ldm
  (modes vh-multi)
  (encoding
    (opcode 4)
    (operand x :width 1)
    (operand y :width 1)
    (for-choice vh-p3 (operand z :width 1))
    (sub-opcode
      (holes 0)
      (variant (choice vh-p2) (sub 0))
      (variant (choice vh-p3) (sub 1))))
  (semantics (set! a 0)))

(fiveam:test byte-varying-hole-counts-multi-hole-element
  (let ((two (assembly-cells (assemble "ldm [1, 2]" :machine 'varying-hole-byte-test-machine)))
        (three (assembly-cells (assemble "ldm [1, 2, 3]" :machine 'varying-hole-byte-test-machine))))
    (fiveam:is (equalp #(4 0 1 2) two))
    (fiveam:is (equalp #(4 1 1 2 3) three))
    (fiveam:is (equal '(vh-p2 vh-p2) (instruction-descriptor-sub-choices (%byte-vh-variant 'ldm 2))))
    (fiveam:is (equal '(vh-p3 vh-p3 vh-p3) (instruction-descriptor-sub-choices (%byte-vh-variant 'ldm 3))))
    (fiveam:is (equal '(1 2 3) (nth-value 1 (decode-instruction-at (vector-cell-reader three) 0
                                                                     'varying-hole-byte-test-machine))))))

;; An extra hole's :mode width comes from the alternative that owns it.
(defmode vh-nar expr :width 1)
(defmode vh-wide "[" expr "," expr "]" :width 2)
(defmode vh-wmode (one-of vh-nar vh-wide))

(definstruction varying-hole-byte-test-machine ldw
  (modes vh-wmode)
  (encoding
    (opcode 5)
    (operand src :mode
      (variant (choice vh-nar) (sub 0))
      (variant (choice vh-wide) (sub 1)))
    (for-choice vh-wide (operand off :mode)))
  (semantics (set! a 0)))

(fiveam:test byte-varying-hole-counts-extra-hole-width-from-alternative
  (fiveam:is (equal '(1) (instruction-descriptor-operand-widths (%byte-vh-variant 'ldw 1))))
  (fiveam:is (equal '(2 2) (instruction-descriptor-operand-widths (%byte-vh-variant 'ldw 2))))
  (fiveam:is (equalp #(5 1 1 0 2 0) (assembly-cells (assemble "ldw [1, 2]"
                                                              :machine 'varying-hole-byte-test-machine)))))

(defmacro %signals-byte-varying-error (&body form)
  `(fiveam:signals error (eval '(progn ,@form))))

(fiveam:test byte-varying-hole-counts-require-a-selector
  (%signals-byte-varying-error
    (definstruction varying-hole-byte-test-machine bogus
      (modes vh-mode)
      (encoding (opcode 9) (operand :width 1) (operand :width 1)
                (for-choice vh-idx (operand :width 1)))
      (semantics nil))))

(fiveam:test byte-varying-hole-counts-selector-must-cover-every-varying-element
  (%signals-byte-varying-error
    (definstruction varying-hole-byte-test-machine bogus
      (modes vh-pair)
      (encoding (opcode 9) (operand :width 1) (operand :width 1)
                (for-choice (p vh-idx) (operand :width 1))
                (for-choice (q vh-idx) (operand :width 1))
                (sub-opcode (holes 0)
                            (variant (choice vh-reg) (sub 0))
                            (variant (choice vh-idx) (sub 1))))
      (semantics nil))))

(fiveam:test byte-varying-hole-counts-selector-on-later-element-hole-rejected
  (%signals-byte-varying-error
    (definstruction varying-hole-byte-test-machine bogus
      (modes vh-multi)
      (encoding (opcode 9) (operand :width 1) (operand :width 1)
                (for-choice vh-p3 (operand :width 1))
                (sub-opcode (variant (choice vh-p2 vh-p2) (sub 0))
                            (variant (choice vh-p2 vh-p3) (sub 1))
                            (variant (choice vh-p3 vh-p2) (sub 2))
                            (variant (choice vh-p3 vh-p3) (sub 3))))
      (semantics nil))))

(fiveam:test byte-varying-hole-counts-missing-for-choice-rejected
  (%signals-byte-varying-error
    (definstruction varying-hole-byte-test-machine bogus
      (modes vh-mode)
      (encoding (opcode 9) (operand :width 1)
                (operand :width 1 (variant (choice vh-reg) (sub 0)) (variant (choice vh-idx) (sub 1))))
      (semantics nil))))

(fiveam:test byte-varying-hole-counts-extra-hole-selector-rejected
  (%signals-byte-varying-error
    (definstruction varying-hole-byte-test-machine bogus
      (modes vh-mode)
      (encoding (opcode 9) (operand :width 1)
                (operand :width 1 (variant (choice vh-reg) (sub 0)) (variant (choice vh-idx) (sub 1)))
                (for-choice vh-idx (operand :width 1 (variant (choice vh-idx) (sub 2)))))
      (semantics nil))))

(defmode vh-nothing "none")
(defmode vh-zero (one-of vh-nothing vh-idx))

(fiveam:test byte-varying-hole-counts-element-without-base-hole-rejected
  (%signals-byte-varying-error
    (definstruction varying-hole-byte-test-machine bogus
      (modes vh-zero)
      (encoding (opcode 9) (for-choice vh-idx (operand :width 1) (operand :width 1)))
      (semantics nil))))

(fiveam:test byte-for-choice-on-non-varying-mode-rejected
  (%signals-byte-varying-error
    (definstruction varying-hole-byte-test-machine bogus
      (modes vh-nar)
      (encoding (opcode 9) (operand :mode) (for-choice vh-idx (operand :width 1)))
      (semantics nil))))

;;; Per-field extra-word width (#135) -- an (extra-word ...) variant's own
;;; :CELLS, defaulting to the layout's WIDTH-CELLS (2, for this 16-bit word
;;; / 8-bit cell machine) when omitted. SETN's fallback is narrower than
;;; that default (1 cell); SETW's is wider (4 cells).

(definstruction word-test-machine setn
  (modes word-imm)
  (encoding
    (opcode 0)
    (operand value :field src
      (variant (range -1 30) inline :bias 1)
      (variant :else (extra-word :escape #x3ff :cells 1))))
  (semantics (set! a operand)))

(definstruction word-test-machine setw
  (modes word-imm)
  (encoding
    (opcode 3)
    (operand value :field src
      (variant (range -1 30) inline :bias 1)
      (variant :else (extra-word :escape #x3ff :cells 4))))
  (semantics (set! a operand)))

(fiveam:test word-extra-word-explicit-cells-narrower-than-layout-default
  (let ((variants (find-instruction-variants 'word-test-machine "SETN")))
    (fiveam:is (equal '(0 1) (mapcar #'instruction-descriptor-extra-cells variants)))
    (fiveam:is (equal '(2 3) (mapcar #'instruction-descriptor-size variants)))))

(fiveam:test word-extra-word-explicit-cells-wider-than-layout-default
  (let ((variants (find-instruction-variants 'word-test-machine "SETW")))
    (fiveam:is (equal '(0 4) (mapcar #'instruction-descriptor-extra-cells variants)))
    (fiveam:is (equal '(2 6) (mapcar #'instruction-descriptor-size variants)))))

(fiveam:test word-extra-word-cells-1-encode-and-round-trip
  ;; 100 is out of SETN's -1..30 inline range but fits comfortably in one
  ;; 8-bit extra cell -- a 3-cell instruction, not a 4-cell one.
  (let ((cells (assembly-cells (assemble "setn #100" :machine 'word-test-machine))))
    (fiveam:is (= 3 (length cells)))
    (multiple-value-bind (descriptor values) (decode-instruction-at (vector-cell-reader cells) 0 'word-test-machine)
      (fiveam:is (string= "SETN" (instruction-descriptor-name descriptor)))
      (fiveam:is (equal '(100) values)))))

(fiveam:test word-extra-word-cells-4-encode-and-round-trip
  ;; 100000 needs more than one 16-bit instruction word could hold as its
  ;; own trailing word -- SETW's 4-cell (32-bit) extra word covers it.
  (let ((cells (assembly-cells (assemble "setw #100000" :machine 'word-test-machine))))
    (fiveam:is (= 6 (length cells)))
    (multiple-value-bind (descriptor values) (decode-instruction-at (vector-cell-reader cells) 0 'word-test-machine)
      (fiveam:is (string= "SETW" (instruction-descriptor-name descriptor)))
      (fiveam:is (equal '(100000) values)))))

(fiveam:test word-extra-word-cells-zero-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction word-test-machine bogus
             (modes word-imm)
             (encoding (opcode 5)
                       (operand value :field src
                         (variant (range -1 30) inline :bias 1)
                         (variant :else (extra-word :escape #x3ff :cells 0))))
             (semantics nil)))))

(fiveam:test word-extra-word-cells-non-integer-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction word-test-machine bogus
             (modes word-imm)
             (encoding (opcode 12)
                       (operand value :field src
                         (variant (range -1 30) inline :bias 1)
                         (variant :else (extra-word :escape #x3ff :cells 1.5))))
             (semantics nil)))))

;; WCXMIX: one field mixing two different :CELLS widths across its
;; CHOICE-selected :EXTRA-WORD variants -- WCM-IND escapes to a 1-cell extra
;; word, WCM-FAR to a 4-cell one, decode telling them apart purely by which
;; alternative the operand's own syntax matched, same as any other
;; CHOICE-selected pair (#104). A dedicated machine/modes, not WORD-TEST-
;; MACHINE's own WC-*: its 4-bit OPCODE field has no slot left free (#134's
;; own decode-failure fixture, tests/emulator.lisp, pins the one remaining
;; opcode as deliberately unregistered), and WC-MEM's "(" ")" syntax is
;; indistinguishable from WC-REG's plain EXPR once parenthesized grouping
;; (parser.lisp) is in play -- "(70000)" parses as EXPR 70000, matching
;; WC-REG first regardless of ONE-OF order.
(defmachine word-cells-mix-test-machine
  (register pc :width 16)
  (register a :width 16)
  (memory ram :width 8 :addr-width 16)
  (instruction-word :width 16
    (field opcode 4)
    (field src 12)))

(defmode wcm-reg expr)
(defmode wcm-ind "[" expr "]")
(defmode wcm-far "#" expr)
(defmode wcm-three (one-of wcm-reg wcm-ind wcm-far))

(definstruction word-cells-mix-test-machine wcxmix
  (modes wcm-three)
  (encoding
    (opcode 1)
    (operand value :field src
      (variant (choice wcm-reg) inline :range (0 7))
      (variant (choice wcm-ind) (extra-word :escape #x3fe :cells 1))
      (variant (choice wcm-far) (extra-word :escape #x3ff :cells 4))))
  (semantics (set! a operand)))

(fiveam:test choice-selected-field-mixes-extra-word-cell-widths
  (let* ((variants (find-instruction-variants 'word-cells-mix-test-machine "WCXMIX"))
         (extra-cells-for (lambda (choice-name)
                             (word-field-choice-extra-cells
                              (first (instruction-descriptor-word-fields
                                      (find choice-name variants
                                            :key (lambda (d) (word-field-choice-choice
                                                               (first (instruction-descriptor-word-fields d)))))))))))
    (fiveam:is (= 3 (length variants)))
    (fiveam:is (= 1 (funcall extra-cells-for 'wcm-ind)))
    (fiveam:is (= 4 (funcall extra-cells-for 'wcm-far)))))

(fiveam:test choice-selected-mixed-extra-word-widths-round-trip
  (let ((narrow (assembly-cells (assemble "wcxmix [200]" :machine 'word-cells-mix-test-machine)))
        (wide (assembly-cells (assemble "wcxmix #70000" :machine 'word-cells-mix-test-machine))))
    (fiveam:is (= 3 (length narrow)))
    (fiveam:is (= 6 (length wide)))
    (multiple-value-bind (d1 v1) (decode-instruction-at (vector-cell-reader narrow) 0 'word-cells-mix-test-machine)
      (fiveam:is (string= "WCXMIX" (instruction-descriptor-name d1)))
      (fiveam:is (equal '(200) v1)))
    (multiple-value-bind (d2 v2) (decode-instruction-at (vector-cell-reader wide) 0 'word-cells-mix-test-machine)
      (fiveam:is (string= "WCXMIX" (instruction-descriptor-name d2)))
      (fiveam:is (equal '(70000) v2)))))

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

(fiveam:test choice-case-shared-semantics-fn-still-dispatches-per-sibling
  ;; #150: WCC's two sibling descriptors share one SEMANTICS-FN object (same
  ;; sharing as WCX above), yet CHOICE-CASE still reaches its WC-REG-only or
  ;; WC-IND-only branch correctly for each -- dispatch reads EXECUTE-
  ;; INSTRUCTION's runtime CHOICES argument, never anything stamped onto the
  ;; descriptor itself, so sharing the function changes nothing about which
  ;; branch a given call takes.
  (let* ((variants (find-instruction-variants 'word-test-machine "WCC"))
         (reg (find 'wc-reg variants
                     :key (lambda (d) (word-field-choice-choice (first (instruction-descriptor-word-fields d))))))
         (ind (find 'wc-ind variants
                     :key (lambda (d) (word-field-choice-choice (first (instruction-descriptor-word-fields d))))))
         (m (make-machine 'word-test-machine)))
    (fiveam:is (eq (instruction-descriptor-semantics-fn reg) (instruction-descriptor-semantics-fn ind)))
    (execute-instruction reg m (list 5) (list (make-word-field-choice :width 6 :shift 0 :kind :inline :choice 'wc-reg)))
    (fiveam:is (= 5 (sref m 'a)))
    (fiveam:is (= 0 (sref m 'b)))
    (execute-instruction ind m (list 7) (list (make-word-field-choice :width 6 :shift 0 :kind :inline :choice 'wc-ind)))
    (fiveam:is (= 5 (sref m 'a)))
    (fiveam:is (= 7 (sref m 'b)))))

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

;;; Per-hole :SIGNED on a ONE-OF alternative, word half (#127, split from
;;; #124). WSI-POS/WSI-NEG disagree on signedness -- legal now that
;;; mode.lisp's %CHECK-ONE-OF-ELEMENTS! no longer rejects :SIGNED inside
;;; ONE-OF -- and both alternatives are CHOICE-selected on FIELD SRC (10
;;; bits), which %CHECK-WORD-ONE-OF-SIGNED (instruction.lisp) requires
;;; whenever a hole's alternatives disagree: it is the decode-time record of
;;; which alternative -- and so which signedness -- applies.

(defmode wsi-pos expr)
(defmode wsi-neg "#" expr :signed t)
(defmode wsi-mix (one-of wsi-pos wsi-neg))

(definstruction mixed-field-test-machine wsi
  (modes wsi-mix)
  (encoding
    (opcode 3)
    (operand value :field src
      (variant (choice wsi-pos) inline :range (0 511) :bias 0)
      (variant (choice wsi-neg) inline :range (-512 -1) :bias 0)))
  (semantics (set! a value)))

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
  (fiveam:signals instruction-definition-error
    (eval '(definstruction word-test-machine bogus
             (modes wc-two)
             (encoding (opcode 12)
                       (operand value :field src
                         (variant (choice wc-reg) inline :range (0 7) :bias #x00)
                         (variant (choice wc-ind) inline :range (0 7) :bias #x08)))
             (semantics (choice-case no-such-operand (wc-reg 1)))))))

(fiveam:test choice-case-key-not-among-hole-alternatives-signals-error
  (fiveam:signals instruction-definition-error
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

;;; Per-hole :SIGNED on a ONE-OF alternative, word half (#127).

(fiveam:test one-of-signed-stamps-word-field-choice-signedp
  ;; #20's combo expansion doesn't guarantee declaration order, so pick each
  ;; sibling by which alternative its own WORD-FIELD-CHOICE-CHOICE names,
  ;; rather than assuming FIRST/SECOND.
  (let* ((descs (find-instruction-variants 'mixed-field-test-machine "WSI"))
         (pos (find 'wsi-pos descs :key (lambda (d) (word-field-choice-choice (first (instruction-descriptor-word-fields d))))))
         (neg (find 'wsi-neg descs :key (lambda (d) (word-field-choice-choice (first (instruction-descriptor-word-fields d)))))))
    (fiveam:is (null (word-field-choice-signedp (first (instruction-descriptor-word-fields pos)))))
    (fiveam:is (eq t (word-field-choice-signedp (first (instruction-descriptor-word-fields neg)))))))

(fiveam:test one-of-signed-word-encode-decode-round-trips-the-negative-alternative
  (let* ((assembly (assemble "wsi #-100" :machine 'mixed-field-test-machine))
         (reader (vector-cell-reader (assembly-cells assembly))))
    (multiple-value-bind (descriptor values size choices) (decode-instruction-at reader 0 'mixed-field-test-machine)
      (declare (ignore size))
      (fiveam:is (string= "WSI" (instruction-descriptor-name descriptor)))
      (fiveam:is (equal '(-100) values))
      (fiveam:is (eq 'wsi-neg (%matched-choice-name choices 0))))))

(fiveam:test one-of-signed-word-encode-decode-round-trips-the-unsigned-alternative
  (let* ((assembly (assemble "wsi 200" :machine 'mixed-field-test-machine))
         (reader (vector-cell-reader (assembly-cells assembly))))
    (multiple-value-bind (descriptor values size choices) (decode-instruction-at reader 0 'mixed-field-test-machine)
      (declare (ignore size))
      (fiveam:is (string= "WSI" (instruction-descriptor-name descriptor)))
      (fiveam:is (equal '(200) values))
      (fiveam:is (eq 'wsi-pos (%matched-choice-name choices 0))))))

(fiveam:test one-of-signed-word-disassembles-both-alternatives
  (let ((lines (disassemble-assembly (assemble "wsi 200
wsi #-100" :machine 'mixed-field-test-machine)
                                      :machine 'mixed-field-test-machine :labels nil)))
    (fiveam:is (string= "wsi $C8" (disassembly-line-text (first lines))))
    (fiveam:is (string= "wsi #-100" (disassembly-line-text (second lines))))))

;; A hole whose ONE-OF alternatives disagree on signedness but has a
;; value-selected (no CHOICE) variant has no decode-time record of which
;; alternative -- and so which signedness -- a raw value came from.
(fiveam:test one-of-signed-word-mixed-field-with-value-selected-fallback-signals-error
  ;; #118's mixed-field fallback would otherwise let a plain (RANGE ...)
  ;; variant (no CHOICE of its own) inherit WSI-NEG's signedness once
  ;; %CHECK-WORD-VARIANT-CHOICES! backfills its CHOICE -- but this signals
  ;; before that backfill even runs: %CHECK-WORD-VARIANTS validates a
  ;; not-yet-CHOICE-selected variant's range as unsigned, and -512..-1 has no
  ;; valid unsigned 10-bit encoding.
  (fiveam:signals instruction-definition-error
    (eval '(definstruction mixed-field-test-machine wsibad
             (modes wsi-mix)
             (encoding (opcode 4)
                       (operand value :field src
                         (variant (choice wsi-pos) inline :range (0 511) :bias 0)
                         (variant (range -512 -1) inline)))
             (semantics nil)))))

;; A mixed field whose value-selected variant's own declared range is
;; unsigned-valid on its face (600..700 fits a 10-bit field's 0..1023
;; unsigned range, so %CHECK-WORD-VARIANTS above raises nothing) still must
;; not resolve to a SIGNED unclaimed alternative -- %WORD-FIELD-CHOICE-FORM
;; would otherwise stamp SIGNEDP T from the backfilled CHOICE alone, and
;; decode would sign-extend a raw value like 650 to -374 before comparing it
;; against the (unsigned-declared) 600..700 range, a DECODE-FAILURE for an
;; encoding that assembled cleanly.
(fiveam:test one-of-signed-word-mixed-field-backfilled-to-a-signed-alternative-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction mixed-field-test-machine wsibad3
             (modes wsi-mix)
             (encoding (opcode 6)
                       (operand value :field src
                         (variant (choice wsi-pos) inline :range (0 100))
                         (variant (range 600 700) inline)))
             (semantics nil)))))

;; A hole whose ONE-OF alternatives disagree on signedness but has *no*
;; CHOICE-selected variant at all (both value-selected) -- there is no
;; mixed-field backfill to give either one a decode-time record of which
;; alternative it came from, so %CHECK-WORD-ONE-OF-SIGNED itself (not
;; %CHECK-WORD-VARIANTS' unsigned-range check above) is what rejects this.
(fiveam:test one-of-signed-word-wholly-value-selected-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction mixed-field-test-machine wsibad2
             (modes wsi-mix)
             (encoding (opcode 4)
                       (operand value :field src
                         (variant (range 0 511) inline)
                         (variant :else (extra-word :escape 1023))))
             (semantics nil)))))

;; A hole whose ONE-OF alternatives *agree* on signedness needs no full
;; CHOICE coverage at all -- the hole's signedness is static regardless of
;; which one matched, same shortcut as the byte half.
(defmode wsi-agree-a expr)
(defmode wsi-agree-b "[" expr "]")
(defmode wsi-agree (one-of wsi-agree-a wsi-agree-b))

(definstruction mixed-field-test-machine wsiok
  (modes wsi-agree)
  (encoding (opcode 5) (operand value :field src))
  (semantics (set! a value)))

(fiveam:test one-of-signed-word-agreeing-alternatives-need-no-full-choice-coverage
  (fiveam:finishes (find-instruction-variants 'mixed-field-test-machine "WSIOK")))

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
  (fiveam:signals instruction-definition-error
    (eval '(definstruction word-test-machine bogus
             (encoding (opcode 16))
             (semantics nil)))))

;;; #62 (M4): whole-mode and per-hole :RELATIVE on a word-encoded machine --
;;; replaces the old unconditional ban (WORD-RELATIVE-MODE-SIGNALS-ERROR). A
;;; dedicated machine, not WORD-TEST-MACHINE (whose 4-bit opcode field is
;;; already saturated by every other word-path suite above), so these tests'
;;; own opcode space can't collide with siblings elsewhere in this file or
;;; in tests/emulator.lisp.

(defmachine word-relative-test-machine
  (register pc :width 16)
  (register a :width 16)
  (memory ram :width 8 :addr-width 16)
  (instruction-word :width 16
    (field opcode 4)
    (field dst 2)
    (field src 10)))

(definstruction word-relative-test-machine wbra
  (modes relative)
  (encoding (opcode 1) (operand value :field src
                          (variant (range -256 255) inline :bias 0)
                          (variant :else (extra-word :escape #x200))))
  (semantics (set! pc (+ pc value))))

;; Inline-only, no (extra-word ...) fallback -- for assembler tests proving
;; an out-of-range relative offset is an unconditional ASSEMBLY-ERROR when
;; there is nowhere wider to relax into (#62).
(definstruction word-relative-test-machine wbrs
  (modes relative)
  (encoding (opcode 6) (operand value :field src (variant (range -8 7) inline :bias 0)))
  (semantics (set! a (wrap-value (+ a value) 16))))

;; Filler with a known, fixed size -- pads a program out far enough to force
;; WBRA's inline variant out of range and its extra-word one to relax in.
(definstruction word-relative-test-machine wnop
  (encoding (opcode 7))
  (semantics nil))

(definstruction word-relative-test-machine whlt
  (encoding (opcode 8))
  (semantics (trap :halt)))

;; WBRN (#135): an extra-word fallback narrower than WBRA's own default --
;; :CELLS 1 (signed -128..127) is a *subset* of the inline range (-256..255)
;; here, so no offset ever genuinely needs it; its only purpose is to give
;; %WORD-RELATIVE-OFFSET-FITS-P a narrow width to reject against, for the
;; assembler-side test of an offset that fits neither the inline range nor
;; this narrow extra word.
(definstruction word-relative-test-machine wbrn
  (modes relative)
  (encoding (opcode 9) (operand value :field src
                          (variant (range -256 255) inline :bias 0)
                          (variant :else (extra-word :escape #x200 :cells 1))))
  (semantics (set! pc (+ pc value))))

(fiveam:test word-whole-mode-relative-descriptors-carry-relative-holes
  (let ((variants (find-instruction-variants 'word-relative-test-machine "WBRA")))
    (fiveam:is (= 2 (length variants)))
    (fiveam:is (every (lambda (d) (equal '(t) (instruction-descriptor-relative-holes d))) variants))))

(fiveam:test word-whole-mode-relative-stamps-signedp-on-word-fields-and-alternatives
  (let ((inline-d (first (find-instruction-variants 'word-relative-test-machine "WBRA"))))
    (fiveam:is (word-field-choice-signedp (first (instruction-descriptor-word-fields inline-d))))
    (fiveam:is (every (lambda (l) (every #'word-field-choice-signedp l))
                       (instruction-descriptor-word-alternatives inline-d)))))

(fiveam:test word-relative-mode-with-no-explicit-variant-defaults-to-full-signed-range
  ;; #62: the implicit no-(variant...) default is the field's *signed* bound
  ;; for a relative hole, not its unsigned one -- else a backward branch
  ;; could never encode at all.
  (eval '(definstruction word-relative-test-machine wbra-default
           (modes relative)
           (encoding (opcode 2) (operand value :field src))
           (semantics (set! pc (+ pc value)))))
  (let* ((d (find-instruction 'word-relative-test-machine 'wbra-default))
         (choice (first (instruction-descriptor-word-fields d))))
    (fiveam:is (word-field-choice-signedp choice))
    (fiveam:is (equal '(-512 . 511) (word-field-choice-range choice)))))

(defmode wrel-abs expr)
(defmode wrel-rel "#" expr :relative t)
(defmode wrel-two (one-of wrel-abs wrel-rel))

(fiveam:test word-per-hole-relative-one-of-requires-choice-selector-on-disagreement
  (fiveam:signals instruction-definition-error
    (eval '(definstruction word-relative-test-machine bogus
             (modes wrel-two)
             (encoding (opcode 3)
                       (operand value :field src
                         (variant (range 0 15) inline :bias 0)
                         (variant :else (extra-word :escape #x200))))
             (semantics nil)))))

(definstruction word-relative-test-machine wjmr
  (modes wrel-two)
  (encoding
    (opcode 4)
    (operand value :field src
      (variant (choice wrel-abs) inline :range (256 511) :bias 0)
      (variant (choice wrel-rel) inline :range (-256 255) :bias 0)))
  (semantics (choice-case value
               (wrel-abs (set! a value))
               (wrel-rel (set! a (wrap-value (+ a value) 16))))))

(fiveam:test word-per-hole-relative-one-of-relative-holes-follow-matched-choice
  (let* ((variants (find-instruction-variants 'word-relative-test-machine "WJMR"))
         (abs-d (find-if (lambda (d) (eq 'wrel-abs (word-field-choice-choice (first (instruction-descriptor-word-fields d)))))
                          variants))
         (rel-d (find-if (lambda (d) (eq 'wrel-rel (word-field-choice-choice (first (instruction-descriptor-word-fields d)))))
                          variants)))
    (fiveam:is (equal '(nil) (instruction-descriptor-relative-holes abs-d)))
    (fiveam:is (equal '(t) (instruction-descriptor-relative-holes rel-d)))
    (fiveam:is (not (word-field-choice-signedp (first (instruction-descriptor-word-fields abs-d)))))
    (fiveam:is (word-field-choice-signedp (first (instruction-descriptor-word-fields rel-d))))))

(defmode wrel-both (one-of wrel-abs wrel-rel) "," (one-of wrel-abs wrel-rel))

(definstruction word-relative-test-machine wrelboth
  (modes wrel-both)
  (encoding (opcode 5)
            (operand a-val :field dst
              (variant (choice wrel-abs) inline :range (0 1) :bias 0)
              (variant (choice wrel-rel) inline :range (-2 -1) :bias 0))
            (operand b-val :field src
              (variant (choice wrel-abs) inline :range (0 255) :bias 0)
              (variant (choice wrel-rel) inline :range (-256 -1) :bias 0)))
  (semantics nil))

(fiveam:test word-two-relative-holes-in-one-combo
  (let* ((a (assemble "wrelboth #*, #*" :machine 'word-relative-test-machine))
         (cells (assembly-cells a)))
    (multiple-value-bind (descriptor values)
        (decode-instruction-at (vector-cell-reader cells) 0 'word-relative-test-machine)
      (fiveam:is (equal '(t t) (instruction-descriptor-relative-holes descriptor)))
      (fiveam:is (equal '(-2 -2) values)))))

(defmode word-attribute-mix (expr :relative t) "," (expr :signed t))
(definstruction word-relative-test-machine wattr
  (modes word-attribute-mix)
  (encoding (opcode 10)
            (operand target :field dst (variant (range -2 1) inline))
            (operand delta :field src (variant (range -256 255) inline)))
  (semantics nil))

(fiveam:test word-direct-hole-attributes
  (let ((a (assemble "wattr *, -3" :machine 'word-relative-test-machine)))
    (multiple-value-bind (descriptor values)
        (decode-instruction-at (vector-cell-reader (assembly-cells a)) 0 'word-relative-test-machine)
      (fiveam:is (equal '(t nil) (instruction-descriptor-relative-holes descriptor)))
      (fiveam:is (equal '(-2 -3) values)))))

(fiveam:test word-direct-relative-hole-overflow-signals-error
  (fiveam:signals assembly-error
    (assemble "wattr $1000, 0" :machine 'word-relative-test-machine)))

(defmode word-attribute-alt-plain "[" expr "," expr "]")
(defmode word-attribute-alt-relative "#" expr "," (expr :relative t))
(defmode word-attribute-alternatives
  (one-of word-attribute-alt-plain word-attribute-alt-relative))
(definstruction word-relative-test-machine walt
  (modes word-attribute-alternatives)
  (encoding (opcode 11)
            (operand first-value :field dst
              (variant (choice word-attribute-alt-plain) inline :range (0 1))
              (variant (choice word-attribute-alt-relative) inline :range (2 3)))
            (operand second-value :field src
              (variant (choice word-attribute-alt-plain) inline :range (0 255))
              (variant (choice word-attribute-alt-relative) inline :range (-256 -1))))
  (semantics nil))

(fiveam:test word-multi-hole-alternative-keeps-its-own-relative-hole
  (let* ((a (assemble "walt #2, *" :machine 'word-relative-test-machine))
         (line (first (disassemble-assembly a :machine 'word-relative-test-machine :labels nil))))
    (multiple-value-bind (descriptor values)
        (decode-instruction-at (vector-cell-reader (assembly-cells a)) 0 'word-relative-test-machine)
      (fiveam:is (equal '(nil t) (instruction-descriptor-relative-holes descriptor)))
      (fiveam:is (equal '(2 -2) values)))
    (fiveam:is (equalp (assembly-cells a)
                       (assembly-cells (assemble (disassembly-line-text line)
                                                 :machine 'word-relative-test-machine))))))

(fiveam:test word-two-relative-holes-disassemble-and-reassemble
  (let* ((a (assemble "wrelboth #*, #*" :machine 'word-relative-test-machine))
         (line (first (disassemble-assembly a :machine 'word-relative-test-machine :labels nil))))
    (fiveam:is (equalp (assembly-cells a)
                       (assembly-cells (assemble (disassembly-line-text line)
                                                 :machine 'word-relative-test-machine))))))

;; #105/#62: %CHECK-OPCODE-DECODABLE!'s co-tenant ambiguity analysis
;; must include a RELATIVE-stamped signed field's wrapped negative chunk as it
;; already does for an explicitly :SIGNED one (#127) -- WBRA's own inline
;; range (-256..255) wraps to raw 768..1023, so a co-tenant claiming any of
;; that range at opcode 1 is indistinguishable, while one claiming 256..511
;; (untouched by WBRA's own inline chunks or WBOTHER's own escape at #x200)
;; is not.
(fiveam:test word-relative-signed-field-co-tenant-overlap-signals-opcode-conflict
  (fiveam:signals opcode-conflict
    (eval '(definstruction word-relative-test-machine wbra-collide
             (modes word-abs)
             (encoding (opcode 1) (operand v :field src (variant (range 800 900) inline :bias 0)))
             (semantics nil)))))

(fiveam:test word-relative-signed-field-co-tenant-disjoint-range-registers-cleanly
  (eval '(definstruction word-relative-test-machine wbra-fine
           (modes word-abs)
           (encoding (opcode 1) (operand v :field src (variant (range 256 400) inline :bias 0)))
           (semantics nil)))
  (fiveam:is (find-instruction 'word-relative-test-machine 'wbra-fine)))

(fiveam:test word-multi-mode-single-hole-without-operand-subclause-signals-error
  ;; unlike the byte-encoded multi-mode form, a word-encoded single-hole mode
  ;; still requires an explicit (operand ...) subclause -- there is no
  ;; default field to fall back to
  (fiveam:signals instruction-definition-error
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
  (register v :width 8 :names (v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 va vb vc vd ve vf))
  (register i :width 12)
  (memory ram :width 8 :addr-width 12))

(defmode chip8-v-imm expr "," "#" expr)
(defmode chip8-v-only expr)

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
  (let ((a (assemble "ldv v0, #$fa
ldv v1, #5
addv v0, #10
ldi #$ffe
addi v1
jp skip
ldv v2, #99
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

;;; Per-instruction, non-uniform instruction-word layouts (#64) --
;;; CHIP8WORDFOO mirrors examples/chip8word.lisp: CHIP8's own layout-only
;;; opcode families -- JP/CALL/LD-I on a 4/12 NNN layout, LD/ADD/SE Vx,byte
;;; on a 4/4/8 XNN layout, DRW on the default 4/4/4/4 -- sharing one 16-bit
;;; instruction word and OPCODE field.

(defmachine chip8wordfoo-test-machine
  (register pc :width 16)
  (register v :width 8 :count 16)
  (register i :width 16)
  (memory ram :width 8 :addr-width 16)
  (stack cs :width 16 :depth 16)
  (instruction-word :width 16
    (field opcode 4) (field x 4) (field y 4) (field n 4)
    (layout xnn (field opcode 4) (field x 4) (field nn 8))
    (layout nnn (field opcode 4) (field nnn 12))))

(defmode chip8word-nnn expr)
(defmode chip8word-ximm "V" expr "," "#" expr)
(defmode chip8word-xyn "V" expr "," "V" expr "," expr)

(definstruction chip8wordfoo-test-machine hlt
  (encoding (opcode 0))
  (semantics (trap :halt)))

(definstruction chip8wordfoo-test-machine jp
  (modes chip8word-nnn)
  (encoding (opcode 1) (layout nnn)
    (operand addr :field nnn))
  (semantics (set! pc addr)))

(definstruction chip8wordfoo-test-machine call
  (modes chip8word-nnn)
  (encoding (opcode 2) (layout nnn)
    (operand addr :field nnn))
  (semantics (push pc cs) (set! pc addr)))

(definstruction chip8wordfoo-test-machine se
  (modes chip8word-ximm)
  (encoding (opcode 3) (layout xnn)
    (operand x :field x)
    (operand nn :field nn))
  (semantics (when (= (v x) nn) (set! pc (+ pc 2)))))

(definstruction chip8wordfoo-test-machine ret
  (encoding (opcode 9))
  (semantics (set! pc (pop cs))))

(definstruction chip8wordfoo-test-machine ld
  (modes chip8word-ximm)
  (encoding (opcode 6) (layout xnn)
    (operand x :field x)
    (operand nn :field nn))
  (semantics (set! (v x) nn)))

(definstruction chip8wordfoo-test-machine add
  (modes chip8word-ximm)
  (encoding (opcode 7) (layout xnn)
    (operand x :field x)
    (operand nn :field nn))
  (semantics (set! (v x) (wrap-value (+ (v x) nn) 8))))

(definstruction chip8wordfoo-test-machine ldi
  (modes chip8word-nnn)
  (encoding (opcode 10) (layout nnn)
    (operand addr :field nnn))
  (semantics (set! i addr)))

(definstruction chip8wordfoo-test-machine drw
  (modes chip8word-xyn)
  (encoding (opcode #xd)
    (operand x :field x)
    (operand y :field y)
    (operand n :field n))
  (semantics (declare (ignore y n)) (setf (mref machine 'ram i) (v x))))

(fiveam:test chip8word-encode-instruction-per-layout
  (let ((ld (first (find-instruction-variants 'chip8wordfoo-test-machine "ld")))
        (jp (first (find-instruction-variants 'chip8wordfoo-test-machine "jp")))
        (drw (first (find-instruction-variants 'chip8wordfoo-test-machine "drw"))))
    (fiveam:is (eq 'xnn (instruction-descriptor-word-layout-name ld)))
    (fiveam:is (eq 'nnn (instruction-descriptor-word-layout-name jp)))
    (fiveam:is (null (instruction-descriptor-word-layout-name drw)))
    ;; LD V0, #21 -> opcode 6 << 12 | 0 << 8 | 21 = #x6015
    (fiveam:is (equalp #(#x15 #x60) (coerce (encode-instruction ld '(0 21)) 'vector)))
    ;; JP 256 -> opcode 1 << 12 | 256 = #x1100
    (fiveam:is (equalp #(#x00 #x11) (coerce (encode-instruction jp '(256)) 'vector)))))

(fiveam:test chip8word-machine-end-to-end
  ;; Mirrors examples/chip8word.lisp's *SOURCE* verbatim.
  (let ((a (assemble "  ld    V 0, #21
  ldi   $100
  call  double
  drw   V 0, V 1, 3
  se    V 0, #42
  ld    V 0, #99
  jp    done
double:
  add   V 0, #21
  ret
done:
  hlt" :machine 'chip8wordfoo-test-machine)))
    (fiveam:is (= 20 (length (assembly-cells a))))
    (let ((m (make-machine 'chip8wordfoo-test-machine)))
      (load-program m a)
      (multiple-value-bind (reason steps) (run m)
        (fiveam:is (eq :trap reason))
        (fiveam:is (= 9 steps))
        (fiveam:is (= 42 (regref m 'v 0)))
        (fiveam:is (= 256 (sref m 'i)))
        (fiveam:is (= 42 (mref m 'ram 256)))
        (fiveam:is (zerop (stack-depth m 'cs)))))))

;;; Word-addressed memory + bitfield/variant encoding combined (#55, M4) --
;;; DCPU16FOO mirrors examples/dcpu16.lisp: DCPU-16's real instruction-word
;;; layout (6-bit AV, 5-bit BV, 5-bit OPCODE fields) over :CELL-WIDTH 16
;;; memory, and a banked (#13) 16-bit REG register carrying DCPU-16's own
;;; register names (#72's :names) rather than bare indices.

(defmachine dcpu16-test-machine
  (register pc :width 16)
  (register reg :width 16 :names (a b c x y z i j))
  (memory ram :width 16 :addr-width 16 :cell-width 16)
  (instruction-word :width 16
    (field av 6)
    (field bv 5)
    (field opcode 5)))

(defmode dcpu16-rr expr "," expr)

(definstruction dcpu16-test-machine set
  (modes dcpu16-rr)
  (encoding
    (opcode 1)
    (operand dst :field bv)
    (operand src :field av
      (variant (range -1 30) inline :bias 33)
      (variant :else (extra-word :escape #x1f))))
  (semantics (set! (reg dst) src)))

(definstruction dcpu16-test-machine add
  (modes dcpu16-rr)
  (encoding
    (opcode 2)
    (operand dst :field bv)
    (operand src :field av
      (variant (range -1 30) inline :bias 33)
      (variant :else (extra-word :escape #x1f))))
  (semantics (set! (reg dst) (wrap-value (+ (reg dst) src) 16))))

(definstruction dcpu16-test-machine addr
  (modes dcpu16-rr)
  (encoding
    (opcode 3)
    (operand dst :field bv)
    (operand srcreg :field av))
  (semantics (set! (reg dst) (wrap-value (+ (reg dst) (reg srcreg)) 16))))

(definstruction dcpu16-test-machine sto
  (modes dcpu16-rr)
  (encoding
    (opcode 4)
    (operand addr :field av
      (variant (range -1 30) inline :bias 33)
      (variant :else (extra-word :escape #x1f)))
    (operand dst :field bv))
  (semantics (setf (mref machine 'ram addr) (reg dst))))

(definstruction dcpu16-test-machine hlt
  (encoding (opcode 5))
  (semantics (trap :halt)))

(fiveam:test dcpu16-set-small-value-packs-inline-one-cell
  (let ((a (assemble "set a, 5" :machine 'dcpu16-test-machine)))
    (fiveam:is (= 1 (length (assembly-cells a))))))

(fiveam:test dcpu16-set-large-value-escapes-to-extra-cell
  (let ((a (assemble "set b, 1000" :machine 'dcpu16-test-machine)))
    (fiveam:is (= 2 (length (assembly-cells a))))
    (fiveam:is (= 1000 (aref (assembly-cells a) 1)))))

(fiveam:test dcpu16-addr-registers-both-plain-inline-one-cell
  (let ((a (assemble "addr a, b" :machine 'dcpu16-test-machine)))
    (fiveam:is (= 1 (length (assembly-cells a))))))

(fiveam:test dcpu16-machine-end-to-end
  ;; Mirrors examples/dcpu16.lisp's *SOURCE* verbatim.
  (let ((a (assemble "set a, 5
set b, 1000
addr a, b
sto result, a
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

;; The mixed-kind case must also work: one candidate's
;; field is :EXTRA-WORD (a single escape value), the other's is :INLINE (a
;; whole biased range) -- disjointness must hold in the direction where the
;; *new* descriptor being registered is the :EXTRA-WORD one and the
;; *already-registered* co-tenant is the :INLINE one, the reverse of
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

;;; Byte-machine sub-opcode cell (#123's design, implemented per #125): an
;;; explicit (opcode n :sub s) subclause reserves the cell right after the
;;; opcode as a second, purely discriminating value, giving decode something
;;; to key off two modes of one mnemonic (or two different mnemonics) sharing
;;; one opcode on a byte-encoded machine -- the case #105 above otherwise
;;; forbids outright, since a byte encoding alone has no per-field
;;; discriminator.

;; One mnemonic, two modes, one opcode, distinct :SUB values -- both modes
;; register cleanly and the bucket holds both descriptors.
(definstruction instr-test-machine subld
  (modes
    (immediate (opcode #xB5 :sub 0) (operand :mode) (semantics (set! a operand)))
    (absolute  (opcode #xB5 :sub 1) (operand :mode) (semantics (set! a (mref machine 'ram operand))))))

(fiveam:test sub-opcode-two-modes-one-mnemonic-share-opcode
  (fiveam:is (= 2 (length (find-instruction-descriptors-by-opcode 'instr-test-machine #xB5))))
  (let ((imm (find-instruction 'instr-test-machine 'subld :mode 'immediate))
        (abs (find-instruction 'instr-test-machine 'subld :mode 'absolute)))
    (fiveam:is (= 0 (instruction-descriptor-sub-opcode imm)))
    (fiveam:is (= 1 (instruction-descriptor-sub-opcode abs)))
    (fiveam:is (= #xB5 (instruction-descriptor-opcode imm) (instruction-descriptor-opcode abs)))))

;; Two *different* mnemonics sharing one opcode, told apart the same way.
(definstruction instr-test-machine subfoo
  (modes immediate)
  (encoding (opcode #xB6 :sub 0) (operand :mode))
  (semantics (set! a operand)))

(definstruction instr-test-machine subbar
  (modes immediate)
  (encoding (opcode #xB6 :sub 1) (operand :mode))
  (semantics (set! x operand)))

(fiveam:test sub-opcode-two-mnemonics-share-opcode
  (fiveam:is (= 2 (length (find-instruction-descriptors-by-opcode 'instr-test-machine #xB6))))
  (fiveam:is (find "SUBFOO" (find-instruction-descriptors-by-opcode 'instr-test-machine #xB6)
                    :key #'instruction-descriptor-name :test #'string=))
  (fiveam:is (find "SUBBAR" (find-instruction-descriptors-by-opcode 'instr-test-machine #xB6)
                    :key #'instruction-descriptor-name :test #'string=)))

(fiveam:test sub-opcode-duplicate-value-signals-error
  (fiveam:signals opcode-conflict
    (eval '(definstruction instr-test-machine subdup
             (modes
               (immediate (opcode #xB7 :sub 0) (operand :mode) (semantics (set! a operand)))
               (absolute (opcode #xB7 :sub 0) (operand :mode) (semantics (set! a operand)))))))
  (handler-case
      (eval '(definstruction instr-test-machine subdup
               (modes
                 (immediate (opcode #xB7 :sub 0) (operand :mode) (semantics (set! a operand)))
                 (absolute (opcode #xB7 :sub 0) (operand :mode) (semantics (set! a operand))))))
    (opcode-conflict (c) (fiveam:is (eq :duplicate-sub-opcode (opcode-conflict-reason c))))))

;; Mixing a :SUB-bearing mode with a :SUB-less one at the same opcode is still
;; an error -- decode couldn't tell whether the cell after the opcode is a
;; sub-opcode or the first operand. Both orderings (sub-first, sub-second).
(fiveam:test sub-opcode-mixed-with-sub-less-signals-error
  (handler-case
      (eval '(definstruction instr-test-machine submix1
               (modes
                 (immediate (opcode #xB8 :sub 0) (operand :mode) (semantics (set! a operand)))
                 (absolute (opcode #xB8) (operand :mode) (semantics (set! a operand))))))
    (opcode-conflict (c) (fiveam:is (eq :sub-opcode-required (opcode-conflict-reason c))))
    (:no-error (&rest values) (declare (ignore values)) (fiveam:fail "expected OPCODE-CONFLICT")))
  (handler-case
      (eval '(definstruction instr-test-machine submix2
               (modes
                 (immediate (opcode #xB9) (operand :mode) (semantics (set! a operand)))
                 (absolute (opcode #xB9 :sub 0) (operand :mode) (semantics (set! a operand))))))
    (opcode-conflict (c) (fiveam:is (eq :sub-opcode-required (opcode-conflict-reason c))))
    (:no-error (&rest values) (declare (ignore values)) (fiveam:fail "expected OPCODE-CONFLICT"))))

(fiveam:test sub-opcode-on-word-machine-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction word-test-machine subwrong
             (modes wc-two)
             (encoding (opcode 15 :sub 0) (operand v :field src))
             (semantics (set! a v))))))

(fiveam:test sub-opcode-too-wide-for-cell-width-signals-error
  ;; INSTR-TEST-MACHINE's code cell is 8 bits wide (its sole memory element,
  ;; RAM, is :WIDTH 8) -- 256 doesn't fit.
  (fiveam:signals instruction-definition-error
    (eval '(definstruction instr-test-machine subwide
             (modes immediate)
             (encoding (opcode #xBA :sub 256) (operand :mode))
             (semantics (set! a operand))))))

(fiveam:test sub-opcode-adds-one-cell-to-descriptor-size
  (let ((imm (find-instruction 'instr-test-machine 'subld :mode 'immediate)))
    ;; 1 (opcode) + 1 (sub) + 1 (operand width) = 3
    (fiveam:is (= 3 (instruction-descriptor-size imm)))))

(fiveam:test sub-opcode-encode-instruction-emits-opcode-sub-then-operands
  (let ((imm (find-instruction 'instr-test-machine 'subld :mode 'immediate)))
    (fiveam:is (equal (list #xB5 0 #x42) (encode-instruction imm '(#x42))))))

(fiveam:test sub-opcode-decode-round-trips-both-modes
  ;; INSTR-TEST-MACHINE's RAM is :ADDR-WIDTH 16 over an 8-bit cell, so
  ;; ABSOLUTE's default operand width (no explicit (operand :width n) given
  ;; here) is 2 cells, unlike IMMEDIATE's own declared :WIDTH 1 -- SUBLD's
  ;; two modes are genuinely different sizes, both still 1 (opcode) + 1 (sub)
  ;; wider than their SUB-less equivalent would be.
  (let ((cells (make-array 7 :element-type '(unsigned-byte 8)
                             :initial-contents (list #xB5 0 #x42 #xB5 1 #x10 0))))
    (multiple-value-bind (descriptor values size)
        (decode-instruction-at (vector-cell-reader cells) 0 'instr-test-machine)
      (fiveam:is (string= "SUBLD" (instruction-descriptor-name descriptor)))
      (fiveam:is (eq (find-mode-descriptor 'immediate) (instruction-descriptor-mode descriptor)))
      (fiveam:is (equal '(#x42) values))
      (fiveam:is (= 3 size)))
    (multiple-value-bind (descriptor values size)
        (decode-instruction-at (vector-cell-reader cells) 3 'instr-test-machine)
      (fiveam:is (string= "SUBLD" (instruction-descriptor-name descriptor)))
      (fiveam:is (eq (find-mode-descriptor 'absolute) (instruction-descriptor-mode descriptor)))
      (fiveam:is (equal '(#x10) values))
      (fiveam:is (= 4 size)))))

(fiveam:test sub-opcode-decode-unmatched-sub-value-is-decode-failure
  (let ((cells (make-array 3 :element-type '(unsigned-byte 8) :initial-contents (list #xB5 99 0))))
    (fiveam:is (eq :decode-failure (decode-instruction-at (vector-cell-reader cells) 0 'instr-test-machine)))))

;;; Hole-selected sub-opcode (#126): the byte-machine analogue of #104's
;;; (choice mode) -- a (variant (choice m) (sub s)) form on an (operand ...)
;;; subclause whose hole came from a ONE-OF pattern element lets that hole's
;;; own matched alternative choose #125's sub-opcode cell, rather than the
;;; whole (modes ...) clause fixing it once. Reuses OO-INSTR-REG/OO-INSTR-IND
;;; (defined above, #103) as the carrying hole's two alternatives.

(defmode sc-instr-one (one-of oo-instr-reg oo-instr-ind))

(definstruction instr-test-machine scld
  (modes sc-instr-one)
  (encoding (opcode #xC0)
            (operand src :width 1
              (variant (choice oo-instr-reg) (sub 0))
              (variant (choice oo-instr-ind) (sub 1))))
  (semantics (choice-case src
               (oo-instr-reg (set! a src))
               (oo-instr-ind (set! a (mref machine 'ram src))))))

(fiveam:test hole-selected-sub-opcode-expands-one-descriptor-per-alternative
  (let ((variants (find-instruction-variants 'instr-test-machine 'scld)))
    (fiveam:is (= 2 (length variants)))
    (fiveam:is (every (lambda (v) (eq (find-mode-descriptor 'sc-instr-one) (instruction-descriptor-mode v)))
                       variants)))
  (let* ((descs (find-instruction-descriptors-by-opcode 'instr-test-machine #xC0))
         (reg (find 0 descs :key #'instruction-descriptor-sub-opcode))
         (ind (find 1 descs :key #'instruction-descriptor-sub-opcode)))
    (fiveam:is (= 2 (length descs)))
    (fiveam:is (not (null reg)))
    (fiveam:is (not (null ind)))
    (fiveam:is (equal '(oo-instr-reg) (instruction-descriptor-sub-choices reg)))
    (fiveam:is (equal '(oo-instr-ind) (instruction-descriptor-sub-choices ind)))))

(fiveam:test hole-selected-sub-opcode-siblings-share-one-semantics-fn
  ;; #150: SCLD's SUB-SPEC pairs share OPCODE, OPERAND-NAMES, and SEMANTICS-
  ;; FORMS (%BYTE-DESCRIPTOR-FORMS' own docstring) -- %SEMANTICS-FN-FORM's
  ;; expansion is built once for the whole SUB-SPEC, not once per pair, so
  ;; SCLD's two sibling descriptors share one SEMANTICS-FN object even though
  ;; their own SUB-OPCODE/SUB-CHOICES differ.
  (let ((variants (find-instruction-variants 'instr-test-machine 'scld)))
    (fiveam:is (= 2 (length variants)))
    (fiveam:is (eq (instruction-descriptor-semantics-fn (first variants))
                   (instruction-descriptor-semantics-fn (second variants))))))

;; Registration's own pairwise check (REGISTER-INSTRUCTION-VARIANTS!) has no
;; sibling exemption on the byte path -- it runs on these two expanded
;; descriptors the same as any unrelated co-tenants, and passes exactly
;; because their :SUB values are pairwise distinct (%CHECK-BYTE-SUB-
;; VARIANTS! guarantees this at DEFINSTRUCTION time). SCLD registering with
;; no error at all, above, already exercises this; nothing further to add.

(fiveam:test hole-selected-sub-opcode-encode-emits-opcode-sub-operand
  (let* ((descs (find-instruction-descriptors-by-opcode 'instr-test-machine #xC0))
         (reg (find 0 descs :key #'instruction-descriptor-sub-opcode))
         (ind (find 1 descs :key #'instruction-descriptor-sub-opcode)))
    (fiveam:is (equal (list #xC0 0 #x05) (encode-instruction reg '(5))))
    (fiveam:is (equal (list #xC0 1 #x05) (encode-instruction ind '(5))))))

(fiveam:test hole-selected-sub-opcode-assembler-picks-matching-sibling
  ;; The regression %CHOICES-ELIGIBLE-P (assembler.lisp) exists to prevent:
  ;; both siblings share one mode and one INSTRUCTION-DESCRIPTOR-SIZE, so
  ;; without it declaration order alone would win regardless of which
  ;; alternative the operand's own syntax actually matched.
  (fiveam:is (equalp #(#xC0 0 5) (assembly-cells (assemble "scld 5" :machine 'instr-test-machine))))
  (fiveam:is (equalp #(#xC0 1 5) (assembly-cells (assemble "scld [5]" :machine 'instr-test-machine)))))

(fiveam:test hole-selected-sub-opcode-decode-reports-matched-choices
  ;; #126: DECODE-INSTRUCTION-AT's fourth CHOICES value is no longer
  ;; unconditionally NIL on a byte-encoded machine -- it carries the matched
  ;; descriptor's own SUB-CHOICES.
  (multiple-value-bind (descriptor values size choices)
      (decode-instruction-at (vector-cell-reader (vector #xC0 0 5)) 0 'instr-test-machine)
    (fiveam:is (string= "SCLD" (instruction-descriptor-name descriptor)))
    (fiveam:is (equal '(5) values))
    (fiveam:is (= 3 size))
    (fiveam:is (eq 'oo-instr-reg (%matched-choice-name choices 0))))
  (multiple-value-bind (descriptor values size choices)
      (decode-instruction-at (vector-cell-reader (vector #xC0 1 5)) 0 'instr-test-machine)
    (declare (ignore descriptor values size))
    (fiveam:is (eq 'oo-instr-ind (%matched-choice-name choices 0)))))

(fiveam:test hole-selected-sub-opcode-choice-case-dispatches-on-byte-machine
  ;; The first time CHOICE-CASE is reachable at all on a cell-encoded
  ;; machine (#73/#122) -- before #126, EXECUTE-INSTRUCTION's CHOICES was
  ;; always NIL there, so every clause fell through to NO-MATCHING-CHOICE.
  (let* ((m (make-machine 'instr-test-machine))
         (descs (find-instruction-descriptors-by-opcode 'instr-test-machine #xC0))
         (reg (find 0 descs :key #'instruction-descriptor-sub-opcode))
         (ind (find 1 descs :key #'instruction-descriptor-sub-opcode)))
    (setf (mref m 'ram 5) 77)
    (execute-instruction reg m '(5) (instruction-descriptor-sub-choices reg))
    (fiveam:is (= 5 (sref m 'a)))
    (execute-instruction ind m '(5) (instruction-descriptor-sub-choices ind))
    (fiveam:is (= 77 (sref m 'a)))))

(fiveam:test hole-selected-sub-opcode-round-trip-disassembles-matched-alternative
  (let ((assembly (assemble "scld 5" :machine 'instr-test-machine)))
    (let ((lines (disassemble-assembly assembly :machine 'instr-test-machine :labels nil :suffixes nil)))
      (fiveam:is (string= "scld $5" (disassembly-line-text (first lines))))))
  (let ((assembly (assemble "scld [5]" :machine 'instr-test-machine)))
    (let ((lines (disassemble-assembly assembly :machine 'instr-test-machine :labels nil :suffixes nil)))
      (fiveam:is (string= "scld [$5]" (disassembly-line-text (first lines)))))))

;; More than one operand hole carrying a sub selector -- the sub-opcode cell
;; is singular, so two holes each wanting to pick it has no coherent meaning.
(fiveam:test hole-selected-sub-opcode-two-holes-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction instr-test-machine scbad1
             (modes oo-instr-two)
             (encoding (opcode #xC1)
                       (operand dst :width 1
                         (variant (choice oo-instr-reg) (sub 0))
                         (variant (choice oo-instr-ind) (sub 1)))
                       (operand src :width 1
                         (variant (choice oo-instr-reg) (sub 0))
                         (variant (choice oo-instr-ind) (sub 1))))
             (semantics nil)))))

;; A sub selector on a hole that isn't a ONE-OF at all has nothing to select
;; between.
(fiveam:test hole-selected-sub-opcode-non-one-of-hole-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction instr-test-machine scbad2
             (modes immediate)
             (encoding (opcode #xC2)
                       (operand src :width 1
                         (variant (choice oo-instr-reg) (sub 0))))
             (semantics nil)))))

;; An unclaimed alternative -- unlike #118's word-machine mixed-field rule,
;; there is no value-selected fallback for it to resolve into here.
(fiveam:test hole-selected-sub-opcode-unclaimed-alternative-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction instr-test-machine scbad3
             (modes sc-instr-one)
             (encoding (opcode #xC3)
                       (operand src :width 1
                         (variant (choice oo-instr-reg) (sub 0))))
             (semantics nil)))))

;; Two alternatives claiming the same sub value can never be told apart.
(fiveam:test hole-selected-sub-opcode-duplicate-sub-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction instr-test-machine scbad4
             (modes sc-instr-one)
             (encoding (opcode #xC4)
                       (operand src :width 1
                         (variant (choice oo-instr-reg) (sub 0))
                         (variant (choice oo-instr-ind) (sub 0))))
             (semantics nil)))))

;; An explicit (opcode n :sub s) and a hole-selected selector would both be
;; writing the same cell.
(fiveam:test hole-selected-sub-opcode-conflicts-with-explicit-sub-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction instr-test-machine scbad5
             (modes sc-instr-one)
             (encoding (opcode #xC5 :sub 9)
                       (operand src :width 1
                         (variant (choice oo-instr-reg) (sub 0))
                         (variant (choice oo-instr-ind) (sub 1))))
             (semantics nil)))))

;; A sub value that doesn't fit the machine's code cell width.
(fiveam:test hole-selected-sub-opcode-too-wide-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction instr-test-machine scbad6
             (modes sc-instr-one)
             (encoding (opcode #xC6)
                       (operand src :width 1
                         (variant (choice oo-instr-reg) (sub 0))
                         (variant (choice oo-instr-ind) (sub 256))))
             (semantics nil)))))

;; A hole-selected sub-opcode selector is a byte-machine-only mechanism, same
;; as #125's plain :SUB -- on WORD-TEST-MACHINE, the byte-style
;; (operand NAME :width n (variant ...)) spec doesn't even parse as a
;; word-encoded (operand NAME :field f ...) subclause.
(fiveam:test hole-selected-sub-opcode-on-word-machine-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction word-test-machine scwordbad
             (modes wc-two)
             (encoding (opcode 20)
                       (operand v :width 1
                         (variant (choice wc-reg) (sub 0))
                         (variant (choice wc-ind) (sub 1))))
             (semantics (set! a v))))))

;; A sub-selected mode coexisting on one mnemonic with a genuinely different
;; mode whose own syntax happens to overlap one of the sub-selected hole's
;; alternatives (both a bare EXPR) -- %CHOICES-ELIGIBLE-P (assembler.lisp)
;; must filter out only the sub-selected mode's own ineligible sibling
;; (OO-INSTR-IND, whose selector doesn't match), leaving the ordinary
;; declaration-order/width tiebreak (%MAYBE-WARN-AMBIGUOUS-MODE) to run
;; exactly as it would with no sub selector involved at all.
(definstruction instr-test-machine scld2
  (modes
    (sc-instr-one (opcode #xCA)
      (operand src :width 1
        (variant (choice oo-instr-reg) (sub 0))
        (variant (choice oo-instr-ind) (sub 1)))
      (semantics (choice-case src
                   (oo-instr-reg (set! a src))
                   (oo-instr-ind (set! a (mref machine 'ram src))))))
    (absolute (opcode #xCB)
      (semantics (set! a (mref machine 'ram operand))))))

(fiveam:test hole-selected-sub-opcode-coexists-with-unrelated-tied-mode
  ;; "[5]" only matches SC-INSTR-ONE's OO-INSTR-IND alternative -- ABSOLUTE's
  ;; bare-EXPR syntax doesn't match "[...]" at all, so there is exactly one
  ;; candidate and no ambiguity.
  (fiveam:is (equalp #(#xCA 1 5) (assembly-cells (assemble "scld2 [5]" :machine 'instr-test-machine))))
  ;; "5" matches both SC-INSTR-ONE's OO-INSTR-REG alternative (size 3: opcode
  ;; + sub + 1-cell operand) and ABSOLUTE's own bare-EXPR pattern (size 3:
  ;; opcode + ABSOLUTE's 2-cell default width, from INSTR-TEST-MACHINE's
  ;; 16-bit-addressed RAM) -- a genuine tie between two *different* modes,
  ;; resolved the ordinary way (declaration order: SC-INSTR-ONE first), with
  ;; an AMBIGUOUS-MODE warning exactly as it would without any sub selector
  ;; in the mix.
  (fiveam:signals ambiguous-mode
    (assemble "scld2 5" :machine 'instr-test-machine))
  (handler-bind ((ambiguous-mode #'muffle-warning))
    (fiveam:is (equalp #(#xCA 0 5) (assembly-cells (assemble "scld2 5" :machine 'instr-test-machine))))))

;;; Per-hole :SIGNED on a ONE-OF alternative, byte half (#124, split from
;;; #126's hole-selected sub-opcode cell) -- SI-INSTR-POS/SI-INSTR-NEG
;;; disagree on signedness, and SIGND's carrying hole (a hole-selected
;;; (variant (choice m) (sub s)) selector, same mechanism #126 gave SCLD
;;; above) is what makes the disagreement decodable at all.

(defmode si-instr-pos expr)
(defmode si-instr-neg "#" expr :signed t)
(defmode si-instr-one (one-of si-instr-pos si-instr-neg))

(definstruction instr-test-machine signd
  (modes si-instr-one)
  (encoding (opcode #xC7)
            (operand val :width 1
              (variant (choice si-instr-pos) (sub 0))
              (variant (choice si-instr-neg) (sub 1))))
  (semantics (set! a val)))

(fiveam:test one-of-signed-stamps-operand-signedness-per-descriptor
  (let* ((descs (find-instruction-descriptors-by-opcode 'instr-test-machine #xC7))
         (pos (find 0 descs :key #'instruction-descriptor-sub-opcode))
         (neg (find 1 descs :key #'instruction-descriptor-sub-opcode)))
    (fiveam:is (equal '(nil) (instruction-descriptor-operand-signedness pos)))
    (fiveam:is (equal '(t) (instruction-descriptor-operand-signedness neg)))))

(fiveam:test one-of-signed-encode-decode-round-trips-the-negative-alternative
  (fiveam:is (equalp #(#xC7 1 156) (assembly-cells (assemble "signd #-100" :machine 'instr-test-machine))))
  (multiple-value-bind (descriptor values size choices)
      (decode-instruction-at (vector-cell-reader (assembly-cells (assemble "signd #-100" :machine 'instr-test-machine)))
                              0 'instr-test-machine)
    (declare (ignore size))
    (fiveam:is (string= "SIGND" (instruction-descriptor-name descriptor)))
    (fiveam:is (equal '(-100) values))
    (fiveam:is (eq 'si-instr-neg (%matched-choice-name choices 0)))))

(fiveam:test one-of-signed-encode-decode-round-trips-the-unsigned-alternative
  (fiveam:is (equalp #(#xC7 0 200) (assembly-cells (assemble "signd 200" :machine 'instr-test-machine))))
  (multiple-value-bind (descriptor values size choices)
      (decode-instruction-at (vector-cell-reader (assembly-cells (assemble "signd 200" :machine 'instr-test-machine)))
                              0 'instr-test-machine)
    (declare (ignore size))
    (fiveam:is (string= "SIGND" (instruction-descriptor-name descriptor)))
    (fiveam:is (equal '(200) values))
    (fiveam:is (eq 'si-instr-pos (%matched-choice-name choices 0)))))

(fiveam:test one-of-signed-disassembles-both-alternatives
  (let ((lines (disassemble-assembly (assemble "signd 200
signd #-100" :machine 'instr-test-machine)
                                      :machine 'instr-test-machine :labels nil :suffixes nil)))
    (fiveam:is (string= "signd $C8" (disassembly-line-text (first lines))))
    (fiveam:is (string= "signd #-100" (disassembly-line-text (second lines))))))

;; A hole whose ONE-OF alternatives disagree on signedness but carries no
;; hole-selected sub-opcode selector at all has no decode-time record of
;; which alternative matched -- %CHECK-BYTE-ONE-OF-SIGNED must reject it.
(fiveam:test one-of-signed-disagreement-without-selector-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction instr-test-machine signdbad1
             (modes si-instr-one)
             (encoding (opcode #xC8)
                       (operand val :width 1))
             (semantics nil)))))

;; A hole whose ONE-OF alternatives *agree* on signedness needs no selector
;; at all -- the hole's signedness is static regardless of which one matched.
(defmode si-instr-agree-a expr)
(defmode si-instr-agree-b "[" expr "]")
(defmode si-instr-agree (one-of si-instr-agree-a si-instr-agree-b))

(definstruction instr-test-machine signdok
  (modes si-instr-agree)
  (encoding (opcode #xC9)
            (operand val :width 1))
  (semantics (set! a val)))

(fiveam:test one-of-signed-agreeing-alternatives-need-no-selector
  (let ((descs (find-instruction-descriptors-by-opcode 'instr-test-machine #xC9)))
    (fiveam:is (= 1 (length descs)))
    (fiveam:is (equal '(nil) (instruction-descriptor-operand-signedness (first descs))))))

;; Nested ONE-OF: SI-INSTR-NESTED-INNER's own :SIGNED alternative is two
;; levels down from SIGND-NESTED's hole -- mode.lisp's %CHECK-ONE-OF-
;; ELEMENTS! already rejects this at DEFMODE time (tests/mode.lisp), so it
;; never reaches DEFINSTRUCTION at all; nothing further to test here beyond
;; confirming SI-INSTR-ONE itself (a plain, non-nested ONE-OF) works, above.

;;; Per-hole :WIDTH on a ONE-OF alternative, byte half (#129) --
;;; WI-INSTR-NARROW/WI-INSTR-WIDE disagree on width, and WIDTHD's carrying
;;; hole (a hole-selected (variant (choice m) (sub s)) selector, the same
;;; mechanism #126/#127 gave SIGND above) is what makes the disagreement
;;; decodable, and each sibling descriptor's own size constant, at all.

(defmode wi-instr-narrow expr :width 1)
(defmode wi-instr-wide "#" expr :width 2)
(defmode wi-instr-one (one-of wi-instr-narrow wi-instr-wide))

(definstruction instr-test-machine widthd
  (modes wi-instr-one)
  (encoding (opcode #x60)
            (operand val :mode
              (variant (choice wi-instr-narrow) (sub 0))
              (variant (choice wi-instr-wide) (sub 1))))
  (semantics (set! a val)))

(fiveam:test one-of-width-stamps-operand-widths-per-descriptor
  (let* ((descs (find-instruction-descriptors-by-opcode 'instr-test-machine #x60))
         (narrow (find 0 descs :key #'instruction-descriptor-sub-opcode))
         (wide (find 1 descs :key #'instruction-descriptor-sub-opcode)))
    (fiveam:is (equal '(1) (instruction-descriptor-operand-widths narrow)))
    (fiveam:is (equal '(2) (instruction-descriptor-operand-widths wide)))))

(fiveam:test one-of-width-encode-decode-round-trips-the-narrow-alternative
  (fiveam:is (equalp #(#x60 0 200) (assembly-cells (assemble "widthd 200" :machine 'instr-test-machine))))
  (multiple-value-bind (descriptor values size choices)
      (decode-instruction-at (vector-cell-reader (assembly-cells (assemble "widthd 200" :machine 'instr-test-machine)))
                              0 'instr-test-machine)
    (fiveam:is (string= "WIDTHD" (instruction-descriptor-name descriptor)))
    (fiveam:is (equal '(200) values))
    (fiveam:is (= 3 size))
    (fiveam:is (eq 'wi-instr-narrow (%matched-choice-name choices 0)))))

(fiveam:test one-of-width-encode-decode-round-trips-the-wide-alternative
  (fiveam:is (equalp #(#x60 1 44 1) (assembly-cells (assemble "widthd #300" :machine 'instr-test-machine))))
  (multiple-value-bind (descriptor values size choices)
      (decode-instruction-at (vector-cell-reader (assembly-cells (assemble "widthd #300" :machine 'instr-test-machine)))
                              0 'instr-test-machine)
    (fiveam:is (string= "WIDTHD" (instruction-descriptor-name descriptor)))
    (fiveam:is (equal '(300) values))
    (fiveam:is (= 4 size))
    (fiveam:is (eq 'wi-instr-wide (%matched-choice-name choices 0)))))

(fiveam:test one-of-width-disassembles-both-alternatives
  (let ((lines (disassemble-assembly (assemble "widthd 200
widthd #300" :machine 'instr-test-machine)
                                      :machine 'instr-test-machine :labels nil :suffixes nil)))
    (fiveam:is (string= "widthd $C8" (disassembly-line-text (first lines))))
    (fiveam:is (string= "widthd #$12C" (disassembly-line-text (second lines))))))

;; A hole whose ONE-OF alternatives disagree on width, its own (operand ...)
;; subclause using (operand :mode) (so the disagreement actually takes
;; effect, %BYTE-OPERAND-WIDTHS), but carrying no hole-selected sub-opcode
;; selector at all, has no decode-time record of which alternative matched
;; -- %CHECK-BYTE-ONE-OF-WIDTH must reject it.
(fiveam:test one-of-width-disagreement-without-selector-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction instr-test-machine widthdbad1
             (modes wi-instr-one)
             (encoding (opcode #x61)
                       (operand val :mode))
             (semantics nil)))))

;; The same disagreement is fine with no selector at all when the hole's own
;; (operand ...) subclause gives an explicit :WIDTH -- the explicit width
;; always wins over a matched alternative's own :WIDTH, so there is nothing
;; for a selector to disambiguate.
(definstruction instr-test-machine widthdexplicit
  (modes wi-instr-one)
  (encoding (opcode #x62)
            (operand val :width 1))
  (semantics (set! a val)))

(fiveam:test one-of-width-explicit-override-needs-no-selector
  (let ((descs (find-instruction-descriptors-by-opcode 'instr-test-machine #x62)))
    (fiveam:is (= 1 (length descs)))
    (fiveam:is (equal '(1) (instruction-descriptor-operand-widths (first descs))))
    (fiveam:is (equalp #(#x62 200) (assembly-cells (assemble "widthdexplicit 200" :machine 'instr-test-machine))))
    (fiveam:is (equalp #(#x62 200) (assembly-cells (assemble "widthdexplicit #200" :machine 'instr-test-machine))))))

;; A hole whose ONE-OF alternatives *agree* on width needs no selector at
;; all -- the hole's width is static regardless of which one matched.
(defmode wi-instr-agree-a expr :width 1)
(defmode wi-instr-agree-b "[" expr "]" :width 1)
(defmode wi-instr-agree (one-of wi-instr-agree-a wi-instr-agree-b))

(definstruction instr-test-machine widthdok
  (modes wi-instr-agree)
  (encoding (opcode #x63)
            (operand val :mode))
  (semantics (set! a val)))

(fiveam:test one-of-width-agreeing-alternatives-need-no-selector
  (let ((descs (find-instruction-descriptors-by-opcode 'instr-test-machine #x63)))
    (fiveam:is (= 1 (length descs)))
    (fiveam:is (equal '(1) (instruction-descriptor-operand-widths (first descs))))))

;; Per-hole :WIDTH is permanently out of scope on a word-encoded machine --
;; OPERAND-WIDTHS is always NIL there, so it has nothing to mean; any ONE-OF
;; hole whose alternatives declare :WIDTH at all (agreeing or not) is a
;; DEFINSTRUCTION-time error rather than a silently inert declaration.
(fiveam:test one-of-width-on-word-machine-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction mixed-field-test-machine widthdword
             (modes wi-instr-one)
             (encoding (opcode 7)
                       (operand value :field src
                         (variant (choice wi-instr-narrow) inline :range (0 511))
                         (variant (choice wi-instr-wide) inline :range (0 511))))
             (semantics nil)))))

;;; Per-hole :RELATIVE on a ONE-OF alternative, byte half (#130) --
;;; RL-INSTR-ABS/RL-INSTR-REL disagree on :RELATIVE, and RELD's carrying
;;; hole (the same (variant (choice m) (sub s)) selector mechanism #124/
;;; #127/#129 above reuse) is what makes the disagreement decodable, and
;;; both encode (%RELATIVE-OFFSET) and disassembly rendering
;;; (%OPERAND-RENDER-VALUES) correct, at all.

(defmode rl-instr-abs expr :width 1)
(defmode rl-instr-rel "#" expr :width 1 :relative t)
(defmode rl-instr-one (one-of rl-instr-abs rl-instr-rel))

(definstruction instr-test-machine reld
  (modes rl-instr-one)
  (encoding (opcode #x65)
            (operand val :width 1
              (variant (choice rl-instr-abs) (sub 0))
              (variant (choice rl-instr-rel) (sub 1))))
  (semantics (choice-case val
               (rl-instr-abs (set! pc val))
               (rl-instr-rel (set! pc (+ pc val))))))

(fiveam:test one-of-relative-stamps-relative-holes-per-descriptor
  (let* ((descs (find-instruction-descriptors-by-opcode 'instr-test-machine #x65))
         (abs (find 0 descs :key #'instruction-descriptor-sub-opcode))
         (rel (find 1 descs :key #'instruction-descriptor-sub-opcode)))
    (fiveam:is (equal '(nil) (instruction-descriptor-relative-holes abs)))
    (fiveam:is (equal '(t) (instruction-descriptor-relative-holes rel)))))

(fiveam:test one-of-relative-encode-decode-round-trips-the-absolute-alternative
  (fiveam:is (equalp #(#x65 0 200) (assembly-cells (assemble "reld 200" :machine 'instr-test-machine))))
  (multiple-value-bind (descriptor values size choices)
      (decode-instruction-at (vector-cell-reader (assembly-cells (assemble "reld 200" :machine 'instr-test-machine)))
                              0 'instr-test-machine)
    (declare (ignore size))
    (fiveam:is (string= "RELD" (instruction-descriptor-name descriptor)))
    (fiveam:is (equal '(200) values))
    (fiveam:is (eq 'rl-instr-abs (%matched-choice-name choices 0)))))

(fiveam:test one-of-relative-encode-decode-round-trips-the-relative-alternative
  ;; "reld #*" behaves like a self-referencing RELD: RELD is 3 bytes
  ;; (opcode, sub, 1-cell operand), so the offset from its own next
  ;; instruction (address 3) back to itself (address 0) is -3.
  (fiveam:is (equalp #(#x65 1 #xFD) (assembly-cells (assemble "reld #*" :machine 'instr-test-machine))))
  (multiple-value-bind (descriptor values size choices)
      (decode-instruction-at (vector-cell-reader (assembly-cells (assemble "reld #*" :machine 'instr-test-machine)))
                              0 'instr-test-machine)
    (declare (ignore size))
    (fiveam:is (string= "RELD" (instruction-descriptor-name descriptor)))
    (fiveam:is (equal '(-3) values))
    (fiveam:is (eq 'rl-instr-rel (%matched-choice-name choices 0)))))

(fiveam:test one-of-relative-disassembles-both-alternatives
  ;; RL-INSTR-ABS's hole renders its plain decoded value; RL-INSTR-REL's
  ;; renders the resolved absolute target (address 3 + size 3 + offset -3
  ;; = 3, RELD's own address -- "reld #*" is self-referencing).
  (let ((lines (disassemble-assembly (assemble "reld 200
reld #*" :machine 'instr-test-machine)
                                      :machine 'instr-test-machine :labels nil :suffixes nil)))
    (fiveam:is (string= "reld $C8" (disassembly-line-text (first lines))))
    (fiveam:is (string= "reld #$3" (disassembly-line-text (second lines))))))

;; A hole whose ONE-OF alternatives disagree on :RELATIVE but carries no
;; hole-selected sub-opcode selector at all has no decode-time record of
;; which alternative matched -- %CHECK-BYTE-ONE-OF-RELATIVE must reject it,
;; mirroring %CHECK-BYTE-ONE-OF-SIGNED's own selector requirement.
(fiveam:test one-of-relative-disagreement-without-selector-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction instr-test-machine reldbad1
             (modes rl-instr-one)
             (encoding (opcode #x66)
                       (operand val :width 1))
             (semantics nil)))))

;; A hole whose ONE-OF alternatives *agree* on :RELATIVE needs no selector
;; at all -- the hole's relativeness is static regardless of which one
;; matched. Both agree on being non-relative here.
(defmode rl-instr-agree-a expr :width 1)
(defmode rl-instr-agree-b "[" expr "]" :width 1)
(defmode rl-instr-agree (one-of rl-instr-agree-a rl-instr-agree-b))

(definstruction instr-test-machine reldok
  (modes rl-instr-agree)
  (encoding (opcode #x67)
            (operand val :width 1))
  (semantics (set! a val)))

(fiveam:test one-of-relative-agreeing-alternatives-need-no-selector
  (let ((descs (find-instruction-descriptors-by-opcode 'instr-test-machine #x67)))
    (fiveam:is (= 1 (length descs)))
    (fiveam:is (equal '(nil) (instruction-descriptor-relative-holes (first descs))))))

;; Each selected alternative supplies its own hole's relative flag.
(defmode rl-instr-two-abs expr :width 1)
(defmode rl-instr-two-rel "#" expr :width 1 :relative t)
(defmode rl-instr-two (one-of rl-instr-two-abs rl-instr-two-rel) ","
                       (one-of rl-instr-two-abs rl-instr-two-rel))

(definstruction instr-test-machine reldboth
  (modes rl-instr-two)
  (encoding (opcode #x68)
            (operand v1 :mode)
            (operand v2 :mode)
            (sub-opcode
              (variant (choice rl-instr-two-abs rl-instr-two-abs) (sub 0))
              (variant (choice rl-instr-two-abs rl-instr-two-rel) (sub 1))
              (variant (choice rl-instr-two-rel rl-instr-two-abs) (sub 2))
              (variant (choice rl-instr-two-rel rl-instr-two-rel) (sub 3))))
  (semantics nil))

(fiveam:test one-of-two-relative-holes-in-one-sibling
  (fiveam:is (equalp #(#x68 3 #xFC #xFC)
                     (assembly-cells (assemble "reldboth #*, #*" :machine 'instr-test-machine)))))

;; Identical field ranges cannot distinguish the selected alternative.
(fiveam:test overlapping-word-relative-alternatives-signal-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction mixed-field-test-machine reldword
             (modes rl-instr-one)
             (encoding (opcode 8)
                       (operand value :field src
                         (variant (choice rl-instr-abs) inline :range (0 511))
                         (variant (choice rl-instr-rel) inline :range (0 511))))
             (semantics nil)))))

;; :RELATIVE and :WIDTH disagreeing at the SAME hole (not two different
;; holes, the way BRW above puts them -- examples/subtable.lisp) -- one
;; sub-opcode selector satisfying both %CHECK-BYTE-ONE-OF-WIDTH and
;; %CHECK-BYTE-ONE-OF-RELATIVE at once, since each reads SUB-CHOICES
;; independently. Left open by #130's own design comment as unverified
;; composition; this confirms it composes cleanly with no new machinery.
(defmode rw-narrow-abs expr :width 1)
(defmode rw-wide-rel "#" expr :width 2 :relative t)
(defmode rw-one (one-of rw-narrow-abs rw-wide-rel))

(definstruction instr-test-machine relwd
  (modes rw-one)
  (encoding (opcode #x6A)
            (operand val :mode
              (variant (choice rw-narrow-abs) (sub 0))
              (variant (choice rw-wide-rel) (sub 1))))
  (semantics (choice-case val
               (rw-narrow-abs (set! a val))
               (rw-wide-rel (set! pc (+ pc val))))))

;; #133: :SIGNED and :RELATIVE disagreeing INDEPENDENTLY at the same hole --
;; not two different holes (BRW above), and not two holes both narrowing to
;; the same MODE-DESCRIPTOR-SIGNEDP the way :WIDTH's own disagreement can
;; (#130's own closing comment left this unverified). RS-SIGNED-ABS is a
;; plain signed, non-relative immediate; RS-REL is a PC-relative offset --
;; MODE-DESCRIPTOR-SIGNEDP is (OR RELATIVE SIGNED), so both alternatives
;; report SIGNEDP = T (agreeing, invisible to %ONE-OF-SIGNED-DISAGREEMENT),
;; while only RELATIVEP genuinely differs (caught by
;; %ONE-OF-RELATIVE-DISAGREEMENT testing RELATIVEP directly, per its own
;; docstring). One sub-opcode selector satisfies both.
(defmode rs-signed-abs "#" expr :width 1 :signed t)
(defmode rs-rel "&" expr :width 1 :relative t)
(defmode rs-one (one-of rs-signed-abs rs-rel))

(definstruction instr-test-machine rsig
  (modes rs-one)
  (encoding (opcode #x0F)
            (operand val :mode
              (variant (choice rs-signed-abs) (sub 0))
              (variant (choice rs-rel) (sub 1))))
  (semantics (choice-case val
               (rs-signed-abs (set! a val))
               (rs-rel (set! pc (+ pc val))))))

(fiveam:test one-of-signed-and-relative-disagreeing-at-the-same-hole-is-legal
  (let* ((descs (find-instruction-descriptors-by-opcode 'instr-test-machine #x0F))
         (signed (find 0 descs :key #'instruction-descriptor-sub-opcode))
         (relative (find 1 descs :key #'instruction-descriptor-sub-opcode)))
    ;; Both siblings are signed; only the relative flags distinguish them.
    (fiveam:is (equal '(t) (instruction-descriptor-operand-signedness signed)))
    (fiveam:is (equal '(nil) (instruction-descriptor-relative-holes signed)))
    (fiveam:is (equal '(t) (instruction-descriptor-operand-signedness relative)))
    (fiveam:is (equal '(t) (instruction-descriptor-relative-holes relative)))))

(fiveam:test one-of-signed-and-relative-round-trips-the-plain-signed-alternative
  ;; The offset arithmetic actually differs, not just the descriptor slots:
  ;; a plain signed immediate encodes/decodes its literal value untouched.
  (let ((cells (assembly-cells (assemble "rsig #-5" :machine 'instr-test-machine))))
    (fiveam:is (equalp #(#x0F 0 #xFB) cells))
    (multiple-value-bind (descriptor values size choices)
        (decode-instruction-at (vector-cell-reader cells) 0 'instr-test-machine)
      (declare (ignore size))
      (fiveam:is (string= "RSIG" (instruction-descriptor-name descriptor)))
      (fiveam:is (equal '(-5) values))
      (fiveam:is (eq 'rs-signed-abs (%matched-choice-name choices 0))))))

(fiveam:test one-of-signed-and-relative-round-trips-the-relative-alternative
  ;; The relative alternative instead goes through %RELATIVE-OFFSET's
  ;; PC-relative arithmetic: RSIG is 3 bytes (opcode, sub, 1-cell val), NOP
  ;; is 1, so TARGET at address 4 is offset 4 - 3 = 1 from RSIG's own next
  ;; instruction.
  (let* ((source "rsig &target
nop
target: nop")
         (assembly (assemble source :machine 'instr-test-machine))
         (cells (assembly-cells assembly)))
    (fiveam:is (equalp #(#x0F 1 1 #xEA #xEA) cells))
    (multiple-value-bind (descriptor values size choices)
        (decode-instruction-at (vector-cell-reader cells) 0 'instr-test-machine)
      (declare (ignore size))
      (fiveam:is (string= "RSIG" (instruction-descriptor-name descriptor)))
      (fiveam:is (equal '(1) values))
      (fiveam:is (eq 'rs-rel (%matched-choice-name choices 0))))
    (let ((lines (disassemble-assembly assembly :machine 'instr-test-machine :labels nil :suffixes nil)))
      (fiveam:is (string= "rsig &$4" (disassembly-line-text (first lines)))))))

(fiveam:test one-of-relative-and-width-disagreeing-at-the-same-hole-is-legal
  (let* ((descs (find-instruction-descriptors-by-opcode 'instr-test-machine #x6A))
         (narrow (find 0 descs :key #'instruction-descriptor-sub-opcode))
         (wide (find 1 descs :key #'instruction-descriptor-sub-opcode)))
    (fiveam:is (equal '(1) (instruction-descriptor-operand-widths narrow)))
    (fiveam:is (equal '(nil) (instruction-descriptor-relative-holes narrow)))
    (fiveam:is (equal '(2) (instruction-descriptor-operand-widths wide)))
    (fiveam:is (equal '(t) (instruction-descriptor-relative-holes wide)))))

(fiveam:test one-of-relative-and-width-round-trips-the-narrow-non-relative-alternative
  (fiveam:is (equalp #(#x6A 0 200) (assembly-cells (assemble "relwd 200" :machine 'instr-test-machine))))
  (multiple-value-bind (descriptor values size choices)
      (decode-instruction-at (vector-cell-reader (assembly-cells (assemble "relwd 200" :machine 'instr-test-machine)))
                              0 'instr-test-machine)
    (fiveam:is (string= "RELWD" (instruction-descriptor-name descriptor)))
    (fiveam:is (equal '(200) values))
    (fiveam:is (= 3 size))
    (fiveam:is (eq 'rw-narrow-abs (%matched-choice-name choices 0)))))

(fiveam:test one-of-relative-and-width-round-trips-the-wide-relative-alternative
  ;; "relwd #*" is self-referencing: RELWD is 4 bytes (opcode, sub, 2-cell
  ;; operand), so the offset from its own next-instruction address (4) back
  ;; to itself (0) is -4, encoded 2's-complement over 2 cells (little-endian
  ;; #xFC #xFF) -- RW-WIDE-REL's own 2-cell width, not the 1-cell width its
  ;; RW-NARROW-ABS sibling declares.
  (fiveam:is (equalp #(#x6A 1 #xFC #xFF) (assembly-cells (assemble "relwd #*" :machine 'instr-test-machine))))
  (multiple-value-bind (descriptor values size choices)
      (decode-instruction-at (vector-cell-reader (assembly-cells (assemble "relwd #*" :machine 'instr-test-machine)))
                              0 'instr-test-machine)
    (fiveam:is (string= "RELWD" (instruction-descriptor-name descriptor)))
    (fiveam:is (equal '(-4) values))
    (fiveam:is (= 4 size))
    (fiveam:is (eq 'rw-wide-rel (%matched-choice-name choices 0)))))

;; #133's own closing paragraph: a hole with all THREE of :SIGNED, :WIDTH,
;; and :RELATIVE disagreeing at once, worth checking together with #131 --
;; RW-ONE's own two alternatives above (RW-NARROW-ABS/RW-WIDE-REL) already
;; are this case, not just the :WIDTH/:RELATIVE pair its own comment names:
;; MODE-DESCRIPTOR-SIGNEDP is (OR RELATIVE SIGNED), so RW-WIDE-REL's
;; :RELATIVE T makes it SIGNEDP T too, disagreeing with RW-NARROW-ABS's
;; plain NIL -- the third attribute was disagreeing all along. One shared
;; sub-opcode selector composes it with no new machinery.
(fiveam:test one-of-signed-width-and-relative-disagreeing-at-the-same-hole-is-legal
  (let* ((descs (find-instruction-descriptors-by-opcode 'instr-test-machine #x6A))
         (narrow (find 0 descs :key #'instruction-descriptor-sub-opcode))
         (wide (find 1 descs :key #'instruction-descriptor-sub-opcode)))
    (fiveam:is (equal '(nil) (instruction-descriptor-operand-signedness narrow)))
    (fiveam:is (equal '(t) (instruction-descriptor-operand-signedness wide)))))

;; The same three-way disagreement, but at one hole of a genuine multi-hole
;; (sub-opcode ...) table (#131) rather than the single-hole selector sugar
;; above -- TW-A1/TW-A2 disagree on all three attributes at hole 0; hole 1
;; (TW-MID1/TW-MID2) is a second ONE-OF hole whose own alternatives agree on
;; everything, so (holes 0) leaves it uncovered, exactly the #131 shape.
(defmode tw-a1 expr :width 1)
(defmode tw-a2 "&" expr :width 2 :relative t)
(defmode tw-mid1 expr)
(defmode tw-mid2 "[" expr "]")
(defmode tw-two (one-of tw-a1 tw-a2) "," (one-of tw-mid1 tw-mid2))

(definstruction instr-test-machine trisig
  (modes tw-two)
  (encoding (opcode #x01)
            (operand val :mode)
            (operand mid :width 1)
            (sub-opcode
              (holes 0)
              (variant (choice tw-a1) (sub 0))
              (variant (choice tw-a2) (sub 1))))
  (semantics (choice-case val
               (tw-a1 (set! a val))
               (tw-a2 (set! pc (+ pc val))))))

(fiveam:test one-of-three-way-disagreement-inside-a-sub-opcode-table-is-legal
  (let* ((descs (find-instruction-descriptors-by-opcode 'instr-test-machine #x01))
         (narrow (find 0 descs :key #'instruction-descriptor-sub-opcode))
         (wide (find 1 descs :key #'instruction-descriptor-sub-opcode)))
    (fiveam:is (equal '(1 1) (instruction-descriptor-operand-widths narrow)))
    (fiveam:is (equal '(nil nil) (instruction-descriptor-operand-signedness narrow)))
    (fiveam:is (equal '(nil nil) (instruction-descriptor-relative-holes narrow)))
    (fiveam:is (equal '(2 1) (instruction-descriptor-operand-widths wide)))
    (fiveam:is (equal '(t nil) (instruction-descriptor-operand-signedness wide)))
    (fiveam:is (equal '(t nil) (instruction-descriptor-relative-holes wide)))))

(fiveam:test one-of-three-way-disagreement-round-trips-the-narrow-alternative
  (let ((cells (assembly-cells (assemble "trisig 5, 7" :machine 'instr-test-machine))))
    (fiveam:is (equalp #(#x01 0 5 7) cells))
    (multiple-value-bind (descriptor values size choices)
        (decode-instruction-at (vector-cell-reader cells) 0 'instr-test-machine)
      (declare (ignore size))
      (fiveam:is (string= "TRISIG" (instruction-descriptor-name descriptor)))
      (fiveam:is (equal '(5 7) values))
      (fiveam:is (eq 'tw-a1 (%matched-choice-name choices 0)))
      ;; Hole 1 is uncovered by (holes 0) -- no CHOICES record, exactly the
      ;; #131 subsetting shape composed with #133's three-way disagreement.
      (fiveam:is (null (%matched-choice-name choices 1))))))

(fiveam:test one-of-three-way-disagreement-round-trips-the-wide-relative-alternative
  (let* ((source "trisig &target, [7]
nop
target: nop")
         (assembly (assemble source :machine 'instr-test-machine))
         (cells (assembly-cells assembly)))
    ;; TRISIG is 5 bytes here (opcode, sub, 2-cell val, 1-cell mid); TARGET
    ;; sits after it and one NOP, at address 6 -- offset from TRISIG's own
    ;; next-instruction address (5) is 6 - 5 = 1.
    (fiveam:is (equalp #(#x01 1 1 0 7 #xEA #xEA) cells))
    (multiple-value-bind (descriptor values size choices)
        (decode-instruction-at (vector-cell-reader cells) 0 'instr-test-machine)
      (declare (ignore size))
      (fiveam:is (string= "TRISIG" (instruction-descriptor-name descriptor)))
      (fiveam:is (equal '(1 7) values))
      (fiveam:is (eq 'tw-a2 (%matched-choice-name choices 0)))
      (fiveam:is (null (%matched-choice-name choices 1))))
    ;; Hole 1's own CHOICES entry is NIL (uncovered by (holes 0)), so
    ;; disassembly falls back to its first alternative's plain syntax
    ;; (TW-MID1, no brackets) regardless of which one was actually written
    ;; -- both encode the identical value, exactly [Modes, "What one-of
    ;; does and does not do"] describes for a hole with no decode-time
    ;; record.
    (let ((lines (disassemble-assembly assembly :machine 'instr-test-machine :labels nil :suffixes nil)))
      (fiveam:is (string= "trisig &$6,$7" (disassembly-line-text (first lines)))))))

;;; Multi-hole sub-opcode selection, a (sub-opcode ...) table (#128, the
;;; follow-up #126 filed for itself) -- several ONE-OF holes jointly
;;; selecting the sub-opcode cell, rather than #126's single carrying hole.
;;; Reuses OO-INSTR-TWO (defined above, #18/#104-shaped: two ONE-OF holes,
;;; each (one-of oo-instr-reg oo-instr-ind)) so the table has a genuine 2x2
;;; cross product to cover.

(definstruction instr-test-machine sctab
  (modes oo-instr-two)
  (encoding (opcode #xD3)
            (operand dst :width 1)
            (operand src :width 1)
            (sub-opcode
              (variant (choice oo-instr-reg oo-instr-reg) (sub 0))
              (variant (choice oo-instr-reg oo-instr-ind) (sub 1))
              (variant (choice oo-instr-ind oo-instr-reg) (sub 2))
              (variant (choice oo-instr-ind oo-instr-ind) (sub 3))))
  (semantics
    (let ((s (choice-case src
               (oo-instr-reg src)
               (oo-instr-ind (mref machine 'ram src)))))
      (choice-case dst
        (oo-instr-reg (setf (mref machine 'ram dst) s))
        (oo-instr-ind (setf (mref machine 'ram (mref machine 'ram dst)) s))))))

(fiveam:test sub-opcode-table-expands-one-descriptor-per-combination
  (let ((variants (find-instruction-variants 'instr-test-machine 'sctab)))
    (fiveam:is (= 4 (length variants))))
  (let* ((descs (find-instruction-descriptors-by-opcode 'instr-test-machine #xD3))
         (rr (find 0 descs :key #'instruction-descriptor-sub-opcode))
         (ri (find 1 descs :key #'instruction-descriptor-sub-opcode))
         (ir (find 2 descs :key #'instruction-descriptor-sub-opcode))
         (ii (find 3 descs :key #'instruction-descriptor-sub-opcode)))
    (fiveam:is (= 4 (length descs)))
    (fiveam:is (equal '(oo-instr-reg oo-instr-reg) (instruction-descriptor-sub-choices rr)))
    (fiveam:is (equal '(oo-instr-reg oo-instr-ind) (instruction-descriptor-sub-choices ri)))
    (fiveam:is (equal '(oo-instr-ind oo-instr-reg) (instruction-descriptor-sub-choices ir)))
    (fiveam:is (equal '(oo-instr-ind oo-instr-ind) (instruction-descriptor-sub-choices ii)))))

(fiveam:test sub-opcode-table-encode-emits-each-combinations-own-sub
  (let* ((descs (find-instruction-descriptors-by-opcode 'instr-test-machine #xD3))
         (rr (find 0 descs :key #'instruction-descriptor-sub-opcode))
         (ri (find 1 descs :key #'instruction-descriptor-sub-opcode))
         (ir (find 2 descs :key #'instruction-descriptor-sub-opcode))
         (ii (find 3 descs :key #'instruction-descriptor-sub-opcode)))
    (fiveam:is (equal (list #xD3 0 5 10) (encode-instruction rr '(5 10))))
    (fiveam:is (equal (list #xD3 1 5 10) (encode-instruction ri '(5 10))))
    (fiveam:is (equal (list #xD3 2 5 10) (encode-instruction ir '(5 10))))
    (fiveam:is (equal (list #xD3 3 5 10) (encode-instruction ii '(5 10))))))

(fiveam:test sub-opcode-table-assembler-picks-matching-combination
  (fiveam:is (equalp #(#xD3 0 5 10) (assembly-cells (assemble "sctab 5, 10" :machine 'instr-test-machine))))
  (fiveam:is (equalp #(#xD3 1 5 10) (assembly-cells (assemble "sctab 5, [10]" :machine 'instr-test-machine))))
  (fiveam:is (equalp #(#xD3 2 5 10) (assembly-cells (assemble "sctab [5], 10" :machine 'instr-test-machine))))
  (fiveam:is (equalp #(#xD3 3 5 10) (assembly-cells (assemble "sctab [5], [10]" :machine 'instr-test-machine)))))

(fiveam:test sub-opcode-table-decode-reports-both-holes-choices
  (multiple-value-bind (descriptor values size choices)
      (decode-instruction-at (vector-cell-reader (vector #xD3 1 5 10)) 0 'instr-test-machine)
    (declare (ignore values size))
    (fiveam:is (string= "SCTAB" (instruction-descriptor-name descriptor)))
    (fiveam:is (eq 'oo-instr-reg (%matched-choice-name choices 0)))
    (fiveam:is (eq 'oo-instr-ind (%matched-choice-name choices 1)))))

(fiveam:test sub-opcode-table-choice-case-dispatches-on-both-holes
  (let* ((m (make-machine 'instr-test-machine))
         (descs (find-instruction-descriptors-by-opcode 'instr-test-machine #xD3))
         (ri (find 1 descs :key #'instruction-descriptor-sub-opcode)))
    (setf (mref m 'ram 10) 77)
    (execute-instruction ri m '(5 10) (instruction-descriptor-sub-choices ri))
    (fiveam:is (= 77 (mref m 'ram 5)))))

(fiveam:test sub-opcode-table-round-trip-disassembles-matched-alternative-at-both-holes
  (let ((lines (disassemble-assembly (assemble "sctab 5, [10]" :machine 'instr-test-machine)
                                      :machine 'instr-test-machine :labels nil :suffixes nil)))
    (fiveam:is (string= "sctab $5,[$A]" (disassembly-line-text (first lines))))))

;; Mixed plain/ONE-OF holes: arity is the mode's ONE-OF hole count, not its
;; total hole count, and HOLE-INDICES must map back to the right positions --
;; the middle and last holes here, skipping the first (plain EXPR) one.
(defmode oo-instr-mixed-three expr "," (one-of oo-instr-reg oo-instr-ind) "," (one-of oo-instr-reg oo-instr-ind))

(definstruction instr-test-machine scmix
  (modes oo-instr-mixed-three)
  (encoding (opcode #xD4)
            (operand :width 1)
            (operand :width 1)
            (operand :width 1)
            (sub-opcode
              (variant (choice oo-instr-reg oo-instr-reg) (sub 0))
              (variant (choice oo-instr-reg oo-instr-ind) (sub 1))
              (variant (choice oo-instr-ind oo-instr-reg) (sub 2))
              (variant (choice oo-instr-ind oo-instr-ind) (sub 3))))
  (semantics nil))

(fiveam:test sub-opcode-table-mixed-plain-and-one-of-holes-populates-right-indices
  (let* ((descs (find-instruction-descriptors-by-opcode 'instr-test-machine #xD4))
         (rr (find 0 descs :key #'instruction-descriptor-sub-opcode))
         (ri (find 1 descs :key #'instruction-descriptor-sub-opcode))
         (ir (find 2 descs :key #'instruction-descriptor-sub-opcode))
         (ii (find 3 descs :key #'instruction-descriptor-sub-opcode)))
    (fiveam:is (= 4 (length descs)))
    (fiveam:is (equal '(nil oo-instr-reg oo-instr-reg) (instruction-descriptor-sub-choices rr)))
    (fiveam:is (equal '(nil oo-instr-reg oo-instr-ind) (instruction-descriptor-sub-choices ri)))
    (fiveam:is (equal '(nil oo-instr-ind oo-instr-reg) (instruction-descriptor-sub-choices ir)))
    (fiveam:is (equal '(nil oo-instr-ind oo-instr-ind) (instruction-descriptor-sub-choices ii)))))

;; Per-hole :SIGNED (#124/#127's byte half) on TWO holes at once -- #128
;; lifts %CHECK-BYTE-ONE-OF-SIGNED's one-hole cap alongside the sub-opcode
;; one, since per-hole :SIGNED uses the same selector as its own decode-time
;; discriminator.
(defmode sit-pos-a expr)
(defmode sit-neg-a "#" expr :signed t)
(defmode sit-pos-b expr)
(defmode sit-neg-b "[" expr "]" :signed t)
(defmode sit-two (one-of sit-pos-a sit-neg-a) "," (one-of sit-pos-b sit-neg-b))

(definstruction instr-test-machine sigtab
  (modes sit-two)
  (encoding (opcode #xD5)
            (operand v1 :width 1)
            (operand v2 :width 1)
            (sub-opcode
              (variant (choice sit-pos-a sit-pos-b) (sub 0))
              (variant (choice sit-pos-a sit-neg-b) (sub 1))
              (variant (choice sit-neg-a sit-pos-b) (sub 2))
              (variant (choice sit-neg-a sit-neg-b) (sub 3))))
  (semantics (set! a v1) (set! x v2)))

(fiveam:test sub-opcode-table-signed-disagreement-on-two-holes-is-legal
  (let* ((descs (find-instruction-descriptors-by-opcode 'instr-test-machine #xD5))
         (pp (find 0 descs :key #'instruction-descriptor-sub-opcode))
         (pn (find 1 descs :key #'instruction-descriptor-sub-opcode))
         (np (find 2 descs :key #'instruction-descriptor-sub-opcode))
         (nn (find 3 descs :key #'instruction-descriptor-sub-opcode)))
    (fiveam:is (equal '(nil nil) (instruction-descriptor-operand-signedness pp)))
    (fiveam:is (equal '(nil t) (instruction-descriptor-operand-signedness pn)))
    (fiveam:is (equal '(t nil) (instruction-descriptor-operand-signedness np)))
    (fiveam:is (equal '(t t) (instruction-descriptor-operand-signedness nn)))))

(fiveam:test sub-opcode-table-signed-round-trips-negative-values-at-both-holes
  (fiveam:is (equalp #(#xD5 3 156 200) (assembly-cells (assemble "sigtab #-100, [-56]" :machine 'instr-test-machine))))
  (multiple-value-bind (descriptor values size choices)
      (decode-instruction-at (vector-cell-reader (assembly-cells (assemble "sigtab #-100, [-56]" :machine 'instr-test-machine)))
                              0 'instr-test-machine)
    (declare (ignore size choices))
    (fiveam:is (string= "SIGTAB" (instruction-descriptor-name descriptor)))
    (fiveam:is (equal '(-100 -56) values))))

;; Per-hole :WIDTH (#129) on TWO holes at once, via a (sub-opcode ...) table
;; -- the multi-hole generalization %CHECK-BYTE-ONE-OF-WIDTH inherits from
;; %CHECK-BYTE-ONE-OF-SIGNED (#128 lifted the one-hole cap for both at once,
;; since both use the same carrying-hole-set selector as their decode-time
;; discriminator).
(defmode wit-narrow-a expr :width 1)
(defmode wit-wide-a "#" expr :width 2)
(defmode wit-narrow-b expr :width 1)
(defmode wit-wide-b "[" expr "]" :width 2)
(defmode wit-two (one-of wit-narrow-a wit-wide-a) "," (one-of wit-narrow-b wit-wide-b))

(definstruction instr-test-machine widthtab
  (modes wit-two)
  (encoding (opcode #x64)
            (operand v1 :mode)
            (operand v2 :mode)
            (sub-opcode
              (variant (choice wit-narrow-a wit-narrow-b) (sub 0))
              (variant (choice wit-narrow-a wit-wide-b) (sub 1))
              (variant (choice wit-wide-a wit-narrow-b) (sub 2))
              (variant (choice wit-wide-a wit-wide-b) (sub 3))))
  (semantics (set! a v1) (set! x v2)))

(fiveam:test sub-opcode-table-width-disagreement-on-two-holes-is-legal
  (let* ((descs (find-instruction-descriptors-by-opcode 'instr-test-machine #x64))
         (nn (find 0 descs :key #'instruction-descriptor-sub-opcode))
         (nw (find 1 descs :key #'instruction-descriptor-sub-opcode))
         (wn (find 2 descs :key #'instruction-descriptor-sub-opcode))
         (ww (find 3 descs :key #'instruction-descriptor-sub-opcode)))
    (fiveam:is (equal '(1 1) (instruction-descriptor-operand-widths nn)))
    (fiveam:is (equal '(1 2) (instruction-descriptor-operand-widths nw)))
    (fiveam:is (equal '(2 1) (instruction-descriptor-operand-widths wn)))
    (fiveam:is (equal '(2 2) (instruction-descriptor-operand-widths ww)))))

(fiveam:test sub-opcode-table-width-round-trips-wide-values-at-both-holes
  (fiveam:is (equalp #(#x64 3 44 1 144 1)
                      (assembly-cells (assemble "widthtab #300, [400]" :machine 'instr-test-machine))))
  (multiple-value-bind (descriptor values size choices)
      (decode-instruction-at (vector-cell-reader (assembly-cells (assemble "widthtab #300, [400]" :machine 'instr-test-machine)))
                              0 'instr-test-machine)
    (declare (ignore choices))
    (fiveam:is (string= "WIDTHTAB" (instruction-descriptor-name descriptor)))
    (fiveam:is (equal '(300 400) values))
    (fiveam:is (= 6 size))))

;; Missing combination -- the table's own generalization of #126's "every
;; alternative claimed" rule to "every combination claimed".
(fiveam:test sub-opcode-table-missing-combination-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction instr-test-machine sctbad1
             (modes oo-instr-two)
             (encoding (opcode #xE0)
                       (operand dst :width 1)
                       (operand src :width 1)
                       (sub-opcode
                         (variant (choice oo-instr-reg oo-instr-reg) (sub 0))
                         (variant (choice oo-instr-reg oo-instr-ind) (sub 1))
                         (variant (choice oo-instr-ind oo-instr-reg) (sub 2))))
             (semantics nil)))))

;; Duplicated combination -- two entries claiming the same (choice ...) can
;; never be told apart either.
(fiveam:test sub-opcode-table-duplicated-combination-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction instr-test-machine sctbad2
             (modes oo-instr-two)
             (encoding (opcode #xE1)
                       (operand dst :width 1)
                       (operand src :width 1)
                       (sub-opcode
                         (variant (choice oo-instr-reg oo-instr-reg) (sub 0))
                         (variant (choice oo-instr-reg oo-instr-reg) (sub 1))
                         (variant (choice oo-instr-reg oo-instr-ind) (sub 2))
                         (variant (choice oo-instr-ind oo-instr-reg) (sub 3))
                         (variant (choice oo-instr-ind oo-instr-ind) (sub 4))))
             (semantics nil)))))

;; (choice ...) arity must equal the mode's participating ONE-OF hole count.
(fiveam:test sub-opcode-table-wrong-arity-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction instr-test-machine sctbad3
             (modes oo-instr-two)
             (encoding (opcode #xE2)
                       (operand dst :width 1)
                       (operand src :width 1)
                       (sub-opcode
                         (variant (choice oo-instr-reg) (sub 0))))
             (semantics nil)))))

;; A (choice ...) name that isn't one of its own hole's ONE-OF alternatives.
(fiveam:test sub-opcode-table-unknown-alternative-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction instr-test-machine sctbad4
             (modes oo-instr-two)
             (encoding (opcode #xE3)
                       (operand dst :width 1)
                       (operand src :width 1)
                       (sub-opcode
                         (variant (choice immediate oo-instr-reg) (sub 0))
                         (variant (choice oo-instr-reg oo-instr-ind) (sub 1))
                         (variant (choice oo-instr-ind oo-instr-reg) (sub 2))
                         (variant (choice oo-instr-ind oo-instr-ind) (sub 3))))
             (semantics nil)))))

;; Two combinations claiming the same sub value.
(fiveam:test sub-opcode-table-duplicate-sub-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction instr-test-machine sctbad5
             (modes oo-instr-two)
             (encoding (opcode #xE4)
                       (operand dst :width 1)
                       (operand src :width 1)
                       (sub-opcode
                         (variant (choice oo-instr-reg oo-instr-reg) (sub 0))
                         (variant (choice oo-instr-reg oo-instr-ind) (sub 0))
                         (variant (choice oo-instr-ind oo-instr-reg) (sub 2))
                         (variant (choice oo-instr-ind oo-instr-ind) (sub 3))))
             (semantics nil)))))

;; The cross product itself is too large for the machine's code cell width
;; to distinguish, regardless of how many entries the table actually lists.
(defmachine subtab-narrow-machine
  (register a :width 8)
  (memory ram :width 2 :addr-width 8))

(defmode nw-a1 expr)
(defmode nw-a2 "[" expr "]")
(defmode nw-a3 "(" expr ")")
(defmode nw-two (one-of nw-a1 nw-a2 nw-a3) "," (one-of nw-a1 nw-a2 nw-a3))

(fiveam:test sub-opcode-table-product-too-wide-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction subtab-narrow-machine sctbad6
             (modes nw-two)
             (encoding (opcode 0)
                       (operand dst :width 1)
                       (operand src :width 1)
                       (sub-opcode
                         (variant (choice nw-a1 nw-a1) (sub 0))))
             (semantics nil)))))

;; A (sub-opcode ...) table and a per-hole (variant (choice m) (sub s))
;; selector on another hole would both be writing the same cell.
(fiveam:test sub-opcode-table-conflicts-with-per-hole-selector-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction instr-test-machine sctbad7
             (modes oo-instr-two)
             (encoding (opcode #xE5)
                       (operand dst :width 1
                         (variant (choice oo-instr-reg) (sub 0))
                         (variant (choice oo-instr-ind) (sub 1)))
                       (operand src :width 1)
                       (sub-opcode
                         (variant (choice oo-instr-reg oo-instr-reg) (sub 0))
                         (variant (choice oo-instr-reg oo-instr-ind) (sub 1))
                         (variant (choice oo-instr-ind oo-instr-reg) (sub 2))
                         (variant (choice oo-instr-ind oo-instr-ind) (sub 3))))
             (semantics nil)))))

;; A (sub-opcode ...) table and an explicit (opcode n :sub s) would also both
;; be writing the same cell.
(fiveam:test sub-opcode-table-conflicts-with-explicit-sub-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction instr-test-machine sctbad8
             (modes oo-instr-two)
             (encoding (opcode #xE6 :sub 9)
                       (operand dst :width 1)
                       (operand src :width 1)
                       (sub-opcode
                         (variant (choice oo-instr-reg oo-instr-reg) (sub 0))
                         (variant (choice oo-instr-reg oo-instr-ind) (sub 1))
                         (variant (choice oo-instr-ind oo-instr-reg) (sub 2))
                         (variant (choice oo-instr-ind oo-instr-ind) (sub 3))))
             (semantics nil)))))

;; (sub-opcode ...) is a byte-machine-only mechanism, same as a hole-selected
;; single-hole selector.
(fiveam:test sub-opcode-table-on-word-machine-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction word-test-machine sctwordbad
             (modes wc-two)
             (encoding (opcode 20)
                       (operand v)
                       (sub-opcode
                         (variant (choice wc-reg wc-reg) (sub 0))))
             (semantics (set! a v))))))

;; A mode with no ONE-OF hole at all has nothing for a table to select
;; between.
(fiveam:test sub-opcode-table-no-one-of-hole-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction instr-test-machine sctbad9
             (modes absolute)
             (encoding (opcode #xE7)
                       (operand src :width 1)
                       (sub-opcode
                         (variant (choice absolute) (sub 0))))
             (semantics nil)))))

;;; (holes ...) subsetting (#131): a table may cover fewer than every ONE-OF
;;; hole of the mode, naming the ones it covers by 0-based pattern-order
;;; index. HOLES-THREE has three ONE-OF holes; the tables below cover only
;;; holes 0 and 2, leaving hole 1 uncovered -- its own alternatives (B1/B2)
;;; agree on :SIGNED/:WIDTH/:RELATIVE (neither declares any of them), so no
;;; selector is required for it, exactly as an ungoverned or fully-uncovered
;;; ONE-OF hole always needed none before #128 existed at all.
(defmode holes-a1 expr)
(defmode holes-a2 "[" expr "]")
(defmode holes-b1 expr)
(defmode holes-b2 "[" expr "]")
(defmode holes-c1 expr)
(defmode holes-c2 "[" expr "]")
(defmode holes-three
    (one-of holes-a1 holes-a2) "," (one-of holes-b1 holes-b2) "," (one-of holes-c1 holes-c2))

(definstruction instr-test-machine holtab
  (modes holes-three)
  (encoding (opcode #xE8)
            (operand h0 :width 1)
            (operand h1 :width 1)
            (operand h2 :width 1)
            (sub-opcode
              (holes 0 2)
              (variant (choice holes-a1 holes-c1) (sub 0))
              (variant (choice holes-a1 holes-c2) (sub 1))
              (variant (choice holes-a2 holes-c1) (sub 2))
              (variant (choice holes-a2 holes-c2) (sub 3))))
  (semantics
    (choice-case h0
      (holes-a1 (set! a h0))
      (holes-a2 (set! a (mref machine 'ram h0))))))

(fiveam:test sub-opcode-table-holes-clause-subsets-participating-holes
  (let* ((descs (find-instruction-descriptors-by-opcode 'instr-test-machine #xE8))
         (d0 (find 0 descs :key #'instruction-descriptor-sub-opcode))
         (d1 (find 1 descs :key #'instruction-descriptor-sub-opcode))
         (d2 (find 2 descs :key #'instruction-descriptor-sub-opcode))
         (d3 (find 3 descs :key #'instruction-descriptor-sub-opcode)))
    (fiveam:is (= 4 (length descs)))
    ;; Hole 1 (the uncovered one) is NIL in every sibling's SUB-CHOICES --
    ;; only holes 0 and 2, the ones (holes 0 2) actually named, are populated.
    (fiveam:is (equal '(holes-a1 nil holes-c1) (instruction-descriptor-sub-choices d0)))
    (fiveam:is (equal '(holes-a1 nil holes-c2) (instruction-descriptor-sub-choices d1)))
    (fiveam:is (equal '(holes-a2 nil holes-c1) (instruction-descriptor-sub-choices d2)))
    (fiveam:is (equal '(holes-a2 nil holes-c2) (instruction-descriptor-sub-choices d3)))))

(fiveam:test sub-opcode-table-holes-clause-round-trips-through-assembler-and-decoder
  (fiveam:is (equalp #(#xE8 2 5 20 7)
                      (assembly-cells (assemble "holtab [5], 20, 7" :machine 'instr-test-machine))))
  (multiple-value-bind (descriptor values size choices)
      (decode-instruction-at (vector-cell-reader (vector #xE8 2 5 20 7)) 0 'instr-test-machine)
    (declare (ignore size))
    (fiveam:is (string= "HOLTAB" (instruction-descriptor-name descriptor)))
    (fiveam:is (equal '(5 20 7) values))
    (fiveam:is (eq 'holes-a2 (%matched-choice-name choices 0)))
    (fiveam:is (null (%matched-choice-name choices 1)))
    (fiveam:is (eq 'holes-c1 (%matched-choice-name choices 2))))
  (let ((lines (disassemble-assembly (assemble "holtab [5], 20, 7" :machine 'instr-test-machine)
                                      :machine 'instr-test-machine :labels nil :suffixes nil)))
    (fiveam:is (string= "holtab [$5],$14,$7" (disassembly-line-text (first lines))))))

;; A CHOICE-CASE naming the uncovered hole's own alternatives still parses
;; (HOLE-ALTERNATIVES is non-NIL there -- it IS a ONE-OF hole, just not one
;; this table covers): %CHECK-CHOICE-CASE-KEYS! only checks ONE-OF
;; membership, not selector coverage. At runtime SUB-CHOICES' entry for that
;; hole is always NIL, so every clause falls through to OTHERWISE or
;; NO-MATCHING-CHOICE -- the same pre-#126 behavior any ONE-OF hole with no
;; selector at all has always had (see HOLE-SELECTED-SUB-OPCODE-CHOICE-CASE-
;; DISPATCHES-ON-BYTE-MACHINE's own comment above).
(definstruction instr-test-machine holtabcc
  (modes holes-three)
  (encoding (opcode #xE9)
            (operand h0 :width 1)
            (operand h1 :width 1)
            (operand h2 :width 1)
            (sub-opcode
              (holes 0 2)
              (variant (choice holes-a1 holes-c1) (sub 0))
              (variant (choice holes-a1 holes-c2) (sub 1))
              (variant (choice holes-a2 holes-c1) (sub 2))
              (variant (choice holes-a2 holes-c2) (sub 3))))
  (semantics
    (choice-case h1
      (holes-b1 (set! a 1))
      (holes-b2 (set! a 2)))))

(fiveam:test sub-opcode-table-holes-clause-choice-case-on-uncovered-hole-never-matches
  (let* ((m (make-machine 'instr-test-machine))
         (descs (find-instruction-descriptors-by-opcode 'instr-test-machine #xE9))
         (d0 (find 0 descs :key #'instruction-descriptor-sub-opcode)))
    (fiveam:signals no-matching-choice
      (execute-instruction d0 m '(5 20 7) (instruction-descriptor-sub-choices d0)))))

;; Out-of-order (holes 2 0): still names the same set {0, 2} -- HOLE-INDICES
;; is always sorted into ascending pattern order internally, so (choice ...)
;; stays positional in pattern order (hole 0's alternative first, hole 2's
;; second) regardless of how (holes ...) itself was written. A buggy
;; unsorted scatter would misassign which choice belongs to which hole here,
;; which -- since HOLES-A1/A2 and HOLES-C1/C2 are disjoint mode-name sets --
;; would surface as a DEFINSTRUCTION-time "not one of this hole's ONE-OF
;; alternatives" error, not a silent mis-decode; this instruction being
;; accepted at all, with a correct round trip, is the assertion.
(definstruction instr-test-machine holtabrev
  (modes holes-three)
  (encoding (opcode #xF2)
            (operand h0 :width 1)
            (operand h1 :width 1)
            (operand h2 :width 1)
            (sub-opcode
              (holes 2 0)
              (variant (choice holes-a1 holes-c1) (sub 0))
              (variant (choice holes-a1 holes-c2) (sub 1))
              (variant (choice holes-a2 holes-c1) (sub 2))
              (variant (choice holes-a2 holes-c2) (sub 3))))
  (semantics nil))

(fiveam:test sub-opcode-table-holes-clause-out-of-order-is-still-positional
  (fiveam:is (equalp #(#xF2 2 5 20 7)
                      (assembly-cells (assemble "holtabrev [5], 20, 7" :machine 'instr-test-machine))))
  (multiple-value-bind (descriptor values size choices)
      (decode-instruction-at (vector-cell-reader (vector #xF2 2 5 20 7)) 0 'instr-test-machine)
    (declare (ignore size))
    (fiveam:is (string= "HOLTABREV" (instruction-descriptor-name descriptor)))
    (fiveam:is (equal '(5 20 7) values))
    (fiveam:is (eq 'holes-a2 (%matched-choice-name choices 0)))
    (fiveam:is (eq 'holes-c1 (%matched-choice-name choices 2)))))

;; (holes) with no indices at all names nothing to cover.
(fiveam:test sub-opcode-table-holes-clause-empty-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction instr-test-machine holtabbad1
             (modes holes-three)
             (encoding (opcode #xEB)
                       (operand :width 1) (operand :width 1) (operand :width 1)
                       (sub-opcode
                         (holes)
                         (variant (choice holes-a1 holes-c1) (sub 0))
                         (variant (choice holes-a2 holes-c2) (sub 1))))
             (semantics nil)))))

;; A duplicate hole index in (holes ...).
(fiveam:test sub-opcode-table-holes-clause-duplicate-index-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction instr-test-machine holtabbad2
             (modes holes-three)
             (encoding (opcode #xEC)
                       (operand :width 1) (operand :width 1) (operand :width 1)
                       (sub-opcode
                         (holes 0 0)
                         (variant (choice holes-a1) (sub 0))
                         (variant (choice holes-a2) (sub 1))))
             (semantics nil)))))

;; A hole index out of range for this mode's own hole count.
(fiveam:test sub-opcode-table-holes-clause-out-of-range-index-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction instr-test-machine holtabbad3
             (modes holes-three)
             (encoding (opcode #xED)
                       (operand :width 1) (operand :width 1) (operand :width 1)
                       (sub-opcode
                         (holes 0 5)
                         (variant (choice holes-a1 holes-c1) (sub 0))
                         (variant (choice holes-a2 holes-c2) (sub 1))))
             (semantics nil)))))

;; A hole index naming a plain EXPR hole (not a ONE-OF) has nothing for the
;; table to select between at that hole.
(defmode holes-mixed expr "," (one-of holes-a1 holes-a2))

(fiveam:test sub-opcode-table-holes-clause-non-one-of-index-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction instr-test-machine holtabbad4
             (modes holes-mixed)
             (encoding (opcode #xEE)
                       (operand :width 1) (operand :width 1)
                       (sub-opcode
                         (holes 0)
                         (variant (choice holes-a1) (sub 0))
                         (variant (choice holes-a2) (sub 1))))
             (semantics nil)))))

;; Arity must match the NARROWED (holes ...) hole count, not the mode's full
;; ONE-OF hole count -- a 3-name (choice ...) against a 2-hole (holes 0 2)
;; subset is a mismatch.
(fiveam:test sub-opcode-table-holes-clause-wrong-arity-signals-error
  (fiveam:signals instruction-definition-error
    (eval '(definstruction instr-test-machine holtabbad5
             (modes holes-three)
             (encoding (opcode #xEF)
                       (operand :width 1) (operand :width 1) (operand :width 1)
                       (sub-opcode
                         (holes 0 2)
                         (variant (choice holes-a1 holes-b1 holes-c1) (sub 0))
                         (variant (choice holes-a2 holes-b2 holes-c2) (sub 1))))
             (semantics nil)))))

;;; :ENDIAN (#66)

(fiveam:test encode-value-cells-big-endian-reverses-cell-order
  ;; The exact mirror of ENCODE-VALUE-CELLS-SPLITS-INTO-16-BIT-CELLS above,
  ;; :BIG instead of the default :LITTLE -- the high cell comes first.
  (fiveam:is (equal (list #x0001 #x1234) (%encode-value-cells #x00011234 2 16 :big))))

(defmachine bigendian-test-machine
  (register a :width 8)
  (register pc :width 16)
  (memory ram :width 8 :addr-width 16 :endian :big))

(definstruction bigendian-test-machine adc
  (modes absolute)
  (encoding (opcode #x6D) (operand :mode))
  (semantics (set! a (wrap-value (+ a (mref machine 'ram operand)) 8))))

;; A :SIGNED absolute-shaped mode -- BIGENDIAN-TEST-MACHINE has no built-in
;; signed multi-cell mode, so this pins that ENCODE-INSTRUCTION's endian
;; ordering and SIGNED-VALUE's own reinterpretation compose independently
;; (SIGNED-VALUE runs on the reassembled integer, after cell order is
;; already resolved) -- same rationale as instr-test-machine's SBC-style
;; signed modes elsewhere in this file.
(defmode big-simm expr :signed t)

(definstruction bigendian-test-machine sbc
  (modes big-simm)
  (encoding (opcode #x6E) (operand :mode))
  (semantics (set! a (wrap-value (+ a operand) 8))))

(fiveam:test encode-absolute-instruction-big-endian
  ;; #x1234's two cells swap order relative to ENCODE-ABSOLUTE-INSTRUCTION-
  ;; LITTLE-ENDIAN's little-endian (#6D #x00 #x10) result on the same value.
  (let ((adc (find-instruction 'bigendian-test-machine 'adc)))
    (fiveam:is (equal (list #x6D #x12 #x34) (encode-instruction adc (list #x1234))))))

(fiveam:test encode-decode-round-trip-big-endian-multi-cell-operand
  ;; ENCODE-INSTRUCTION then DECODE-INSTRUCTION-AT over a bare cell vector
  ;; (VECTOR-CELL-READER) must recover the same operand value -- proves
  ;; %DECODE-CELL-INSTRUCTION's %FETCH-CELLS-based reassembly (post-#66
  ;; dedup) agrees with the encoder on a big-endian machine.
  (let* ((adc (find-instruction 'bigendian-test-machine 'adc))
         (cells (coerce (encode-instruction adc (list #xBEEF)) 'vector)))
    (multiple-value-bind (descriptor values)
        (decode-instruction-at (vector-cell-reader cells) 0 'bigendian-test-machine :memory 'ram)
      (fiveam:is (eq adc descriptor))
      (fiveam:is (equal (list #xBEEF) values)))))

(fiveam:test encode-decode-round-trip-big-endian-signed-multi-cell-operand
  ;; Same round trip, a negative two-cell operand -- pins SIGNED-VALUE as
  ;; endian-invariant: it reinterprets the already-reassembled integer, so
  ;; a big-endian machine sign-extends identically to a little-endian one.
  (let* ((sbc (find-instruction 'bigendian-test-machine 'sbc))
         (cells (coerce (encode-instruction sbc (list -100)) 'vector)))
    (multiple-value-bind (descriptor values)
        (decode-instruction-at (vector-cell-reader cells) 0 'bigendian-test-machine :memory 'ram)
      (fiveam:is (eq sbc descriptor))
      (fiveam:is (equal (list -100) values)))))

(fiveam:test defmachine-rejects-invalid-endian
  (fiveam:signals machine-definition-error
    (eval '(defmachine endian-bad-machine
             (memory ram :width 8 :addr-width 8 :endian :middle)))))

(fiveam:test defmachine-endian-defaults-to-little
  ;; INSTR-TEST-MACHINE (top of this file) declares no :ENDIAN at all --
  ;; must default to :LITTLE.
  (fiveam:is (eq :little (%machine-endian 'instr-test-machine))))

(fiveam:test defmachine-disagreeing-memory-endian-requires-explicit-memory
  (eval '(defmachine endian-ambiguous-machine
           (memory rom :width 8 :addr-width 8 :endian :big)
           (memory ram :width 8 :addr-width 8 :endian :little)))
  (fiveam:signals error (%machine-endian 'endian-ambiguous-machine))
  (fiveam:is (eq :big (%machine-endian 'endian-ambiguous-machine 'rom)))
  (fiveam:is (eq :little (%machine-endian 'endian-ambiguous-machine 'ram))))

(defmachine encoding-memory-test-machine
  (register pc :width 8)
  (register a :width 16)
  (memory rom :width 8 :addr-width 8 :endian :big)
  (memory ram :width 16 :addr-width 8 :cell-width 16 :endian :little))

(defmode encoding-memory-test-mode expr)

(definstruction encoding-memory-test-machine load2
  (modes encoding-memory-test-mode)
  (encoding (opcode 1) (operand :width 2))
  (semantics (set! a operand)))

(fiveam:test encoding-uses-selected-memory-properties
  (let ((instruction (find-instruction 'encoding-memory-test-machine 'load2)))
    (fiveam:is (equal '(1 #x12 #x34)
                      (encode-instruction instruction '(#x1234) :memory 'rom)))
    (fiveam:is (equal '(1 #x1234 0)
                      (encode-instruction instruction '(#x1234) :memory 'ram)))
    (fiveam:signals error (encode-instruction instruction '(#x1234)))
    (dolist (case '((rom #(1 #x12 #x34) 8) (ram #(1 #x1234 0) 16)))
      (destructuring-bind (memory expected width) case
        (let* ((assembly (assemble "load2 4660" :machine 'encoding-memory-test-machine
                                   :memory memory))
               (machine (make-machine 'encoding-memory-test-machine)))
          (fiveam:is (equalp expected (assembly-cells assembly)))
          (fiveam:is (= width (assembly-cell-width assembly)))
          (load-program machine assembly :memory memory)
          (step-machine machine :memory memory)
          (fiveam:is (= #x1234 (sref machine 'a))))))))

(fiveam:test encoding-follows-machine-redefinition
  (eval '(defmachine encoding-redefine-test-machine
           (memory ram :width 8 :addr-width 8 :endian :little)))
  (eval '(definstruction encoding-redefine-test-machine load2
           (modes encoding-memory-test-mode)
           (encoding (opcode 1) (operand :width 2))
           (semantics nil)))
  (let ((instruction (find-instruction 'encoding-redefine-test-machine 'load2)))
    (fiveam:is (equal '(1 #x34 #x12) (encode-instruction instruction '(#x1234))))
    (eval '(defmachine encoding-redefine-test-machine
             (memory ram :width 8 :addr-width 8 :endian :big)))
    (fiveam:is (equal '(1 #x12 #x34) (encode-instruction instruction '(#x1234))))))

(fiveam:test assembly-resolves-cell-properties-once
  (let ((old-width (symbol-function '%machine-cell-width))
        (old-endian (symbol-function '%machine-endian))
        (width-calls 0)
        (endian-calls 0))
    (unwind-protect
         (progn
           (setf (symbol-function '%machine-cell-width)
                 (lambda (&rest args) (incf width-calls) (apply old-width args))
                 (symbol-function '%machine-endian)
                 (lambda (&rest args) (incf endian-calls) (apply old-endian args)))
           (fiveam:is (equalp #(1 #x12 #x34 1 #x56 #x78)
                              (assembly-cells
                               (assemble "load2 4660
load2 22136" :machine 'encoding-memory-test-machine :memory 'rom))))
           (fiveam:is (= 1 width-calls))
           (fiveam:is (= 1 endian-calls)))
      (setf (symbol-function '%machine-cell-width) old-width
            (symbol-function '%machine-endian) old-endian))))

;;; extra-word-order (#191) -- trailing words follow the declared field
;;; order rather than operand hole order.

(defmachine order-test-machine
  (register pc :width 16)
  (memory ram :width 16 :addr-width 16 :cell-width 16)
  (instruction-word :width 16
    (field opcode 4)
    (field dst 6)
    (field src 6)
    (extra-word-order src dst)))

(defmode order-pair expr "," expr)

(definstruction order-test-machine mv
  (modes order-pair)
  (encoding
    (opcode 1)
    (operand dst :field dst
      (variant (range 0 7) inline)
      (variant :else (extra-word :escape 63)))
    (operand src :field src
      (variant (range 0 7) inline)
      (variant :else (extra-word :escape 63))))
  (semantics nil))

(defun order-cells (source)
  (coerce (assembly-cells (assemble source :machine 'order-test-machine)) 'list))

(fiveam:test extra-word-order-emits-src-word-before-dst-word
  (fiveam:is (equal (list (logior (ash 1 12) (ash 63 6) 63) 200 100)
                    (order-cells "mv 100, 200"))))

(fiveam:test extra-word-order-single-extra-word-is-unaffected
  (fiveam:is (equal (list (logior (ash 1 12) (ash 63 6) 2) 100) (order-cells "mv 100, 2")))
  (fiveam:is (equal (list (logior (ash 1 12) (ash 3 6) 63) 200) (order-cells "mv 3, 200"))))

(fiveam:test extra-word-order-round-trips-through-the-disassembler
  (let* ((asm (assemble "mv 100, 200" :machine 'order-test-machine))
         (lines (disassemble-assembly asm :machine 'order-test-machine :labels nil)))
    (fiveam:is (= 1 (length lines)))
    (fiveam:is (equalp (assembly-cells asm)
                       (assembly-cells (assemble (disassembly-text lines :origin 0)
                                                 :machine 'order-test-machine))))))

(fiveam:test extra-word-order-decodes-values-in-hole-order
  (let ((cells (coerce (order-cells "mv 100, 200") 'vector)))
    (multiple-value-bind (descriptor values size)
        (decode-instruction-at (lambda (a) (aref cells a)) 0 'order-test-machine)
      (declare (ignore descriptor))
      (fiveam:is (equal '(100 200) values))
      (fiveam:is (= 3 size)))))

(fiveam:test extra-word-order-rejects-an-undeclared-field
  (fiveam:signals machine-definition-error
    (eval '(defmachine bogus-order-machine
             (memory ram :width 16 :addr-width 16 :cell-width 16)
             (instruction-word :width 16 (field opcode 8) (field dst 8)
               (extra-word-order nope))))))

(fiveam:test extra-word-order-rejects-a-repeated-field
  (fiveam:signals machine-definition-error
    (eval '(defmachine bogus-order-machine
             (memory ram :width 16 :addr-width 16 :cell-width 16)
             (instruction-word :width 16 (field opcode 8) (field dst 8)
               (extra-word-order dst dst))))))

(fiveam:test sibling-registration-does-not-repeat-conflict-checks
  (let ((original (symbol-function '%check-opcode-decodable!))
        (checks 0)
        (variants (find-instruction-variants 'order-test-machine 'mv)))
    (unwind-protect
         (progn
           (setf (symbol-function '%check-opcode-decodable!)
                 (lambda (&rest args)
                   (incf checks)
                   (apply original args)))
           (register-instruction-variants! 'order-test-machine variants)
           (fiveam:is (zerop checks))
           (fiveam:is (= 4 (length (find-instruction-variants 'order-test-machine 'mv)))))
      (setf (symbol-function '%check-opcode-decodable!) original))))

;;; (fallback): a general co-tenant decoding behind strictly more specific ones.

(defmachine fallback-test-machine
  (register pc :width 16)
  (register a :width 16)
  (memory ram :width 8 :addr-width 16)
  (instruction-word :width 16
    (field opcode 4) (field nnn 12)))

(defmode fallback-nnn expr)

(definstruction fallback-test-machine fbsys
  (modes fallback-nnn)
  (encoding (opcode 0) (fallback) (operand addr :field nnn))
  (semantics nil))

(definstruction fallback-test-machine fbcls
  (encoding (opcode 0) (field-value nnn #xe0))
  (semantics nil))

(definstruction fallback-test-machine fbret
  (encoding (opcode 0) (field-value nnn #xee))
  (semantics nil))

;; Fallback declared before its specific co-tenant.
(definstruction fallback-test-machine fbjp
  (modes fallback-nnn)
  (encoding (opcode 1) (fallback) (operand addr :field nnn))
  (semantics nil))

(definstruction fallback-test-machine fbpin
  (encoding (opcode 1) (field-value nnn 7))
  (semantics nil))

(defun fallback-decoded-name (source)
  (let ((cells (assembly-cells (assemble source :machine 'fallback-test-machine))))
    (instruction-descriptor-name
     (decode-instruction-at (vector-cell-reader cells) 0 'fallback-test-machine))))

(fiveam:test fallback-decodes-specific-before-general
  (fiveam:is (string= "FBCLS" (fallback-decoded-name "fbcls")))
  (fiveam:is (string= "FBRET" (fallback-decoded-name "fbret")))
  (fiveam:is (string= "FBSYS" (fallback-decoded-name "fbsys $123")))
  (fiveam:is (string= "FBPIN" (fallback-decoded-name "fbpin")))
  (fiveam:is (string= "FBJP" (fallback-decoded-name "fbjp 8"))))

(fiveam:test fallback-sits-last-in-its-bucket
  (fiveam:is (string= "FBSYS" (instruction-descriptor-name
                               (car (last (find-instruction-descriptors-by-opcode
                                           'fallback-test-machine 0))))))
  (fiveam:is (string= "FBJP" (instruction-descriptor-name
                              (car (last (find-instruction-descriptors-by-opcode
                                          'fallback-test-machine 1)))))))

(fiveam:test fallback-redefinition-stays-last
  (eval '(definstruction fallback-test-machine fbsys
           (modes fallback-nnn)
           (encoding (opcode 0) (fallback) (operand addr :field nnn))
           (semantics nil)))
  (fiveam:is (string= "FBSYS" (instruction-descriptor-name
                               (car (last (find-instruction-descriptors-by-opcode
                                           'fallback-test-machine 0))))))
  (fiveam:is (string= "FBCLS" (fallback-decoded-name "fbcls"))))

(fiveam:test fallback-rejects-assembling-a-shadowed-encoding
  (dolist (source '("fbsys $e0" "fbsys $ee" "fbjp 7"))
    (fiveam:signals assembly-error (assemble source :machine 'fallback-test-machine)))
  (fiveam:finishes (assemble "fbsys $e1" :machine 'fallback-test-machine)))

(fiveam:test fallback-requires-strictly-more-specific-co-tenant
  ;; Each bogus fallback overlaps a plain co-tenant without containing it
  ;; strictly: equal sets, partial overlap, and the narrower side.
  (eval '(definstruction fallback-test-machine fbplain
           (modes fallback-nnn)
           (encoding (opcode 2) (operand addr :field nnn))
           (semantics nil)))
  (dolist (form '((definstruction fallback-test-machine fbequal
                    (modes fallback-nnn)
                    (encoding (opcode 2) (fallback) (operand addr :field nnn))
                    (semantics nil))
                  (definstruction fallback-test-machine fbnarrow
                    (encoding (opcode 2) (fallback) (field-value nnn 5))
                    (semantics nil))))
    (handler-case (eval form)
      (opcode-conflict (c) (fiveam:is (eq :indistinguishable (opcode-conflict-reason c))))
      (:no-error () (fiveam:fail "expected OPCODE-CONFLICT")))))

(fiveam:test fallback-partial-overlap-is-rejected
  (eval '(definstruction fallback-test-machine fblow
           (modes fallback-nnn)
           (encoding (opcode 3) (operand addr :field nnn (variant (range 0 100) inline)))
           (semantics nil)))
  (handler-case
      (eval '(definstruction fallback-test-machine fbhigh
               (modes fallback-nnn)
               (encoding (opcode 3) (fallback)
                 (operand addr :field nnn (variant (range 50 200) inline)))
               (semantics nil)))
    (opcode-conflict (c) (fiveam:is (eq :indistinguishable (opcode-conflict-reason c))))
    (:no-error () (fiveam:fail "expected OPCODE-CONFLICT"))))

(fiveam:test fallback-rejected-on-byte-machine
  (fiveam:signals instruction-definition-error
    (eval '(definstruction test-machine fbbyte
             (encoding (opcode 201) (fallback))
             (semantics nil)))))

(defmachine fallback-wide-test-machine
  (register pc :width 16)
  (memory ram :width 8 :addr-width 16)
  (instruction-word :width 24 (field opcode 8) (field value 16)))

(definstruction fallback-wide-test-machine wpin
  (encoding (opcode 1) (field-value value #xbeef))
  (semantics nil))

(definstruction fallback-wide-test-machine wgen
  (modes fallback-nnn)
  (encoding (opcode 1) (fallback) (operand v :field value))
  (semantics nil))

(fiveam:test fallback-orders-wide-words-without-a-dispatch-table
  (flet ((decoded (source)
           (instruction-descriptor-name
            (decode-instruction-at
             (vector-cell-reader (assembly-cells (assemble source :machine 'fallback-wide-test-machine)))
             0 'fallback-wide-test-machine))))
    (fiveam:is (string= "WPIN" (decoded "wpin")))
    (fiveam:is (string= "WGEN" (decoded "wgen 5")))))

;;; Mixed endian (#145)

(fiveam:test mixed-endian-cell-order
  (fiveam:is (equal '(#x0B #x0A #x0D #x0C)
                    (%encode-value-cells #x0A0B0C0D 4 8 '(:big :little 2))))
  (fiveam:is (equal '(#x0C #x0D #x0A #x0B)
                    (%encode-value-cells #x0A0B0C0D 4 8 '(:little :big 2))))
  (fiveam:is (equal '(#x0B #x0A) (%encode-value-cells #x0A0B 2 8 '(:big :little 2))))
  (fiveam:is (equal '(#x0A #x0B) (%encode-value-cells #x0A0B 2 8 '(:big :big 2))))
  (fiveam:is (equal '(#x0C #x0B #x0A) (%encode-value-cells #x0A0B0C 3 8 '(:little :little 2))))
  (fiveam:is (equal '(#x0A #x0C #x0B) (%encode-value-cells #x0A0B0C 3 8 '(:big :little 2)))))

(fiveam:test mixed-endian-fetch-inverts-encode
  (dolist (spec '((:big :little 2) (:little :big 2) (:big :big 3) (:little :little 3)))
    (dolist (width '(1 2 3 4 5))
      (let* ((value (ldb (byte (* 8 width) 0) #x0123456789))
             (cells (coerce (%encode-value-cells value width 8 spec) 'vector)))
        (fiveam:is (= value (%fetch-cells (lambda (a) (aref cells a)) 0 width 8 spec)))))))

(fiveam:test check-endian-rejects-malformed-specs
  (dolist (bad '((:big :middle 2) (:big :little 1) (:big) (:big :little 2 3) (:big :little :x) :pdp))
    (fiveam:signals error (%check-endian bad 'ram)))
  (fiveam:is (equal '(:big :little 2) (%check-endian '(:big :little 2) 'ram))))

;;; Nested varying ONE-OF: NW-IND varies in hole count and sits inside NW-MODE's
;;; own ONE-OF; a path such as (nw-ind vh-idx) names the inner alternative.

(defmode nw-ind (one-of vh-reg vh-idx))
(defmode nw-lit "#" expr)
(defmode nw-mode expr "," (one-of nw-ind nw-lit))

(definstruction varying-hole-test-machine nwl
  (modes nw-mode)
  (encoding
    (opcode 6)
    (operand dst :field dst)
    (operand src :field src
      (variant (choice (nw-ind vh-reg)) inline :range (0 7) :bias #x00)
      (variant (choice (nw-ind vh-idx)) inline :range (0 7) :bias #x10)
      (variant (choice nw-lit) inline :range (0 7) :bias #x20))
    (for-choice (src nw-ind vh-idx) (operand off :trailing-word)))
  (semantics
    (choice-case src
      (nw-lit (set! (a dst) src))
      (nw-ind (choice-case (src nw-ind)
                (vh-reg (set! (a dst) (a src)))
                (vh-idx (set! (a dst) (mref machine 'ram (+ (a src) off)))))))))

(fiveam:test nested-varying-word-expands-one-descriptor-per-path
  (let ((variants (find-instruction-variants 'varying-hole-test-machine 'nwl)))
    (fiveam:is (equal '(2 2 3)
                      (sort (mapcar (lambda (d) (length (instruction-descriptor-operand-names d)))
                                    variants)
                            #'<)))))

(fiveam:test nested-varying-word-assemble-decode-round-trip
  (dolist (case '(("nwl 1, 5" 1) ("nwl 1, [3, 100]" 2) ("nwl 1, #4" 1)))
    (destructuring-bind (source length) case
      (let ((cells (assembly-cells (assemble source :machine 'varying-hole-test-machine))))
        (fiveam:is (= length (length cells)))
        (multiple-value-bind (descriptor values size)
            (decode-instruction-at (lambda (addr) (aref cells addr)) 0 'varying-hole-test-machine)
          (fiveam:is (string= "NWL" (instruction-descriptor-name descriptor)))
          (fiveam:is (= length size))
          (fiveam:is (= (if (= length 2) 3 2) (length values))))))))

(fiveam:test nested-varying-word-decoded-choices-carry-the-path
  (let ((cells (assembly-cells (assemble "nwl 1, [3, 100]" :machine 'varying-hole-test-machine))))
    (multiple-value-bind (descriptor values size choices)
        (decode-instruction-at (lambda (addr) (aref cells addr)) 0 'varying-hole-test-machine)
      (declare (ignore descriptor size))
      (fiveam:is (equal '(1 3 100) values))
      (fiveam:is (equal '((nw-ind vh-idx) (nw-ind vh-idx))
                        (mapcar (lambda (c) (and c (word-field-choice-choice c))) (rest choices)))))))

(fiveam:test nested-varying-word-semantics-dispatch-on-the-inner-pick
  (let ((m (make-machine 'varying-hole-test-machine)))
    (setf (regref m 'a 3) 3
          (mref m 'ram 103) 999)
    (load-program m (assembly-cells (assemble "nwl 2, [3, 100]" :machine 'varying-hole-test-machine)))
    (step-machine m)
    (fiveam:is (= 999 (regref m 'a 2))))
  (let ((m (make-machine 'varying-hole-test-machine)))
    (setf (regref m 'a 3) 77)
    (load-program m (assembly-cells (assemble "nwl 2, 3" :machine 'varying-hole-test-machine)))
    (step-machine m)
    (fiveam:is (= 77 (regref m 'a 2))))
  (let ((m (make-machine 'varying-hole-test-machine)))
    (load-program m (assembly-cells (assemble "nwl 2, #5" :machine 'varying-hole-test-machine)))
    (step-machine m)
    (fiveam:is (= 5 (regref m 'a 2)))))

(fiveam:test nested-varying-word-disassembles-each-shape
  (dolist (case '(("nwl 1, 5" "$5") ("nwl 1, [3, 100]" "[$3,$64]") ("nwl 1, #4" "#$4")))
    (let* ((cells (assembly-cells (assemble (first case) :machine 'varying-hole-test-machine)))
           (lines (disassemble-cells cells :machine 'varying-hole-test-machine)))
      (fiveam:is (search (second case) (disassembly-line-text (first lines)))))))

(defmacro %nested-word-instruction (&rest encoding)
  "A bogus NW-MODE instruction; the valid (src nw-ind vh-idx) for-choice is always
present, so an error comes from the ENCODING under test."
  `(eval '(definstruction varying-hole-test-machine bogus
            (modes nw-mode)
            (encoding (opcode 7) (operand dst :field dst) ,@encoding
                      (for-choice (src nw-ind vh-idx) (operand off :trailing-word)))
            (semantics nil))))

(defun %error-text (thunk)
  (handler-case (progn (funcall thunk) nil)
    (error (c) (princ-to-string c))))

(fiveam:test nested-varying-word-rejects-a-bare-varying-choice
  (let ((text (%error-text
               (lambda ()
                 (%nested-word-instruction
                  (operand src :field src
                    (variant (choice nw-ind) inline :range (0 7) :bias #x00)
                    (variant (choice nw-lit) inline :range (0 7) :bias #x20)))))))
    (fiveam:is (search "varies in hole count" text))))

(fiveam:test nested-varying-word-rejects-an-unknown-inner-choice
  (let ((text (%error-text
               (lambda ()
                 (%nested-word-instruction
                  (operand src :field src
                    (variant (choice (nw-ind vh-reg)) inline :range (0 7) :bias #x00)
                    (variant (choice (nw-ind nw-lit)) inline :range (0 7) :bias #x10)
                    (variant (choice nw-lit) inline :range (0 7) :bias #x20)))))))
    (fiveam:is (search "is not one of this hole's ONE-OF alternatives" text))))

(fiveam:test nested-varying-word-rejects-a-one-name-path
  (let ((text (%error-text
               (lambda ()
                 (%nested-word-instruction
                  (operand src :field src
                    (variant (choice (nw-ind)) inline :range (0 7) :bias #x00)))))))
    (fiveam:is (search "at least two mode names" text))))

(fiveam:test nested-varying-word-requires-a-for-choice-per-path
  (let ((text (%error-text
               (lambda ()
                 (eval '(definstruction varying-hole-test-machine bogus
                          (modes nw-mode)
                          (encoding
                            (opcode 7)
                            (operand dst :field dst)
                            (operand src :field src
                              (variant (choice (nw-ind vh-reg)) inline :range (0 7) :bias #x00)
                              (variant (choice (nw-ind vh-idx)) inline :range (0 7) :bias #x10)
                              (variant (choice nw-lit) inline :range (0 7) :bias #x20)))
                          (semantics nil)))))))
    (fiveam:is (search "missing FOR-CHOICE for (NW-IND VH-IDX)" text))))

(fiveam:test nested-varying-word-rejects-a-short-for-choice-on-a-varying-alternative
  (let ((text (%error-text
               (lambda ()
                 (eval '(definstruction varying-hole-test-machine bogus
                          (modes nw-mode)
                          (encoding
                            (opcode 7)
                            (operand dst :field dst)
                            (operand src :field src
                              (variant (choice (nw-ind vh-reg)) inline :range (0 7) :bias #x00)
                              (variant (choice (nw-ind vh-idx)) inline :range (0 7) :bias #x10)
                              (variant (choice nw-lit) inline :range (0 7) :bias #x20))
                            (for-choice nw-ind (operand off :trailing-word)))
                          (semantics nil)))))))
    (fiveam:is (search "varies in hole count" text))))

(fiveam:test nested-varying-choice-case-rejects-bad-qualification
  (dolist (case '(((choice-case (src nw-lit) (vh-reg 1)) "does not name a nested varying")
                  ((choice-case (src nw-ind) (nw-lit 1)) "is not one of this operand's")
                  ((choice-case (src nw-ind vh-reg) (vh-reg 1)) "does not name a nested varying")))
    (destructuring-bind (body expected) case
      (let ((text (%error-text
                   (lambda ()
                     (eval `(definstruction varying-hole-test-machine bogus
                              (modes nw-mode)
                              (encoding
                                (opcode 7)
                                (operand dst :field dst)
                                (operand src :field src
                                  (variant (choice (nw-ind vh-reg)) inline :range (0 7) :bias #x00)
                                  (variant (choice (nw-ind vh-idx)) inline :range (0 7) :bias #x10)
                                  (variant (choice nw-lit) inline :range (0 7) :bias #x20))
                                (for-choice (src nw-ind vh-idx) (operand off :trailing-word)))
                              (semantics ,body)))))))
        (fiveam:is (search expected text))))))

;;; Byte-encoded nesting, three levels deep: NB-FAR is a varying alternative of
;;; NB-DEEP, which is itself a varying alternative of NB-MODE.

(defmode nb-abs expr)
(defmode nb-idx "[" expr "," expr "]")
(defmode nb-far "<" expr "," (one-of nb-abs nb-idx) ">")
(defmode nb-deep (one-of nb-abs nb-far))
(defmode nb-lit "#" expr)
(defmode nb-mode (one-of nb-deep nb-lit))

(definstruction varying-hole-byte-test-machine nbd
  (modes nb-mode)
  (encoding
    (opcode 11)
    (operand src :width 1
      (variant (choice (nb-deep nb-abs)) (sub 0))
      (variant (choice (nb-deep nb-far nb-abs)) (sub 1))
      (variant (choice (nb-deep nb-far nb-idx)) (sub 2))
      (variant (choice nb-lit) (sub 3)))
    (for-choice (src nb-deep nb-far nb-abs) (operand x1 :width 1))
    (for-choice (src nb-deep nb-far nb-idx) (operand x1 :width 1) (operand x2 :width 1)))
  (semantics
    (choice-case src
      (nb-lit (set! a src))
      (nb-deep (choice-case (src nb-deep)
                 (nb-abs (set! a src))
                 (nb-far (choice-case (src nb-deep nb-far)
                           (nb-abs (set! a (+ src x1)))
                           (nb-idx (set! a (+ src x1 x2))))))))))

(fiveam:test nested-varying-byte-three-levels-round-trip
  (dolist (case '(("nbd 7" #(11 0 7) (nb-deep nb-abs))
                  ("nbd <1, 2>" #(11 1 1 2) (nb-deep nb-far nb-abs))
                  ("nbd <1, [2, 3]>" #(11 2 1 2 3) (nb-deep nb-far nb-idx))
                  ("nbd #4" #(11 3 4) nb-lit)))
    (destructuring-bind (source cells key) case
      (let ((assembled (assembly-cells (assemble source :machine 'varying-hole-byte-test-machine))))
        (fiveam:is (equalp cells assembled))
        (multiple-value-bind (descriptor values size choices)
            (decode-instruction-at (vector-cell-reader assembled) 0 'varying-hole-byte-test-machine)
          (fiveam:is (string= "NBD" (instruction-descriptor-name descriptor)))
          (fiveam:is (= (length cells) size))
          (fiveam:is (equal (coerce (subseq cells 2) 'list) values))
          (fiveam:is (every (lambda (c) (equal key c)) (remove nil choices))))))))

(fiveam:test nested-varying-byte-three-levels-semantics-dispatch
  (dolist (case '(("nbd 7" 7) ("nbd <1, 2>" 3) ("nbd <1, [2, 3]>" 6) ("nbd #4" 4)))
    (let ((m (make-machine 'varying-hole-byte-test-machine)))
      (load-program m (assembly-cells (assemble (first case) :machine 'varying-hole-byte-test-machine)))
      (step-machine m)
      (fiveam:is (= (second case) (sref m 'a))))))

(fiveam:test nested-varying-byte-disassembles-each-path
  (dolist (case '(("nbd 7" "$7") ("nbd <1, 2>" "<$1,$2>") ("nbd <1, [2, 3]>" "<$1,[$2,$3]>")
                  ("nbd #4" "#$4")))
    (let* ((cells (assembly-cells (assemble (first case) :machine 'varying-hole-byte-test-machine)))
           (lines (disassemble-cells cells :machine 'varying-hole-byte-test-machine)))
      (fiveam:is (search (second case) (disassembly-line-text (first lines)))))))

(fiveam:test nested-varying-byte-rejects-a-bare-varying-choice
  (let ((text (%error-text
               (lambda ()
                 (eval '(definstruction varying-hole-byte-test-machine bogus
                          (modes nb-mode)
                          (encoding
                            (opcode 12)
                            (operand src :width 1
                              (variant (choice nb-deep) (sub 0))
                              (variant (choice nb-lit) (sub 1)))
                            (for-choice (src nb-deep nb-far nb-abs) (operand x1 :width 1))
                            (for-choice (src nb-deep nb-far nb-idx) (operand x1 :width 1) (operand x2 :width 1)))
                          (semantics nil)))))))
    (fiveam:is (search "varies in hole count" text))))

;;; lowcell()/highcell() and comparison operators

(fiveam:test lowcell-and-highcell-split-by-the-machine-cell-width
  (fiveam:is (equalp #(#x5678 #x1234)
                     (assembly-cells (assemble ".cell lowcell($12345678), highcell($12345678)"
                                               :machine 'wordaddr-test-machine))))
  (fiveam:is (equalp #(#x34 #x12)
                     (assembly-cells (assemble ".byte lowcell($1234), highcell($1234)"
                                               :machine 'varying-hole-byte-test-machine)))))

(fiveam:test defined-tests-the-symbol-table
  (let ((symbols (make-hash-table :test 'equal)))
    (setf (gethash "a" symbols) 1)
    (fiveam:is (= 1 (eval-expr (%expr "defined(a)") :symbols symbols)))
    (fiveam:is (= 0 (eval-expr (%expr "defined(b)") :symbols symbols)))
    (fiveam:is (= 0 (eval-expr (%expr "defined(a)"))))))

(fiveam:test defined-inside-a-logical-operator
  (let ((symbols (make-hash-table :test 'equal)))
    (fiveam:is (= 0 (eval-expr (%expr "defined(x) && x > 1") :symbols symbols)))))

(fiveam:test lowcell-and-highcell-need-a-machine
  (fiveam:signals assembly-error (eval-expr-constant (%expr "lowcell(1)")))
  (fiveam:signals assembly-error (eval-expr-constant (%expr "highcell(1)"))))

(fiveam:test comparison-operators-fold-to-one-or-zero
  (fiveam:is (equalp #(1 0 1 0 1 1 0 1 1 0)
                     (assembly-cells
                      (assemble ".byte 2 > 1, 1 > 2, 1 < 2, 2 < 1, 2 >= 2, 2 <= 2, 1 == 2, 1 != 2, 1 | 2 == 3, $F & 1 == 0"
                                :machine 'varying-hole-byte-test-machine)))))

(fiveam:test angle-bracket-delimiters-survive-comparison-operators
  (dolist (case '(("nbd <1, 2>" #(11 1 1 2))
                  ("nbd <(1 > 0), 2>" #(11 1 1 2))
                  ("nbd <1 < 2, 3>" #(11 1 1 3))
                  ("nbd <3 - 1, [2, 3]>" #(11 2 2 2 3))))
    (destructuring-bind (source cells) case
      (fiveam:is (equalp cells (assembly-cells (assemble source :machine 'varying-hole-byte-test-machine)))
                 "~A" source))))

;;; Sub-opcode tables that select a ONE-OF element by slot (#218), including
;;; nested alternatives with hole-less options (#219).

(defmachine slot-sub-machine
  (register pc :width 16)
  (register a :width 8)
  (memory ram :width 8 :addr-width 16))

(defmode ss-sp "SP")
(defmode ss-pc "PC")
(defmode ss-reg expr)
(defmode ss-none "-")
(defmode ss-idx "[" expr "," expr "]")
(defmode ss-pop "POP")
(defmode ss-stk (one-of ss-pop ss-idx))
(defmode ss-fixed (one-of (fixed ss-sp ss-pc)))
(defmode ss-kind (one-of (kind ss-sp ss-pc ss-reg)))
(defmode ss-opt (one-of (opt ss-none ss-idx)))
(defmode ss-nested (one-of (nest ss-reg ss-stk)))
(defmode ss-both (one-of (kind ss-sp ss-reg)) "," (one-of ss-reg ss-idx))

(definstruction slot-sub-machine fx
  (modes ss-fixed)
  (encoding (opcode #x10)
            (sub-opcode (variant (choice ss-sp) (sub 0))
                        (variant (choice ss-pc) (sub 1))))
  (semantics (choice-case fixed (ss-sp (set! a 1)) (ss-pc (set! a 2)))))

(definstruction slot-sub-machine kd
  (modes ss-kind)
  (encoding (opcode #x11)
            (sub-opcode (holes kind)
                        (variant (choice ss-sp) (sub 0))
                        (variant (choice ss-pc) (sub 1))
                        (variant (choice ss-reg) (sub 2)))
            (for-choice (kind ss-reg) (operand v :width 1)))
  (semantics (choice-case kind (ss-sp (set! a 1)) (ss-pc (set! a 2)) (ss-reg (set! a v)))))

(definstruction slot-sub-machine op
  (modes ss-opt)
  (encoding (opcode #x12)
            (sub-opcode (variant (choice ss-none) (sub 0))
                        (variant (choice ss-idx) (sub 1)))
            (for-choice (opt ss-idx) (operand base :width 1) (operand off :width 1)))
  (semantics (choice-case opt (ss-none (set! a 9)) (ss-idx (set! a (+ base off))))))

(definstruction slot-sub-machine nd
  (modes ss-nested)
  (encoding (opcode #x13)
            (sub-opcode (holes nest)
                        (variant (choice ss-reg) (sub 0))
                        (variant (choice (ss-stk ss-pop)) (sub 1))
                        (variant (choice (ss-stk ss-idx)) (sub 2)))
            (for-choice (nest ss-reg) (operand v :width 1))
            (for-choice (nest ss-stk ss-idx) (operand base :width 1) (operand off :width 1)))
  (semantics (choice-case (nest ss-stk)
               (ss-pop (set! a 9))
               (ss-idx (set! a (+ base off)))
               (otherwise (set! a v)))))

(definstruction slot-sub-machine bt
  (modes ss-both)
  (encoding (opcode #x14)
            (sub-opcode (holes kind 0)
                        (variant (choice ss-sp ss-reg) (sub 0))
                        (variant (choice ss-sp ss-idx) (sub 1))
                        (variant (choice ss-reg ss-reg) (sub 2))
                        (variant (choice ss-reg ss-idx) (sub 3)))
            (operand right :width 1)
            (for-choice (kind ss-reg) (operand left :width 1))
            (for-choice ss-idx (operand off :width 1)))
  (semantics (set! a (+ (choice-case kind (ss-sp 100) (ss-reg left)) right (or off 0)))))

(definstruction slot-sub-machine hlt
  (encoding (opcode #x00))
  (semantics (trap :halt)))

(defun %slot-sub-run (source)
  (let ((machine (make-machine 'slot-sub-machine))
        (assembly (assemble (format nil "~A~%hlt" source) :machine 'slot-sub-machine)))
    (load-program machine assembly)
    (run machine)
    (values (sref machine 'a) (assembly-cells assembly))))

(fiveam:test hole-less-slot-selections-encode-decode-and-run
  (loop for (source cells result text) in
        '(("fx SP" #(#x10 0 0) 1 "fx SP")
          ("fx PC" #(#x10 1 0) 2 "fx PC")
          ("kd SP" #(#x11 0 0) 1 "kd SP")
          ("kd PC" #(#x11 1 0) 2 "kd PC")
          ("kd 5" #(#x11 2 5 0) 5 "kd $5")
          ("op -" #(#x12 0 0) 9 "op -")
          ("op [3, 4]" #(#x12 1 3 4 0) 7 "op [$3,$4]")
          ("nd 5" #(#x13 0 5 0) 5 "nd $5")
          ("nd POP" #(#x13 1 0) 9 "nd POP")
          ("nd [3, 4]" #(#x13 2 3 4 0) 7 "nd [$3,$4]")
          ("bt SP, 6" #(#x14 0 6 0) 106 "bt SP,$6")
          ("bt SP, [6, 1]" #(#x14 1 6 1 0) 107 "bt SP,[$6,$1]")
          ("bt 4, 6" #(#x14 2 4 6 0) 10 "bt $4,$6")
          ("bt 4, [6, 1]" #(#x14 3 4 6 1 0) 11 "bt $4,[$6,$1]"))
        do (multiple-value-bind (a assembled) (%slot-sub-run source)
             (fiveam:is (equalp cells assembled))
             (fiveam:is (= result a))
             (fiveam:is (string= text (disassembly-line-text
                                       (first (disassemble-assembly
                                               (assemble source :machine 'slot-sub-machine)
                                               :machine 'slot-sub-machine :labels nil :suffixes nil))))))))

(fiveam:test decode-reports-hole-less-slot-selections
  (loop for (cells selections) in
        '((#(#x10 1) ((fixed . ss-pc)))
          (#(#x11 0) ((kind . ss-sp)))
          (#(#x13 1) ((nest ss-stk ss-pop)))
          (#(#x13 2 3 4) ((nest ss-stk ss-idx)))
          (#(#x14 1 6 1) ((kind . ss-sp))))
        do (multiple-value-bind (descriptor values size choices found)
               (decode-instruction-at (vector-cell-reader cells) 0 'slot-sub-machine)
             (declare (ignore descriptor values size choices))
             (fiveam:is (equal selections found)))))

(defun %slot-sub-definition-error (form)
  (handler-case (progn (eval form) nil)
    (instruction-definition-error (c) (princ-to-string c))))

(defmode ss-unnamed (one-of ss-none ss-idx))

(fiveam:test hole-less-slot-requires-a-selector
  (fiveam:is-true
   (search "name it"
           (%slot-sub-definition-error
            '(definstruction slot-sub-machine bad1
               (modes ss-unnamed)
               (encoding (opcode #x20)
                         (for-choice ss-idx (operand base :width 1) (operand off :width 1)))
               (semantics nil)))))
  (fiveam:is-true
   (search "slot FIXED"
           (%slot-sub-definition-error
            '(definstruction slot-sub-machine bad2
               (modes ss-fixed)
               (encoding (opcode #x21))
               (semantics nil)))))
  (fiveam:is-true
   (search "slot KIND"
           (%slot-sub-definition-error
            '(definstruction slot-sub-machine bad3
               (modes ss-both)
               (encoding (opcode #x22)
                         (sub-opcode (holes 0)
                                     (variant (choice ss-reg) (sub 0))
                                     (variant (choice ss-idx) (sub 1)))
                         (operand right :width 1)
                         (for-choice (kind ss-reg) (operand left :width 1))
                         (for-choice ss-idx (operand off :width 1)))
               (semantics nil)))))
  (fiveam:is-true
   (search "not a hole index or a named ONE-OF slot"
           (%slot-sub-definition-error
            '(definstruction slot-sub-machine bad4
               (modes ss-fixed)
               (encoding (opcode #x23)
                         (sub-opcode (holes nope)
                                     (variant (choice ss-sp) (sub 0))
                                     (variant (choice ss-pc) (sub 1))))
               (semantics nil))))))

(defmode ss-unnamed-fixed (one-of ss-sp ss-pc))

(fiveam:test unnamed-hole-less-one-of-requires-a-slot
  (fiveam:is-true
   (search "unnamed ONE-OF"
           (%slot-sub-definition-error
            '(definstruction slot-sub-machine bad5
               (modes ss-unnamed-fixed)
               (encoding (opcode #x24))
               (semantics nil))))))
