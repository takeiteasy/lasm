;;;; tests/assembler.lisp
;;;; fiveam tests for the assembler pass (assembler.lisp), including M2's
;;;; multi-mode selection. Reuses the INSTR-TEST-MACHINE fixture and its
;;;; LDX/ADC/BNE/NOP/JMPFAR/LDA instructions from tests/instruction.lisp.

(in-package #:lasm)

(fiveam:def-suite assembler :in lasm)
(fiveam:in-suite assembler)

(fiveam:test label-binds-to-its-address
  (let ((a (assemble "start: ldx #10
sta: nop" :machine 'instr-test-machine)))
    (fiveam:is (= 0 (gethash "start" (assembly-symbols a))))
    (fiveam:is (= 2 (gethash "sta" (assembly-symbols a))))))

(fiveam:test forward-label-reference-resolves
  ;; bne .loop appears before .loop: is bound -- only a layout pass gets this.
  (let ((a (assemble "bne target
target: nop" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#xD0 3 0 #xEA) (assembly-bytes a)))))

(fiveam:test backward-label-reference-resolves
  (let ((a (assemble "target: nop
bne target" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#xEA #xD0 0 0) (assembly-bytes a)))))

(fiveam:test origin-offsets-bytes-and-symbols
  (let ((a (assemble "start: nop
bne start" :machine 'instr-test-machine :origin #x200)))
    (fiveam:is (= #x200 (assembly-origin a)))
    (fiveam:is (= #x200 (gethash "start" (assembly-symbols a))))
    (fiveam:is (equalp #(#xEA #xD0 0 2) (assembly-bytes a)))))

(fiveam:test duplicate-label-signals-assembly-error
  (fiveam:signals assembly-error
    (assemble "a: nop
a: nop" :machine 'instr-test-machine)))

(fiveam:test undefined-label-signals-unresolved-label
  (fiveam:signals unresolved-label
    (assemble "bne nowhere" :machine 'instr-test-machine)))

(fiveam:test missing-operand-signals-assembly-error
  (fiveam:signals assembly-error
    (assemble "ldx" :machine 'instr-test-machine)))

(fiveam:test unexpected-operand-signals-assembly-error
  (fiveam:signals assembly-error
    (assemble "nop #10" :machine 'instr-test-machine)))

(fiveam:test too-many-operands-signals-assembly-error
  (fiveam:signals assembly-error
    (assemble "ldx #10, #20" :machine 'instr-test-machine)))

(fiveam:test bare-label-line-emits-no-bytes
  (let ((a (assemble "start:
nop" :machine 'instr-test-machine)))
    (fiveam:is (= 0 (gethash "start" (assembly-symbols a))))
    (fiveam:is (equalp #(#xEA) (assembly-bytes a)))))

(fiveam:test unknown-mnemonic-signals-unknown-instruction
  (fiveam:signals unknown-instruction
    (assemble "frobnicate" :machine 'instr-test-machine)))

(fiveam:test no-operand-instruction-encodes-alone
  (let ((a (assemble "nop" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#xEA) (assembly-bytes a)))))

(fiveam:test immediate-and-absolute-mix
  (let ((a (assemble "ldx #10
adc target
target: nop" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#xA2 10 #x6D 5 0 #xEA) (assembly-bytes a)))))

;;; M2 multi-mode selection (mode.lisp, #18) -- LDA declares
;;; immediate/zero-page/absolute (tests/instruction.lisp), zero-page and
;;; absolute sharing identical operand syntax and differing only by width.

(fiveam:test constant-operand-picks-narrowest-fitting-mode
  (let ((a (assemble "lda $10" :machine 'instr-test-machine)))
    ;; zero-page (opcode #x11), not absolute (#x12) -- 2 bytes total
    (fiveam:is (equalp #(#x11 #x10) (assembly-bytes a)))))

(fiveam:test constant-operand-too-wide-for-zero-page-picks-absolute
  (let ((a (assemble "lda $1000" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#x12 #x00 #x10) (assembly-bytes a)))))

(fiveam:test label-operand-narrows-to-zero-page-after-layout-converges
  ;; "target" resolves to address 2, comfortably zero-page -- relaxation
  ;; starts LDA at its narrowest mode, lays out, and confirms it fits once a
  ;; provisional address is available, rather than always taking the widest
  ;; matching mode.
  (let ((a (assemble "lda target
target: nop" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#x11 2 #xEA) (assembly-bytes a)))
    (fiveam:is (= 2 (gethash "target" (assembly-symbols a))))))

(fiveam:test label-operand-stays-absolute-when-it-must
  ;; "target" resolves past zero page, so relaxation widens LDA to absolute.
  (let ((a (assemble "lda target
.res $300
target: nop" :machine 'instr-test-machine)))
    (fiveam:is (= #x12 (aref (assembly-bytes a) 0)))
    (fiveam:is (= #x303 (gethash "target" (assembly-symbols a))))))

(fiveam:test label-operand-narrowing-cascades-across-iterations
  ;; Pass 1 guesses LDA is zero-page, putting A at 256 -- too wide for zero
  ;; page. Pass 2 widens LDA to absolute, putting A at 257. Pass 3 confirms
  ;; absolute still fits at 257. Requires more than one relaxation pass to
  ;; converge.
  (let ((a (assemble "lda a
.res 254
a: nop" :machine 'instr-test-machine)))
    (fiveam:is (= #x101 (gethash "a" (assembly-symbols a))))
    (fiveam:is (equalp #(#x12 #x01 #x01) (subseq (assembly-bytes a) 0 3)))))

(fiveam:test label-operand-self-reference-narrows-to-zero-page
  (let ((a (assemble "here: lda here" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#x11 0) (assembly-bytes a)))))

(fiveam:test backward-label-operand-narrows-to-zero-page
  (let ((a (assemble "target: nop
lda target" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#xEA #x11 0) (assembly-bytes a)))))

(fiveam:test org-decouples-relaxation-of-code-before-it
  ;; Narrowing "lda a" (before the .org) must not move anything after the
  ;; .org -- "after" is at $8000 either way.
  (let ((a (assemble "lda a
a: nop
.org $8000
after: nop" :machine 'instr-test-machine)))
    (fiveam:is (= #x8000 (gethash "after" (assembly-symbols a))))))

(fiveam:test leading-org-with-non-zero-assembly-origin
  ;; A leading .org takes precedence over a non-zero :origin passed to
  ;; ASSEMBLE -- exercises %APPLY-ORIGIN-DIRECTIVE's EMITTED-P NIL branch,
  ;; which the relaxation loop's .org backward-move clamp must not touch.
  (let ((a (assemble ".org $100
start: nop" :machine 'instr-test-machine :origin #x200)))
    (fiveam:is (= #x100 (assembly-origin a)))
    (fiveam:is (= #x100 (gethash "start" (assembly-symbols a))))))

(fiveam:test relaxed-layout-is-stable-across-repeated-assembly
  ;; Assembling the same program twice yields identical output -- guards
  ;; against per-iteration state (floors, provisional symbols) leaking
  ;; between calls to ASSEMBLE.
  (let ((a0 (assemble "lda a
.res 254
a: nop" :machine 'instr-test-machine))
        (a1 (assemble "lda a
.res 254
a: nop" :machine 'instr-test-machine)))
    (fiveam:is (equalp (assembly-bytes a0) (assembly-bytes a1)))
    (fiveam:is (= (gethash "a" (assembly-symbols a0)) (gethash "a" (assembly-symbols a1))))))

(fiveam:test immediate-operand-still-selects-immediate-mode-among-variants
  (let ((a (assemble "lda #7" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#x10 7) (assembly-bytes a)))))

(fiveam:test no-matching-mode-signals-assembly-error
  ;; ADC only declares ABSOLUTE -- an immediate-syntax operand matches none
  ;; of its variants.
  (fiveam:signals assembly-error
    (assemble "adc #10" :machine 'instr-test-machine)))

;;; RELATIVE mode (#23) -- BRA (tests/instruction.lisp), kept separate from
;;; the existing ABSOLUTE-mode BNE tests above.

(fiveam:test relative-branch-forward-offset
  ;; 0: bra end (2 bytes) / 2: nop (1 byte) / 3: end: nop -- next-pc after
  ;; BRA is 2, target is 3, offset is +1.
  (let ((a (assemble "bra end
nop
end: nop" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#x90 1 #xEA #xEA) (assembly-bytes a)))))

(fiveam:test relative-branch-backward-offset
  ;; 0: loop: nop (1 byte) / 1: bra loop (2 bytes) -- next-pc after BRA is 3,
  ;; target is 0, offset is -3 (#xFD).
  (let ((a (assemble "loop: nop
bra loop" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#xEA #x90 #xFD) (assembly-bytes a)))))

(fiveam:test relative-branch-to-itself-is-minus-two
  ;; next-pc is this instruction's own address + 2; branching to its own
  ;; address is therefore offset -2.
  (let ((a (assemble "here: bra here" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#x90 #xFE) (assembly-bytes a)))))

(fiveam:test relative-branch-offset-is-independent-of-origin
  ;; The offset is relative to the branch's own address, so it must not
  ;; change when the whole program is shifted by an origin.
  (let ((a0 (assemble "loop: nop
bra loop" :machine 'instr-test-machine))
        (a1 (assemble "loop: nop
bra loop" :machine 'instr-test-machine :origin #x200)))
    (fiveam:is (equalp (assembly-bytes a0) (assembly-bytes a1)))))

(fiveam:test relative-branch-forward-out-of-range-signals-assembly-error
  ;; 200 filler NOPs put "end" 200 bytes past BRA -- out of a signed 1-byte
  ;; offset's [-128, 127) range.
  (fiveam:signals assembly-error
    (assemble (format nil "bra end~%~{~A~%~}end: nop"
                       (make-list 200 :initial-element "nop"))
              :machine 'instr-test-machine)))

(fiveam:test relative-branch-backward-out-of-range-signals-assembly-error
  (fiveam:signals assembly-error
    (assemble (format nil "loop: nop~%~{~A~%~}bra loop"
                       (make-list 200 :initial-element "nop"))
              :machine 'instr-test-machine)))

;;; RELATIVE alongside another mode sharing the same syntax (#31) -- BRX
;;; (tests/instruction.lisp) declares RELATIVE before ABSOLUTE, so relaxation
;;; must be able to narrow a label operand to the 2-byte relative encoding,
;;; not always take ABSOLUTE by default.

(fiveam:test relative-candidate-narrows-when-target-is-in-range
  ;; next-pc after a 2-byte BRX is address+2; a target one byte forward
  ;; fits the signed 1-byte relative offset, so relaxation picks RELATIVE.
  (let ((a (assemble "brx target
target: nop" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#x91 0 #xEA) (assembly-bytes a)))))

(fiveam:test relative-candidate-widens-when-target-is-out-of-range
  ;; 200 filler NOPs put "end" out of a signed 1-byte offset's range --
  ;; relaxation must widen to the 3-byte ABSOLUTE encoding instead of
  ;; signalling an out-of-range error the way a single-mode RELATIVE
  ;; instruction (BRA) would.
  (let ((a (assemble (format nil "brx end~%~{~A~%~}end: nop"
                              (make-list 200 :initial-element "nop"))
                      :machine 'instr-test-machine)))
    (fiveam:is (= #x92 (aref (assembly-bytes a) 0)))))

;;; SIGNED, non-RELATIVE (#30) mode selection -- LDSI (tests/instruction.lisp)
;;; declares the signed 1-byte mode before a wider unsigned one, sharing the
;;; same "#" expr syntax, mirroring the BRX case above but for the signed
;;; fit test (%FITS-SIGNED-WIDTH-P) instead of the RELATIVE one.

(fiveam:test signed-candidate-selected-when-value-fits-signed-range
  (let ((a (assemble "ldsi #-5" :machine 'instr-test-machine)))
    (fiveam:is (equalp (vector #x93 (wrap-value -5 8)) (assembly-bytes a)))))

(fiveam:test signed-candidate-rejected-when-value-is-unsigned-only
  ;; 200 fits one byte unsigned but not signed -- before #30, this would
  ;; still have selected the narrower (then RELATIVE-blind) candidate via
  ;; %FITS-WIDTH-P; now it must widen to the 2-byte unsigned mode instead.
  (let ((a (assemble "ldsi #200" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#x94 200 0) (assembly-bytes a)))))

;;; Multi-operand instructions -- MOVI/FLEX (tests/instruction.lisp).

(fiveam:test multi-operand-statement-sizes-as-sum-of-field-widths
  ;; MOVI: opcode + 1-byte dst + 2-byte src = 4 bytes total.
  (let ((a (assemble "movi $10, $2200
next: nop" :machine 'instr-test-machine)))
    (fiveam:is (= 4 (gethash "next" (assembly-symbols a))))
    (fiveam:is (equalp #(#xF8 #x10 #x00 #x22 #xEA) (assembly-bytes a)))))

(fiveam:test multi-operand-label-in-second-hole-resolves
  (let ((a (assemble "movi $1, target
target: nop" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#xF8 1 4 0 #xEA) (assembly-bytes a)))))

(fiveam:test multi-operand-variant-selected-by-syntax-among-differing-arities
  (let ((imm (assemble "flex #5" :machine 'instr-test-machine))
        (two (assemble "flex $1, $2" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#x20 5) (assembly-bytes imm)))
    (fiveam:is (equalp #(#x21 1 2) (assembly-bytes two)))))

;;; Directives (directive.lisp, #14) -- .ORG / .BYTE / .WORD / .RES dispatch
;;; in %LAYOUT/%ENCODE, reusing INSTR-TEST-MACHINE.

(fiveam:test byte-directive-emits-one-byte-per-value
  (let ((a (assemble ".byte 1, 2, 3" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(1 2 3) (assembly-bytes a)))))

(fiveam:test word-directive-emits-little-endian-words
  (let ((a (assemble ".word $1234" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#x34 #x12) (assembly-bytes a)))))

(fiveam:test byte-directive-with-label-argument-resolves-in-pass-2
  ;; nop (1 byte, address 0), target: nop (address 1) -- .byte target should
  ;; fold to 1 once pass 2 has the symbol table, even though pass 1 (where
  ;; .byte's own size is just its argument count) never evaluates it.
  (let ((a (assemble "nop
target: nop
.byte target" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#xEA #xEA 1) (assembly-bytes a)))))

(fiveam:test leading-org-sets-assembly-origin
  (let ((a (assemble ".org $8000
start: nop" :machine 'instr-test-machine)))
    (fiveam:is (= #x8000 (assembly-origin a)))
    (fiveam:is (= #x8000 (gethash "start" (assembly-symbols a))))
    (fiveam:is (equalp #(#xEA) (assembly-bytes a)))))

(fiveam:test label-on-org-line-binds-to-new-address
  (let ((a (assemble "here: .org $8000" :machine 'instr-test-machine)))
    (fiveam:is (= #x8000 (gethash "here" (assembly-symbols a))))))

(fiveam:test mid-program-forward-org-zero-fills-the-gap
  (let ((a (assemble "nop
.org 4
nop" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#xEA 0 0 0 #xEA) (assembly-bytes a)))))

(fiveam:test mid-program-backward-org-signals-assembly-error
  (fiveam:signals assembly-error
    (assemble "nop
nop
nop
.org 1
nop" :machine 'instr-test-machine)))

(fiveam:test org-with-label-operand-signals-assembly-error
  ;; .org's operand must fold label-free in pass 1 -- there is no symbol
  ;; table yet to resolve TARGET against.
  (fiveam:signals assembly-error
    (assemble ".org target
target: nop" :machine 'instr-test-machine)))

(fiveam:test res-directive-reserves-zero-filled-bytes
  (let ((a (assemble "nop
.res 4
next: nop" :machine 'instr-test-machine)))
    (fiveam:is (= 5 (gethash "next" (assembly-symbols a))))
    (fiveam:is (equalp #(#xEA 0 0 0 0 #xEA) (assembly-bytes a)))))

(fiveam:test res-directive-with-negative-count-signals-assembly-error
  (fiveam:signals assembly-error
    (assemble ".res -1" :machine 'instr-test-machine)))

(fiveam:test org-directive-with-wrong-arity-signals-assembly-error
  (fiveam:signals assembly-error
    (assemble ".org $10, $20" :machine 'instr-test-machine)))

(fiveam:test directive-mnemonic-is-not-confused-with-an-instruction
  ;; ".byte" is not a registered mnemonic on INSTR-TEST-MACHINE -- confirms
  ;; directive dispatch in %LAYOUT doesn't fall through to
  ;; FIND-INSTRUCTION-VARIANTS (which would signal UNKNOWN-INSTRUCTION).
  (let ((a (assemble ".byte 1" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(1) (assembly-bytes a)))))

;;; Location-counter symbol ("*", #15)

(fiveam:test word-directive-with-location-counter-emits-own-address
  ;; nop occupies address 0, so the .word entry starts at address 1 -- "*"
  ;; there must fold to 1, not 0 (the statement's address, not the program's).
  (let ((a (assemble "nop
.word *" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#xEA 1 0) (assembly-bytes a)))))

(fiveam:test byte-directive-with-two-location-counters-emits-two-different-values
  ;; Each "*" resolves to its own element's address, not the directive
  ;; statement's -- ".byte *, *" at address 0 emits 0 then 1.
  (let ((a (assemble ".byte *, *" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(0 1) (assembly-bytes a)))))

(fiveam:test location-counter-operand-picks-narrowest-fitting-mode
  ;; "*" at address 0 is 0, a zero-page-fitting value -- same variant
  ;; selection as an equivalent literal constant.
  (let ((a (assemble "lda *" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#x11 0) (assembly-bytes a)))))

(fiveam:test location-counter-with-offset-in-operand
  (let ((a (assemble "lda *+3" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#x11 3) (assembly-bytes a)))))

(fiveam:test relative-branch-to-location-counter-is-minus-two
  ;; "bra *" should behave exactly like the equivalent self-referencing
  ;; label ("here: bra here", see RELATIVE-BRANCH-TO-ITSELF-IS-MINUS-TWO).
  (let ((a (assemble "bra *" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#x90 #xFE) (assembly-bytes a)))))

(fiveam:test org-with-location-counter-pads-forward-from-current-address
  (let ((a (assemble "nop
.org *+4
nop" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#xEA 0 0 0 0 #xEA) (assembly-bytes a)))))

(fiveam:test location-counter-with-no-address-known-signals-unresolved-location
  ;; EVAL-EXPR-CONSTANT with no :PC (e.g. a standalone caller, not the
  ;; assembler) can't fold "*" at all.
  (fiveam:signals unresolved-location
    (eval-expr-constant (parse-expression (tokenize "*")))))

;;; Local-label scoping (#16) -- a ".name" label/reference is qualified
;;; against the nearest preceding non-local ("global") label.

(fiveam:test local-labels-in-different-scopes-do-not-collide
  ;; Both routines use ".loop:" -- without scoping this would be a duplicate
  ;; label; qualified as "a.loop"/"b.loop" they coexist, and each BRA .LOOP
  ;; resolves to its own routine's loop, producing the identical -3 offset.
  (let ((a (assemble "a: nop
.loop: nop
bra .loop
b: nop
.loop: nop
bra .loop" :machine 'instr-test-machine)))
    (fiveam:is (= 0 (gethash "a" (assembly-symbols a))))
    (fiveam:is (= 1 (gethash "a.loop" (assembly-symbols a))))
    (fiveam:is (= 4 (gethash "b" (assembly-symbols a))))
    (fiveam:is (= 5 (gethash "b.loop" (assembly-symbols a))))
    (fiveam:is (equalp #(#xEA #xEA #x90 #xFD #xEA #xEA #x90 #xFD) (assembly-bytes a)))))

(fiveam:test duplicate-local-label-within-the-same-scope-still-signals-assembly-error
  (fiveam:signals assembly-error
    (assemble "a: nop
.loop: nop
.loop: nop" :machine 'instr-test-machine)))

(fiveam:test local-label-reference-does-not-see-a-different-scope
  ;; ".loop" is only ever bound under "a" -- a reference under "b" looks for
  ;; "b.loop", which doesn't exist.
  (fiveam:signals unresolved-label
    (assemble "a: nop
.loop: nop
b: nop
bra .loop" :machine 'instr-test-machine)))

(fiveam:test local-label-definition-with-no-enclosing-global-signals-assembly-error
  (fiveam:signals assembly-error
    (assemble ".loop: nop" :machine 'instr-test-machine)))

(fiveam:test local-label-reference-with-no-enclosing-global-signals-assembly-error
  (fiveam:signals assembly-error
    (assemble "bra .loop" :machine 'instr-test-machine)))
