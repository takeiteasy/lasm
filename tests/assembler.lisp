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
    (fiveam:is (equalp #(#xD0 3 0 #xEA) (assembly-cells a)))))

(fiveam:test backward-label-reference-resolves
  (let ((a (assemble "target: nop
bne target" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#xEA #xD0 0 0) (assembly-cells a)))))

(fiveam:test origin-offsets-bytes-and-symbols
  (let ((a (assemble "start: nop
bne start" :machine 'instr-test-machine :origin #x200)))
    (fiveam:is (= #x200 (assembly-origin a)))
    (fiveam:is (= #x200 (gethash "start" (assembly-symbols a))))
    (fiveam:is (equalp #(#xEA #xD0 0 2) (assembly-cells a)))))

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
    (fiveam:is (equalp #(#xEA) (assembly-cells a)))))

(fiveam:test unknown-mnemonic-signals-unknown-instruction
  (fiveam:signals unknown-instruction
    (assemble "frobnicate" :machine 'instr-test-machine)))

(fiveam:test no-operand-instruction-encodes-alone
  (let ((a (assemble "nop" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#xEA) (assembly-cells a)))))

(fiveam:test immediate-and-absolute-mix
  (let ((a (assemble "ldx #10
adc target
target: nop" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#xA2 10 #x6D 5 0 #xEA) (assembly-cells a)))))

;;; M2 multi-mode selection (mode.lisp, #18) -- LDA declares
;;; immediate/zero-page/absolute (tests/instruction.lisp), zero-page and
;;; absolute sharing identical operand syntax and differing only by width.

(fiveam:test constant-operand-picks-narrowest-fitting-mode
  (let ((a (assemble "lda $10" :machine 'instr-test-machine)))
    ;; zero-page (opcode #x11), not absolute (#x12) -- 2 bytes total
    (fiveam:is (equalp #(#x11 #x10) (assembly-cells a)))))

(fiveam:test constant-operand-too-wide-for-zero-page-picks-absolute
  (let ((a (assemble "lda $1000" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#x12 #x00 #x10) (assembly-cells a)))))

(fiveam:test label-operand-narrows-to-zero-page-after-layout-converges
  ;; "target" resolves to address 2, comfortably zero-page -- relaxation
  ;; starts LDA at its narrowest mode, lays out, and confirms it fits once a
  ;; provisional address is available, rather than always taking the widest
  ;; matching mode.
  (let ((a (assemble "lda target
target: nop" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#x11 2 #xEA) (assembly-cells a)))
    (fiveam:is (= 2 (gethash "target" (assembly-symbols a))))))

(fiveam:test label-operand-stays-absolute-when-it-must
  ;; "target" resolves past zero page, so relaxation widens LDA to absolute.
  (let ((a (assemble "lda target
.res $300
target: nop" :machine 'instr-test-machine)))
    (fiveam:is (= #x12 (aref (assembly-cells a) 0)))
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
    (fiveam:is (equalp #(#x12 #x01 #x01) (subseq (assembly-cells a) 0 3)))))

(fiveam:test label-operand-self-reference-narrows-to-zero-page
  (let ((a (assemble "here: lda here" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#x11 0) (assembly-cells a)))))

(fiveam:test backward-label-operand-narrows-to-zero-page
  (let ((a (assemble "target: nop
lda target" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#xEA #x11 0) (assembly-cells a)))))

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
    (fiveam:is (equalp (assembly-cells a0) (assembly-cells a1)))
    (fiveam:is (= (gethash "a" (assembly-symbols a0)) (gethash "a" (assembly-symbols a1))))))

(fiveam:test immediate-operand-still-selects-immediate-mode-among-variants
  (let ((a (assemble "lda #7" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#x10 7) (assembly-cells a)))))

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
    (fiveam:is (equalp #(#x90 1 #xEA #xEA) (assembly-cells a)))))

(fiveam:test relative-branch-backward-offset
  ;; 0: loop: nop (1 byte) / 1: bra loop (2 bytes) -- next-pc after BRA is 3,
  ;; target is 0, offset is -3 (#xFD).
  (let ((a (assemble "loop: nop
bra loop" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#xEA #x90 #xFD) (assembly-cells a)))))

(fiveam:test relative-branch-to-itself-is-minus-two
  ;; next-pc is this instruction's own address + 2; branching to its own
  ;; address is therefore offset -2.
  (let ((a (assemble "here: bra here" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#x90 #xFE) (assembly-cells a)))))

(fiveam:test relative-branch-offset-is-independent-of-origin
  ;; The offset is relative to the branch's own address, so it must not
  ;; change when the whole program is shifted by an origin.
  (let ((a0 (assemble "loop: nop
bra loop" :machine 'instr-test-machine))
        (a1 (assemble "loop: nop
bra loop" :machine 'instr-test-machine :origin #x200)))
    (fiveam:is (equalp (assembly-cells a0) (assembly-cells a1)))))

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
    (fiveam:is (equalp #(#x91 0 #xEA) (assembly-cells a)))))

(fiveam:test relative-candidate-widens-when-target-is-out-of-range
  ;; 200 filler NOPs put "end" out of a signed 1-byte offset's range --
  ;; relaxation must widen to the 3-byte ABSOLUTE encoding instead of
  ;; signalling an out-of-range error the way a single-mode RELATIVE
  ;; instruction (BRA) would.
  (let ((a (assemble (format nil "brx end~%~{~A~%~}end: nop"
                              (make-list 200 :initial-element "nop"))
                      :machine 'instr-test-machine)))
    (fiveam:is (= #x92 (aref (assembly-cells a) 0)))))

;;; SIGNED, non-RELATIVE (#30) mode selection -- LDSI (tests/instruction.lisp)
;;; declares the signed 1-byte mode before a wider unsigned one, sharing the
;;; same "#" expr syntax, mirroring the BRX case above but for the signed
;;; fit test (%FITS-SIGNED-WIDTH-P) instead of the RELATIVE one.

(fiveam:test signed-candidate-selected-when-value-fits-signed-range
  (let ((a (assemble "ldsi #-5" :machine 'instr-test-machine)))
    (fiveam:is (equalp (vector #x93 (wrap-value -5 8)) (assembly-cells a)))))

(fiveam:test signed-candidate-rejected-when-value-is-unsigned-only
  ;; 200 fits one byte unsigned but not signed -- before #30, this would
  ;; still have selected the narrower (then RELATIVE-blind) candidate via
  ;; %FITS-WIDTH-P; now it must widen to the 2-byte unsigned mode instead.
  (let ((a (assemble "ldsi #200" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#x94 200 0) (assembly-cells a)))))

;;; Multi-operand instructions -- MOVI/FLEX (tests/instruction.lisp).

(fiveam:test multi-operand-statement-sizes-as-sum-of-field-widths
  ;; MOVI: opcode + 1-byte dst + 2-byte src = 4 bytes total.
  (let ((a (assemble "movi $10, $2200
next: nop" :machine 'instr-test-machine)))
    (fiveam:is (= 4 (gethash "next" (assembly-symbols a))))
    (fiveam:is (equalp #(#xF8 #x10 #x00 #x22 #xEA) (assembly-cells a)))))

(fiveam:test multi-operand-label-in-second-hole-resolves
  (let ((a (assemble "movi $1, target
target: nop" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#xF8 1 4 0 #xEA) (assembly-cells a)))))

(fiveam:test multi-operand-variant-selected-by-syntax-among-differing-arities
  (let ((imm (assemble "flex #5" :machine 'instr-test-machine))
        (two (assemble "flex $1, $2" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#x20 5) (assembly-cells imm)))
    (fiveam:is (equalp #(#x21 1 2) (assembly-cells two)))))

;;; Directives (directive.lisp, #14) -- .ORG / .BYTE / .WORD / .RES dispatch
;;; in %LAYOUT/%ENCODE, reusing INSTR-TEST-MACHINE.

(fiveam:test byte-directive-emits-one-byte-per-value
  (let ((a (assemble ".byte 1, 2, 3" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(1 2 3) (assembly-cells a)))))

(fiveam:test word-directive-emits-little-endian-words
  (let ((a (assemble ".word $1234" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#x34 #x12) (assembly-cells a)))))

(fiveam:test byte-directive-with-label-argument-resolves-in-pass-2
  ;; nop (1 byte, address 0), target: nop (address 1) -- .byte target should
  ;; fold to 1 once pass 2 has the symbol table, even though pass 1 (where
  ;; .byte's own size is just its argument count) never evaluates it.
  (let ((a (assemble "nop
target: nop
.byte target" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#xEA #xEA 1) (assembly-cells a)))))

(fiveam:test leading-org-sets-assembly-origin
  (let ((a (assemble ".org $8000
start: nop" :machine 'instr-test-machine)))
    (fiveam:is (= #x8000 (assembly-origin a)))
    (fiveam:is (= #x8000 (gethash "start" (assembly-symbols a))))
    (fiveam:is (equalp #(#xEA) (assembly-cells a)))))

(fiveam:test label-on-org-line-binds-to-new-address
  (let ((a (assemble "here: .org $8000" :machine 'instr-test-machine)))
    (fiveam:is (= #x8000 (gethash "here" (assembly-symbols a))))))

(fiveam:test mid-program-forward-org-zero-fills-the-gap
  (let ((a (assemble "nop
.org 4
nop" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#xEA 0 0 0 #xEA) (assembly-cells a)))))

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
    (fiveam:is (equalp #(#xEA 0 0 0 0 #xEA) (assembly-cells a)))))

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
    (fiveam:is (equalp #(1) (assembly-cells a)))))

;;; Location-counter symbol ("*", #15)

(fiveam:test word-directive-with-location-counter-emits-own-address
  ;; nop occupies address 0, so the .word entry starts at address 1 -- "*"
  ;; there must fold to 1, not 0 (the statement's address, not the program's).
  (let ((a (assemble "nop
.word *" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#xEA 1 0) (assembly-cells a)))))

(fiveam:test byte-directive-with-two-location-counters-emits-two-different-values
  ;; Each "*" resolves to its own element's address, not the directive
  ;; statement's -- ".byte *, *" at address 0 emits 0 then 1.
  (let ((a (assemble ".byte *, *" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(0 1) (assembly-cells a)))))

(fiveam:test location-counter-operand-picks-narrowest-fitting-mode
  ;; "*" at address 0 is 0, a zero-page-fitting value -- same variant
  ;; selection as an equivalent literal constant.
  (let ((a (assemble "lda *" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#x11 0) (assembly-cells a)))))

(fiveam:test location-counter-with-offset-in-operand
  (let ((a (assemble "lda *+3" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#x11 3) (assembly-cells a)))))

(fiveam:test relative-branch-to-location-counter-is-minus-two
  ;; "bra *" should behave exactly like the equivalent self-referencing
  ;; label ("here: bra here", see RELATIVE-BRANCH-TO-ITSELF-IS-MINUS-TWO).
  (let ((a (assemble "bra *" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#x90 #xFE) (assembly-cells a)))))

(fiveam:test org-with-location-counter-pads-forward-from-current-address
  (let ((a (assemble "nop
.org *+4
nop" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#xEA 0 0 0 0 #xEA) (assembly-cells a)))))

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
    (fiveam:is (equalp #(#xEA #xEA #x90 #xFD #xEA #xEA #x90 #xFD) (assembly-cells a)))))

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

;;; .EQU / symbol assignment (#35) -- both spellings (".equ name, value" and
;;; "name = value"), layout-time backward-only folding, the same
;;; duplicate-symbol rule a label uses, and the pure-.EQU restriction on
;;; .ORG/.RES (#41 tracks lifting it). DEFDIRECTIVE registration and arity
;;; live in tests/directive.lisp -- these exercise statement-level dispatch,
;;; the same split that file documents for .ORG/.BYTE/.WORD/.RES.

(fiveam:test equ-binds-a-constant-value-not-an-address
  (let ((a (assemble ".equ x, 5
.byte x" :machine 'instr-test-machine)))
    (fiveam:is (= 5 (gethash "x" (assembly-symbols a))))
    (fiveam:is (equalp #(5) (assembly-cells a)))))

(fiveam:test equ-contributes-no-bytes-and-does-not-move-the-address-counter
  (let ((a (assemble "nop
.equ x, 5
next: nop" :machine 'instr-test-machine)))
    (fiveam:is (= 1 (gethash "next" (assembly-symbols a))))
    (fiveam:is (equalp #(#xEA #xEA) (assembly-cells a)))))

(fiveam:test equ-value-may-reference-an-earlier-equ
  (let ((a (assemble ".equ a, 1
.equ b, a + 1
.byte b" :machine 'instr-test-machine)))
    (fiveam:is (= 2 (gethash "b" (assembly-symbols a))))
    (fiveam:is (equalp #(2) (assembly-cells a)))))

(fiveam:test equ-value-may-reference-the-location-counter
  ;; The ticket's motivating example: a size computed from "*" and a
  ;; backward label.
  (let ((a (assemble "start: nop
nop
.equ size, * - start
.byte size" :machine 'instr-test-machine)))
    (fiveam:is (= 2 (gethash "size" (assembly-symbols a))))
    (fiveam:is (equalp #(#xEA #xEA 2) (assembly-cells a)))))

(fiveam:test equ-forward-reference-signals-assembly-error
  (fiveam:signals assembly-error
    (assemble ".equ x, y
.equ y, 1" :machine 'instr-test-machine)))

(fiveam:test equ-duplicate-against-a-label-signals-assembly-error
  (fiveam:signals assembly-error
    (assemble "foo: nop
.equ foo, 5" :machine 'instr-test-machine)))

(fiveam:test label-duplicate-against-an-earlier-equ-signals-assembly-error
  (fiveam:signals assembly-error
    (assemble ".equ foo, 5
foo: nop" :machine 'instr-test-machine)))

(fiveam:test equ-duplicate-against-an-earlier-equ-signals-assembly-error
  (fiveam:signals assembly-error
    (assemble ".equ foo, 1
.equ foo, 2" :machine 'instr-test-machine)))

(fiveam:test equ-first-operand-must-be-a-bare-identifier
  (fiveam:signals assembly-error
    (assemble ".equ 1 + 2, 5" :machine 'instr-test-machine)))

(fiveam:test local-equ-name-is-scoped-like-a-local-label
  (let ((a (assemble "loop: nop
.equ .n, 3
.byte .n" :machine 'instr-test-machine)))
    (fiveam:is (= 3 (gethash "loop.n" (assembly-symbols a))))
    (fiveam:is (equalp #(#xEA 3) (assembly-cells a)))))

(fiveam:test equ-used-as-instruction-operand-narrows-to-zero-page
  (let ((a (assemble ".equ addr, $10
lda addr" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#x11 #x10) (assembly-cells a)))))

(fiveam:test equ-used-as-instruction-operand-widens-to-absolute-when-it-must
  (let ((a (assemble ".equ addr, $1000
lda addr" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#x12 #x00 #x10) (assembly-cells a)))))

(fiveam:test pure-equ-may-be-referenced-by-res
  (let ((a (assemble ".equ n, 4
.res n
next: nop" :machine 'instr-test-machine)))
    (fiveam:is (= 4 (gethash "next" (assembly-symbols a))))
    (fiveam:is (equalp #(0 0 0 0 #xEA) (assembly-cells a)))))

(fiveam:test pure-equ-may-be-referenced-by-org
  (let ((a (assemble ".equ base, $8000
.org base
start: nop" :machine 'instr-test-machine)))
    (fiveam:is (= #x8000 (assembly-origin a)))
    (fiveam:is (= #x8000 (gethash "start" (assembly-symbols a))))))

(fiveam:test address-dependent-equ-referenced-by-res-signals-assembly-error
  ;; "n" depends on "*", so it's absent from the pure-.EQU table .RES reads
  ;; from -- see this file's header and assembler.lisp's #35 paragraph (#41
  ;; tracks lifting this restriction).
  (fiveam:signals assembly-error
    (assemble "start: nop
.equ n, * - start
.res n" :machine 'instr-test-machine)))

(fiveam:test address-dependent-equ-referenced-by-org-signals-assembly-error
  (fiveam:signals assembly-error
    (assemble "start: nop
.equ base, * + $100
.org base" :machine 'instr-test-machine)))

(fiveam:test equals-sugar-is-equivalent-to-equ
  (let ((a (assemble "x = 5
.byte x" :machine 'instr-test-machine)))
    (fiveam:is (= 5 (gethash "x" (assembly-symbols a))))
    (fiveam:is (equalp #(5) (assembly-cells a)))))

(fiveam:test equals-sugar-keeps-a-label-on-the-same-line
  (let ((a (assemble "here: x = 5
.byte x, here" :machine 'instr-test-machine)))
    (fiveam:is (= 0 (gethash "here" (assembly-symbols a))))
    (fiveam:is (= 5 (gethash "x" (assembly-symbols a))))
    (fiveam:is (equalp #(5 0) (assembly-cells a)))))

;;; Scope-aware symbol metadata (#37) -- ASSEMBLY-SYMBOL-INFO tags every
;;; ASSEMBLY-SYMBOLS entry with its unqualified name, enclosing scope, and
;;; kind (:LABEL or :EQU), captured at bind time rather than recovered later
;;; by splitting the qualified name (#36).

(fiveam:test symbol-info-tags-a-global-label
  (let* ((a (assemble "start: nop" :machine 'instr-test-machine))
         (info (gethash "start" (assembly-symbol-info a))))
    (fiveam:is (string= "start" (symbol-info-name info)))
    (fiveam:is (string= "start" (symbol-info-qualified-name info)))
    (fiveam:is (null (symbol-info-scope info)))
    (fiveam:is (eq :label (symbol-info-kind info)))
    (fiveam:is (not (symbol-info-localp info)))
    (fiveam:is (= 0 (symbol-info-value info)))))

(fiveam:test symbol-info-tags-a-local-label-with-its-enclosing-scope
  (let* ((a (assemble "loop: nop
.next: nop" :machine 'instr-test-machine))
         (info (gethash "loop.next" (assembly-symbol-info a))))
    (fiveam:is (string= ".next" (symbol-info-name info)))
    (fiveam:is (string= "loop.next" (symbol-info-qualified-name info)))
    (fiveam:is (string= "loop" (symbol-info-scope info)))
    (fiveam:is (eq :label (symbol-info-kind info)))
    (fiveam:is (symbol-info-localp info))
    (fiveam:is (= 1 (symbol-info-value info)))))

(fiveam:test symbol-info-tags-a-top-level-equ
  (let* ((a (assemble ".equ bufsize, 16" :machine 'instr-test-machine))
         (info (gethash "bufsize" (assembly-symbol-info a))))
    (fiveam:is (string= "bufsize" (symbol-info-name info)))
    (fiveam:is (null (symbol-info-scope info)))
    (fiveam:is (eq :equ (symbol-info-kind info)))
    (fiveam:is (not (symbol-info-localp info)))
    (fiveam:is (= 16 (symbol-info-value info)))))

(fiveam:test symbol-info-tags-a-local-equ-with-its-enclosing-scope
  (let* ((a (assemble "loop: nop
.equ .n, 3" :machine 'instr-test-machine))
         (info (gethash "loop.n" (assembly-symbol-info a))))
    (fiveam:is (string= ".n" (symbol-info-name info)))
    (fiveam:is (string= "loop" (symbol-info-scope info)))
    (fiveam:is (eq :equ (symbol-info-kind info)))
    (fiveam:is (symbol-info-localp info))
    (fiveam:is (= 3 (symbol-info-value info)))))

(fiveam:test symbol-info-equ-does-not-become-the-scope-of-a-later-local-label
  ;; An .EQU on its own never becomes SCOPE (assembler.lisp) -- a local
  ;; label after a top-level .EQU still has no enclosing global.
  (fiveam:signals assembly-error
    (assemble ".equ x, 1
.loop: nop" :machine 'instr-test-machine)))

(fiveam:test symbol-info-distinguishes-a-same-spelled-global-from-a-qualified-local
  ;; #36's hazard: a global literally named "loop.next" and a local ".next"
  ;; under a *different* global "loop" both produce the ASSEMBLY-SYMBOLS key
  ;; "loop.next" -- but they must remain distinguishable via SYMBOL-INFO's
  ;; SCOPE, which string-splitting the key alone could never recover (the
  ;; global's own SCOPE is NIL; the local's is "loop").
  (fiveam:signals assembly-error
    ;; Both would bind the literal key "loop.next" -- this is the pre-
    ;; existing #36 collision, still a duplicate-symbol error today.
    (assemble "loop: nop
.next: nop
loop.next: nop" :machine 'instr-test-machine))
  ;; With no collision, the discriminator works as intended: a real global
  ;; spelled with a dot in it is recorded with SCOPE NIL, not confused for
  ;; anyone's local.
  (let* ((a (assemble "loop.next: nop" :machine 'instr-test-machine))
         (info (gethash "loop.next" (assembly-symbol-info a))))
    (fiveam:is (null (symbol-info-scope info)))
    (fiveam:is (not (symbol-info-localp info)))))

(fiveam:test assembly-symbols-shape-is-unchanged-by-symbol-info
  ;; ASSEMBLY-SYMBOLS itself stays a flat string -> integer table -- adding
  ;; ASSEMBLY-SYMBOL-INFO must not change its shape or values.
  (let ((a (assemble "loop: nop
.next: nop
.equ .n, 3" :machine 'instr-test-machine)))
    (maphash (lambda (k v)
               (declare (ignore k))
               (fiveam:is (integerp v)))
             (assembly-symbols a))
    (fiveam:is (= 3 (hash-table-count (assembly-symbols a))))
    (fiveam:is (= 3 (hash-table-count (assembly-symbol-info a))))))

;;; Forced addressing-mode suffix (#40, e.g. "lda.w"/"lda.z") -- LDA's three
;;; variants (immediate #x10, zero-page #x11, absolute #x12) share bare-expr
;;; syntax between zero-page/absolute, so they're the pair a forced suffix
;;; actually overrides.

(fiveam:test mode-suffix-z-forces-zero-page
  (let ((a (assemble "lda.z $10" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#x11 #x10) (assembly-cells a)))))

(fiveam:test mode-suffix-w-forces-absolute-even-when-zero-page-would-fit
  (let ((a (assemble "lda.w $05" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#x12 5 0) (assembly-cells a)))))

(fiveam:test mode-suffix-z-bypasses-value-filter-and-wraps-silently
  ;; $1000 doesn't fit a zero-page byte at all -- forced ZERO-PAGE skips the
  ;; value filter entirely and ENCODE-INSTRUCTION's WRAP-VALUE truncates to
  ;; the low byte, exactly like a single-mode M1 instruction always did
  ;; (#28 tracks diagnosing this class of silent wrap generally).
  (let ((a (assemble "lda.z $1000" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#x11 #x00) (assembly-cells a)))))

(fiveam:test unknown-mode-suffix-signals-assembly-error
  (fiveam:signals assembly-error
    (assemble "lda.q $05" :machine 'instr-test-machine)))

(fiveam:test mode-suffix-naming-a-mode-the-mnemonic-has-no-variant-for-signals-assembly-error
  ;; LDX only declares an IMMEDIATE variant -- ZERO-PAGE is a real,
  ;; registered mode (so the suffix itself resolves), but LDX has no variant
  ;; using it.
  (fiveam:signals assembly-error
    (assemble "ldx.z $05" :machine 'instr-test-machine)))

(fiveam:test mode-suffix-not-matching-forced-modes-syntax-signals-assembly-error
  ;; ".z" forces ZERO-PAGE, whose pattern is a bare expr -- "$10,X" doesn't
  ;; match it (that's INDEXED-X's syntax, a mode LDA has no variant for
  ;; regardless), so this is a syntax mismatch against the forced mode
  ;; itself, not "no addressing mode matches this operand" generically.
  (fiveam:signals assembly-error
    (assemble "lda.z $10,X" :machine 'instr-test-machine)))

(fiveam:test mode-suffix-on-a-directive-signals-assembly-error
  (fiveam:signals assembly-error
    (assemble ".byte.w 1" :machine 'instr-test-machine)))

(fiveam:test mode-suffix-forced-statement-does-not-disrupt-relaxation-convergence
  ;; A forced .w statement ahead of a label-bearing LDA that itself needs
  ;; more than one relaxation pass to converge (the same cascading scenario
  ;; as LABEL-OPERAND-NARROWING-CASCADES-ACROSS-ITERATIONS above, offset by
  ;; the forced statement's fixed 3-byte width) -- the forced statement's
  ;; own width never changes across passes, so it can't be the statement
  ;; that keeps the fixpoint from being reached.
  (let ((a (assemble "lda.w $05
lda a
.res 254
a: nop" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#x12 5 0) (subseq (assembly-cells a) 0 3)))
    (fiveam:is (= (+ 3 #x101) (gethash "a" (assembly-symbols a))))
    (fiveam:is (equalp #(#x12 4 1) (subseq (assembly-cells a) 3 6)))))

;; A custom RELATIVE mode with a suffix (#40): LASM's built-in RELATIVE mode
;; ships with no suffix (only ZERO-PAGE/ABSOLUTE do), so forcing a RELATIVE
;; variant needs a machine that declares its own suffixed relative mode --
;; mirrors BRX (tests/instruction.lisp), which already pairs RELATIVE with
;; ABSOLUTE on one mnemonic.
(defmode test-relative-suffixed-mode expr :width 1 :relative t :suffix "r")

(definstruction instr-test-machine brxs
  (modes
    (test-relative-suffixed-mode (opcode #x95) (semantics (set! pc (+ pc operand))))
    (absolute (opcode #x96) (semantics (set! pc operand)))))

(fiveam:test mode-suffix-forced-relative-out-of-range-still-signals-assembly-error
  ;; Unlike ZERO-PAGE/ABSOLUTE, a forced RELATIVE mode is not exempt from
  ;; range checking -- %RELATIVE-OFFSET (encode time) always range-checks,
  ;; regardless of whether the mode was chosen by relaxation or forced.
  (fiveam:signals assembly-error
    (assemble "brxs.r target
.res 200
target: nop" :machine 'instr-test-machine)))

;;; Word-encoded relaxation (#20) -- reuses WORD-TEST-MACHINE and its SET/HLT
;;; instructions from tests/instruction.lisp. SET's operand packs into a
;;; 10-bit SRC field inline for -1..30 (biased +1), or escapes to its own
;;; following word otherwise -- %CHOOSE-VARIANT (assembler.lisp) picks
;;; between the two exactly like it picks between two addressing-mode
;;; widths, generalized via INSTRUCTION-DESCRIPTOR-SIZE.

(fiveam:test word-instruction-picks-inline-variant-for-small-value
  (let ((a (assemble "set #5" :machine 'word-test-machine)))
    (fiveam:is (= 2 (length (assembly-cells a))))
    (fiveam:is (equalp #(#x06 #x10) (assembly-cells a)))))

(fiveam:test word-instruction-picks-extra-word-variant-for-large-value
  (let ((a (assemble "set #1000" :machine 'word-test-machine)))
    (fiveam:is (= 4 (length (assembly-cells a))))
    (fiveam:is (equalp #(#xff #x13 #xe8 #x03) (assembly-cells a)))))

(fiveam:test word-instruction-picks-extra-word-variant-for-negative-out-of-range-value
  (let ((a (assemble "set #-5" :machine 'word-test-machine)))
    (fiveam:is (= 4 (length (assembly-cells a))))))

(fiveam:test word-instruction-with-small-forward-label-stays-inline
  ;; TARGET's address (2, right after SET's own inline-sized instruction)
  ;; fits SET's -1..30 inline range, so relaxation's narrowest first guess
  ;; (SET always starts inline when the operand doesn't resolve yet) turns
  ;; out to already be correct -- no widening pass needed.
  (let ((a (assemble "set #target
target: hlt" :machine 'word-test-machine)))
    (fiveam:is (= 4 (length (assembly-cells a))))
    (fiveam:is (= 2 (gethash "target" (assembly-symbols a))))
    (fiveam:is (equalp #(#x03 #x10 #x00 #x20) (assembly-cells a)))))

(fiveam:test word-instruction-with-large-forward-label-widens-across-passes
  ;; TARGET's address (44, well past the -1..30 inline range) doesn't fit --
  ;; relaxation's first pass still guesses inline (TARGET is unresolved on
  ;; pass 1), then widens to the extra-word variant once TARGET's real
  ;; address is known, growing SET from 2 bytes to 4 and shifting TARGET by
  ;; 2 -- exercising the same sticky-widening fixpoint as the byte-encoded
  ;; addressing-mode case, generalized to extra-word count.
  (let ((a (assemble "set #target
.res 40
target: hlt" :machine 'word-test-machine)))
    (fiveam:is (= (+ 4 40 2) (length (assembly-cells a))))
    (fiveam:is (= 44 (gethash "target" (assembly-symbols a))))))

(fiveam:test word-instruction-no-operand-hlt-encodes-as-single-word
  (let ((a (assemble "hlt" :machine 'word-test-machine)))
    (fiveam:is (equalp #(#x00 #x20) (assembly-cells a)))))

;;; Cell-width-typed assembler output (#53) -- WORDADDR-TEST-MACHINE
;;; (tests/instruction.lisp) declares :CELL-WIDTH 16 memory with an ordinary
;;; (not INSTRUCTION-WORD/#20) opcode-plus-operand-cells encoding. The
;;; discriminating case is LABEL-BOUND-IN-CELLS-NOT-BYTES: if the assembler's
;;; location counter still advanced in 8-bit units under the hood, a label
;;; after a 2-cell instruction would bind to address 2 instead of 1.

(fiveam:test assembly-cells-element-type-matches-machine-cell-width
  (let ((a (assemble "nop" :machine 'wordaddr-test-machine)))
    (fiveam:is (= 16 (assembly-cell-width a)))
    (fiveam:is (equal '(unsigned-byte 16) (array-element-type (assembly-cells a))))))

(fiveam:test label-bound-in-cells-not-bytes
  ;; NOP is 1 cell; JMP is 2 (opcode + one operand cell, ADDR-WIDTH 12
  ;; rounding up to 1 cell of 16 bits) -- START must bind to 0 and the
  ;; second statement to 1, not 0/1 vs. a byte-counted 0/2.
  (let ((a (assemble "start: nop
jmp start" :machine 'wordaddr-test-machine)))
    (fiveam:is (= 0 (gethash "start" (assembly-symbols a))))
    (fiveam:is (equalp #(0 2 0) (assembly-cells a)))))

(fiveam:test label-after-multi-cell-instruction-binds-in-cells-not-bytes
  ;; SECOND follows JMP's 2-cell instruction (opcode + one 16-bit operand
  ;; cell) and must bind to address 2, and JMP's own encoded operand must be
  ;; that same value 2, not 4 -- a stray byte-splitting of the 16-bit
  ;; operand (the actual pre-#53 bug: %encode-value-bytes always masked to
  ;; 8 bits regardless of the machine's real cell width) would truncate the
  ;; encoded operand to #x02 either way here, but would corrupt a value
  ;; that doesn't fit 8 bits -- see ENCODE-INSTRUCTION-RETURNS-CELLS-NOT-
  ;; BYTE-PAIRS (tests/instruction.lisp) for that direct check.
  (let ((a (assemble "jmp second
second: nop" :machine 'wordaddr-test-machine)))
    (fiveam:is (= 2 (gethash "second" (assembly-symbols a))))
    (fiveam:is (equalp #(2 2 0) (assembly-cells a)))))

(fiveam:test byte-directive-lays-one-cell-per-value-on-word-addressed-machine
  (let ((a (assemble ".byte 1, 2, 3" :machine 'wordaddr-test-machine)))
    (fiveam:is (equalp #(1 2 3) (assembly-cells a)))))

(fiveam:test word-directive-lays-two-cells-per-value-on-word-addressed-machine
  ;; #x12340001 split into two 16-bit cells, little-endian: low #x0001, high
  ;; #x1234 -- ".word" means two of THIS machine's cells, not two 8-bit bytes.
  (let ((a (assemble ".word $12340001" :machine 'wordaddr-test-machine)))
    (fiveam:is (equalp #(#x0001 #x1234) (assembly-cells a)))))

(fiveam:test res-directive-reserves-cells-not-bytes-on-word-addressed-machine
  (let ((a (assemble "nop
.res 4
jmp *" :machine 'wordaddr-test-machine)))
    (fiveam:is (= (+ 1 4 2) (length (assembly-cells a))))))

(fiveam:test org-gap-is-zero-filled-in-cells-on-word-addressed-machine
  (let ((a (assemble "nop
.org 5
nop" :machine 'wordaddr-test-machine)))
    (fiveam:is (equalp #(0 0 0 0 0 0) (assembly-cells a)))))

;;; ONE-OF (#103): end-to-end assembly through MOO (tests/instruction.lisp,
;;; opcode #xF7), whose OO-INSTR-TWO mode gives each of its two operand
;;; holes an independent choice between a bare register-shaped EXPR and a
;;; "[" expr "]" indirection. Because #103 delivers only the syntax --
;;; mode-selected field codes are a follow-up (see the tracker) -- every
;;; combination currently encodes identically; these tests pin that down as
;;; today's documented behavior, not an oversight.

(fiveam:test one-of-mode-assembles-first-alternative-on-both-holes
  (let ((a (assemble "moo $10, $20" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#xF7 #x10 #x20) (assembly-cells a)))))

(fiveam:test one-of-mode-assembles-mixed-alternatives-per-hole
  ;; dst via "[" expr "]", src via a bare expr -- each hole's choice is
  ;; independent of the other's.
  (let ((a (assemble "moo [$10], $20" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#xF7 #x10 #x20) (assembly-cells a)))))

(fiveam:test one-of-mode-assembles-both-holes-bracketed
  (let ((a (assemble "moo [$10], [$20]" :machine 'instr-test-machine)))
    (fiveam:is (equalp #(#xF7 #x10 #x20) (assembly-cells a)))))
