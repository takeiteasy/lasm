;;;; tests/emulator.lisp
;;;; fiveam tests for the M1 emulator loop (emulator.lisp), plus the M1
;;;; milestone's end-to-end counter-loop regression test.

(in-package #:lasm)

(fiveam:def-suite emulator :in lasm)
(fiveam:in-suite emulator)

;; A dedicated fixture with an HLT instruction (via TRAP) so RUN has a clean
;; way to stop -- INSTR-TEST-MACHINE (tests/instruction.lisp) has no such
;; instruction.
(defmachine emu-test-machine
  (register x :width 8)
  (register pc :width 16)
  (memory ram :width 8 :addr-width 16)
  (flags z))

(definstruction emu-test-machine ldx
  (modes immediate)
  (encoding (opcode #xA2) (operand :mode))
  (semantics (set! x operand) (set-flags! (z (zero? x)))))

(definstruction emu-test-machine dex
  (encoding (opcode #xCA))
  (semantics (set! x (wrap-value (1- x) 8)) (set-flags! (z (zero? x)))))

(definstruction emu-test-machine bne
  (modes absolute)
  (encoding (opcode #xD0) (operand :mode))
  (semantics (when (zerop z) (set! pc operand))))

;; RELATIVE mode (#23): same "branch if Z clear" condition as BNE above, just
;; RELATIVE instead of ABSOLUTE -- kept as a separate mnemonic so BNE's
;; existing ABSOLUTE-mode tests/byte expectations elsewhere in this file are
;; undisturbed.
(definstruction emu-test-machine bra
  (modes relative)
  (encoding (opcode #x90) (operand :mode))
  (semantics (when (zerop z) (set! pc (+ pc operand)))))

(definstruction emu-test-machine sta
  (modes absolute)
  (encoding (opcode #x8D) (operand :mode))
  (semantics (setf (mref machine 'ram operand) x)))

(definstruction emu-test-machine hlt
  (encoding (opcode #x00))
  (semantics (trap :halt)))

;; Sub-opcode cell (#125): IMMEDIATE and ABSOLUTE share opcode #xB0, told
;; apart at decode time by their own :SUB value rather than by opcode --
;; proves STEP-MACHINE (via DECODE-INSTRUCTION-AT) runs the *right* mode's
;; semantics for each, not just that decode picks the right descriptor.
(definstruction emu-test-machine subop
  (modes
    (immediate (opcode #xB0 :sub 0) (operand :mode) (semantics (set! x operand)))
    (absolute (opcode #xB0 :sub 1) (operand :mode) (semantics (set! x (mref machine 'ram operand))))))

;; Multi-hole sub-opcode table (#128): two ONE-OF holes jointly select the
;; sub-opcode cell -- proves STEP-MACHINE runs the right combination's own
;; semantics, not just that decode picks the right descriptor (SUBOP above
;; already covers the single-hole/whole-mode case).
(defmode emu-oo-reg expr)
(defmode emu-oo-ind "[" expr "]")
(defmode emu-oo-two (one-of emu-oo-reg emu-oo-ind) "," (one-of emu-oo-reg emu-oo-ind))

(definstruction emu-test-machine subtab
  (modes emu-oo-two)
  (encoding (opcode #xB1)
            (operand dst :width 1)
            (operand src :width 1)
            (sub-opcode
              (variant (choice emu-oo-reg emu-oo-reg) (sub 0))
              (variant (choice emu-oo-reg emu-oo-ind) (sub 1))
              (variant (choice emu-oo-ind emu-oo-reg) (sub 2))
              (variant (choice emu-oo-ind emu-oo-ind) (sub 3))))
  (semantics
    (let ((s (choice-case src
               (emu-oo-reg src)
               (emu-oo-ind (mref machine 'ram src)))))
      (choice-case dst
        (emu-oo-reg (setf (mref machine 'ram dst) s))
        (emu-oo-ind (setf (mref machine 'ram (mref machine 'ram dst)) s))))))

;; Per-hole :WIDTH (#129): the hole-selected sub-opcode selector doubling as
;; a decode-time width discriminator, the same way SUBOP above uses it for
;; per-mode semantics -- proves STEP-MACHINE runs the right alternative's own
;; semantics with operand values read at each alternative's own width, not
;; just that decode picks the right descriptor.
(defmode emu-wi-narrow expr :width 1)
(defmode emu-wi-wide "#" expr :width 2)
(defmode emu-wi-one (one-of emu-wi-narrow emu-wi-wide))

(definstruction emu-test-machine subwid
  (modes emu-wi-one)
  (encoding (opcode #xB2)
            (operand val :mode
              (variant (choice emu-wi-narrow) (sub 0))
              (variant (choice emu-wi-wide) (sub 1))))
  (semantics (set! x (wrap-value val 8))))

(fiveam:test step-machine-sub-opcode-width-runs-each-alternative-own-width
  (let* ((m (make-machine 'emu-test-machine))
         (a (assemble "subwid 200
subwid #300" :machine 'emu-test-machine))
         (descs (find-instruction-descriptors-by-opcode 'emu-test-machine #xB2))
         (narrow (find 0 descs :key #'instruction-descriptor-sub-opcode))
         (wide (find 1 descs :key #'instruction-descriptor-sub-opcode)))
    (load-program m a)
    (let ((descriptor (step-machine m)))                       ; subwid 200 -- narrow, 1-byte operand
      (fiveam:is (eq narrow descriptor))
      (fiveam:is (= 200 (sref m 'x))))
    (let ((descriptor (step-machine m)))                       ; subwid #300 -- wide, 2-byte operand
      (fiveam:is (eq wide descriptor))
      ;; 300 wraps to 8-bit X via the semantics' own WRAP-VALUE -- what
      ;; matters here is that VAL itself decoded as 300, not 44 (300 mod
      ;; 256), proving the wide alternative's own 2-byte width was read.
      (fiveam:is (= 44 (sref m 'x))))))

;; Multi-mode (mode.lisp, #18): LDA's IMMEDIATE and ZERO-PAGE variants share
;; one mnemonic but distinct opcodes/semantics -- proves opcode decode
;; (FIND-INSTRUCTION-BY-OPCODE) stays 1:1 per variant once a mnemonic
;; registers more than one INSTRUCTION-DESCRIPTOR.
(definstruction emu-test-machine lda
  (modes
    (immediate (opcode #xA1) (semantics (set! x operand)))
    (zero-page (opcode #xA6) (semantics (set! x (mref machine 'ram operand)))))
  (semantics (set! x operand)))

;; Multi-operand: a two-hole mode wiring two operand encoding fields --
;; proves STEP-MACHINE fetches each field at its own offset and advances PC
;; past their combined width.
(defmode emu-two-hole-test-mode expr "," expr :width 1)

(definstruction emu-test-machine movi
  (modes emu-two-hole-test-mode)
  (encoding (opcode #x01) (operand addr :width 1) (operand val :width 1))
  (semantics (setf (mref machine 'ram addr) val)))

;; SIGNED, non-RELATIVE (#30) -- a signed immediate that is not a branch
;; offset, proving sign-extension in STEP-MACHINE is keyed off SIGNEDP, not
;; RELATIVEP. TRAP's optional DATA (semantics.lisp) carries OPERAND out so
;; the test can inspect the reinterpreted value directly, since writing it
;; into a register would re-wrap it unsigned (STORAGE.LISP's WRAP-VALUE).
(defmode emu-signed-imm "#" expr :width 1 :signed t)

(definstruction emu-test-machine ldsi
  (modes emu-signed-imm)
  (encoding (opcode #x02) (operand :mode))
  (semantics (trap :ldsi operand)))

;; SIGNED with more than one hole -- RELATIVE modes are restricted to a
;; single hole (%CHECK-RELATIVE-MODE-HOLES, instruction.lisp), but a plain
;; SIGNED mode is not; this proves STEP-MACHINE sign-extends every hole by
;; its own width rather than assuming (and rebuilding from) a single value.
(defmode emu-two-hole-signed-test-mode expr "," expr :width 1 :signed t)

(definstruction emu-test-machine movsi
  (modes emu-two-hole-signed-test-mode)
  (encoding (opcode #x03) (operand a :width 1) (operand b :width 1))
  (semantics (trap :movsi (list a b))))

;;; load-program

(fiveam:test load-program-places-bytes-and-sets-pc
  (let ((m (make-machine 'emu-test-machine))
        (a (assemble "ldx #10" :machine 'emu-test-machine :origin #x100)))
    (load-program m a)
    (fiveam:is (= #xA2 (mref m 'ram #x100)))
    (fiveam:is (= 10 (mref m 'ram #x101)))
    (fiveam:is (= #x100 (sref m 'pc)))))

(fiveam:test load-program-accepts-raw-byte-sequence
  (let ((m (make-machine 'emu-test-machine)))
    (load-program m (list #xEA) :origin #x10)
    (fiveam:is (= #xEA (mref m 'ram #x10)))
    (fiveam:is (= #x10 (sref m 'pc)))))

;;; Cell-width-typed load/run (#53) -- WORDADDR-TEST-MACHINE
;;; (tests/instruction.lisp) declares :CELL-WIDTH 16 memory.

(fiveam:test load-program-places-cells-on-word-addressed-machine
  (let ((m (make-machine 'wordaddr-test-machine))
        (a (assemble "lda #$1234" :machine 'wordaddr-test-machine :origin 4)))
    (load-program m a)
    (fiveam:is (= 1 (mref m 'ram 4)))
    (fiveam:is (= #x1234 (mref m 'ram 5)))
    (fiveam:is (= 4 (sref m 'pc)))))

(fiveam:test load-program-signals-on-cell-width-mismatch
  ;; An assembly built against EMU-TEST-MACHINE's 8-bit memory loaded into a
  ;; 16-bit-cell one would otherwise silently place every assembled cell one
  ;; address too far apart, with no other symptom -- LOAD-PROGRAM must catch
  ;; the mismatch instead.
  (let ((m (make-machine 'wordaddr-test-machine))
        (a (assemble "ldx #10" :machine 'emu-test-machine)))
    (fiveam:signals error (load-program m a))))

(fiveam:test run-word-addressed-machine-round-trip-end-to-end
  (let ((m (make-machine 'wordaddr-test-machine))
        (a (assemble "lda #$2A
hlt" :machine 'wordaddr-test-machine)))
    (load-program m a)
    (multiple-value-bind (reason steps) (run m)
      (fiveam:is (eq :trap reason))
      (fiveam:is (= 2 steps))
      (fiveam:is (= #x2A (sref m 'a))))))

;;; step-machine

(fiveam:test step-machine-advances-pc-and-executes
  (let ((m (make-machine 'emu-test-machine))
        (a (assemble "ldx #10" :machine 'emu-test-machine)))
    (load-program m a)
    (let ((descriptor (step-machine m)))
      (fiveam:is (eq (find-instruction 'emu-test-machine 'ldx) descriptor))
      (fiveam:is (= 2 (sref m 'pc)))
      (fiveam:is (= 10 (sref m 'x))))))

(fiveam:test step-machine-branch-overrides-pc-increment
  (let ((m (make-machine 'emu-test-machine))
        ;; "target" (address 6) is well past bne's own post-fetch increment
        ;; (address 3) -- if step-machine's increment ran after semantics
        ;; instead of before, this would observe pc=3, not 6.
        (a (assemble "bne target
sta $2000
target: hlt" :machine 'emu-test-machine)))
    (load-program m a)
    ;; z starts at 0 (fresh machine), so bne's (zerop z) condition holds and
    ;; the branch to "target" is taken -- its semantics' (set! pc operand)
    ;; must win over step-machine's own post-fetch increment.
    (step-machine m)
    (fiveam:is (= 6 (sref m 'pc)))))

(fiveam:test step-machine-sub-opcode-runs-each-mode-own-semantics
  (let ((m (make-machine 'emu-test-machine))
        (a (assemble "subop #10
subop $2000" :machine 'emu-test-machine)))
    (load-program m a)
    (setf (mref m 'ram #x2000) 99)
    (let ((descriptor (step-machine m)))                       ; subop #10 -- IMMEDIATE
      (fiveam:is (eq (find-instruction 'emu-test-machine 'subop :mode 'immediate) descriptor))
      (fiveam:is (= 10 (sref m 'x))))
    (let ((descriptor (step-machine m)))                       ; subop $2000 -- ABSOLUTE
      (fiveam:is (eq (find-instruction 'emu-test-machine 'subop :mode 'absolute) descriptor))
      (fiveam:is (= 99 (sref m 'x))))))

(fiveam:test step-machine-sub-opcode-table-runs-each-combination-own-semantics
  ;; Target addresses (100/110/120) are chosen well clear of the program
  ;; itself, which occupies low addresses in this von Neumann RAM -- SUBOP
  ;; above avoids the same trap by using a far ABSOLUTE address ($2000).
  (let ((m (make-machine 'emu-test-machine))
        (a (assemble "subtab 100, 10
subtab 110, [120]" :machine 'emu-test-machine)))
    (load-program m a)
    (setf (mref m 'ram 120) 42)
    (step-machine m)                                           ; subtab 100, 10 -- reg,reg
    (fiveam:is (= 10 (mref m 'ram 100)))
    (step-machine m)                                           ; subtab 110, [120] -- reg,ind
    (fiveam:is (= 42 (mref m 'ram 110)))))

(fiveam:test step-machine-branch-not-taken-falls-through
  (let ((m (make-machine 'emu-test-machine))
        (a (assemble "sta $2000
bne skip
hlt
skip: hlt" :machine 'emu-test-machine)))
    (load-program m a)
    (setf (flag m 'z) t)  ; z set -> bne's branch condition (zerop z) is false
    (step-machine m)      ; sta, address 0 -> 3
    (step-machine m)      ; bne, not taken -> falls through to address 6
    (fiveam:is (= 6 (sref m 'pc)))))

;;; RELATIVE mode (#23) -- BRA, kept separate from the existing ABSOLUTE-mode
;;; BNE tests above.

(fiveam:test step-machine-relative-branch-taken-forward
  ;; bra target (address 0, 2 bytes) / sta $2000 (address 2, 3 bytes) /
  ;; target: hlt (address 5) -- BRA is narrower than BNE's ABSOLUTE encoding,
  ;; so target lands at 5, not 6 as in the BNE test above.
  (let ((m (make-machine 'emu-test-machine))
        (a (assemble "bra target
sta $2000
target: hlt" :machine 'emu-test-machine)))
    (load-program m a)
    (step-machine m)
    (fiveam:is (= 5 (sref m 'pc)))))

(fiveam:test step-machine-relative-branch-taken-backward-sign-extends
  ;; target: dex (address 0, 1 byte) / bra target (address 1, 2 bytes) --
  ;; next-pc after BRA is 3, target is 0, so the encoded offset is -3 (#xFD);
  ;; taking the branch must land back on pc=0, proving STEP-MACHINE
  ;; sign-extends the fetched operand rather than adding it as unsigned 253.
  ;; (No NOP on this fixture -- DEX stands in as the no-operand filler.)
  (let ((m (make-machine 'emu-test-machine))
        (a (assemble "target: dex
bra target" :machine 'emu-test-machine)))
    (load-program m a)
    (step-machine m)  ; dex, address 0 -> 1, x wraps to 255 so z clears
    (step-machine m)  ; bra, taken (z=0) -> back to 0
    (fiveam:is (= 0 (sref m 'pc)))))

(fiveam:test step-machine-relative-branch-not-taken-falls-through
  ;; sta $2000 (address 0, 3 bytes) / bra skip (address 3, 2 bytes) / hlt
  ;; (address 5) / skip: hlt (address 6) -- not taken falls through to the
  ;; plain HLT at 5, one byte earlier than the BNE test above since BRA
  ;; encodes narrower.
  (let ((m (make-machine 'emu-test-machine))
        (a (assemble "sta $2000
bra skip
hlt
skip: hlt" :machine 'emu-test-machine)))
    (load-program m a)
    (setf (flag m 'z) t)  ; z set -> bra's branch condition (zerop z) is false
    (step-machine m)      ; sta, address 0 -> 3
    (step-machine m)      ; bra, not taken -> falls through to address 5
    (fiveam:is (= 5 (sref m 'pc)))))

(fiveam:test counter-loop-end-to-end-with-relative-branch
  (let* ((m (make-machine 'emu-test-machine))
         (a (assemble "        ldx #10
loop:   dex
        bra loop
        sta $1000
        hlt" :machine 'emu-test-machine)))
    (load-program m a)
    (multiple-value-bind (reason steps) (run m)
      (fiveam:is (eq :trap reason))
      (fiveam:is (= 23 steps))
      (fiveam:is (= 0 (sref m 'x)))
      (fiveam:is (= 0 (mref m 'ram #x1000))))))

;;; Per-hole :RELATIVE on a ONE-OF alternative (#130) -- EMU-REL-ABS/
;;; EMU-REL-REL disagree on :RELATIVE; BRR's carrying hole (a hole-selected
;;; sub-opcode selector) is what makes the disagreement decodable and, at
;;; execution time, is what tells STEP-MACHINE's fetch/decode path
;;; (DECODE-INSTRUCTION-AT, sign-extension via OPERAND-SIGNEDNESS) that this
;;; particular decode's operand is signed -- unconditional jump/branch, kept
;;; simple to isolate the per-hole :RELATIVE mechanism from BRA's own
;;; zero-flag condition above.

(defmode emu-rel-abs expr :width 1)
(defmode emu-rel-rel "#" expr :width 1 :relative t)
(defmode emu-rel-one (one-of emu-rel-abs emu-rel-rel))

(definstruction emu-test-machine brr
  (modes emu-rel-one)
  (encoding (opcode #x91)
            (operand tgt :width 1
              (variant (choice emu-rel-abs) (sub 0))
              (variant (choice emu-rel-rel) (sub 1))))
  (semantics (choice-case tgt
               (emu-rel-abs (set! pc tgt))
               (emu-rel-rel (set! pc (+ pc tgt))))))

(fiveam:test step-machine-per-hole-relative-branch-sign-extends-backward
  ;; target: dex (address 0, 1 byte) / brr #target (address 1, 3 bytes) --
  ;; next-pc after BRR is 4, target is 0, so the encoded offset is -4
  ;; (#xFC); taking the branch must land back on pc=0, proving the per-hole
  ;; :RELATIVE alternative sign-extends the fetched operand exactly as a
  ;; whole-mode RELATIVE mode's single hole already does.
  (let ((m (make-machine 'emu-test-machine))
        (a (assemble "target: dex
brr #target" :machine 'emu-test-machine)))
    (load-program m a)
    (step-machine m)  ; dex, address 0 -> 1
    (step-machine m)  ; brr, unconditional -> back to 0
    (fiveam:is (= 0 (sref m 'pc)))))

(fiveam:test step-machine-per-hole-relative-absolute-alternative-jumps-plainly
  (let ((m (make-machine 'emu-test-machine))
        (a (assemble "brr 5
hlt" :machine 'emu-test-machine)))
    (load-program m a)
    (step-machine m)  ; brr, absolute alternative -> jumps to 5
    (fiveam:is (= 5 (sref m 'pc)))))

;;; SIGNED, non-RELATIVE (#30)

(fiveam:test step-machine-signed-non-relative-operand-sign-extends
  (let ((m (make-machine 'emu-test-machine))
        (a (assemble "ldsi #-1" :machine 'emu-test-machine)))
    (load-program m a)
    (handler-case (progn (step-machine m) (fiveam:fail "expected LASM-TRAP"))
      (lasm-trap (c) (fiveam:is (= -1 (lasm-trap-data c)))))))

(fiveam:test step-machine-signed-non-relative-operand-positive-value-unaffected
  (let ((m (make-machine 'emu-test-machine))
        (a (assemble "ldsi #10" :machine 'emu-test-machine)))
    (load-program m a)
    (handler-case (progn (step-machine m) (fiveam:fail "expected LASM-TRAP"))
      (lasm-trap (c) (fiveam:is (= 10 (lasm-trap-data c)))))))

(fiveam:test step-machine-multi-hole-signed-mode-sign-extends-each-hole
  (let ((m (make-machine 'emu-test-machine))
        (a (assemble "movsi -1, -2" :machine 'emu-test-machine)))
    (load-program m a)
    (handler-case (progn (step-machine m) (fiveam:fail "expected LASM-TRAP"))
      (lasm-trap (c) (fiveam:is (equal '(-1 -2) (lasm-trap-data c)))))))

(fiveam:test step-machine-decode-failure-on-unknown-opcode
  (let ((m (make-machine 'emu-test-machine)))
    (load-program m (list #xFF))
    (fiveam:is (eq :decode-failure (step-machine m)))))

;;; run

(fiveam:test run-stops-on-trap
  (let ((m (make-machine 'emu-test-machine))
        (a (assemble "ldx #5
hlt" :machine 'emu-test-machine)))
    (load-program m a)
    (multiple-value-bind (reason steps condition) (run m)
      (fiveam:is (eq :trap reason))
      (fiveam:is (= 2 steps))
      (fiveam:is (eq :halt (lasm-trap-tag condition)))
      (fiveam:is (= 5 (sref m 'x))))))

(fiveam:test run-stops-on-decode-failure
  (let ((m (make-machine 'emu-test-machine)))
    (load-program m (list #xFF))
    (multiple-value-bind (reason steps) (run m)
      (fiveam:is (eq :decode-failure reason))
      (fiveam:is (= 0 steps)))))

(fiveam:test run-stops-on-max-steps
  (let ((m (make-machine 'emu-test-machine))
        (a (assemble "loop: dex
bne loop" :machine 'emu-test-machine)))
    (load-program m a)
    (multiple-value-bind (reason steps) (run m :max-steps 4)
      (fiveam:is (eq :max-steps reason))
      (fiveam:is (= 4 steps)))))

;;; Multi-mode opcode decode (mode.lisp, #18)

(fiveam:test step-machine-decodes-each-mode-variant-independently
  (let ((m (make-machine 'emu-test-machine))
        (a (assemble "lda #5
lda $10" :machine 'emu-test-machine)))
    (setf (mref m 'ram #x10) 99)
    (load-program m a)
    (let ((first-descriptor (step-machine m)))
      (fiveam:is (eq (find-instruction 'emu-test-machine 'lda :mode 'immediate) first-descriptor))
      (fiveam:is (= 5 (sref m 'x))))
    (let ((second-descriptor (step-machine m)))
      (fiveam:is (eq (find-instruction 'emu-test-machine 'lda :mode 'zero-page) second-descriptor))
      (fiveam:is (= 99 (sref m 'x))))))

;;; Multi-operand instructions

(fiveam:test multi-operand-instruction-round-trip-through-run
  (let* ((m (make-machine 'emu-test-machine))
         (a (assemble "movi $20, $99
hlt" :machine 'emu-test-machine)))
    (load-program m a)
    (multiple-value-bind (reason steps) (run m)
      (fiveam:is (eq :trap reason))
      (fiveam:is (= 2 steps))
      (fiveam:is (= #x99 (mref m 'ram #x20))))))

;;; M1 milestone target: end-to-end counter loop, assembled and run

(fiveam:test counter-loop-end-to-end
  (let* ((m (make-machine 'emu-test-machine))
         (a (assemble "        ldx #10
loop:   dex
        bne loop
        sta $1000
        hlt" :machine 'emu-test-machine)))
    (load-program m a)
    (multiple-value-bind (reason steps) (run m)
      (fiveam:is (eq :trap reason))
      ;; 1 ldx + 10 dex + 10 bne + 1 sta + 1 hlt -- proves the loop actually
      ;; looped ten times rather than falling through.
      (fiveam:is (= 23 steps))
      (fiveam:is (= 0 (sref m 'x)))
      (fiveam:is (= 0 (mref m 'ram #x1000))))))

;;; M3 milestone target: a pure stack-based fantasy CPU, assembled and run
;;; end to end (#51). STACK-TEST-MACHINE declares no general-purpose
;;; registers at all -- only PC (still a plain register, by the %RESOLVE-PC
;;; convention), a data stack, and RAM -- mirroring examples/stack.lisp.

(defmachine stack-test-machine
  (register pc :width 16)
  (stack ds :width 8 :depth 32)
  (memory ram :width 8 :addr-width 16))

(definstruction stack-test-machine psh
  (modes immediate)
  (encoding (opcode #x01) (operand :mode))
  (semantics (push operand ds)))

(definstruction stack-test-machine ldm
  (modes absolute)
  (encoding (opcode #x02) (operand :mode))
  (semantics (push (mref machine 'ram operand) ds)))

(definstruction stack-test-machine sto
  (modes absolute)
  (encoding (opcode #x03) (operand :mode))
  (semantics (setf (mref machine 'ram operand) (pop ds))))

(definstruction stack-test-machine add
  (encoding (opcode #x04))
  (semantics (let ((b (pop ds)) (a (pop ds)))
               (push (wrap-value (+ a b) 8) ds))))

(definstruction stack-test-machine sub
  (encoding (opcode #x05))
  (semantics (let ((b (pop ds)) (a (pop ds)))
               (push (wrap-value (- a b) 8) ds))))

;; #57: STACK-TEST-MACHINE declares exactly one stack (DS), so its semantics
;; can also use PUSH/POP's sole-stack default -- ADX is ADD via the design
;; draft's own bare form, (push (+ (pop) (pop))), proving it compiles and
;; runs against a real instruction rather than only a standalone WITH-MACHINE
;; body.
(definstruction stack-test-machine adx
  (encoding (opcode #x08))
  (semantics (push (wrap-value (+ (pop) (pop)) 8))))

(definstruction stack-test-machine jz
  (modes relative)
  (encoding (opcode #x06) (operand :mode))
  (semantics (let ((v (pop ds)))
               (when (zerop v) (set! pc (+ pc operand))))))

(definstruction stack-test-machine jmp
  (modes relative)
  (encoding (opcode #x07) (operand :mode))
  (semantics (set! pc (+ pc operand))))

(definstruction stack-test-machine hlt
  (encoding (opcode #x00))
  (semantics (trap :halt)))

;; A machine with a one-deep data stack, just for exercising overflow/
;; underflow -- STACK-TEST-MACHINE's own DS is deliberately roomy so its
;; other tests never trip either condition by accident.
(defmachine shallow-stack-test-machine
  (register pc :width 16)
  (stack ds :width 8 :depth 1)
  (memory ram :width 8 :addr-width 16))

(definstruction shallow-stack-test-machine psh
  (modes immediate)
  (encoding (opcode #x01) (operand :mode))
  (semantics (push operand ds)))

(definstruction shallow-stack-test-machine add
  (encoding (opcode #x02))
  (semantics (let ((b (pop ds)) (a (pop ds)))
               (push (wrap-value (+ a b) 8) ds))))

(definstruction shallow-stack-test-machine hlt
  (encoding (opcode #x00))
  (semantics (trap :halt)))

(fiveam:test stack-machine-arithmetic
  ;; LIFO order matters for non-commutative ops: (ldm a)(psh b)(sub) must
  ;; compute a - b, not b - a -- SUB's semantics pop B (top, pushed last)
  ;; before A.
  (let* ((m (make-machine 'stack-test-machine))
         (a (assemble "psh #10
psh #3
sub
sto $2000
psh #2
psh #5
add
sto $2001
hlt" :machine 'stack-test-machine)))
    (load-program m a)
    (multiple-value-bind (reason steps) (run m)
      (fiveam:is (eq :trap reason))
      (fiveam:is (= 9 steps))
      (fiveam:is (= 7 (mref m 'ram #x2000)))
      (fiveam:is (= 7 (mref m 'ram #x2001)))
      (fiveam:is (= 0 (stack-depth m 'ds))))))

(fiveam:test stack-machine-end-to-end
  ;; The M3 milestone's actual validation case: a counted loop (5+4+3+2+1)
  ;; built entirely on PC + one stack + RAM, no general-purpose registers,
  ;; no flags. See examples/stack.lisp for the annotated version and the
  ;; write-up of what this does (and doesn't) require of the storage model.
  (let* ((m (make-machine 'stack-test-machine))
         (a (assemble "        psh #5
        sto $0000
        psh #0
        sto $1000
loop:   ldm $0000
        jz end
        ldm $0000
        ldm $1000
        add
        sto $1000
        ldm $0000
        psh #1
        sub
        sto $0000
        jmp loop
end:    hlt" :machine 'stack-test-machine)))
    (load-program m a)
    (multiple-value-bind (reason steps) (run m)
      (fiveam:is (eq :trap reason))
      ;; Proves the loop actually iterated 5 times rather than falling
      ;; through: 4 setup + 5*(11 loop-body steps) + 1 final jz + 1 hlt.
      (fiveam:is (= 62 steps))
      (fiveam:is (= 15 (mref m 'ram #x1000)))
      (fiveam:is (= 0 (stack-depth m 'ds))))))

(fiveam:test stack-machine-implicit-stack-instruction
  ;; #57: ADX's semantics use PUSH/POP with no stack name at all, resolving
  ;; to STACK-TEST-MACHINE's sole stack DS -- commutative here, so operand
  ;; order doesn't matter for the result (unlike SUB above).
  (let* ((m (make-machine 'stack-test-machine))
         (a (assemble "psh #2
psh #5
adx
sto $2000
hlt" :machine 'stack-test-machine)))
    (load-program m a)
    (multiple-value-bind (reason steps) (run m)
      (fiveam:is (eq :trap reason))
      (fiveam:is (= 5 steps))
      (fiveam:is (= 7 (mref m 'ram #x2000)))
      (fiveam:is (= 0 (stack-depth m 'ds))))))

;; #51 finding: stack over/underflow are STORAGE-ERROR conditions
;; (storage.lisp), not caught by RUN/STEP-MACHINE (which only handle
;; LASM-TRAP and UNKNOWN-INSTRUCTION respectively) -- so they propagate out
;; of RUN as a raw Lisp error rather than becoming a stop reason like :TRAP
;; or :DECODE-FAILURE. These two tests pin down that current behaviour;
;; see the follow-up ticket asking whether RUN should instead catch
;; STORAGE-ERROR and return a new stop reason.

(fiveam:test stack-underflow-escapes-run
  (let* ((m (make-machine 'stack-test-machine))
         (a (assemble "add
hlt" :machine 'stack-test-machine)))
    (load-program m a)
    (fiveam:signals stack-underflow (run m))))

(fiveam:test stack-overflow-escapes-run
  (let* ((m (make-machine 'shallow-stack-test-machine))
         (a (assemble "psh #1
psh #2
hlt" :machine 'shallow-stack-test-machine)))
    (load-program m a)
    (fiveam:signals stack-overflow (run m))))

;;; M3 milestone target: a hybrid machine -- accumulator + index registers +
;;; an implicit call stack (#52), mirroring examples/hybrid.lisp. JSR/RTS are
;;; built entirely from PUSH/POP of PC onto S, no dedicated call-stack
;;; primitive, and DOUBLE reaches its argument with STACK-RELATIVE addressing
;;; (#50) since JSR's own return address sits on top of it on the same S.

(defmachine hybrid-test-machine
  (register a :width 8)
  (register x :width 8)
  (register y :width 8)
  (register pc :width 16)
  (stack s :width 16 :depth 64)
  (memory ram :width 8 :addr-width 16)
  (flags z))

(definstruction hybrid-test-machine lda
  (modes
    (immediate      (opcode #xA9) (semantics (set! a operand)))
    (stack-relative (opcode #xA3) (semantics (set! a (stack-ref machine 's operand))))
    (absolute       (opcode #xAD)))
  (semantics (set! a (mref machine 'ram operand))))

(definstruction hybrid-test-machine sta
  (modes
    (stack-relative (opcode #x83) (semantics (setf (stack-ref machine 's operand) a)))
    (absolute       (opcode #x8D)))
  (semantics (setf (mref machine 'ram operand) a)))

(definstruction hybrid-test-machine sty
  (modes absolute)
  (encoding (opcode #x8C) (operand :mode))
  (semantics (setf (mref machine 'ram operand) y)))

(definstruction hybrid-test-machine pha
  (encoding (opcode #x48))
  (semantics (push a s)))

(definstruction hybrid-test-machine pla
  (encoding (opcode #x68))
  (semantics (set! a (pop s))))

(definstruction hybrid-test-machine asl
  (encoding (opcode #x0A))
  (semantics (set! a (wrap-value (* a 2) 8))))

(definstruction hybrid-test-machine ldx
  (modes immediate)
  (encoding (opcode #xA2) (operand :mode))
  (semantics (set! x operand) (set-flags! (z (zero? x)))))

(definstruction hybrid-test-machine dex
  (encoding (opcode #xCA))
  (semantics (set! x (wrap-value (1- x) 8)) (set-flags! (z (zero? x)))))

(definstruction hybrid-test-machine iny
  (encoding (opcode #xC8))
  (semantics (set! y (wrap-value (1+ y) 8))))

(definstruction hybrid-test-machine bne
  (modes relative)
  (encoding (opcode #xD0) (operand :mode))
  (semantics (when (zerop z) (set! pc (+ pc operand)))))

(definstruction hybrid-test-machine jsr
  (modes absolute)
  (encoding (opcode #x20) (operand :mode))
  (semantics (push pc s) (set! pc operand)))

(definstruction hybrid-test-machine rts
  (encoding (opcode #x60))
  (semantics (set! pc (pop s))))

(definstruction hybrid-test-machine hlt
  (encoding (opcode #x00))
  (semantics (trap :halt)))

(fiveam:test jsr-rts-round-trips-through-stack
  ;; JSR pushes the address of the instruction after itself; RTS pops it
  ;; back into PC, so control returns to that exact address rather than to
  ;; DOUBLE's own body or one byte off in either direction.
  (let* ((m (make-machine 'hybrid-test-machine))
         (a (assemble "        lda #5
        jsr double
        sta $1000
        hlt
double: rts" :machine 'hybrid-test-machine)))
    (load-program m a)
    (multiple-value-bind (reason steps) (run m)
      (fiveam:is (eq :trap reason))
      (fiveam:is (= 5 steps)) ; lda, jsr, rts, sta, hlt
      (fiveam:is (= 5 (mref m 'ram #x1000)))
      (fiveam:is (= 0 (stack-depth m 's))))))

(fiveam:test stack-relative-reads-past-return-address
  ;; DOUBLE's argument is pushed before JSR's own return address, so it
  ;; sits at 1,S -- not 0,S, which is the return address itself.
  (let* ((m (make-machine 'hybrid-test-machine))
         (a (assemble "        lda #10
        pha
        jsr double
        pla
        sta $1000
        hlt
double: lda 1,S
        asl
        sta 1,S
        rts" :machine 'hybrid-test-machine)))
    (load-program m a)
    (multiple-value-bind (reason steps) (run m)
      (declare (ignore steps))
      (fiveam:is (eq :trap reason))
      (fiveam:is (= 20 (mref m 'ram #x1000)))
      (fiveam:is (= 0 (stack-depth m 's))))))

(fiveam:test hybrid-machine-end-to-end
  ;; The M3 milestone's second validation case: DOUBLE called three times
  ;; through the same JSR/RTS + STACK-RELATIVE machinery as
  ;; examples/hybrid.lisp, doubling ram[$1000] each time (1 -> 2 -> 4 -> 8)
  ;; and counting the calls into ram[$1001] via Y.
  (let* ((m (make-machine 'hybrid-test-machine))
         (a (assemble "        ldx #3
        lda #1
        sta $1000
loop:   lda $1000
        pha
        jsr double
        pla
        sta $1000
        dex
        bne loop
        sty $1001
        hlt

double: iny
        lda 1,S
        asl
        sta 1,S
        rts" :machine 'hybrid-test-machine)))
    (load-program m a)
    (multiple-value-bind (reason steps) (run m)
      (fiveam:is (eq :trap reason))
      ;; Pinned down by running examples/hybrid.lisp, not hand-counted.
      (fiveam:is (= 41 steps))
      (fiveam:is (= 8 (mref m 'ram #x1000)))
      (fiveam:is (= 3 (mref m 'ram #x1001)))
      (fiveam:is (= 0 (stack-depth m 's))))))

;;; Word-encoded emulation (#20) -- reuses WORD-TEST-MACHINE/SET/HLT
;;; (tests/instruction.lisp). The acceptance test for the whole ticket:
;;; assemble -> LOAD-PROGRAM -> STEP-MACHINE must recover the same operand
;;; value regardless of which variant (inline or extra-word) the assembler
;;; picked, and PC must advance by the actual number of words consumed.

(fiveam:test step-machine-word-encoded-inline-round-trip
  ;; STEP-MACHINE's returned descriptor need not be EQ to whichever variant
  ;; FIND-INSTRUCTION returns -- REGISTER-INSTRUCTION-VARIANTS! (instruction
  ;; .lisp) lets any sibling combo occupy the shared opcode slot, since every
  ;; sibling's WORD-ALTERNATIVES decodes identically -- so this checks name
  ;; and the actually-executed effect, not descriptor identity.
  (let ((m (make-machine 'word-test-machine))
        (a (assemble "set #5
hlt" :machine 'word-test-machine)))
    (load-program m a)
    (fiveam:is (string= "SET" (instruction-descriptor-name (step-machine m))))
    (fiveam:is (= 5 (sref m 'a)))
    (fiveam:is (= 2 (sref m 'pc)))))    ; one word consumed

(fiveam:test step-machine-word-encoded-extra-word-round-trip
  (let ((m (make-machine 'word-test-machine))
        (a (assemble "set #1000
hlt" :machine 'word-test-machine)))
    (load-program m a)
    (fiveam:is (string= "SET" (instruction-descriptor-name (step-machine m))))
    (fiveam:is (= 1000 (sref m 'a)))
    (fiveam:is (= 4 (sref m 'pc)))))    ; instruction word + one extra word

(fiveam:test step-machine-word-encoded-negative-inline-round-trip
  ;; Exercises the :BIAS mechanism's actual reason for existing: -1 packs
  ;; into an unsigned field via bias +1, decoded back to -1, not treated as
  ;; a sign-extended two's-complement quantity the way a byte-encoded SIGNED
  ;; mode would (#20 restriction -- word-encoded fields have no SIGNED
  ;; sign-extension of their own; a negative inline value's sign is carried
  ;; entirely by its variant's bias).
  (let ((m (make-machine 'word-test-machine))
        (a (assemble "set #-1
hlt" :machine 'word-test-machine)))
    (load-program m a)
    (step-machine m)
    ;; A's own :SET! stores the decoded value through WRAP-VALUE like any
    ;; other register write, so -1 reads back as 16-bit 65535, not -1 --
    ;; SIGNED-VALUE recovers the two's-complement reading a semantics body
    ;; wanting a real negative would apply itself.
    (fiveam:is (= 65535 (sref m 'a)))
    (fiveam:is (= -1 (signed-value (sref m 'a) 16)))))

(fiveam:test run-word-encoded-machine-round-trip-end-to-end
  (let ((m (make-machine 'word-test-machine))
        (a (assemble "set #5
set #1000
hlt" :machine 'word-test-machine)))
    (load-program m a)
    (multiple-value-bind (reason steps) (run m)
      (fiveam:is (eq :trap reason))
      (fiveam:is (= 3 steps))
      (fiveam:is (= 1000 (sref m 'a))))))

(fiveam:test step-machine-choice-selected-inline-round-trip
  ;; #104: WCX's field is CHOICE-selected, not value-selected -- WC-REG
  ;; syntax packs inline biased #x00.
  (let ((m (make-machine 'word-test-machine))
        (a (assemble "wcx 5
hlt" :machine 'word-test-machine)))
    (load-program m a)
    (fiveam:is (string= "WCX" (instruction-descriptor-name (step-machine m))))
    (fiveam:is (= 5 (sref m 'a)))
    (fiveam:is (= 2 (sref m 'pc)))))     ; one word consumed, same as SET's inline case

(fiveam:test step-machine-choice-selected-unconditional-extra-word-round-trip
  ;; #104: WCXW's WC-IND alternative always spills to a trailing word once
  ;; matched, regardless of the value -- unlike SET's :ELSE, which only
  ;; escapes when the value itself doesn't fit inline. 5 would fit an inline
  ;; field easily, but the CHOICE (syntax), not the value, decides here.
  (let ((m (make-machine 'word-test-machine))
        (a (assemble "wcxw [5]
hlt" :machine 'word-test-machine)))
    (load-program m a)
    (fiveam:is (string= "WCXW" (instruction-descriptor-name (step-machine m))))
    (fiveam:is (= 5 (sref m 'b)))
    (fiveam:is (= 4 (sref m 'pc)))))     ; instruction word + one extra word

(fiveam:test step-machine-choice-case-dispatches-differently-per-alternative
  ;; #73: WCC (tests/instruction.lisp) writes to A for WC-REG syntax and B
  ;; for WC-IND, despite both packing the identical value 5 into disjoint
  ;; halves of one field -- STEP-MACHINE threads DECODE-INSTRUCTION-AT's
  ;; CHOICES through to EXECUTE-INSTRUCTION, so this is the real per-hole
  ;; alternative recovered from the fetched instruction word, not a value
  ;; handed in directly by a test.
  (let ((m (make-machine 'word-test-machine))
        (a (assemble "wcc 5
wcc [5]
hlt" :machine 'word-test-machine)))
    (load-program m a)
    (fiveam:is (string= "WCC" (instruction-descriptor-name (step-machine m))))
    (fiveam:is (= 5 (sref m 'a)))
    (fiveam:is (= 0 (sref m 'b)))
    (fiveam:is (string= "WCC" (instruction-descriptor-name (step-machine m))))
    (fiveam:is (= 5 (sref m 'b)))))

(fiveam:test step-machine-choice-case-dispatches-differently-per-alternative-on-a-mixed-field
  ;; #118: WCM (tests/instruction.lisp, MIXED-FIELD-TEST-MACHINE) mixes a
  ;; CHOICE-selected WC-REG variant with a value-selected WC-IND one on one
  ;; field -- CHOICE-CASE dispatches on the stamped WC-IND name for the
  ;; value-selected row exactly as it would for a CHOICE-selected one, so
  ;; "wcm [50]" reaches B, not A, despite WC-IND never declaring its own
  ;; (choice ...) variant.
  (let ((m (make-machine 'mixed-field-test-machine))
        (a (assemble "wcm 5
wcm [50]
hlt" :machine 'mixed-field-test-machine)))
    (load-program m a)
    (fiveam:is (string= "WCM" (instruction-descriptor-name (step-machine m))))
    (fiveam:is (= 5 (sref m 'a)))
    (fiveam:is (= 0 (sref m 'b)))
    (fiveam:is (string= "WCM" (instruction-descriptor-name (step-machine m))))
    (fiveam:is (= 50 (sref m 'b)))))

(fiveam:test step-machine-word-encoded-decode-failure-on-unregistered-opcode
  (let ((m (make-machine 'word-test-machine)))
    (setf (mref m 'ram 0) 0)
    (setf (mref m 'ram 1) #xf0)         ; opcode #xf, unregistered
    (fiveam:is (eq :decode-failure (step-machine m)))
    (fiveam:is (= 0 (sref m 'pc)))))    ; PC not advanced on decode failure

;;; Cycle-cost model, clock speed, cycle-accurate execution (#75)

;; CLOCK-SPEED declared; NOP has no (cycles n) (defaults to 1); SLOW has a
;; fixed (cycles 5); LDA's two modes give each its own per-mode cost,
;; overriding the top-level (cycles 1) default -- IMMEDIATE cheaper than
;; ZERO-PAGE, the shape a real ISA's addressing-mode cost table takes.
(defmachine cycle-test-machine
  (register x :width 8)
  (register pc :width 16)
  (memory ram :width 8 :addr-width 16)
  (flags z)
  (clock-speed 1000000)) ; 1 MHz -- 1 cycle = 1 microsecond

(definstruction cycle-test-machine nop
  (encoding (opcode #x00))
  (semantics nil))

(definstruction cycle-test-machine slow
  (encoding (opcode #x01))
  (semantics nil)
  (cycles 5))

(definstruction cycle-test-machine hlt
  (encoding (opcode #x02))
  (semantics (trap :halt)))

(definstruction cycle-test-machine lda
  (modes
    (immediate (opcode #x10) (semantics (set! x operand)) (cycles 2))
    (zero-page (opcode #x11) (semantics (set! x (mref machine 'ram operand))) (cycles 4)))
  (semantics (set! x operand))
  (cycles 1))

;; No CLOCK-SPEED declared -- for RUN-FOR-DURATION's missing-clause error.
(defmachine no-clock-test-machine
  (register pc :width 16)
  (memory ram :width 8 :addr-width 16))

(definstruction no-clock-test-machine hlt
  (encoding (opcode #x00))
  (semantics (trap :halt)))

;; A deliberately slow clock (1 kHz -- 1 cycle = 1 millisecond), its own
;; fixture rather than mutating CYCLE-TEST-MACHINE's declared clock speed,
;; for RUN-FOR-DURATION-THROTTLE-PACES-REAL-TIME below -- pacing a handful
;; of milliseconds of simulated time takes real wall-clock time to observe
;; without making the test suite slow.
(defmachine slow-clock-test-machine
  (register pc :width 16)
  (memory ram :width 8 :addr-width 16)
  (clock-speed 1000))

(definstruction slow-clock-test-machine nop
  (encoding (opcode #x00))
  (semantics nil))

(fiveam:test step-machine-default-cycle-cost-is-one
  (let ((m (make-machine 'cycle-test-machine)))
    (load-program m (list #x00)) ; nop, no (cycles n)
    (multiple-value-bind (descriptor cost) (step-machine m)
      (fiveam:is (eq (find-instruction 'cycle-test-machine 'nop) descriptor))
      (fiveam:is (= 1 cost))
      (fiveam:is (= 1 (machine-cycles m))))))

(fiveam:test step-machine-honours-declared-cycles
  (let ((m (make-machine 'cycle-test-machine)))
    (load-program m (list #x01)) ; slow, (cycles 5)
    (multiple-value-bind (descriptor cost) (step-machine m)
      (fiveam:is (eq (find-instruction 'cycle-test-machine 'slow) descriptor))
      (fiveam:is (= 5 cost))
      (fiveam:is (= 5 (machine-cycles m))))))

(fiveam:test step-machine-per-mode-cycles-override-shared-default
  (let ((m (make-machine 'cycle-test-machine))
        (a (assemble "lda #5
lda $10" :machine 'cycle-test-machine)))
    (load-program m a)
    (multiple-value-bind (descriptor cost) (step-machine m)
      (fiveam:is (eq (find-instruction 'cycle-test-machine 'lda :mode 'immediate) descriptor))
      (fiveam:is (= 2 cost)))
    (multiple-value-bind (descriptor cost) (step-machine m)
      (fiveam:is (eq (find-instruction 'cycle-test-machine 'lda :mode 'zero-page) descriptor))
      (fiveam:is (= 4 cost)))
    (fiveam:is (= 6 (machine-cycles m)))))

(fiveam:test machine-cycles-accumulates-across-run-and-reset-zeroes-it
  (let ((m (make-machine 'cycle-test-machine))
        (a (assemble "slow
slow
hlt" :machine 'cycle-test-machine)))
    (load-program m a)
    (run m)
    (fiveam:is (= 11 (machine-cycles m))) ; 5 + 5 + 1 (hlt's own default cost)
    (reset m)
    (fiveam:is (= 0 (machine-cycles m)))))

(fiveam:test run-for-cycles-stops-exactly-on-budget-when-a-step-lands-on-it
  (let ((m (make-machine 'cycle-test-machine))
        (a (assemble "slow
nop
nop" :machine 'cycle-test-machine)))
    (load-program m a)
    ;; slow (5) then nop (1) lands exactly on 6 -- the budget check trips
    ;; with no overshoot on this schedule.
    (multiple-value-bind (reason steps) (run-for-cycles m 6)
      (fiveam:is (eq :max-cycles reason))
      (fiveam:is (= 2 steps))
      (fiveam:is (= 6 (machine-cycles m))))))

(fiveam:test run-for-cycles-overshoots-by-at-most-one-instructions-cost
  (let ((m (make-machine 'cycle-test-machine))
        (a (assemble "slow
nop" :machine 'cycle-test-machine)))
    (load-program m a)
    ;; Budget of 3 lands mid-instruction: SLOW alone (its own cost of 5) is
    ;; already the first step, so the budget is checked only after SLOW has
    ;; executed -- the actual overshoot this pins down is 5 - 3 = 2, bounded
    ;; by SLOW's own cost, never unbounded.
    (multiple-value-bind (reason steps) (run-for-cycles m 3)
      (fiveam:is (eq :max-cycles reason))
      (fiveam:is (= 1 steps))
      (fiveam:is (= 5 (machine-cycles m))))))

(fiveam:test run-for-cycles-trap-counts-its-cycles-decode-failure-counts-none
  (let ((m (make-machine 'cycle-test-machine))
        (a (assemble "hlt" :machine 'cycle-test-machine)))
    (load-program m a)
    (multiple-value-bind (reason steps) (run-for-cycles m 100)
      (fiveam:is (eq :trap reason))
      (fiveam:is (= 1 steps))
      (fiveam:is (= 1 (machine-cycles m))))) ; hlt's default cost of 1 still counted
  (let ((m (make-machine 'cycle-test-machine)))
    (load-program m (list #xFF)) ; unregistered opcode
    (multiple-value-bind (reason steps) (run-for-cycles m 100)
      (fiveam:is (eq :decode-failure reason))
      (fiveam:is (= 0 steps))
      (fiveam:is (= 0 (machine-cycles m))))))

(fiveam:test machine-elapsed-seconds-is-pure-arithmetic-no-clock-needed
  (let ((m (make-machine 'cycle-test-machine))
        (a (assemble "slow
hlt" :machine 'cycle-test-machine)))
    (load-program m a)
    (run m)
    ;; 6 cycles at 1 MHz (1 cycle = 1 microsecond) = 6e-6 seconds.
    (fiveam:is (= 6 (machine-cycles m)))
    (fiveam:is (= 6.0d-6 (machine-elapsed-seconds m)))))

(fiveam:test machine-elapsed-seconds-signals-without-clock-speed
  (let ((m (make-machine 'no-clock-test-machine)))
    (fiveam:signals error (machine-elapsed-seconds m))))

(fiveam:test run-for-duration-signals-without-clock-speed
  (let ((m (make-machine 'no-clock-test-machine))
        (a (assemble "hlt" :machine 'no-clock-test-machine)))
    (load-program m a)
    (fiveam:signals error (run-for-duration m 1.0d0))))

(fiveam:test run-for-duration-stops-on-duration-budget
  (let ((m (make-machine 'cycle-test-machine))
        (a (assemble "loop: slow
nop
nop" :machine 'cycle-test-machine)))
    (load-program m a)
    ;; 6 microseconds of simulated time at 1 MHz = 6 cycles' worth.
    (multiple-value-bind (reason steps) (run-for-duration m 6.0d-6)
      (fiveam:is (eq :duration reason))
      (fiveam:is (> steps 0))
      (fiveam:is (>= (machine-cycles m) 6)))))

(fiveam:test defmachine-rejects-non-positive-clock-speed
  (fiveam:signals error (eval '(defmachine bad-clock-test-machine
                                 (register pc :width 16)
                                 (memory ram :width 8 :addr-width 16)
                                 (clock-speed 0))))
  (fiveam:signals error (eval '(defmachine bad-clock-test-machine
                                 (register pc :width 16)
                                 (memory ram :width 8 :addr-width 16)
                                 (clock-speed -1)))))

;; Tolerance-based (elapsed >= expected * 0.8), not exact-match, since real
;; wall-clock timing is inherently noisy -- and kept to a tiny duration so
;; the suite doesn't hang. Proves :THROTTLE T actually paces (sleeps), not
;; just that it computes the same stop condition as the untimed default.
(fiveam:test run-for-duration-throttle-paces-real-time
  (let ((m (make-machine 'slow-clock-test-machine))
        ;; 1 kHz machine, run-for-duration for 0.05s (50ms) of simulated
        ;; time -- one NOP (1 cycle) per simulated millisecond, so this
        ;; takes real wall-clock time to pace through with :THROTTLE T.
        (a (loop repeat 200 collect #x00))) ; 200 nops
    (load-program m a)
    (let ((start (trivial-high-precision-timer:make-precision-timer)))
      (multiple-value-bind (reason steps) (run-for-duration m 0.05d0 :throttle t)
        (declare (ignore reason steps))
        (let ((elapsed (trivial-high-precision-timer:sec
                        start (trivial-high-precision-timer:now start))))
          (fiveam:is (>= elapsed (* 0.05d0 0.8d0))))))))
