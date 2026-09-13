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
