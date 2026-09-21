;;;; tests/disassembler.lisp
;;;; fiveam tests for decoder.lisp's DECODE-INSTRUCTION-AT and
;;;; disassembler.lisp's DISASSEMBLE-CELLS/-ASSEMBLY/-MEMORY (#21, M7).

(in-package #:lasm)

(fiveam:def-suite disassembler :in lasm)
(fiveam:in-suite disassembler)

;;; Byte-encoded fixture -- covers no-operand, immediate, a suffix-bearing
;;; multi-mode pair (zero-page/absolute), the three built-in modes with
;;; punctuation literals (indexed-x/indirect-y/stack-relative), a two-hole
;;; mode, a signed operand, and a relative branch.

(defmachine disasm-test-machine
  (register pc :width 16)
  (register x :width 8)
  (stack s :width 8 :depth 4)
  (memory ram :width 8 :addr-width 16)
  (flags z))

(defmode disasm-two-hole expr "," expr :width 1)
(defmode disasm-signed-imm "#" expr :width 1 :signed t)

(definstruction disasm-test-machine hlt
  (encoding (opcode #x00))
  (semantics (trap :halt)))

(definstruction disasm-test-machine ldx
  (modes immediate)
  (encoding (opcode #xA2) (operand :mode))
  (semantics (set! x operand)))

;; ZERO-PAGE/ABSOLUTE (#40): the only pair of built-in modes sharing operand
;; syntax, each carrying its own forced-mode suffix ("z"/"w") -- proves
;; %RENDER-MNEMONIC only emits a suffix when a mnemonic actually has more
;; than one registered variant to disambiguate.
(definstruction disasm-test-machine lda
  (modes
    (zero-page (opcode #xA5) (semantics (set! x (mref machine 'ram operand))))
    (absolute (opcode #xAD) (semantics (set! x (mref machine 'ram operand))))))

(definstruction disasm-test-machine ldix
  (modes indexed-x)
  (encoding (opcode #xB5) (operand :mode))
  (semantics (set! x operand)))

(definstruction disasm-test-machine ldiy
  (modes indirect-y)
  (encoding (opcode #xB1) (operand :mode))
  (semantics (set! x operand)))

(definstruction disasm-test-machine ldsr
  (modes stack-relative)
  (encoding (opcode #xA3) (operand :mode))
  (semantics (set! x operand)))

;; Multi-operand (#18-shaped, mirrors tests/emulator.lisp's MOVI): a two-hole
;; mode wiring two named encoding fields -- proves values are paired by hole
;; order, not by name.
(definstruction disasm-test-machine movi
  (modes disasm-two-hole)
  (encoding (opcode #x01) (operand addr :width 1) (operand val :width 1))
  (semantics (setf (mref machine 'ram addr) val)))

;; SIGNED, non-RELATIVE (#30) -- proves sign-extension and negative rendering
;; are keyed off SIGNEDP, not RELATIVEP.
(definstruction disasm-test-machine ldsi
  (modes disasm-signed-imm)
  (encoding (opcode #x02) (operand :mode))
  (semantics (trap :ldsi operand)))

(definstruction disasm-test-machine bra
  (modes relative)
  (encoding (opcode #x90) (operand :mode))
  (semantics (when (zerop z) (set! pc (+ pc operand)))))

;; ONE-OF (#103): a decoded word carries no record of which alternative was
;; assembled -- see %RENDER-OPERAND-TEXT's own docstring -- so disassembly
;; always renders the first alternative, DISASM-OO-REG here, regardless of
;; which one was actually written.
(defmode disasm-oo-reg expr)
(defmode disasm-oo-ind "[" expr "]")
(defmode disasm-oo (one-of disasm-oo-reg disasm-oo-ind))

(definstruction disasm-test-machine moo
  (modes disasm-oo)
  (encoding (opcode #x04) (operand :mode))
  (semantics (set! x operand)))

;; Sub-opcode cell (#125): IMMEDIATE and ABSOLUTE share opcode #x03, told
;; apart by their own :SUB value rather than by opcode -- the disassembler
;; and listing (both routing through DECODE-INSTRUCTION-AT and its
;; accumulated SIZE, per #125's design) need no code of their own to render
;; this correctly; ROUND-TRIP-SUB-OPCODE-DECODES-EACH-MODE-BACK-TO-ITSELF
;; below is what actually checks that claim rather than trusting it.
(definstruction disasm-test-machine subop
  (modes
    (immediate (opcode #x03 :sub 0) (operand :mode) (semantics (set! x operand)))
    (absolute (opcode #x03 :sub 1) (operand :mode) (semantics (set! x (mref machine 'ram operand))))))

;; Per-hole :RELATIVE (#130), alongside a plain sibling hole -- the one
;; surface #124/#127/#129's own per-hole work never touched, since all
;; three were pure encode/decode-VALUE changes; none of them changed how a
;; value is *rendered* at disassembly time. BRM's TGT hole (DISASM-REL-ABS/
;; DISASM-REL-REL, a hole-selected sub-opcode selector) is either an
;; absolute value or a PC-relative offset; its sibling N hole is always
;; plain -- %OPERAND-RENDER-VALUES must adjust only TGT's own value.
(defmode disasm-rel-abs expr :width 1)
(defmode disasm-rel-rel "#" expr :width 1 :relative t)
(defmode disasm-rel-two (one-of disasm-rel-abs disasm-rel-rel) "," expr :width 1)

(definstruction disasm-test-machine brm
  (modes disasm-rel-two)
  (encoding (opcode #x05)
            (operand tgt :mode
              (variant (choice disasm-rel-abs) (sub 0))
              (variant (choice disasm-rel-rel) (sub 1)))
            (operand n :width 1))
  (semantics (choice-case tgt
               (disasm-rel-abs (set! x tgt))
               (disasm-rel-rel (set! pc (+ pc tgt))))))

;;; Word-encoded fixture -- DCPU-16-shaped (examples/dcpu16.lisp): a 6-bit
;;; field A, a 5-bit field B, a 5-bit OPCODE field, MSB-first. SET's operand
;;; order (dst = field B, shift 5; src = field A, shift 10) is declared
;;; opposite its shift order -- the shape that catches a hole-order/
;;; shift-order mixup. MOVX gives *both* fields an inline-or-extra-word
;;; variant, so a single instruction can spend two extra words at once,
;;; proving they decode back in declaration order.

(defmachine disasm-word-machine
  (register pc :width 16)
  (register reg :width 16 :count 8)
  (memory ram :width 16 :addr-width 16 :cell-width 16)
  (instruction-word :width 16
    (field a 6)
    (field b 5)
    (field opcode 5)))

(defmode disasm-rr expr "," expr)

(definstruction disasm-word-machine set
  (modes disasm-rr)
  (encoding
    (opcode 1)
    (operand dst :field b)
    (operand src :field a
      (variant (range -1 30) inline :bias 33)
      (variant :else (extra-word :escape #x1f))))
  (semantics (set! (reg dst) src)))

(definstruction disasm-word-machine hlt
  (encoding (opcode 2))
  (semantics (trap :halt)))

(definstruction disasm-word-machine movx
  (modes disasm-rr)
  (encoding
    (opcode 3)
    (operand dst :field b
      (variant (range -1 14) inline :bias 15)
      (variant :else (extra-word :escape #x1f)))
    (operand src :field a
      (variant (range -1 30) inline :bias 33)
      (variant :else (extra-word :escape #x1f))))
  (semantics (set! (reg dst) src)))

;; CHOICE-selected field (#104/#117): unlike SET/MOVX above, COO's field A
;; encoding depends on which of DISASM-OO's ONE-OF alternatives (declared
;; alongside MOO above, on the byte-encoded machine -- modes are a global
;; registry, not per-machine, so reusing it here exercises the same ONE-OF
;; against a word-encoded field) the operand actually matched, not its
;; value -- and DECODE-INSTRUCTION-AT's matched WORD-FIELD-CHOICE record
;; (decoder.lisp) lets %RENDER-OPERAND-TEXT render the real alternative back,
;; unlike MOO's byte-encoded (no-record) fallback above.
(definstruction disasm-word-machine coo
  (modes disasm-oo)
  (encoding
    (opcode 4)
    (operand val :field a
      (variant (choice disasm-oo-reg) inline :range (0 15) :bias 0)
      (variant (choice disasm-oo-ind) inline :range (0 15) :bias 16)))
  (semantics (set! (reg 0) val)))

;; COOM (#118): a *mixed* field, unlike COO above -- DISASM-OO-REG is
;; CHOICE-selected, but DISASM-OO-IND has no (choice ...) variant of its
;; own; %CHECK-WORD-VARIANT-CHOICES! stamps the value-selected (range 16 31)
;; variant with DISASM-OO-IND. %RENDER-OPERAND-TEXT must render the real
;; matched alternative back for *both* rows, not just the CHOICE-selected
;; one -- see DISASSEMBLE-ONE-OF-RENDERS-THE-MATCHED-ALTERNATIVE-ON-A-MIXED-
;; FIELD below.
(definstruction disasm-word-machine coom
  (modes disasm-oo)
  (encoding
    (opcode 6)
    (operand val :field a
      (variant (choice disasm-oo-reg) inline :range (0 15) :bias 0)
      (variant (range 16 31) inline)))
  (semantics (set! (reg 0) val)))

;; Shared opcode (#105): two mnemonics, decode-distinguishable purely by
;; their field A's own disjoint bias range -- what returned :DECODE-FAILURE
;; for one form and mis-decoded the other before this ticket.
(definstruction disasm-word-machine sh1
  (modes disasm-rr)
  (encoding
    (opcode 5)
    (operand dst :field b)
    (operand src :field a (variant (range 0 30) inline :bias 0)))
  (semantics (set! (reg dst) src)))

(definstruction disasm-word-machine sh2
  (modes disasm-rr)
  (encoding
    (opcode 5)
    (operand dst :field b)
    (operand src :field a (variant (range 0 30) inline :bias 32)))
  (semantics (set! (reg dst) (+ src 1))))

;;; Regression gate -- the EMULATOR suite (tests/emulator.lisp) exercising
;;; STEP-MACHINE must still pass unchanged after the DECODE-INSTRUCTION-AT
;;; extraction; no test here duplicates that, but every test below that
;;; compares against STEP-MACHINE is the extraction's actual thesis.

;;; Decode/step equivalence

(fiveam:test decode-matches-step-byte-machine
  ;; HLT's semantics TRAP -- STEP-MACHINE already advances PC before running
  ;; semantics (its own docstring), so PC still matches the decoded SIZE even
  ;; on the trapping step; only the EQ-descriptor check is skipped there,
  ;; since a signalled condition unwinds past STEP-MACHINE's own return.
  (let* ((m (make-machine 'disasm-test-machine))
         (a (assemble "ldx #10
lda $20
hlt" :machine 'disasm-test-machine)))
    (load-program m a)
    (loop repeat 3
          do (let* ((pc-before (sref m 'pc))
                     (reader (machine-cell-reader m 'ram)))
               (multiple-value-bind (descriptor values size)
                   (decode-instruction-at reader pc-before 'disasm-test-machine :memory 'ram)
                 (declare (ignore values))
                 (handler-case
                     (let ((executed (step-machine m)))
                       (fiveam:is (eq descriptor executed))
                       (fiveam:is (= (+ pc-before size) (sref m 'pc))))
                   (lasm-trap ()
                     (fiveam:is (= (+ pc-before size) (sref m 'pc))))))))))

(fiveam:test decode-matches-step-word-machine
  (let* ((m (make-machine 'disasm-word-machine))
         (a (assemble "set 5,5
set 1,1000
hlt" :machine 'disasm-word-machine)))
    (load-program m a)
    (loop repeat 3
          do (let* ((pc-before (sref m 'pc))
                     (reader (machine-cell-reader m 'ram)))
               (multiple-value-bind (descriptor values size)
                   (decode-instruction-at reader pc-before 'disasm-word-machine :memory 'ram)
                 (declare (ignore values))
                 (handler-case
                     (let ((executed (step-machine m)))
                       (fiveam:is (eq descriptor executed))
                       (fiveam:is (= (+ pc-before size) (sref m 'pc))))
                   (lasm-trap ()
                     (fiveam:is (= (+ pc-before size) (sref m 'pc))))))))))

(fiveam:test decode-values-sign-extended
  (let ((a (assemble "ldsi #-1" :machine 'disasm-test-machine)))
    (multiple-value-bind (descriptor values size)
        (decode-instruction-at (vector-cell-reader (assembly-cells a)) 0 'disasm-test-machine)
      (declare (ignore size))
      (fiveam:is (string= "LDSI" (instruction-descriptor-name descriptor)))
      (fiveam:is (equal (list -1) values)))))

(fiveam:test decode-word-machine-never-sign-extends
  ;; Field A's high bit is set by an inline value near its top (30, biased to
  ;; 63) -- if this were wrongly sign-extended it would decode negative.
  ;; DISASM-RR (the mode SET declares) has no :SIGNED of its own, so this
  ;; stays true after #63 exactly as before it -- #63 only makes a
  ;; *:SIGNED T* hole's value-selected field sign-extend, never an
  ;; ungoverned one.
  (let ((a (assemble "set 0,30" :machine 'disasm-word-machine)))
    (multiple-value-bind (descriptor values size)
        (decode-instruction-at (vector-cell-reader (assembly-cells a)) 0 'disasm-word-machine)
      (declare (ignore size))
      (fiveam:is (string= "SET" (instruction-descriptor-name descriptor)))
      (fiveam:is (equal (list 0 30) values)))))

;; #63: a :SIGNED T word-encoded hole's value-selected field, by contrast,
;; must decode sign-extended -- SIGNSET (tests/instruction.lisp,
;; SIGNED-WORD-TEST-MACHINE) is the positive case DECODE-WORD-MACHINE-NEVER-
;; SIGN-EXTENDS above is deliberately not.
(fiveam:test decode-signed-word-field-sign-extends
  (let ((a (assemble "signset #-5" :machine 'signed-word-test-machine)))
    (multiple-value-bind (descriptor values size)
        (decode-instruction-at (vector-cell-reader (assembly-cells a)) 0 'signed-word-test-machine)
      (declare (ignore size))
      (fiveam:is (string= "SIGNSET" (instruction-descriptor-name descriptor)))
      (fiveam:is (equal (list -5) values)))))

;;; Rendering

(fiveam:test disassemble-byte-program-text
  (let* ((a (assemble "ldx #10
hlt" :machine 'disasm-test-machine))
         (lines (disassemble-assembly a :machine 'disasm-test-machine :labels nil)))
    (fiveam:is (= 2 (length lines)))
    (fiveam:is (string= "ldx #$A" (disassembly-line-text (first lines))))
    (fiveam:is (string= "hlt" (disassembly-line-text (second lines))))))

(fiveam:test disassemble-operand-literal-concatenation
  (let* ((a (assemble "ldx #$10
ldix $10,X
ldiy ($10),Y
ldsr $2,S" :machine 'disasm-test-machine))
         (lines (disassemble-assembly a :machine 'disasm-test-machine :labels nil)))
    (fiveam:is (string= "ldx #$10" (disassembly-line-text (first lines))))
    (fiveam:is (string= "ldix $10,X" (disassembly-line-text (second lines))))
    (fiveam:is (string= "ldiy ($10),Y" (disassembly-line-text (third lines))))
    (fiveam:is (string= "ldsr $2,S" (disassembly-line-text (fourth lines))))))

(fiveam:test disassemble-relative-renders-absolute-target
  ;; A backward branch to its own address: at address 0, BRA is 2 cells wide,
  ;; so the next-instruction address is 2 -- an offset of -2 targets 0 again.
  (let* ((a (assemble "bra *" :machine 'disasm-test-machine))
         (lines (disassemble-assembly a :machine 'disasm-test-machine :labels nil)))
    (fiveam:is (string= "bra $0" (disassembly-line-text (first lines))))))

(fiveam:test disassemble-multi-hole-order
  (let* ((a (assemble "movi $10,$20" :machine 'disasm-test-machine))
         (lines (disassemble-assembly a :machine 'disasm-test-machine :labels nil)))
    (fiveam:is (string= "movi $10,$20" (disassembly-line-text (first lines))))
    (fiveam:is (equal (list #x10 #x20) (disassembly-line-values (first lines))))))

(fiveam:test disassemble-relative-sibling-hole-renders-only-its-own-hole
  ;; BRM is 4 cells (opcode, sub, 1-cell TGT, 1-cell N); "brm #*, 9" is
  ;; self-referencing, so TGT's offset from the next instruction (address 4)
  ;; back to BRM's own address (0) is -4 -- rendered back as the absolute
  ;; target 0 + 4 + -4 = 0. N's own value (9) renders plainly, untouched by
  ;; that adjustment.
  (let* ((a (assemble "brm #*, 9" :machine 'disasm-test-machine))
         (lines (disassemble-assembly a :machine 'disasm-test-machine :labels nil)))
    (fiveam:is (string= "brm #$0,$9" (disassembly-line-text (first lines))))
    ;; DISASSEMBLY-LINE-VALUES holds DECODE-INSTRUCTION-AT's raw values --
    ;; TGT's still-relative offset (-4), not the rendered absolute target
    ;; (0) the line's own TEXT shows.
    (fiveam:is (equal (list -4 9) (disassembly-line-values (first lines))))))

(fiveam:test disassemble-one-of-with-no-choice-record-renders-first-alternative
  ;; #103/#117: MOO is byte-encoded (DISASM-TEST-MACHINE, plain (operand
  ;; :mode)), so it carries no WORD-FIELD-CHOICE record of which alternative
  ;; assembled it at all -- the operand was written bracketed
  ;; ([DISASM-OO-IND]), but disassembly still renders it via the *first*
  ;; alternative's own syntax (DISASM-OO-REG, a bare expr), same as before
  ;; #104: mode-selected field codes are a word-encoding-only feature (see
  ;; DISASSEMBLE-ONE-OF-RENDERS-THE-MATCHED-ALTERNATIVE below for the case
  ;; where a real record exists) -- a documented fallback, not a bug in this
  ;; test.
  (let* ((a (assemble "moo [$10]" :machine 'disasm-test-machine))
         (lines (disassemble-assembly a :machine 'disasm-test-machine :labels nil)))
    (fiveam:is (string= "moo $10" (disassembly-line-text (first lines))))))

(fiveam:test disassemble-one-of-renders-the-matched-alternative
  ;; #104/#117: COO is word-encoded with a CHOICE-selected field, so unlike
  ;; MOO's byte-encoded fallback above, the real alternative comes back --
  ;; "5" and "[5]" render distinctly, matching what was actually written.
  (let* ((bare (disassemble-assembly (assemble "coo 5" :machine 'disasm-word-machine)
                                      :machine 'disasm-word-machine :labels nil))
         (indirect (disassemble-assembly (assemble "coo [5]" :machine 'disasm-word-machine)
                                          :machine 'disasm-word-machine :labels nil)))
    (fiveam:is (string= "coo $5" (disassembly-line-text (first bare))))
    (fiveam:is (string= "coo [$5]" (disassembly-line-text (first indirect))))))

(fiveam:test disassemble-nested-one-of-does-not-reuse-outer-choice
  (let ((choice (make-word-field-choice :width 1 :shift 0 :kind :inline
                                         :range '(0 . 1) :choice 'no-inner)))
    (fiveam:is
     (string= "$5"
              (%render-operand-text (find-mode-descriptor 'no-outer) '(5) 'default
                                    (make-hash-table) (list choice))))))

(fiveam:test disassemble-one-of-renders-the-matched-alternative-on-a-mixed-field
  ;; #118: COOM mixes a CHOICE-selected row (DISASM-OO-REG) with a
  ;; value-selected one stamped DISASM-OO-IND -- both must render their own
  ;; matched syntax back, not just the CHOICE-selected one.
  (let* ((bare (disassemble-assembly (assemble "coom 5" :machine 'disasm-word-machine)
                                      :machine 'disasm-word-machine :labels nil))
         (indirect (disassemble-assembly (assemble "coom [20]" :machine 'disasm-word-machine)
                                          :machine 'disasm-word-machine :labels nil)))
    (fiveam:is (string= "coom $5" (disassembly-line-text (first bare))))
    (fiveam:is (string= "coom [$14]" (disassembly-line-text (first indirect))))))

(fiveam:test disassemble-no-operand-instruction
  (let* ((a (assemble "hlt" :machine 'disasm-test-machine))
         (lines (disassemble-assembly a :machine 'disasm-test-machine :labels nil)))
    (fiveam:is (string= "hlt" (disassembly-line-text (first lines))))))

(fiveam:test disassemble-word-inline-and-extra-word
  (let* ((a (assemble "set 0,5
set 1,1000
hlt" :machine 'disasm-word-machine))
         (lines (disassemble-assembly a :machine 'disasm-word-machine :labels nil)))
    (fiveam:is (= 1 (disassembly-line-size (first lines))))
    (fiveam:is (equal (list 0 5) (disassembly-line-values (first lines))))
    (fiveam:is (= 2 (disassembly-line-size (second lines))))
    (fiveam:is (equal (list 1 1000) (disassembly-line-values (second lines))))
    (fiveam:is (string= "set $1,$3E8" (disassembly-line-text (second lines))))))

(fiveam:test disassemble-word-two-extra-words
  (let* ((a (assemble "movx 20,1000" :machine 'disasm-word-machine))
         (lines (disassemble-assembly a :machine 'disasm-word-machine :labels nil)))
    (fiveam:is (= 3 (disassembly-line-size (first lines))))
    (fiveam:is (equal (list 20 1000) (disassembly-line-values (first lines))))))

(fiveam:test disassemble-mode-suffix-rendered
  (let* ((a (assemble "lda.z $10
lda.w $10" :machine 'disasm-test-machine))
         (lines (disassemble-assembly a :machine 'disasm-test-machine :labels nil :suffixes t)))
    (fiveam:is (string= "lda.z $10" (disassembly-line-text (first lines))))
    (fiveam:is (string= "lda.w $10" (disassembly-line-text (second lines)))))
  (let* ((a (assemble "lda.z $10" :machine 'disasm-test-machine))
         (lines (disassemble-assembly a :machine 'disasm-test-machine :labels nil :suffixes nil)))
    (fiveam:is (string= "lda $10" (disassembly-line-text (first lines))))))

(fiveam:test disassemble-hex-prefix-from-lexer
  (deflexer disasm-0x-lexer
    (comment-styles (";" :line))
    (number-formats (:hex "0x") (:dec :default))
    (label-suffix ":")
    (ident-chars :alnum "_"))
  (let* ((a (assemble "ldx #10" :machine 'disasm-test-machine))
         (lines (disassemble-assembly a :machine 'disasm-test-machine :labels nil
                                         :lexer 'disasm-0x-lexer)))
    (fiveam:is (string= "ldx #0xA" (disassembly-line-text (first lines))))))

(fiveam:test disassemble-labels-from-symbols
  ;; A single self-targeting BRA: "start" binds to the branch's own address,
  ;; which is also where its (self-relative) operand targets -- one line,
  ;; carrying both its own LABEL and an operand rendered as that same label.
  (let* ((a (assemble "start:
bra start" :machine 'disasm-test-machine))
         (lines (disassemble-assembly a :machine 'disasm-test-machine :labels t)))
    (fiveam:is (string= "start" (disassembly-line-label (first lines))))
    (fiveam:is (string= "bra start" (disassembly-line-text (first lines)))))
  (let* ((a (assemble "start:
bra start" :machine 'disasm-test-machine))
         (lines (disassemble-assembly a :machine 'disasm-test-machine :labels nil)))
    (fiveam:is (null (disassembly-line-label (first lines))))
    (fiveam:is (string= "bra $0" (disassembly-line-text (first lines))))))

(fiveam:test disassemble-equ-value-not-substituted
  ;; FIVE is an .EQU folded to 5, an address no instruction in this program
  ;; starts at -- it must not be substituted into any operand rendering.
  (let* ((a (assemble ".equ five, 5
ldx #5" :machine 'disasm-test-machine))
         (lines (disassemble-assembly a :machine 'disasm-test-machine :labels t)))
    (fiveam:is (string= "ldx #$5" (disassembly-line-text (first lines))))))

(fiveam:test disassemble-symbol-info-fixes-equ-aliasing-and-trailing-label
  ;; #37/#81: X (.EQU, folded to 1) happens to equal the first BRA's own address --
  ;; an instruction start (a "line start"), exactly the ambiguous case
  ;; %REVERSE-SYMBOLS' pre-#37 line-start restriction could not resolve.
  ;; END is a real label bound *after* the last instruction, an address no
  ;; LISTING-LINE/DISASSEMBLY-LINE starts at -- exactly the case that same
  ;; restriction dropped even though the label is perfectly real.
  (let* ((a (assemble ".equ x, 1
hlt
bra x
bra end
end:" :machine 'disasm-test-machine))
         ;; Legacy path: a bare ASSEMBLY-SYMBOLS table, no SYMBOL-INFO --
         ;; the discriminator doesn't exist, so the pre-#81 line-start
         ;; mitigation is all that's left, unchanged.
         (legacy (disassemble-cells (assembly-cells a) :machine 'disasm-test-machine
                                     :symbols (assembly-symbols a) :labels t))
         ;; Fixed path: DISASSEMBLE-ASSEMBLY passes SYMBOL-INFO automatically.
         (fixed (disassemble-assembly a :machine 'disasm-test-machine :labels t)))
    ;; Legacy: X's value (1) is a line start, so it's wrongly substituted --
    ;; the #81 bug, demonstrated here rather than fixed (no SYMBOL-INFO given).
    (fiveam:is (string= "bra x" (disassembly-line-text (second legacy))))
    ;; Legacy: END's value (5) is not a line start, so a real label is missed.
    (fiveam:is (string= "bra $5" (disassembly-line-text (third legacy))))
    ;; Fixed: X is tagged :EQU, so it's never substituted, line-start or not.
    (fiveam:is (string= "bra $1" (disassembly-line-text (second fixed))))
    (fiveam:is (null (disassembly-line-label (second fixed))))
    ;; Fixed: END is tagged :LABEL, so it substitutes despite not being a
    ;; line start -- the real fix #81 asked for.
    (fiveam:is (string= "bra end" (disassembly-line-text (third fixed))))))

(fiveam:test disassemble-symbol-info-works-standalone-without-symbols
  ;; SYMBOL-INFO carries its own QUALIFIED-NAME/VALUE (assembler.lisp) -- a
  ;; caller may pass it with :SYMBOLS NIL and still get every real label.
  (let* ((a (assemble "start:
bra start" :machine 'disasm-test-machine))
         (lines (disassemble-cells (assembly-cells a) :machine 'disasm-test-machine
                                    :symbol-info (assembly-symbol-info a) :labels t)))
    (fiveam:is (string= "start" (disassembly-line-label (first lines))))
    (fiveam:is (string= "bra start" (disassembly-line-text (first lines))))))

;;; Failure and bounds

(fiveam:test disassemble-unknown-opcode-emits-data
  (let* ((lines (disassemble-cells (list #xFF #xA2 10) :machine 'disasm-test-machine :labels nil)))
    (fiveam:is (= 2 (length lines)))
    (fiveam:is (null (disassembly-line-descriptor (first lines))))
    (fiveam:is (= 1 (disassembly-line-size (first lines))))
    (fiveam:is (string= ".byte $FF" (disassembly-line-text (first lines))))
    (fiveam:is (string= "LDX" (instruction-descriptor-name (disassembly-line-descriptor (second lines)))))))

(fiveam:test disassemble-word-unmatched-field-is-failure
  ;; opcode = 1 (SET) at shift 12, dst = 0, src = 100 -- 100 is neither in
  ;; field A's inline range 0..31 nor its escape 1023, so no WORD-FIELD-
  ;; CHOICE matches: a valid opcode with an undecodable field, distinct from
  ;; an outright unregistered opcode.
  (let* ((word (logior (ash 1 12) 100))
         (lo (logand word #xFF))
         (hi (logand (ash word -8) #xFF))
         (lines (disassemble-cells (list lo hi) :machine 'word-test-machine :labels nil)))
    (fiveam:is (null (disassembly-line-descriptor (first lines))))
    (fiveam:is (= 1 (disassembly-line-size (first lines))))
    (fiveam:is (string= (format nil ".byte $~X" lo) (disassembly-line-text (first lines))))))

(fiveam:test disassemble-truncated-trailing-instruction
  (let* ((a (assemble "ldx #10" :machine 'disasm-test-machine))
         (lines (disassemble-cells (assembly-cells a) :machine 'disasm-test-machine
                                                        :end 1 :labels nil)))
    (fiveam:is (= 1 (length lines)))
    (fiveam:is (null (disassembly-line-descriptor (first lines))))
    (fiveam:is (= 1 (disassembly-line-size (first lines))))))

(fiveam:test disassemble-all-invalid-terminates
  (let ((lines (disassemble-cells (list #xFF #xFF #xFF) :machine 'disasm-test-machine :labels nil)))
    (fiveam:is (= 3 (length lines)))
    (fiveam:is (every (lambda (l) (null (disassembly-line-descriptor l))) lines))
    (fiveam:is (every (lambda (l) (= 1 (disassembly-line-size l))) lines))))

(fiveam:test disassemble-non-zero-origin
  (let* ((a (assemble "bra *
hlt" :machine 'disasm-test-machine :origin #x200))
         (lines (disassemble-assembly a :machine 'disasm-test-machine :labels nil)))
    (fiveam:is (= #x200 (disassembly-line-address (first lines))))
    (fiveam:is (= #x202 (disassembly-line-address (second lines))))
    (fiveam:is (string= "bra $200" (disassembly-line-text (first lines))))))

(fiveam:test disassemble-memory-requires-bounds
  (let ((m (make-machine 'disasm-test-machine)))
    (fiveam:signals error (disassemble-memory m))
    (fiveam:signals error (disassemble-memory m :start 0))
    (fiveam:signals error (disassemble-memory m :count 2))))

(fiveam:test disassemble-memory-matches-disassemble-cells
  (let* ((m (make-machine 'disasm-test-machine))
         (a (assemble "ldx #10
hlt" :machine 'disasm-test-machine)))
    (load-program m a)
    (let ((from-memory (disassemble-memory m :start 0 :count (length (assembly-cells a)) :labels nil))
          (from-cells (disassemble-cells (assembly-cells a) :machine 'disasm-test-machine :labels nil)))
      (fiveam:is (= (length from-cells) (length from-memory)))
      (loop for l1 in from-memory
            for l2 in from-cells
            do (fiveam:is (string= (disassembly-line-text l1) (disassembly-line-text l2)))))))

;;; Round trip

(fiveam:test round-trip-assemble-disassemble-assemble
  (let* ((a (assemble "ldx #10
lda.z $20
movi $1,$2
bra *
hlt" :machine 'disasm-test-machine :origin #x200))
         (lines (disassemble-assembly a :machine 'disasm-test-machine :labels nil :suffixes t))
         (text (disassembly-text lines :origin (assembly-origin a)))
         (a2 (assemble text :machine 'disasm-test-machine)))
    (fiveam:is (equalp (assembly-cells a) (assembly-cells a2)))
    (fiveam:is (= (assembly-origin a) (assembly-origin a2)))))

(fiveam:test round-trip-word-machine
  (let* ((a (assemble "set 0,5
set 1,1000
hlt" :machine 'disasm-word-machine))
         (lines (disassemble-assembly a :machine 'disasm-word-machine :labels nil))
         (text (disassembly-text lines))
         (a2 (assemble text :machine 'disasm-word-machine)))
    (fiveam:is (equalp (assembly-cells a) (assembly-cells a2)))))

;;; #64: a program mixing all three of WORD-LAYOUTS-TEST-MACHINE's layouts
;;; (tests/instruction.lisp) -- the check that DECODE-INSTRUCTION-AT picks
;;; the right field split per opcode, not just the machine's default one.

(fiveam:test round-trip-word-layouts-machine
  (let* ((a (assemble "setx 5, 200
setwide 4000
setnarrow 3, 2, 100
hlt" :machine 'word-layouts-test-machine))
         (lines (disassemble-assembly a :machine 'word-layouts-test-machine :labels nil))
         (text (disassembly-text lines))
         (a2 (assemble text :machine 'word-layouts-test-machine)))
    (fiveam:is (equalp (assembly-cells a) (assembly-cells a2)))))

;;; #136: three co-tenants at one opcode (FIELD-VALUE-TEST-MACHINE,
;;; tests/instruction.lisp) told apart purely by their own pinned
;;; (field-value ...) -- INCN/DECN each carry an operand hole, ZEROALL
;;; carries none at all -- the check that DECODE-INSTRUCTION-AT's own
;;; constant pre-pass (%TRY-DECODE-WORD-CANDIDATE, decoder.lisp) picks the
;;; right one of the three, not just the first registered.
(fiveam:test round-trip-field-value-co-tenants
  (let* ((a (assemble "incn 0
decn 0
zeroall
hlt" :machine 'field-value-test-machine))
         (lines (disassemble-assembly a :machine 'field-value-test-machine :labels nil))
         (text (disassembly-text lines))
         (a2 (assemble text :machine 'field-value-test-machine)))
    (fiveam:is (equal '("INCN" "DECN" "ZEROALL" "HLT")
                       (mapcar (lambda (l) (instruction-descriptor-name (disassembly-line-descriptor l))) lines)))
    (fiveam:is (equalp (assembly-cells a) (assembly-cells a2)))))

;;; #62 (M4): a word-encoded RELATIVE hole renders as its absolute target on
;;; disassembly, exactly like the byte path's DISASSEMBLE-RELATIVE-RENDERS-
;;; ABSOLUTE-TARGET, and round-trips through re-assembly in both its inline
;;; and extra-word forms (WBRA, tests/instruction.lisp,
;;; WORD-RELATIVE-TEST-MACHINE).

(fiveam:test disassemble-word-relative-renders-absolute-target
  ;; A backward branch to its own address: at address 0, WBRA is 2 cells
  ;; wide, so the next-instruction address is 2 -- an offset of -2 targets 0
  ;; again.
  (let* ((a (assemble "wbra *" :machine 'word-relative-test-machine))
         (lines (disassemble-assembly a :machine 'word-relative-test-machine :labels nil)))
    (fiveam:is (string= "wbra $0" (disassembly-line-text (first lines))))))

(fiveam:test round-trip-word-relative-inline
  (let* ((a (assemble "loop: wbra loop" :machine 'word-relative-test-machine))
         (lines (disassemble-assembly a :machine 'word-relative-test-machine :labels nil))
         (text (disassembly-text lines))
         (a2 (assemble text :machine 'word-relative-test-machine)))
    (fiveam:is (equalp (assembly-cells a) (assembly-cells a2)))))

(fiveam:test round-trip-word-relative-extra-word
  (let* ((source (with-output-to-string (s)
                   (format s "start: wbra target~%")
                   (dotimes (i 300) (format s "wnop~%"))
                   (format s "target: wnop~%")))
         (a (assemble source :machine 'word-relative-test-machine))
         (lines (disassemble-assembly a :machine 'word-relative-test-machine :labels nil))
         (text (disassembly-text lines))
         (a2 (assemble text :machine 'word-relative-test-machine)))
    (fiveam:is (equalp (assembly-cells a) (assembly-cells a2)))))

(fiveam:test round-trip-sub-opcode-decodes-each-mode-back-to-itself
  ;; #125's own reproduction, at the disassembler level: SUBOP's IMMEDIATE
  ;; and ABSOLUTE modes share opcode #x03 and are told apart only by their
  ;; own :SUB cell -- each must decode and render back to its *own* mode
  ;; (distinguishable here by syntax, "#n" vs "$n", same as LDX/LDA above),
  ;; and the listing's sizes must reflect the extra sub-opcode cell.
  (let* ((a (assemble "subop #10
subop $20
hlt" :machine 'disasm-test-machine))
         (lines (disassemble-assembly a :machine 'disasm-test-machine :labels nil :suffixes nil)))
    (fiveam:is (string= "subop #$A" (disassembly-line-text (first lines))))
    (fiveam:is (string= "subop $20" (disassembly-line-text (second lines))))
    ;; IMMEDIATE: 1 (opcode) + 1 (sub) + 1 (operand) = 3 cells.
    (fiveam:is (= 3 (disassembly-line-size (first lines))))
    ;; ABSOLUTE's default operand width is 2 cells here (RAM is :ADDR-WIDTH
    ;; 16 over an 8-bit cell): 1 + 1 + 2 = 4 cells.
    (fiveam:is (= 4 (disassembly-line-size (second lines))))
    (let* ((text (disassembly-text lines))
           (a2 (assemble text :machine 'disasm-test-machine)))
      (fiveam:is (equalp (assembly-cells a) (assembly-cells a2))))))

(fiveam:test round-trip-relative-sibling-hole-decodes-back-to-itself
  ;; #130's own reproduction, at the disassembler level: BRM's TGT hole
  ;; (relative or absolute, chosen by "#" syntax) and its plain sibling N
  ;; hole must each round-trip through their own text -- TGT via its
  ;; resolved absolute target, N untouched.
  (let* ((a (assemble "brm 10, 5
loop: brm #loop, 9
hlt" :machine 'disasm-test-machine))
         (lines (disassemble-assembly a :machine 'disasm-test-machine :labels nil :suffixes nil)))
    (fiveam:is (string= "brm $A,$5" (disassembly-line-text (first lines))))
    (fiveam:is (string= "brm #$4,$9" (disassembly-line-text (second lines))))
    (let* ((text (disassembly-text lines))
           (a2 (assemble text :machine 'disasm-test-machine)))
      (fiveam:is (equalp (assembly-cells a) (assembly-cells a2))))))

(fiveam:test round-trip-shared-opcode-decodes-each-mnemonic-back-to-itself
  ;; #105's own reproduction, at the disassembler level: SH1 and SH2 share
  ;; opcode 5 and are told apart only by field A's own disjoint bias range --
  ;; each must decode and render back to its *own* mnemonic, not
  ;; :DECODE-FAILURE for one and the other's name for both.
  (let* ((a (assemble "sh1 0,5
sh2 1,5
hlt" :machine 'disasm-word-machine))
         (lines (disassemble-assembly a :machine 'disasm-word-machine :labels nil)))
    (fiveam:is (string= "sh1 $0,$5" (disassembly-line-text (first lines))))
    (fiveam:is (string= "sh2 $1,$5" (disassembly-line-text (second lines))))
    (let* ((text (disassembly-text lines))
           (a2 (assemble text :machine 'disasm-word-machine)))
      (fiveam:is (equalp (assembly-cells a) (assembly-cells a2))))))

;;; #143: register-index operands disassemble as their own #72 :names alias.
;;; Own fixtures, not additions to DISASM-TEST-MACHINE/DISASM-WORD-MACHINE
;;; above -- those two back ~30 tests already, and giving either machine a
;;; :NAMES bank has machine-wide side effects (%BIND-SYMBOL! turns a
;;; colliding label/.EQU into an assembly error; %CHECK-OPERAND-NAMES errors
;;; on any existing operand field name that now collides with a new alias)
;;; that risk breaking tests unrelated to this feature.

(defmachine disasm-alias-machine
  (register pc :width 16)
  (register v :width 8 :names (v0 v1 v2 v3))
  (memory ram :width 8 :addr-width 16))

(defmode disasm-alias-v-imm expr "," "#" expr)

(definstruction disasm-alias-machine ldv
  (modes disasm-alias-v-imm)
  (encoding (opcode #x01) (operand x :width 1 :register v) (operand nn :width 1))
  (semantics (set! (v x) nn)))

(definstruction disasm-alias-machine hlt
  (encoding (opcode #x00))
  (semantics (trap :halt)))

(defmachine disasm-word-alias-machine
  (register pc :width 16)
  (register reg :width 16 :names (a b c d))
  (memory ram :width 16 :addr-width 16 :cell-width 16)
  (instruction-word :width 16
    (field av 6)
    (field bv 5)
    (field opcode 5)))

(defmode disasm-alias-rr expr "," expr)

(definstruction disasm-word-alias-machine addr
  (modes disasm-alias-rr)
  (encoding
    (opcode 1)
    (operand dst :field bv :register reg)
    (operand srcreg :field av :register reg))
  (semantics (set! (reg dst) (wrap-value (+ (reg dst) (reg srcreg)) 16))))

(definstruction disasm-word-alias-machine hlt
  (encoding (opcode 2))
  (semantics (trap :halt)))

(fiveam:test disassemble-byte-register-hole-renders-alias
  (let* ((a (assemble "ldv v1, #10
hlt" :machine 'disasm-alias-machine))
         (lines (disassemble-assembly a :machine 'disasm-alias-machine :labels nil)))
    (fiveam:is (string= "ldv v1,#$A" (disassembly-line-text (first lines))))))

(fiveam:test disassemble-word-register-hole-renders-alias
  (let* ((a (assemble "addr a, b
hlt" :machine 'disasm-word-alias-machine))
         (lines (disassemble-assembly a :machine 'disasm-word-alias-machine :labels nil)))
    (fiveam:is (string= "addr a,b" (disassembly-line-text (first lines))))))

(fiveam:test disassemble-register-hole-out-of-range-index-renders-hex
  ;; DISASM-ALIAS-MACHINE's V bank has 4 cells (V0-V3); a hand-built cell
  ;; naming index 5 (LDV's X field is a full byte, well beyond V's own
  ;; :NAMES) has no alias to render -- REGISTER-ALIAS-AT falls through to
  ;; %RENDER-VALUE's ordinary hex rendering, never NIL/blank.
  (let* ((lines (disassemble-cells (list #x01 5 10 #x00) :machine 'disasm-alias-machine :labels nil)))
    (fiveam:is (string= "ldv $5,#$A" (disassembly-line-text (first lines))))))

(fiveam:test disassemble-register-alias-wins-over-a-same-valued-label
  ;; A label bound to address 0 must not be substituted for a register-index
  ;; hole's own decoded value of 0 -- alias beats label, same as alias beats
  ;; a bare hex render.
  (let* ((a (assemble "ldv v0, #10
hlt" :machine 'disasm-alias-machine))
         (symbols (let ((h (make-hash-table :test 'equal)))
                    (setf (gethash "zero" h) 0)
                    h))
         (lines (disassemble-cells (coerce (assembly-cells a) 'list) :machine 'disasm-alias-machine
                                    :labels t :symbols symbols)))
    (fiveam:is (string= "ldv v0,#$A" (disassembly-line-text (first lines))))))

(fiveam:test round-trip-byte-register-alias
  (let* ((a (assemble "ldv v2, #99
hlt" :machine 'disasm-alias-machine))
         (lines (disassemble-assembly a :machine 'disasm-alias-machine :labels nil))
         (text (disassembly-text lines))
         (a2 (assemble text :machine 'disasm-alias-machine)))
    (fiveam:is (equalp (assembly-cells a) (assembly-cells a2)))))

(fiveam:test round-trip-word-register-alias
  (let* ((a (assemble "addr c, d
hlt" :machine 'disasm-word-alias-machine))
         (lines (disassemble-assembly a :machine 'disasm-word-alias-machine :labels nil))
         (text (disassembly-text lines))
         (a2 (assemble text :machine 'disasm-word-alias-machine)))
    (fiveam:is (equalp (assembly-cells a) (assembly-cells a2)))))

;;; Data regions (#82)

(fiveam:test disassemble-cells-data-region-suppresses-decode
  ;; $A2 $0A is LDX #$A; declared data, it renders as two .byte lines.
  (let ((lines (disassemble-cells (list #xA2 #x0A #xA2 #x0B) :machine 'disasm-test-machine
                                                             :labels nil :data-regions '((0 . 2)))))
    (fiveam:is (equal '(".byte $A2" ".byte $A" "ldx #$B") (mapcar #'disassembly-line-text lines)))
    (fiveam:is (null (disassembly-line-descriptor (first lines))))))

(fiveam:test disassemble-cells-data-region-mid-stream
  (let ((lines (disassemble-cells (list #xA2 #x01 #xA2 #x02 #xA2 #x03) :machine 'disasm-test-machine
                                                                       :labels nil :data-regions '((2 . 4)))))
    (fiveam:is (equal '("ldx #$1" ".byte $A2" ".byte $2" "ldx #$3") (mapcar #'disassembly-line-text lines)))))

(fiveam:test disassemble-cells-data-region-honours-origin
  (let ((lines (disassemble-cells (list #xA2 #x0A) :machine 'disasm-test-machine :origin #x100
                                                    :labels nil :data-regions '((#x100 . #x102)))))
    (fiveam:is (equal '(#x100 #x101) (mapcar #'disassembly-line-address lines)))
    (fiveam:is (every (lambda (l) (null (disassembly-line-descriptor l))) lines))))

(fiveam:test disassemble-cells-instruction-never-straddles-a-region
  ;; LDX is 2 cells; a region starting at 1 forces its first cell to data.
  (let ((lines (disassemble-cells (list #xA2 #x0A #xA2 #x0B) :machine 'disasm-test-machine
                                                             :labels nil :data-regions '((1 . 2)))))
    (fiveam:is (equal '(".byte $A2" ".byte $A" "ldx #$B") (mapcar #'disassembly-line-text lines)))))

(fiveam:test disassemble-cells-data-regions-are-merged-and-unordered-ok
  (let ((lines (disassemble-cells (list #xA2 #x0A #xA2 #x0B) :machine 'disasm-test-machine
                                                             :labels nil :data-regions '((2 . 3) (0 . 2) (1 . 4)))))
    (fiveam:is (= 4 (length lines)))
    (fiveam:is (every (lambda (l) (null (disassembly-line-descriptor l))) lines))))

(fiveam:test disassemble-cells-rejects-malformed-data-region
  (dolist (bad '((2 . 2) (3 . 1) (-1 . 2) (a . 2) 5))
    (fiveam:signals error (disassemble-cells (list 0 0 0) :machine 'disasm-test-machine
                                                          :data-regions (list bad)))))

(fiveam:test disassemble-assembly-derives-data-regions-from-listing
  (let* ((a (assemble "ldx #10
table: .byte $A2, $0A
hlt" :machine 'disasm-test-machine))
         (auto (disassemble-assembly a :machine 'disasm-test-machine :labels nil))
         (off (disassemble-assembly a :machine 'disasm-test-machine :labels nil :data-regions nil)))
    (fiveam:is (equal '("ldx #$A" ".byte $A2" ".byte $A" "hlt") (mapcar #'disassembly-line-text auto)))
    (fiveam:is (equal '("ldx #$A" "ldx #$A" "hlt") (mapcar #'disassembly-line-text off)))))

(fiveam:test disassemble-assembly-explicit-data-regions-override-auto
  (let* ((a (assemble "table: .byte $A2, $0A" :machine 'disasm-test-machine))
         (lines (disassemble-assembly a :machine 'disasm-test-machine :labels nil :data-regions '((0 . 1)))))
    (fiveam:is (equal '(".byte $A2" ".byte $A") (mapcar #'disassembly-line-text lines)))))

(fiveam:test disassemble-memory-data-regions
  (let ((m (make-machine 'disasm-test-machine)))
    (load-program m (list #xA2 #x0A #xA2 #x0B))
    (let ((lines (disassemble-memory m :start 0 :count 4 :labels nil :data-regions '((0 . 2)))))
      (fiveam:is (equal '(".byte $A2" ".byte $A" "ldx #$B") (mapcar #'disassembly-line-text lines))))))
