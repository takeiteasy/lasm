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
  (let ((a (assemble "set 0,30" :machine 'disasm-word-machine)))
    (multiple-value-bind (descriptor values size)
        (decode-instruction-at (vector-cell-reader (assembly-cells a)) 0 'disasm-word-machine)
      (declare (ignore size))
      (fiveam:is (string= "SET" (instruction-descriptor-name descriptor)))
      (fiveam:is (equal (list 0 30) values)))))

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
