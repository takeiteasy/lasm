;;;; tests/diagnostic.lisp
;;;; fiveam tests for the shared diagnostic mechanism (diagnostic.lisp),
;;;; the mode-mismatch/ambiguity diagnostics it feeds (assembler.lisp), and
;;;; the opt-in strict operand range check absorbing #28/#43 (#74).

(in-package #:lasm)

(fiveam:def-suite diagnostic :in lasm)
(fiveam:in-suite diagnostic)

;;; Fixture -- a small machine dedicated to this file, so its modes/opcodes
;;; can't collide with INSTR-TEST-MACHINE (tests/instruction.lisp), which
;;; this file also reuses read-only for the built-in zero-page/absolute
;;; cases below.

(defmachine diag-test-machine
  (register a :width 8)
  (memory ram :width 8 :addr-width 16))

;; Two modes sharing a bare-EXPR pattern *and* width -- unlike ZERO-PAGE/
;; ABSOLUTE (different widths), a value matching either always ties, so an
;; instruction declaring both is the one case %CHOOSE-VARIANT's declaration
;; order alone decides between, i.e. the AMBIGUOUS-MODE warning's target.
(defmode diag-mode-a expr :width 1)
(defmode diag-mode-b expr :width 1)

(definstruction diag-test-machine ambi
  (modes
    (diag-mode-a (opcode #x01) (semantics (set! a operand)))
    (diag-mode-b (opcode #x02) (semantics (set! a operand)))))

;; A :STRICT mode alongside a plain (non-strict) wider one sharing syntax --
;; STIW's two variants let a test pin the known §4 unevenness: a value that
;; fits neither candidate falls back to the *widest* one (STIW-WIDE, not
;; strict), so a per-mode :STRICT that isn't the fallback never fires; only
;; *STRICT-OPERAND-RANGE* (the blanket switch) still catches it.
(defmode diag-strict-imm "#" expr :width 1 :strict t)
(defmode diag-wide-imm "#" expr :width 2)

(definstruction diag-test-machine sti
  (modes diag-strict-imm)
  (encoding (opcode #x05) (operand :mode))
  (semantics (set! a operand)))

(definstruction diag-test-machine stiw
  (modes
    (diag-strict-imm (opcode #x06) (semantics (set! a operand)))
    (diag-wide-imm (opcode #x07) (semantics (set! a operand)))))

(defun %split-into-lines (string)
  (loop with start = 0
        for nl = (position #\Newline string :start start)
        collect (subseq string start nl)
        while nl
        do (setf start (1+ nl))))

;;; DIAGNOSTIC-TEXT rendering

(fiveam:test diagnostic-text-degrades-with-no-line
  (let ((c (make-condition 'lasm-syntax-error :message "just a message")))
    (fiveam:is (string= "just a message" (diagnostic-text c)))))

(fiveam:test diagnostic-text-degrades-with-no-source
  (let ((c (make-condition 'lasm-syntax-error :message "no source" :line 3 :column 2)))
    (fiveam:is (string= "line 3, column 2: no source" (diagnostic-text c)))))

(fiveam:test diagnostic-text-omits-caret-with-no-column
  (let ((c (make-condition 'lasm-syntax-error :message "no column" :line 2)))
    (let ((text (diagnostic-text c :source (format nil "one~%two~%three"))))
      (fiveam:is (search "line 2: no column" text))
      (fiveam:is (search "two" text))
      (fiveam:is (not (search "^" text))))))

(fiveam:test diagnostic-text-renders-source-line-and-caret
  (let ((c (make-condition 'lasm-syntax-error :message "bad thing" :line 2 :column 5)))
    (let ((text (diagnostic-text c :source (format nil "one~%abcdefgh"))))
      (fiveam:is (search "line 2, column 5: bad thing" text))
      (fiveam:is (search "abcdefgh" text))
      (fiveam:is (search "^" text))
      ;; The caret sits under column 5 of "abcdefgh" -- the 'e'. Line label
      ;; "2" is 1 char, so the gutter is 1 space; then " | " (3 chars); then
      ;; 4 spaces of indent (column - 1); the caret is the 9th character,
      ;; index 8.
      (let* ((caret-line (first (last (%split-into-lines text))))
             (caret-pos (position #\^ caret-line)))
        (fiveam:is (= 8 caret-pos))))))

(fiveam:test undefined-label-points-to-label-token
  (handler-case
      (assemble (format nil "nop~%bne missing + 1") :machine 'instr-test-machine)
    (unresolved-label (c)
      (fiveam:is (typep c 'lasm-syntax-error))
      (fiveam:is (string= "missing" (unresolved-label-name c)))
      (fiveam:is (= 2 (lasm-syntax-error-line c)))
      (fiveam:is (= 5 (lasm-syntax-error-column c)))
      (fiveam:is (search "bne missing + 1" (diagnostic-text c)))
      (fiveam:is (search "^" (diagnostic-text c))))))

(fiveam:test standalone-undefined-label-keeps-message-without-source
  (handler-case (eval-expr-constant (parse-expression (tokenize "missing")))
    (unresolved-label (c)
      (fiveam:is (null (lasm-syntax-error-source c)))
      (fiveam:is (search "unresolved label" (diagnostic-text c))))))

(fiveam:test macro-assembly-error-reports-call-and-definition
  (handler-case
      (assemble ".macro bad
ldx $5,X
.endm
bad" :machine 'instr-test-machine)
    (assembly-error (condition)
      (fiveam:is (= 4 (lasm-syntax-error-line condition)))
      (fiveam:is (= 2 (lasm-syntax-error-definition-line condition)))
      (let ((text (diagnostic-text condition)))
        (fiveam:is (search "line 4" text))
        (fiveam:is (search "macro body line 2" text))
        (fiveam:is (search "ldx $5,X" text))))))

(fiveam:test macro-encode-error-reports-call-and-definition
  (let ((*strict-operand-range* t))
    (handler-case
        (assemble ".macro bad
ldx #300
.endm
bad" :machine 'instr-test-machine)
      (assembly-error (condition)
        (fiveam:is (= 4 (lasm-syntax-error-line condition)))
        (fiveam:is (= 2 (lasm-syntax-error-definition-line condition)))))))

;;; WITH-SOURCE-CONTEXT / condition SOURCE slot

(fiveam:test assemble-error-carries-source-on-its-condition
  (handler-case
      (assemble "ldx $5,X" :machine 'instr-test-machine)
    (assembly-error (c)
      (fiveam:is (stringp (lasm-syntax-error-source c)))
      (fiveam:is (search "ldx $5,X" (lasm-syntax-error-source c))))))

(fiveam:test parse-error-carries-source-on-its-condition
  (handler-case
      (parse "ldx 1,,2")
    (parse-failure (c)
      (fiveam:is (stringp (lasm-syntax-error-source c))))))

;;; mode.lisp regression (#74): a nested :EXPR hole's own PARSE-FAILURE used
;;; to reach MATCH-OPERAND-MODE with only its message, dropping line/column.

(fiveam:test mode-match-nested-expr-failure-preserves-position
  (handler-case
      (match-operand-mode (%tokens-for "#)") 'immediate)
    (parse-failure (c)
      (fiveam:is (integerp (lasm-syntax-error-line c)))
      (fiveam:is (integerp (lasm-syntax-error-column c))))))

;;; parser.lisp: "Empty operand" now carries a position

(fiveam:test empty-operand-carries-position
  (handler-case
      (parse "ldx 1,,2")
    (parse-failure (c)
      (fiveam:is (integerp (lasm-syntax-error-line c)))
      (fiveam:is (integerp (lasm-syntax-error-column c))))))

;;; Mode-mismatch diagnostics (assembler.lisp)

(fiveam:test mode-mismatch-names-mnemonic-operand-and-accepted-modes
  (handler-case
      (progn (assemble "ldx $5,X" :machine 'instr-test-machine) (fiveam:fail "did not signal"))
    (assembly-error (c)
      (let ((msg (lasm-syntax-error-message c)))
        (fiveam:is (search "ldx" msg))
        ;; %OPERAND-TEXT joins verbatim TOKEN-TEXT -- a number token's own
        ;; TEXT omits its numeric prefix (lexer.lisp's %MATCH-NUMBER, by
        ;; design), so "$5" round-trips as "5" here, not "$5".
        (fiveam:is (search "5,X" msg))
        (fiveam:is (search "immediate" msg))
        (fiveam:is (search "#expr" msg)))
      (fiveam:is (integerp (lasm-syntax-error-column c))))))

(fiveam:test forced-suffix-mismatch-names-forced-mode-and-operand
  (handler-case
      (progn (assemble "lda.z $10,X" :machine 'instr-test-machine) (fiveam:fail "did not signal"))
    (assembly-error (c)
      (let ((msg (lasm-syntax-error-message c)))
        (fiveam:is (search "zero-page" msg))
        (fiveam:is (search "10,X" msg))))))

(fiveam:test forced-suffix-with-no-variant-on-instruction-lists-accepted-modes
  ;; LDX only declares IMMEDIATE -- ".z" (ZERO-PAGE's suffix) names a real
  ;; mode, just not one of LDX's own variants.
  (handler-case
      (progn (assemble "ldx.z #5" :machine 'instr-test-machine) (fiveam:fail "did not signal"))
    (assembly-error (c)
      (let ((msg (lasm-syntax-error-message c)))
        (fiveam:is (search "immediate" msg))
        (fiveam:is (search "#expr" msg))))))

;;; ONE-OF mode-mismatch diagnostics (#103): %MODE-SYNTAX-TEXT renders a
;;; :ONE-OF element as its alternatives' own syntax joined with "|".

(defmode diag-oo-reg expr)
(defmode diag-oo-ind "[" expr "]")
(defmode diag-oo (one-of (diag-slot diag-oo-reg diag-oo-ind)))

(definstruction diag-test-machine moo
  (modes diag-oo)
  (encoding (opcode #x03) (operand :mode))
  (semantics (set! a operand)))

(fiveam:test one-of-mode-mismatch-renders-alternatives-joined-by-pipe
  (handler-case
      (progn (assemble "moo $10,X" :machine 'diag-test-machine) (fiveam:fail "did not signal"))
    (assembly-error (c)
      (let ((msg (lasm-syntax-error-message c)))
        (fiveam:is (search "expr|[expr]" msg))))))

;;; Ambiguity warning (assembler.lisp, #74)

(fiveam:test tied-width-candidates-signal-ambiguous-mode
  (fiveam:signals ambiguous-mode
    (assemble "ambi $10" :machine 'diag-test-machine)))

(fiveam:test ambiguous-mode-warning-is-a-warning-not-an-error
  ;; Muffled (not unwound past), the assembly still completes -- WARN, not
  ;; ERROR.
  (let (warned)
    (handler-bind ((ambiguous-mode (lambda (c) (setf warned c) (muffle-warning c))))
      (let ((a (assemble "ambi $10" :machine 'diag-test-machine)))
        (fiveam:is (assembly-p a))))
    (fiveam:is (not (null warned)))
    (fiveam:is (string-equal "ambi" (ambiguous-mode-mnemonic warned)))
    (fiveam:is (eq 'diag-mode-a (mode-descriptor-name (ambiguous-mode-chosen warned))))
    (fiveam:is (member 'diag-mode-b (mapcar #'mode-descriptor-name (ambiguous-mode-alternatives warned))))))

(fiveam:test ambiguous-mode-warns-exactly-once-not-once-per-relaxation-pass
  ;; %CHOOSE-VARIANT runs once per %LAYOUT-PASS trial -- FINALP must gate the
  ;; warning so a converging multi-pass layout doesn't over-report.
  (let ((count 0))
    (handler-bind ((ambiguous-mode (lambda (c) (incf count) (muffle-warning c))))
      (assemble "ambi $10
ambi $20
ambi $30" :machine 'diag-test-machine))
    (fiveam:is (= 3 count))))

(fiveam:test zero-page-and-absolute-tie-does-not-warn
  ;; Different widths (ZERO-PAGE 1 byte, ABSOLUTE wider) -- relaxation
  ;; resolves this on its own; it is not the ambiguity the warning targets.
  (let (warned)
    (handler-bind ((ambiguous-mode (lambda (c) (setf warned c) (muffle-warning c))))
      (assemble "lda $10" :machine 'instr-test-machine))
    (fiveam:is (null warned))))

;;; ONE-OF ambiguity warning

;; Identical syntax is only legal when both alternatives declare a :SUFFIX.
(defmode diag-alt-a expr :suffix "da")
(defmode diag-alt-b expr :suffix "db")
(defmode diag-alt-paren "(" expr ")")
(defmode diag-alt-slotted (one-of (diag-alt-slot diag-alt-a diag-alt-b)))
(defmode diag-alt-pair expr "," (one-of diag-alt-a diag-alt-b))
(defmode diag-alt-specific (one-of diag-alt-a diag-alt-paren))
(defmode diag-alt-inner (one-of diag-alt-a diag-alt-b))
(defmode diag-alt-outer (one-of diag-alt-inner diag-alt-paren))

(definstruction diag-test-machine alts
  (modes diag-alt-slotted)
  (encoding (opcode #x0A) (operand :width 1))
  (semantics (set! a operand)))

(definstruction diag-test-machine altp
  (modes diag-alt-pair)
  (encoding (opcode #x0B) (operand :width 1) (operand :width 1))
  (semantics (set! a operand)))

(definstruction diag-test-machine altq
  (modes diag-alt-specific)
  (encoding (opcode #x0C) (operand :width 1))
  (semantics (set! a operand)))

(definstruction diag-test-machine alto
  (modes diag-alt-outer)
  (encoding (opcode #x0D) (operand :width 1))
  (semantics (set! a operand)))

(defmode diag-alt-forced (one-of diag-alt-a diag-alt-b) :suffix "df")

(definstruction diag-test-machine altf
  (modes diag-alt-forced)
  (encoding (opcode #x0E) (operand :width 1))
  (semantics (set! a operand)))

(defmachine diag-reg-machine
  (register r :width 8 :names (r0 r1))
  (memory ram :width 8 :addr-width 8))

(defmode diag-regind "[" (expr :register r) "]")
(defmode diag-ind "[" expr "]")
(defmode diag-reg-first (one-of diag-regind diag-ind))
(defmode diag-ind-first (one-of diag-ind diag-regind))

(definstruction diag-reg-machine ldr
  (modes diag-reg-first)
  (encoding (opcode #x01) (operand :width 1))
  (semantics nil))

(definstruction diag-reg-machine ldi
  (modes diag-ind-first)
  (encoding (opcode #x02) (operand :width 1))
  (semantics nil))

(defmode diag-paren "(" expr ")")
(defmode diag-reg-wrap (one-of diag-regind diag-paren))
(defmode diag-nested-reg (one-of diag-ind diag-reg-wrap))
(defmode diag-regind-a "[" (expr :register r) "]" :suffix "dqa")
(defmode diag-regind-b "[" (expr :register r) "]" :suffix "dqb")
(defmode diag-reg-twins (one-of diag-regind-a diag-regind-b))

(definstruction diag-reg-machine ldn
  (modes diag-nested-reg)
  (encoding (opcode #x03) (operand :width 1))
  (semantics nil))

(definstruction diag-reg-machine ldt
  (modes diag-reg-twins)
  (encoding (opcode #x04) (operand :width 1))
  (semantics nil))

(defun %alternative-warnings (source machine)
  "Every AMBIGUOUS-ALTERNATIVE assembling SOURCE signals, muffled."
  (let (warnings)
    (handler-bind ((ambiguous-alternative (lambda (c) (cl:push c warnings) (muffle-warning c))))
      (assemble source :machine machine))
    (nreverse warnings)))

(fiveam:test tied-one-of-alternatives-signal-ambiguous-alternative
  (let ((warnings (%alternative-warnings "alts 5" 'diag-test-machine)))
    (fiveam:is (= 1 (length warnings)))
    (let ((c (first warnings)))
      (fiveam:is (typep c 'ambiguous-mode))
      (fiveam:is (string-equal "alts" (ambiguous-mode-mnemonic c)))
      (fiveam:is (eq 'diag-alt-a (mode-descriptor-name (ambiguous-mode-chosen c))))
      (fiveam:is (equal '(diag-alt-b) (mapcar #'mode-descriptor-name (ambiguous-mode-alternatives c))))
      (fiveam:is (= 0 (ambiguous-alternative-hole c)))
      (fiveam:is (eq 'diag-alt-slot (ambiguous-alternative-slot c))))))

(fiveam:test ambiguous-alternative-reports-its-hole-index
  (let ((c (first (%alternative-warnings "altp 1, 2" 'diag-test-machine))))
    (fiveam:is (= 1 (ambiguous-alternative-hole c)))
    (fiveam:is (null (ambiguous-alternative-slot c)))))

(fiveam:test ambiguous-alternative-warns-once-per-statement
  (fiveam:is (= 3 (length (%alternative-warnings "alts 1
alts 2
alts 3" 'diag-test-machine)))))

(fiveam:test forced-mnemonic-suffix-still-warns-on-tied-alternative
  (fiveam:is (= 1 (length (%alternative-warnings "altf.df 5" 'diag-test-machine)))))

(fiveam:test more-literal-alternative-does-not-warn
  (fiveam:is (null (%alternative-warnings "altq (5)" 'diag-test-machine))))

(fiveam:test tie-on-losing-path-does-not-warn
  (fiveam:is (null (%alternative-warnings "alto (5)" 'diag-test-machine))))

(fiveam:test register-qualified-alternative-outranks-plain-expr
  (fiveam:is (null (%alternative-warnings "ldr [r0]" 'diag-reg-machine)))
  (fiveam:is (null (%alternative-warnings "ldr [5]" 'diag-reg-machine)))
  (fiveam:is (null (%alternative-warnings "ldi [r0]" 'diag-reg-machine)))
  (fiveam:is (null (%alternative-warnings "ldi [5]" 'diag-reg-machine))))

(fiveam:test nested-register-qualified-alternative-outranks-plain-expr
  (fiveam:is (null (%alternative-warnings "ldn [r0]" 'diag-reg-machine)))
  (fiveam:is (null (%alternative-warnings "ldn [5]" 'diag-reg-machine))))

(fiveam:test equally-register-qualified-alternatives-warn
  (let ((c (first (%alternative-warnings "ldt [r1]" 'diag-reg-machine))))
    (fiveam:is (eq 'diag-regind-a (mode-descriptor-name (ambiguous-mode-chosen c))))
    (fiveam:is (equal '(diag-regind-b)
                      (mapcar #'mode-descriptor-name (ambiguous-mode-alternatives c))))))

;;; Strict operand range (#74, absorbing #28/#43)

(fiveam:test strict-mode-out-of-range-value-signals-assembly-error
  (fiveam:signals assembly-error
    (assemble "sti #300" :machine 'diag-test-machine)))

(fiveam:test strict-mode-in-range-value-does-not-signal
  (fiveam:finishes (assemble "sti #10" :machine 'diag-test-machine)))

(fiveam:test strict-operand-range-defaults-off
  ;; #28's original example, reproduced directly: a plain (non-strict, no
  ;; global switch) single-mode instruction still wraps rather than erroring.
  (fiveam:finishes (assemble "ldx #300" :machine 'instr-test-machine)))

(fiveam:test strict-operand-range-global-switch-catches-modeless-overflow
  (let ((*strict-operand-range* t))
    (fiveam:signals assembly-error
      (assemble "ldx #300" :machine 'instr-test-machine))))

(fiveam:test strict-mode-not-chosen-as-widest-fallback-does-not-error-alone
  ;; #100000 fits neither STIW candidate's width (1 or 2 cells) -- the
  ;; widest-candidate fallback picks STIW-WIDE (not :STRICT), so the
  ;; per-mode check never even runs against DIAG-STRICT-IMM.
  (fiveam:finishes (assemble "stiw #100000" :machine 'diag-test-machine)))

(fiveam:test strict-operand-range-global-switch-still-catches-that-fallback
  (let ((*strict-operand-range* t))
    (fiveam:signals assembly-error
      (assemble "stiw #100000" :machine 'diag-test-machine))))

;;; Per-hole :STRICT on a ONE-OF alternative (#115) -- unlike :WIDTH/:SIGNED/
;;; :RELATIVE/:SUFFIX, :STRICT is a pure encode-time range check with no
;;; decode consequence, so a ONE-OF alternative may declare it independently
;;; of its siblings and of the mode as a whole (mode.lisp's
;;; %CHECK-ONE-OF-ELEMENTS!).

(defmode diag-oo-strict expr :strict t)
(defmode diag-oo-loose "[" expr "]")
(defmode diag-oo-ph (one-of diag-oo-strict diag-oo-loose))

(definstruction diag-test-machine oph
  (modes diag-oo-ph)
  (encoding (opcode #x08) (operand :width 1))
  (semantics (set! a operand)))

(fiveam:test one-of-per-hole-strict-alternative-signals-on-out-of-range-value
  (fiveam:signals assembly-error
    (assemble "oph 300" :machine 'diag-test-machine)))

(fiveam:test one-of-per-hole-strict-sibling-without-strict-still-wraps
  ;; Same instruction, same operand width -- but matched via DIAG-OO-LOOSE
  ;; (bracketed), which declares no :STRICT of its own, so the same
  ;; out-of-range value wraps instead of erroring: strictness is a property
  ;; of the matched hole's own alternative, not the whole ONE-OF.
  (fiveam:finishes (assemble "oph [300]" :machine 'diag-test-machine)))

;;; Per-hole :SIGNED on a ONE-OF alternative (#124), interacting with
;;; per-hole :STRICT (#115) -- %CHECK-STRICT-OPERAND-RANGE! (assembler.lisp)
;;; reads DESCRIPTOR's own OPERAND-SIGNEDNESS, the same source %CHOOSE-
;;; VARIANT's value filter uses, so the strict range error quotes the signed
;;; bound for a hole whose matched alternative is signed, and the unsigned
;;; one for its sibling -- never a mix of the two sources.

(defmode diag-oo-strict-signed "#" expr :strict t :signed t)
(defmode diag-oo-strict-unsigned expr :strict t)
(defmode diag-oo-signed-ph (one-of diag-oo-strict-signed diag-oo-strict-unsigned))

(definstruction diag-test-machine ophs
  (modes diag-oo-signed-ph)
  (encoding (opcode #x09)
            (operand :width 1
              (variant (choice diag-oo-strict-signed) (sub 0))
              (variant (choice diag-oo-strict-unsigned) (sub 1))))
  (semantics (set! a operand)))

(fiveam:test one-of-per-hole-signed-strict-range-quotes-signed-bound
  (handler-case (progn (assemble "ophs #-200" :machine 'diag-test-machine) (fiveam:fail "expected ASSEMBLY-ERROR"))
    (assembly-error (c) (fiveam:is (search "-128 and 127" (lasm-syntax-error-message c))))))

(fiveam:test one-of-per-hole-signed-strict-in-signed-range-does-not-signal
  (fiveam:finishes (assemble "ophs #-100" :machine 'diag-test-machine)))

(fiveam:test one-of-per-hole-signed-strict-sibling-quotes-unsigned-bound
  ;; %OPERAND-RANGE's own unsigned branch accepts the union of the signed
  ;; and unsigned ranges (-128..255 for a 1-cell operand), matching
  ;; %FITS-WIDTH-P -- unrelated to this ticket, just the pre-existing bound
  ;; an unsigned hole's strict check quotes.
  (handler-case (progn (assemble "ophs 300" :machine 'diag-test-machine) (fiveam:fail "expected ASSEMBLY-ERROR"))
    (assembly-error (c) (fiveam:is (search "-128 and 255" (lasm-syntax-error-message c))))))
