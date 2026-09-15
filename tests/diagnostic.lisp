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
(defmode diag-oo (one-of diag-oo-reg diag-oo-ind))

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
