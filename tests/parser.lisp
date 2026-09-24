;;;; tests/parser.lisp
;;;; fiveam tests for the statement grammar and expression parser (parser.lisp).

(in-package #:lasm)

(fiveam:def-suite parser :in lasm)
(fiveam:in-suite parser)

(defun %expr (string)
  "Parse STRING as a single expression using the default lexer."
  (let ((toks (tokenize string)))
    (multiple-value-bind (ast next-i) (parse-expression toks :end (1- (length toks)))
      (fiveam:is (= next-i (1- (length toks))) "expected the whole expression to be consumed")
      ast)))

;;; Statement grammar

(fiveam:test label-only-line
  (let ((stmts (parse "loop:")))
    (fiveam:is (= 1 (length stmts)))
    (fiveam:is (equal "loop" (statement-label (first stmts))))
    (fiveam:is (null (statement-mnemonic (first stmts))))))

(fiveam:test mnemonic-with-no-operands
  (let ((stmts (parse "nop")))
    (fiveam:is (= 1 (length stmts)))
    (fiveam:is (equal "nop" (statement-mnemonic (first stmts))))
    (fiveam:is (null (statement-operands (first stmts))))))

(fiveam:test label-and-mnemonic-and-operands
  (let ((stmts (parse "loop: lda #$10, x")))
    (fiveam:is (= 1 (length stmts)))
    (let ((s (first stmts)))
      (fiveam:is (equal "loop" (statement-label s)))
      (fiveam:is (equal "lda" (statement-mnemonic s)))
      (fiveam:is (= 2 (length (statement-operands s)))))))

(fiveam:test blank-and-comment-only-lines-produce-no-statement
  (let ((stmts (parse "
; just a comment

nop
")))
    (fiveam:is (= 1 (length stmts)))
    (fiveam:is (equal "nop" (statement-mnemonic (first stmts))))))

(fiveam:test multiple-statements-across-lines
  (let ((stmts (parse "start: lda a
sta b
jmp start")))
    (fiveam:is (= 3 (length stmts)))
    (fiveam:is (equal "start" (statement-label (first stmts))))
    (fiveam:is (equal "jmp" (statement-mnemonic (third stmts))))))

(fiveam:test leading-tokens-without-mnemonic-signals-parse-failure
  (fiveam:signals parse-failure (parse "42")))

(fiveam:test empty-operand-signals-parse-failure
  (fiveam:signals parse-failure (parse "lda a,,b")))

(fiveam:test comma-inside-brackets-does-not-split-operands
  ;; #103: bracket depth shares %SPLIT-OPERANDS' DEPTH counter with paren
  ;; depth, so an addressing-mode pattern's own literal "[" ... "]" (e.g. an
  ;; indirect "[" expr "," expr "]" hole) can carry a comma of its own
  ;; without STATEMENT-OPERANDS (a general statement-grammar product, unused
  ;; by mode matching itself -- see docs/modes.md) splitting on it.
  (let ((stmts (parse "foo [1,2],3")))
    (let ((s (first stmts)))
      (fiveam:is (= 2 (length (statement-operands s))))
      (fiveam:is (equalp #(:lbracket 1 :comma 2 :rbracket)
                          (map 'vector #'token-value (operand-tokens (first (statement-operands s))))))
      (fiveam:is (equalp #(3) (map 'vector #'token-value (operand-tokens (second (statement-operands s)))))))))

;;; "name = value" sugar for ".equ name, value" (#35)

(fiveam:test equals-sugar-rewrites-to-equ-mnemonic
  (let ((stmts (parse "x = 5")))
    (fiveam:is (= 1 (length stmts)))
    (let ((s (first stmts)))
      (fiveam:is (equal +assignment-directive-name+ (statement-mnemonic s)))
      (fiveam:is (= 2 (length (statement-operands s))))
      (fiveam:is (equalp #("x") (map 'vector #'token-value (operand-tokens (first (statement-operands s))))))
      (fiveam:is (equalp #(5) (map 'vector #'token-value (operand-tokens (second (statement-operands s)))))))))

(fiveam:test equals-sugar-parses-a-label-on-the-same-line
  ;; Named distinctly from tests/assembler.lisp's
  ;; EQUALS-SUGAR-KEEPS-A-LABEL-ON-THE-SAME-LINE (a behavior-level test
  ;; against ASSEMBLE) -- FiveAM test names are one flat, package-wide
  ;; table, so a repeated name silently shadows the earlier definition
  ;; instead of running both.
  (let ((stmts (parse "here: x = 5")))
    (fiveam:is (= 1 (length stmts)))
    (let ((s (first stmts)))
      (fiveam:is (equal "here" (statement-label s)))
      (fiveam:is (equal +assignment-directive-name+ (statement-mnemonic s))))))

(fiveam:test equals-sugar-value-may-be-a-full-expression
  (let* ((stmts (parse "x = 1 + 2"))
         (value-operand (second (statement-operands (first stmts)))))
    (fiveam:is (= 3 (length (operand-tokens value-operand))))))

(fiveam:test equals-with-nothing-after-it-signals-parse-failure
  (fiveam:signals parse-failure (parse "x =")))

;;; Forced addressing-mode suffix on a mnemonic (#40, e.g. "lda.w")

(fiveam:test mnemonic-suffix-splits-off-into-mode-suffix
  (let ((s (first (parse "lda.w foo"))))
    (fiveam:is (equal "lda" (statement-mnemonic s)))
    (fiveam:is (equal "w" (statement-mode-suffix s)))))

(fiveam:test mnemonic-with-no-suffix-leaves-mode-suffix-nil
  (let ((s (first (parse "lda foo"))))
    (fiveam:is (equal "lda" (statement-mnemonic s)))
    (fiveam:is (null (statement-mode-suffix s)))))

(fiveam:test no-mode-suffix-separator-leaves-dotted-mnemonic-unsplit
  (deflexer no-mode-suffix-parse-syntax
    (number-formats (:dec :default))
    (ident-chars :alnum "_."))
  (let ((s (first (parse "lda.w foo" :lexer 'no-mode-suffix-parse-syntax))))
    (fiveam:is (equal "lda.w" (statement-mnemonic s)))
    (fiveam:is (null (statement-mode-suffix s)))))

(fiveam:test equals-sugar-is-unaffected-by-mode-suffix-splitting
  ;; "x = 5" has no mnemonic identifier to split at all -- the sugar
  ;; rewrite happens before %SPLIT-MNEMONIC-SUFFIX would ever see it.
  (let ((s (first (parse "x = 5"))))
    (fiveam:is (equal +assignment-directive-name+ (statement-mnemonic s)))
    (fiveam:is (null (statement-mode-suffix s)))))

(fiveam:test mnemonic-with-dot-at-position-zero-is-not-split
  ;; A directive mnemonic (".byte") starts with the separator itself --
  ;; %SPLIT-MNEMONIC-SUFFIX requires a non-empty base to its left, so this
  ;; is left whole for the assembler's directive lookup.
  (let ((s (first (parse ".byte 1"))))
    (fiveam:is (equal ".byte" (statement-mnemonic s)))
    (fiveam:is (null (statement-mode-suffix s)))))

;;; Expression parser: precedence & associativity

(fiveam:test precedence-arithmetic
  (let ((ast (%expr "1+2*3")))
    (fiveam:is (expr-binary-p ast))
    (fiveam:is (eq :plus (expr-binary-op ast)))
    (fiveam:is (expr-binary-p (expr-binary-right ast)))
    (fiveam:is (eq :star (expr-binary-op (expr-binary-right ast))))))

(fiveam:test modulo-has-multiplicative-precedence
  (let ((ast (%expr "1+13 % 5*2")))
    (fiveam:is (eq :plus (expr-binary-op ast)))
    (fiveam:is (eq :star (expr-binary-op (expr-binary-right ast))))
    (fiveam:is (eq :percent (expr-binary-op (expr-binary-left
                                            (expr-binary-right ast)))))))

(fiveam:test modulo-is-left-associative
  (let ((ast (%expr "20 % 6 % 3")))
    (fiveam:is (eq :percent (expr-binary-op ast)))
    (fiveam:is (eq :percent (expr-binary-op (expr-binary-left ast))))))

(fiveam:test parentheses-override-precedence
  (let ((ast (%expr "(1+2)*3")))
    (fiveam:is (eq :star (expr-binary-op ast)))
    (fiveam:is (eq :plus (expr-binary-op (expr-binary-left ast))))))

(fiveam:test left-associativity
  (let ((ast (%expr "1-2-3")))
    ;; (1-2)-3, not 1-(2-3)
    (fiveam:is (eq :minus (expr-binary-op ast)))
    (fiveam:is (expr-binary-p (expr-binary-left ast)))
    (fiveam:is (expr-number-p (expr-binary-right ast)))
    (fiveam:is (= 3 (expr-number-value (expr-binary-right ast))))))

(fiveam:test bitwise-precedence-table
  (let ((ast (%expr "1|2^3&4<<5")))
    (fiveam:is (eq :pipe (expr-binary-op ast)))
    (fiveam:is (eq :caret (expr-binary-op (expr-binary-right ast))))
    (fiveam:is (eq :amp (expr-binary-op (expr-binary-right (expr-binary-right ast)))))
    (fiveam:is (eq :shl (expr-binary-op (expr-binary-right (expr-binary-right (expr-binary-right ast))))))))

(fiveam:test unary-operators
  (fiveam:is (eq :neg (expr-unary-op (%expr "-5"))))
  (fiveam:is (eq :pos (expr-unary-op (%expr "+5"))))
  (fiveam:is (eq :lognot (expr-unary-op (%expr "~5"))))
  (fiveam:is (eq :lo (expr-unary-op (%expr "<label"))))
  (fiveam:is (eq :hi (expr-unary-op (%expr ">label")))))

(fiveam:test label-reference-stays-symbolic
  (let ((ast (%expr "start")))
    (fiveam:is (expr-label-p ast))
    (fiveam:is (equal "start" (expr-label-name ast)))
    (fiveam:is (null (expr-label-localp ast)))))

(fiveam:test local-label-reference
  (let ((ast (%expr ".loop")))
    (fiveam:is (expr-label-p ast))
    (fiveam:is (expr-label-localp ast))))

(fiveam:test underscore-prefixed-identifier-is-not-local
  ;; #16: LOCALP is set from the lexer's LOCAL-LABEL-PREFIX (".", for the
  ;; default lexer), not from a "does this start with a letter" heuristic --
  ;; an identifier like "_tmp" is an ordinary global name.
  (let ((ast (%expr "_tmp")))
    (fiveam:is (expr-label-p ast))
    (fiveam:is (null (expr-label-localp ast)))))

;;; Location-counter symbol ("*", #15)

(fiveam:test star-parses-to-expr-location
  (fiveam:is (expr-location-p (%expr "*"))))

(fiveam:test configured-counter-parses-to-expr-location
  (dolist (pair '(("$+2" dollar-counter-syntax)
                  (".+2" dot-counter-syntax)))
    (let ((tokens (tokenize (first pair) :lexer (second pair))))
      (multiple-value-bind (ast next) (parse-expression tokens)
        (fiveam:is (= next (1- (length tokens))))
        (fiveam:is (expr-location-p (expr-binary-left ast)))))))

(fiveam:test star-plus-offset-parses-as-location-plus-number
  (let ((ast (%expr "*+2")))
    (fiveam:is (expr-binary-p ast))
    (fiveam:is (eq :plus (expr-binary-op ast)))
    (fiveam:is (expr-location-p (expr-binary-left ast)))
    (fiveam:is (= 2 (expr-number-value (expr-binary-right ast))))))

(fiveam:test star-still-multiplies-once-a-left-operand-exists
  (let ((ast (%expr "2*3")))
    (fiveam:is (expr-binary-p ast))
    (fiveam:is (eq :star (expr-binary-op ast)))
    (fiveam:is (= 2 (expr-number-value (expr-binary-left ast))))
    (fiveam:is (= 3 (expr-number-value (expr-binary-right ast))))))

(fiveam:test location-times-location-both-resolve
  (let ((ast (%expr "* * *")))
    (fiveam:is (expr-binary-p ast))
    (fiveam:is (eq :star (expr-binary-op ast)))
    (fiveam:is (expr-location-p (expr-binary-left ast)))
    (fiveam:is (expr-location-p (expr-binary-right ast)))))

(fiveam:test parse-expression-stops-mid-run
  (let ((toks (tokenize "1+2,3")))
    (multiple-value-bind (ast next-i) (parse-expression toks)
      (fiveam:is (= 2 (expr-number-value (expr-binary-right ast))))
      (fiveam:is (eq :comma (token-value (aref toks next-i)))))))

(fiveam:test unbalanced-parens-signal-parse-failure
  (fiveam:signals parse-failure (parse-expression (tokenize "(1+2"))))

(fiveam:test trailing-binary-operator-signals-parse-failure
  (fiveam:signals parse-failure (parse-expression (tokenize "1+"))))

(fiveam:test bank-operator-parses-to-a-unary-on-a-label
  (let ((ast (%expr "bank(foo)")))
    (fiveam:is (eq :bank (expr-unary-op ast)))
    (fiveam:is (equal "foo" (expr-label-name (expr-unary-operand ast)))))
  (let ((ast (%expr "<bank(.far)")))
    (fiveam:is (eq :lo (expr-unary-op ast)))
    (fiveam:is (eq :bank (expr-unary-op (expr-unary-operand ast)))))
  (fiveam:is (expr-binary-p (%expr "bank(a) + 1"))))

(fiveam:test bank-operator-parses-the-location-counter
  (let ((ast (%expr "bank(*)")))
    (fiveam:is (eq :bank (expr-unary-op ast)))
    (fiveam:is (expr-location-p (expr-unary-operand ast)))))

(fiveam:test bank-operator-rejects-non-labels
  (fiveam:signals parse-failure (%expr "bank(1)"))
  (fiveam:signals parse-failure (%expr "bank(a + 1)"))
  (fiveam:signals parse-failure (%expr "bank(a")))

(fiveam:test bank-operator-spelling-is-still-a-mnemonic
  (let ((statement (first (parse "bank (x)"))))
    (fiveam:is (equal "bank" (statement-mnemonic statement)))
    (fiveam:is (= 1 (length (statement-operands statement))))))
