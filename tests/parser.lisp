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

;;; Expression parser: precedence & associativity

(fiveam:test precedence-arithmetic
  (let ((ast (%expr "1+2*3")))
    (fiveam:is (expr-binary-p ast))
    (fiveam:is (eq :plus (expr-binary-op ast)))
    (fiveam:is (expr-binary-p (expr-binary-right ast)))
    (fiveam:is (eq :star (expr-binary-op (expr-binary-right ast))))))

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
