;;;; tests/lexer.lisp
;;;; fiveam tests for DEFLEXER/TOKENIZE (lexer.lisp).

(in-package #:lasm)

(fiveam:def-suite lexer :in lasm)
(fiveam:in-suite lexer)

(defun %types (tokens)
  (map 'list #'token-type tokens))

(defun %non-eof (tokens)
  (remove :eof (coerce tokens 'list) :key #'token-type))

(fiveam:test default-lexer-number-formats
  (let ((toks (%non-eof (tokenize "$ff 0x10 %101 0b11 42 'A"))))
    (fiveam:is (equal '(255 16 5 3 42 65) (mapcar #'token-value toks)))
    (fiveam:is (every (lambda (tok) (eq :number (token-type tok))) toks))))

(fiveam:test default-lexer-line-comment
  (let ((toks (%non-eof (tokenize "lda a ; a comment
sta b"))))
    (fiveam:is (equal '(:identifier :identifier :newline :identifier :identifier)
                       (mapcar #'token-type toks)))))

(fiveam:test block-comment-lexer
  (deflexer block-comment-syntax
    (comment-styles ("/*" "*/" :block))
    (number-formats (:dec :default))
    (label-suffix ":")
    (ident-chars :alnum "_"))
  (let ((toks (%non-eof (tokenize "a /* skip
this */ b" :lexer 'block-comment-syntax))))
    (fiveam:is (equal '("a" "b") (mapcar #'token-value toks)))))

(fiveam:test unterminated-block-comment-signals-lex-error
  (deflexer unterminated-block-syntax
    (comment-styles ("/*" "*/" :block))
    (number-formats (:dec :default))
    (ident-chars :alnum "_"))
  (fiveam:signals lex-error (tokenize "/* never closed" :lexer 'unterminated-block-syntax)))

(fiveam:test label-suffix-and-local-label-prefix
  (let ((toks (%non-eof (tokenize "loop: .again: nop"))))
    (fiveam:is (equal '(:identifier :label-suffix :identifier :label-suffix :identifier)
                       (%types toks)))
    (fiveam:is (equal "loop" (token-value (first toks))))
    (fiveam:is (equal ".again" (token-value (third toks))))))

(fiveam:test local-label-prefix-sets-token-localp
  ;; #16: an identifier starting with the descriptor's LOCAL-LABEL-PREFIX
  ;; (".") is flagged LOCALP; an ordinary identifier -- including one
  ;; starting with "_", which the old character-class heuristic mistook for
  ;; local -- is not.
  (let ((toks (%non-eof (tokenize "loop .loop _tmp"))))
    (fiveam:is (equal '(nil t nil) (mapcar #'token-localp toks)))))

(fiveam:test no-local-label-prefix-disables-localp
  (deflexer no-local-prefix-syntax
    (number-formats (:dec :default))
    (ident-chars :alnum "."))
  (let ((toks (%non-eof (tokenize ".loop" :lexer 'no-local-prefix-syntax))))
    (fiveam:is (null (token-localp (first toks))))))

(fiveam:test string-and-escape-literals
  (let ((toks (%non-eof (tokenize "\"hi\\nthere\""))))
    (fiveam:is (= 1 (length toks)))
    (fiveam:is (string= (format nil "hi~%there") (token-value (first toks))))))

(fiveam:test unterminated-string-signals-lex-error
  (fiveam:signals lex-error (tokenize "\"never closed")))

(fiveam:test line-continuation-joins-lines
  (let ((toks (%non-eof (tokenize "lda a \\
sta b"))))
    (fiveam:is (equal '(:identifier :identifier :identifier :identifier) (%types toks)))))

(fiveam:test ident-chars-honoured
  (let ((toks (%non-eof (tokenize "foo_bar.baz"))))
    (fiveam:is (= 1 (length toks)))
    (fiveam:is (string= "foo_bar.baz" (token-value (first toks))))))

(fiveam:test maximal-munch-on-shift-operators
  (let ((toks (%non-eof (tokenize "1<<2>>3"))))
    (fiveam:is (equal '(:shl :shr) (list (token-value (second toks)) (token-value (fourth toks)))))))

(fiveam:test equals-lexes-as-a-punctuation-token
  ;; "=" (#35, "name = value" sugar for .equ) has no expression-parser
  ;; meaning -- see parser.lisp's %BINARY-PRECEDENCE/%UNARY-OPS -- so this
  ;; only checks the lexer hands it back as one punctuator token.
  (let ((toks (%non-eof (tokenize "x = 5"))))
    (fiveam:is (eq :equals (token-value (second toks))))
    (fiveam:is (eq :punctuation (token-type (second toks))))))

(fiveam:test token-line-and-column-tracked
  (let ((toks (remove :newline (%non-eof (tokenize "a
  b")) :key #'token-type)))
    (fiveam:is (= 1 (token-line (first toks))))
    (fiveam:is (= 2 (token-line (second toks))))
    (fiveam:is (= 3 (token-column (second toks))))))

(fiveam:test lex-error-reports-line-and-column
  (handler-case
      (progn (tokenize "\"unterminated") (fiveam:fail "expected lex-error"))
    (lex-error (c)
      (fiveam:is (= 1 (lasm-syntax-error-line c))))))

(fiveam:test two-lexers-tokenize-same-source-differently
  ;; The point of DEFLEXER: a machine's own lexer changes how identical
  ;; source text tokenizes -- this is what "parameterized" actually buys.
  (deflexer percent-comment-syntax
    (comment-styles ("%" :line))
    (number-formats (:hex "$") (:dec :default))
    (ident-chars :alnum "_"))
  (let ((default-toks (%non-eof (tokenize "a %101")))
        (percent-toks (%non-eof (tokenize "a %101" :lexer 'percent-comment-syntax))))
    (fiveam:is (equal '(:identifier :number) (%types default-toks)))
    (fiveam:is (equal '(:identifier) (%types percent-toks)))))
