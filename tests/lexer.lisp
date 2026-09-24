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

(deflexer dollar-counter-syntax
  (number-formats (:hex "$" "0x") (:dec :default))
  (label-suffix ":")
  (local-label-prefix ".")
  (ident-chars :alnum "_.")
  (location-counter "$"))

(deflexer dot-counter-syntax
  (number-formats (:dec :default))
  (label-suffix ":")
  (local-label-prefix ".")
  (ident-chars :alnum "_.")
  (location-counter "."))

(fiveam:test location-counter-alias-preserves-number-and-identifier-tokens
  (let ((dollar (%non-eof (tokenize "$ $FF *" :lexer 'dollar-counter-syntax)))
        (dot (%non-eof (tokenize ". .loop" :lexer 'dot-counter-syntax))))
    (fiveam:is (equal '(:location-counter :number :punctuation) (%types dollar)))
    (fiveam:is (= 255 (token-value (second dollar))))
    (fiveam:is (equal '(:location-counter :identifier) (%types dot)))
    (fiveam:is (string= ".loop" (token-value (second dot))))))

(fiveam:test location-counter-alias-rejects-operators
  (fiveam:signals error
    (eval '(deflexer invalid-counter-syntax (location-counter "+"))))
  (fiveam:signals error
    (eval '(deflexer missing-counter-syntax (location-counter nil)))))

(fiveam:test percent-literal-and-operator-tokens
  (let ((toks (%non-eof (tokenize "%101 13 % 5 13%2 %"))))
    (fiveam:is (equal '(5 13 :percent 5 13 :percent 2 :percent)
                       (mapcar #'token-value toks)))
    (fiveam:is (eq :punctuation (token-type (third toks))))
    (fiveam:is (eq :number (token-type (first toks))))))

(fiveam:test percent-prefixed-binary-digit-keeps-literal-priority
  (let ((toks (%non-eof (tokenize "a %1"))))
    (fiveam:is (equal '(:identifier :number) (%types toks)))
    (fiveam:is (= 1 (token-value (second toks))))))

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

(fiveam:test brackets-lex-as-punctuation-tokens
  ;; "[" / "]" (#103) exist purely so an addressing-mode pattern (defmode,
  ;; mode.lisp) has tokens to match an indirect "[" expr "]" operand form
  ;; against -- same rationale as :HASH/:EQUALS above, so this only checks
  ;; the lexer hands them back as their own punctuator tokens.
  (let ((toks (%non-eof (tokenize "[5]"))))
    (fiveam:is (equal '(:punctuation :number :punctuation) (%types toks)))
    (fiveam:is (eq :lbracket (token-value (first toks))))
    (fiveam:is (eq :rbracket (token-value (third toks))))))

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

;;; MODE-SUFFIX-SEPARATOR (#40)

(fiveam:test default-lexer-mode-suffix-separator-is-dot
  (fiveam:is (string= "." (lexer-descriptor-mode-suffix-separator (find-lexer-descriptor 'default)))))

(fiveam:test mode-suffix-separator-not-in-ident-chars-signals-error
  (fiveam:signals error
    (eval '(deflexer bogus-suffix-separator-syntax
             (number-formats (:dec :default))
             (ident-chars :alnum "_")
             (mode-suffix-separator "/")))))

(fiveam:test mode-suffix-separator-defaults-to-nil
  (deflexer no-mode-suffix-syntax
    (number-formats (:dec :default))
    (ident-chars :alnum "_"))
  (fiveam:is (null (lexer-descriptor-mode-suffix-separator (find-lexer-descriptor 'no-mode-suffix-syntax)))))

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

(fiveam:test hole-prefix-separator-validation
  (fiveam:is (equal ":" (lexer-descriptor-hole-prefix-separator (find-lexer-descriptor 'default))))
  (fiveam:signals error
    (build-lexer-descriptor 'bad-prefix '((ident-chars :alnum "_") (hole-prefix-separator "_"))))
  (fiveam:signals error
    (build-lexer-descriptor 'bad-prefix '((label-suffix ":") (hole-prefix-separator "@"))))
  (fiveam:is (null (lexer-descriptor-hole-prefix-separator
                    (build-lexer-descriptor 'no-prefix '((label-suffix ":")))))))

(fiveam:test function-operator-lexes-only-before-a-paren
  (fiveam:is (equal '(:function-operator :punctuation :identifier :punctuation)
                    (%types (%non-eof (tokenize "bank(x)")))))
  (fiveam:is (equal '(:function-operator :punctuation :identifier :punctuation)
                    (%types (%non-eof (tokenize "BANK (x)")))))
  (fiveam:is (equal '(:function-operator :punctuation :number :punctuation)
                    (%types (%non-eof (tokenize "lowcell(1)")))))
  (fiveam:is (equal '(:function-operator :punctuation :number :punctuation)
                    (%types (%non-eof (tokenize "HighCell(1)")))))
  (fiveam:is (equal '(:identifier) (%types (%non-eof (tokenize "lowcell")))))
  (fiveam:is (equal '(:identifier :punctuation)
                    (%types (%non-eof (tokenize "bank +")))))
  (fiveam:is (equal '(:identifier :label-suffix)
                    (%types (%non-eof (tokenize "bank:")))))
  (fiveam:is (equal '(:identifier :punctuation :identifier :punctuation)
                    (%types (%non-eof (tokenize "banks(x)"))))))

(fiveam:test function-operators-clause
  (fiveam:is (equal '(("bank" . :bank) ("lowcell" . :lowcell) ("highcell" . :highcell)
                     ("defined" . :defined) ("mem" . :mem))
                    (lexer-descriptor-function-operators (find-lexer-descriptor 'default))))
  (deflexer far-syntax
    (number-formats (:dec :default))
    (ident-chars :alnum "_")
    (function-operators ("far" :bank) ("lo" :lowcell)))
  (fiveam:is (equal '(:function-operator :punctuation :identifier :punctuation)
                    (%types (%non-eof (tokenize "far(x)" :lexer 'far-syntax)))))
  (fiveam:is (equal '(:lowcell)
                    (subseq (mapcar #'token-value (%non-eof (tokenize "lo(x)" :lexer 'far-syntax))) 0 1)))
  (fiveam:is (equal '(:identifier :punctuation :identifier :punctuation)
                    (%types (%non-eof (tokenize "bank(x)" :lexer 'far-syntax)))))
  (deflexer no-bank-syntax
    (number-formats (:dec :default))
    (ident-chars :alnum "_"))
  (fiveam:is (null (lexer-descriptor-function-operators (find-lexer-descriptor 'no-bank-syntax))))
  (fiveam:is (equal '(:identifier :punctuation :identifier :punctuation)
                    (%types (%non-eof (tokenize "bank(x)" :lexer 'no-bank-syntax)))))
  (fiveam:signals error
    (build-lexer-descriptor 'bad-op '((ident-chars :alnum "_") (function-operators ("1st" :bank)))))
  (fiveam:signals error
    (build-lexer-descriptor 'bad-op '((ident-chars :alnum "_") (function-operators ("a-b" :bank)))))
  (fiveam:signals error
    (build-lexer-descriptor 'bad-op '((function-operators ("x" :nonsense)))))
  (fiveam:signals error
    (build-lexer-descriptor 'bad-op '((function-operators ("x" :bank) ("X" :lowcell))))))

(fiveam:test comparison-punctuators-use-maximal-munch
  (fiveam:is (equal '(:le :ge :eq :ne :lt :gt :equals :shl :shr)
                    (mapcar #'token-value (%non-eof (tokenize "<= >= == != < > = << >>")))))
  (fiveam:is (equal '(:lt :equals 1)
                    (subseq (mapcar #'token-value (%non-eof (tokenize "< = 1"))) 0 3))))

(fiveam:test logical-punctuators-use-maximal-munch
  (fiveam:is (equal '(:andand :oror :bang :ne :amp :pipe :bang)
                    (mapcar #'token-value (%non-eof (tokenize "&& || ! != & | !"))))))

(fiveam:test location-counter-cannot-shadow-a-punctuator
  (fiveam:signals error
    (build-lexer-descriptor 'bad-lc '((location-counter "!"))))
  (fiveam:signals error
    (build-lexer-descriptor 'bad-lc '((location-counter "<="))))
  (fiveam:finishes
    (build-lexer-descriptor 'ok-lc '((location-counter "$")))))
