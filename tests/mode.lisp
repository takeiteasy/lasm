;;;; tests/mode.lisp
;;;; fiveam tests for DEFMODE and addressing-mode pattern matching (mode.lisp).

(in-package #:lasm)

(fiveam:def-suite mode :in lasm)
(fiveam:in-suite mode)

(defmode test-indexed-x expr "," "X")
(defmode test-indirect-y "(" expr ")" "," "Y")
(defmode test-no-width expr)

(defun %tokens-for (string)
  "Tokenize STRING with the default lexer and return it as a SIMPLE-VECTOR
with the trailing :EOF token dropped -- what a STATEMENT's OPERAND-TOKENS
looks like."
  (let ((tokens (tokenize string)))
    (subseq tokens 0 (1- (length tokens)))))

;;; DEFMODE registration and clause parsing

(fiveam:test defmode-registers-pattern-and-width
  (let ((m (find-mode-descriptor 'test-indexed-x)))
    (fiveam:is (eq 'test-indexed-x (mode-descriptor-name m)))
    (fiveam:is (equal '((:expr) (:literal ",") (:literal "X")) (mode-descriptor-pattern m)))
    (fiveam:is (null (mode-descriptor-width m)))))

(fiveam:test defmode-width-option
  (fiveam:is (= 1 (mode-descriptor-width (find-mode-descriptor 'immediate)))))

(fiveam:test defmode-relative-option
  (fiveam:is (mode-descriptor-relativep (find-mode-descriptor 'relative)))
  (fiveam:is (= 1 (mode-descriptor-width (find-mode-descriptor 'relative)))))

(fiveam:test defmode-relativep-defaults-nil
  (fiveam:is (null (mode-descriptor-relativep (find-mode-descriptor 'absolute)))))

(fiveam:test defmode-no-expr-hole-signals-error
  (fiveam:signals error
    (eval '(defmode bogus-mode "#"))))

(fiveam:test defmode-malformed-pattern-element-signals-error
  (fiveam:signals error
    (eval '(defmode bogus-mode 42 expr))))

(fiveam:test find-mode-descriptor-unknown-signals-error
  (fiveam:signals error
    (find-mode-descriptor 'no-such-mode)))

;;; Built-in modes: match / reject

(fiveam:test immediate-matches-hash-prefix
  (let ((ast (match-operand-mode (%tokens-for "#10") 'immediate)))
    (fiveam:is (expr-number-p ast))
    (fiveam:is (= 10 (expr-number-value ast)))))

(fiveam:test immediate-rejects-bare-expr
  (fiveam:signals parse-failure
    (match-operand-mode (%tokens-for "10") 'immediate)))

(fiveam:test zero-page-and-absolute-both-match-bare-expr
  ;; Same pattern shape -- zero-page/absolute are disambiguated by value,
  ;; not syntax; that's the assembler's job (assembler.lisp), not mode.lisp's.
  (fiveam:is (= 10 (expr-number-value (match-operand-mode (%tokens-for "10") 'zero-page))))
  (fiveam:is (= 10 (expr-number-value (match-operand-mode (%tokens-for "10") 'absolute)))))

(fiveam:test relative-matches-bare-expr-like-absolute
  ;; Same pattern shape as ABSOLUTE -- RELATIVE (#23) is distinguished by
  ;; MODE-DESCRIPTOR-RELATIVEP, not by syntax; the assembler computes the
  ;; actual offset (assembler.lisp), this file only covers pattern matching.
  (fiveam:is (= 10 (expr-number-value (match-operand-mode (%tokens-for "10") 'relative)))))

(fiveam:test indexed-x-matches-expr-comma-x
  (let ((ast (match-operand-mode (%tokens-for "$10,X") 'indexed-x)))
    (fiveam:is (expr-number-p ast))
    (fiveam:is (= #x10 (expr-number-value ast)))))

(fiveam:test indexed-x-literal-match-is-case-insensitive
  (fiveam:is (match-operand-mode (%tokens-for "$10,x") 'indexed-x)))

(fiveam:test indexed-x-rejects-missing-comma-x
  (fiveam:signals parse-failure
    (match-operand-mode (%tokens-for "$10") 'indexed-x)))

(fiveam:test indirect-y-matches-full-pattern
  (let ((ast (match-operand-mode (%tokens-for "($10),Y") 'indirect-y)))
    (fiveam:is (= #x10 (expr-number-value ast)))))

(fiveam:test match-operand-mode-trailing-token-signals-parse-failure
  (fiveam:signals parse-failure
    (match-operand-mode (%tokens-for "#10 20") 'immediate)))

;;; try-match-operand-mode: non-signalling variant

(fiveam:test try-match-operand-mode-succeeds
  (multiple-value-bind (asts okp) (try-match-operand-mode (%tokens-for "#10") 'immediate)
    (fiveam:is (eq t okp))
    (fiveam:is (= 1 (length asts)))
    (fiveam:is (= 10 (expr-number-value (first asts))))))

(fiveam:test try-match-operand-mode-fails-without-signalling
  (multiple-value-bind (asts okp) (try-match-operand-mode (%tokens-for "10") 'immediate)
    (fiveam:is (null okp))
    (fiveam:is (null asts))))

;;; match-operand-mode accepts a MODE-DESCRIPTOR directly, not just a symbol

(fiveam:test match-operand-mode-accepts-descriptor
  (let ((descriptor (find-mode-descriptor 'immediate)))
    (fiveam:is (= 10 (expr-number-value (match-operand-mode (%tokens-for "#10") descriptor))))))
