;;;; tests/mode.lisp
;;;; fiveam tests for DEFMODE and addressing-mode pattern matching (mode.lisp).

(in-package #:lasm)

(fiveam:def-suite mode :in lasm)
(fiveam:in-suite mode)

(defmode test-indexed-x expr "," "X")
(defmode test-indirect-y "(" expr ")" "," "Y")
(defmode test-no-width expr)
(defmode test-signed-imm "#" expr :width 1 :signed t)

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

;;; :SIGNED, split off :RELATIVE (#30)

(fiveam:test defmode-signed-option
  (fiveam:is (mode-descriptor-signedp (find-mode-descriptor 'test-signed-imm)))
  (fiveam:is (null (mode-descriptor-relativep (find-mode-descriptor 'test-signed-imm)))))

(fiveam:test defmode-signedp-defaults-nil
  (fiveam:is (null (mode-descriptor-signedp (find-mode-descriptor 'absolute)))))

(fiveam:test defmode-relative-implies-signed
  (fiveam:is (mode-descriptor-signedp (find-mode-descriptor 'relative))))

(fiveam:test defmode-relative-t-signed-nil-signals-error
  (fiveam:signals error
    (eval '(defmode bogus-relative-unsigned expr :relative t :signed nil))))

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

;;; STACK-RELATIVE (#50) -- "n,S", built in alongside INDEXED-X/INDIRECT-Y.

(fiveam:test stack-relative-matches-expr-comma-s
  (let ((ast (match-operand-mode (%tokens-for "1,S") 'stack-relative)))
    (fiveam:is (expr-number-p ast))
    (fiveam:is (= 1 (expr-number-value ast)))))

(fiveam:test stack-relative-literal-match-is-case-insensitive
  (fiveam:is (match-operand-mode (%tokens-for "1,s") 'stack-relative)))

(fiveam:test stack-relative-rejects-missing-comma-s
  (fiveam:signals parse-failure
    (match-operand-mode (%tokens-for "1") 'stack-relative)))

(fiveam:test stack-relative-rejects-comma-x
  (fiveam:signals parse-failure
    (match-operand-mode (%tokens-for "1,X") 'stack-relative)))

(fiveam:test stack-relative-has-no-suffix
  (fiveam:is (null (mode-descriptor-suffix (find-mode-descriptor 'stack-relative)))))

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

;;; :SUFFIX (#40) -- forced addressing-mode operand syntax

(defmode test-suffixed-mode expr :width 1 :suffix "q")

(fiveam:test defmode-suffix-option-round-trips
  (fiveam:is (string= "q" (mode-descriptor-suffix (find-mode-descriptor 'test-suffixed-mode)))))

(fiveam:test defmode-suffix-defaults-nil
  (fiveam:is (null (mode-descriptor-suffix (find-mode-descriptor 'test-indexed-x)))))

(fiveam:test built-in-zero-page-and-absolute-have-suffixes
  (fiveam:is (string= "z" (mode-descriptor-suffix (find-mode-descriptor 'zero-page))))
  (fiveam:is (string= "w" (mode-descriptor-suffix (find-mode-descriptor 'absolute)))))

(fiveam:test find-mode-by-suffix-hit
  (fiveam:is (eq (find-mode-descriptor 'test-suffixed-mode) (find-mode-by-suffix "q"))))

(fiveam:test find-mode-by-suffix-miss-returns-nil
  (fiveam:is (null (find-mode-by-suffix "no-such-suffix"))))

(fiveam:test find-mode-by-suffix-is-case-insensitive
  (fiveam:is (eq (find-mode-descriptor 'test-suffixed-mode) (find-mode-by-suffix "Q"))))

(fiveam:test redefining-same-mode-with-same-suffix-does-not-signal
  ;; A plain file reload (e.g. under ASDF) re-runs DEFMODE for the same
  ;; name -- %CHECK-SUFFIX-COLLISION must not treat a mode's own suffix as
  ;; already taken by "another" mode.
  (fiveam:finishes (eval '(defmode test-suffixed-mode expr :width 1 :suffix "q"))))

(fiveam:test different-mode-claiming-a-taken-suffix-signals-error
  (fiveam:signals error
    (eval '(defmode test-suffixed-mode-conflict expr :suffix "q"))))

;;; ONE-OF -- orthogonal per-operand addressing modes (#103)

(defmode oo-reg expr)
(defmode oo-ind "[" expr "]")
(defmode oo-lit "#" expr)
(defmode oo-two (one-of oo-reg oo-ind oo-lit) "," (one-of oo-reg oo-ind oo-lit))

(fiveam:test defmode-one-of-registers-pattern
  (let ((m (find-mode-descriptor 'oo-two)))
    (fiveam:is (equal '((:one-of oo-reg oo-ind oo-lit) (:literal ",") (:one-of oo-reg oo-ind oo-lit))
                       (mode-descriptor-pattern m)))))

(fiveam:test one-of-hole-count-is-recursive
  ;; #103: %MODE-HOLE-COUNT sums one hole per alternative's own hole count,
  ;; not one per :ONE-OF element -- OO-TWO has two holes total (one per
  ;; :ONE-OF), each alternative itself contributing exactly one.
  (fiveam:is (= 2 (%mode-hole-count (find-mode-descriptor 'oo-two)))))

(fiveam:test one-of-matches-first-alternative
  (multiple-value-bind (asts okp choices) (try-match-operand-mode (%tokens-for "5,10") 'oo-two)
    (fiveam:is (eq t okp))
    (fiveam:is (equal '(5 10) (mapcar #'expr-number-value asts)))
    (fiveam:is (equal '(oo-reg oo-reg) (mapcar #'mode-descriptor-name choices)))))

(fiveam:test one-of-matches-a-later-alternative
  (multiple-value-bind (asts okp choices) (try-match-operand-mode (%tokens-for "[5],#10") 'oo-two)
    (fiveam:is (eq t okp))
    (fiveam:is (equal '(5 10) (mapcar #'expr-number-value asts)))
    (fiveam:is (equal '(oo-ind oo-lit) (mapcar #'mode-descriptor-name choices)))))

(fiveam:test one-of-choices-independent-per-hole
  ;; Each hole picks its own alternative, orthogonally to the other -- the
  ;; ticket's whole point.
  (multiple-value-bind (asts okp choices) (try-match-operand-mode (%tokens-for "#5,[10]") 'oo-two)
    (declare (ignore asts))
    (fiveam:is (eq t okp))
    (fiveam:is (equal '(oo-lit oo-ind) (mapcar #'mode-descriptor-name choices)))))

(fiveam:test one-of-total-mismatch-fails-without-signalling
  (multiple-value-bind (asts okp choices) (try-match-operand-mode (%tokens-for "5") 'oo-two)
    (fiveam:is (null okp))
    (fiveam:is (null asts))
    (fiveam:is (null choices))))

(fiveam:test one-of-total-mismatch-signals-parse-failure
  (fiveam:signals parse-failure
    (match-operand-mode (%tokens-for "5") 'oo-two)))

;; Backtracking: an alternative that matches locally but leaves the rest of
;; the pattern unable to match must not be committed to -- the next
;; alternative is tried instead. BT-C (bare EXPR) locally matches "5" out of
;; "5 X, Y" and stops (X isn't part of an expression), but the outer
;; pattern's trailing "," "Y" then can't match starting at "X" -- so BT-D
;; (EXPR "X") must be the one chosen instead, consuming "5 X" and leaving
;; ", Y" for the rest of the pattern to match.
(defmode oo-bt-plain expr)
(defmode oo-bt-marked expr "X")
(defmode oo-bt (one-of oo-bt-plain oo-bt-marked) "," "Y")

(fiveam:test one-of-backtracks-when-first-alternative-strands-the-tail
  (multiple-value-bind (asts okp choices) (try-match-operand-mode (%tokens-for "5 X, Y") 'oo-bt)
    (fiveam:is (eq t okp))
    (fiveam:is (equal '(5) (mapcar #'expr-number-value asts)))
    (fiveam:is (equal '(oo-bt-marked) (mapcar #'mode-descriptor-name choices)))))

;;; ONE-OF validation errors

(fiveam:test one-of-fewer-than-two-alternatives-signals-error
  (fiveam:signals error
    (eval '(defmode oo-bad-single (one-of oo-reg)))))

(fiveam:test one-of-unknown-alternative-signals-error
  (fiveam:signals error
    (eval '(defmode oo-bad-unknown (one-of oo-reg no-such-mode)))))

(defmode oo-two-hole expr "," expr)

(fiveam:test one-of-mismatched-hole-counts-signals-error
  (fiveam:signals error
    (eval '(defmode oo-bad-holes (one-of oo-reg oo-two-hole)))))

(defmode oo-signed expr :signed t)
(defmode oo-relative expr :relative t)
(defmode oo-widthed expr :width 1)
(defmode oo-strict expr :strict t)
(defmode oo-suffixed expr :suffix "oo")

(fiveam:test one-of-alternative-with-signed-signals-error
  (fiveam:signals error
    (eval '(defmode oo-bad-signed (one-of oo-reg oo-signed)))))

(fiveam:test one-of-alternative-with-relative-signals-error
  (fiveam:signals error
    (eval '(defmode oo-bad-relative (one-of oo-reg oo-relative)))))

(fiveam:test one-of-alternative-with-width-signals-error
  (fiveam:signals error
    (eval '(defmode oo-bad-width (one-of oo-reg oo-widthed)))))

(fiveam:test one-of-alternative-with-strict-signals-error
  (fiveam:signals error
    (eval '(defmode oo-bad-strict (one-of oo-reg oo-strict)))))

(fiveam:test one-of-alternative-with-suffix-signals-error
  (fiveam:signals error
    (eval '(defmode oo-bad-suffix (one-of oo-reg oo-suffixed)))))

(defmode oo-reg-2 expr)
(defmode oo-indexed-x-lower expr "," "x")

(fiveam:test one-of-duplicate-alternative-patterns-signal-error
  (fiveam:signals error
    (eval '(defmode oo-bad-dup (one-of oo-reg oo-reg-2)))))

(fiveam:test one-of-duplicate-check-is-case-insensitive
  ;; A :LITERAL element matches case-insensitively at match time
  ;; (STRING-EQUAL), so two alternatives differing only in a literal's case
  ;; are the same syntax and must be rejected the same way -- EQUALP, not
  ;; EQUAL, on the pattern comparison.
  (fiveam:signals error
    (eval '(defmode oo-bad-dup-case (one-of test-indexed-x oo-indexed-x-lower)))))
