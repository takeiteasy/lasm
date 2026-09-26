;;;; tests/mode.lisp
;;;; fiveam tests for DEFMODE and addressing-mode pattern matching (mode.lisp).

(in-package #:lasm)

(fiveam:def-suite mode :in lasm)
(fiveam:in-suite mode)

(defmode test-indexed-x expr "," "X")
(defmode test-indirect-y "(" expr ")" "," "Y")
(defmode test-no-width expr)
(defmode test-signed-imm "#" expr :width 1 :signed t)
(defmode test-hole-overrides (expr :relative nil :signed nil) ","
  (expr :relative t) "," (expr :relative nil) :relative t)
(defmode test-plus "[" expr "+" expr "]")
(defmode test-bracket "[" expr "]")
(defmode test-register "[" (expr :register bank) "]")
(defmode test-register-with-attributes "[" (expr :register bank :signed nil) "]")
(defmode test-fixed-sp "SP")
(defmode test-fixed-pc "PC")
(defmode test-fixed-or-value
  (one-of (fixed-slot test-fixed-sp test-fixed-pc test-no-width)))

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
  (fiveam:signals mode-definition-error
    (eval '(defmode bogus-relative-unsigned expr :relative t :signed nil))))

(fiveam:test expr-hole-attributes-override-mode-defaults
  (let ((mode (find-mode-descriptor 'test-hole-overrides)))
    (fiveam:is (equal '(nil t nil) (%mode-hole-attributes mode :relative)))
    (fiveam:is (equal '(nil t t) (%mode-hole-attributes mode :signed)))))

(fiveam:test expr-hole-options-combine-with-register
  (fiveam:is (equal '(:expr bank :signed nil)
                    (second (mode-descriptor-pattern
                             (find-mode-descriptor 'test-register-with-attributes))))))

(fiveam:test expr-relative-implies-signed
  (let ((mode (build-mode-descriptor 'test-hole-relative '((expr :relative t)))))
    (fiveam:is (equal '(t) (%mode-hole-attributes mode :signed)))))

(fiveam:test expr-relative-and-unsigned-signals-error
  (fiveam:signals mode-definition-error
    (eval '(defmode bogus-hole-relative (expr :relative t :signed nil))))
  (fiveam:signals mode-definition-error
    (eval '(defmode bogus-inherited-relative (expr :signed nil) :relative t))))

(defun %outer-options (name)
  (mapcar #'car (%one-of-element-options (first (mode-descriptor-pattern (find-mode-descriptor name))))))

(fiveam:test nested-one-of-records-hole-signedness
  (eval '(defmode nested-attr-signed "#" (expr :signed t)))
  (eval '(defmode nested-attr-plain "[" expr "]"))
  (eval '(defmode nested-attr-inner (one-of nested-attr-signed nested-attr-plain)))
  (eval '(defmode nested-attr-other "@" expr))
  (eval '(defmode nested-attr-outer (one-of nested-attr-inner nested-attr-other)))
  (fiveam:is-true (mode-descriptor-keyedp (find-mode-descriptor 'nested-attr-inner)))
  (fiveam:is-false (mode-descriptor-varyingp (find-mode-descriptor 'nested-attr-inner)))
  (fiveam:is (equal '((nested-attr-inner nested-attr-signed) (nested-attr-inner nested-attr-plain)
                      nested-attr-other)
                    (%outer-options 'nested-attr-outer)))
  (fiveam:is (equal '(t) (%option-hole-attributes '(nested-attr-inner nested-attr-signed) :signed)))
  (fiveam:is (equal '(nil) (%option-hole-attributes '(nested-attr-inner nested-attr-plain) :signed))))

(fiveam:test defmode-allows-literal-only-pattern
  (let ((mode (find-mode-descriptor 'test-fixed-sp)))
    (fiveam:is (equal '((:literal "SP")) (mode-descriptor-pattern mode)))
    (fiveam:is (= 0 (%mode-hole-count mode)))))

(fiveam:test named-one-of-records-zero-hole-selection
  (multiple-value-bind (asts okp choices selections)
      (try-match-operand-mode (%tokens-for "SP") 'test-fixed-or-value)
    (fiveam:is-true okp)
    (fiveam:is (null asts))
    (fiveam:is (null choices))
    (fiveam:is (equal '((fixed-slot . test-fixed-sp)) selections)))
  (multiple-value-bind (asts okp choices selections)
      (try-match-operand-mode (%tokens-for "7") 'test-fixed-or-value)
     (fiveam:is-true okp)
     (fiveam:is (= 1 (length asts)))
     (fiveam:is (equal '(test-no-width) (mapcar #'mode-descriptor-name choices)))
     (fiveam:is (equal '((fixed-slot . test-no-width)) selections))))

(fiveam:test defmode-malformed-pattern-element-signals-error
  (fiveam:signals mode-definition-error
     (eval '(defmode bogus-mode 42 expr))))

(fiveam:test plus-separates-expression-holes
  (multiple-value-bind (asts okp)
      (try-match-operand-mode (%tokens-for "[a + 4]") 'test-plus)
    (fiveam:is-true okp)
    (fiveam:is (= 2 (length asts)))
    (fiveam:is (expr-label-p (first asts)))
    (fiveam:is (= 4 (expr-number-value (second asts))))))

(fiveam:test ordinary-plus-expression-remains-one-hole
  (let ((ast (match-operand-mode (%tokens-for "[label + 2]") 'test-bracket)))
    (fiveam:is (expr-binary-p ast))
     (fiveam:is (eq :plus (expr-binary-op ast)))))

(fiveam:test register-qualified-hole-requires-machine-alias
  (let ((*register-alias-elements*
          (machine-descriptor-register-alias-elements
           (find-machine-descriptor 'test-machine))))
    (fiveam:is (match-operand-mode (%tokens-for "[bank1]") 'test-register))
    (fiveam:signals parse-failure
      (match-operand-mode (%tokens-for "[label]") 'test-register))))

(defmode test-bracket-first (one-of test-bracket test-register))
(defmode test-register-first (one-of test-register test-bracket))
(defmode test-paren "(" expr ")")
(defmode test-register-wrap (one-of test-register test-paren))
(defmode test-nested-register (one-of test-bracket test-register-wrap))

(fiveam:test register-qualified-alternative-wins-in-either-order
  (let ((*register-alias-elements*
          (machine-descriptor-register-alias-elements
           (find-machine-descriptor 'test-machine))))
    (flet ((pick (text mode)
             (mode-descriptor-name
              (first (nth-value 2 (try-match-operand-mode (%tokens-for text) mode))))))
      (fiveam:is (eq 'test-register (pick "[bank1]" 'test-bracket-first)))
      (fiveam:is (eq 'test-register (pick "[bank1]" 'test-register-first)))
      (fiveam:is (eq 'test-bracket (pick "[label]" 'test-register-first)))
      (fiveam:is (eq 'test-register-wrap (pick "[bank1]" 'test-nested-register))))))

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
  (fiveam:signals mode-definition-error
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

(defmode oo-overlap-ind "[" expr "]")
(defmode oo-overlap-idx "[" expr "+" expr "]")
(defmode oo-overlap-ind-first (one-of oo-overlap-ind oo-overlap-idx))
(defmode oo-overlap-idx-first (one-of oo-overlap-idx oo-overlap-ind))

(fiveam:test one-of-prefers-more-literals-in-either-order
  (dolist (mode '(oo-overlap-ind-first oo-overlap-idx-first))
    (multiple-value-bind (asts okp choices) (try-match-operand-mode (%tokens-for "[0 + 4]") mode)
      (fiveam:is (eq t okp))
      (fiveam:is (equal '(0 4) (mapcar #'expr-number-value asts)))
      (fiveam:is (equal '(oo-overlap-idx oo-overlap-idx)
                        (mapcar #'mode-descriptor-name choices))))
    (multiple-value-bind (asts okp choices) (try-match-operand-mode (%tokens-for "[0]") mode)
      (fiveam:is (eq t okp))
      (fiveam:is (equal '(0) (mapcar #'expr-number-value asts)))
      (fiveam:is (equal '(oo-overlap-ind) (mapcar #'mode-descriptor-name choices))))))

(defmode oo-tie-left expr "X")
(defmode oo-tie-right "X" expr)
(defmode oo-tie (one-of oo-tie-left oo-tie-right))

(fiveam:test one-of-equal-literal-count-keeps-declaration-order
  (multiple-value-bind (asts okp choices) (try-match-operand-mode (%tokens-for "X X") 'oo-tie)
    (fiveam:is (eq t okp))
    (fiveam:is (= 1 (length asts)))
    (fiveam:is (equal '(oo-tie-left) (mapcar #'mode-descriptor-name choices)))))

(defmode oo-nested-plain expr)
(defmode oo-nested-marked "X" expr)
(defmode oo-nested-specific (one-of oo-nested-plain oo-nested-marked))
(defmode oo-nested-other "Y" expr)
(defmode oo-nested-outer (one-of oo-nested-specific oo-nested-other))

(fiveam:test nested-one-of-prefers-literals-and-reports-outer-choice
  (multiple-value-bind (asts okp choices) (try-match-operand-mode (%tokens-for "X 5") 'oo-nested-outer)
    (fiveam:is (eq t okp))
    (fiveam:is (equal '(5) (mapcar #'expr-number-value asts)))
    (fiveam:is (equal '(oo-nested-specific) (mapcar #'mode-descriptor-name choices)))))

(defmode oo-prefix (one-of oo-reg oo-bt-marked))

(fiveam:test one-of-rejects-prefix-match-with-trailing-input
  (multiple-value-bind (asts okp choices) (try-match-operand-mode (%tokens-for "5 X") 'oo-prefix)
    (fiveam:is (eq t okp))
    (fiveam:is (equal '(5) (mapcar #'expr-number-value asts)))
    (fiveam:is (equal '(oo-bt-marked) (mapcar #'mode-descriptor-name choices)))))

;;; Recorded picks (#326)

(fiveam:test try-match-operand-mode-records-the-span-of-each-alternative
  (let ((picks (nth-value 7 (try-match-operand-mode (%tokens-for "X 5") 'oo-nested-outer))))
    (fiveam:is (equal '((oo-nested-marked 0 2) (oo-nested-specific 0 2)) picks)))
  (fiveam:is (equal '((oo-overlap-ind 0 3))
                    (nth-value 7 (try-match-operand-mode (%tokens-for "[5]") 'oo-overlap-ind-first))))
  (fiveam:is (null (nth-value 7 (try-match-operand-mode (%tokens-for "5") 'oo-nested-plain)))))

(defmode pk-zero-sp "sp")
(defmode pk-zero-any expr)
(defmode pk-zero-mode (one-of pk-zero-sp pk-zero-any))

(fiveam:test a-zero-hole-alternative-records-its-span
  (fiveam:is (equal '((pk-zero-sp 0 1))
                    (nth-value 7 (try-match-operand-mode (%tokens-for "sp") 'pk-zero-mode))))
  (fiveam:is (equal '((pk-zero-any 0 1))
                    (nth-value 7 (try-match-operand-mode (%tokens-for "7") 'pk-zero-mode)))))

(defmode pk-tail-any expr)
(defmode pk-tail-hash "#" expr)
(defmode pk-tail-plus (one-of pk-tail-any pk-tail-hash) "+" expr)

(fiveam:test an-alternative-ending-in-a-hole-still-yields-to-a-following-plus
  (multiple-value-bind (asts okp) (try-match-operand-mode (%tokens-for "1 + 2") 'pk-tail-plus)
    (fiveam:is (eq t okp))
    (fiveam:is (equal '(1 2) (mapcar #'expr-number-value asts)))
    (fiveam:is (equal '((pk-tail-any 0 1))
                      (nth-value 7 (try-match-operand-mode (%tokens-for "1 + 2") 'pk-tail-plus))))))

;;; ONE-OF validation errors

(fiveam:test one-of-fewer-than-two-alternatives-signals-error
  (fiveam:signals mode-definition-error
    (eval '(defmode oo-bad-single (one-of oo-reg)))))

(fiveam:test one-of-unknown-alternative-signals-error
  (fiveam:signals mode-definition-error
    (eval '(defmode oo-bad-unknown (one-of oo-reg no-such-mode)))))

(defmode oo-two-hole expr "," expr)

;; #120: alternatives of differing hole count are now accepted -- the old
;; equal-hole-count restriction is relaxed for exactly one varying ONE-OF
;; element per mode (%MODE-HOLE-TUPLES, below, expands one tuple per
;; over-count alternative).
(fiveam:test one-of-varying-hole-counts-is-accepted
  (fiveam:finishes (eval '(defmode oo-varying-holes (one-of oo-reg oo-two-hole)))))

(fiveam:test one-of-varying-marks-mode-descriptor-varyingp
  (fiveam:is-true (mode-descriptor-varyingp (find-mode-descriptor 'oo-varying-holes)))
  (fiveam:is-false (mode-descriptor-varyingp (find-mode-descriptor 'oo-two))))

(fiveam:test mode-hole-tuples-single-for-non-varying-mode
  (fiveam:is (= 1 (length (%mode-hole-tuples (find-mode-descriptor 'oo-two)))))
  (fiveam:is (null (mode-hole-tuple-groups (first (%mode-hole-tuples (find-mode-descriptor 'oo-two))))))
  (fiveam:is (equal (%mode-hole-alternatives (find-mode-descriptor 'oo-two))
                     (mode-hole-tuple-hole-alternatives (first (%mode-hole-tuples (find-mode-descriptor 'oo-two)))))))

(fiveam:test mode-hole-tuples-varying-mode-shape
  (let ((tuples (%mode-hole-tuples (find-mode-descriptor 'oo-varying-holes))))
    (fiveam:is (= 2 (length tuples)))
    (destructuring-bind (base extra) tuples
      (fiveam:is (= 1 (length (mode-hole-tuple-hole-alternatives base))))
      (fiveam:is (= 2 (length (mode-hole-tuple-hole-alternatives extra))))
      (fiveam:is (equal '(oo-reg oo-two-hole) (first (mode-hole-tuple-hole-alternatives base))))
      (fiveam:is (equal '(oo-reg oo-two-hole) (first (mode-hole-tuple-hole-alternatives extra))))
      (fiveam:is (equal '(oo-reg oo-two-hole) (second (mode-hole-tuple-hole-alternatives extra)))))))

(fiveam:test mode-hole-count-and-range-for-varying-mode
  (let ((mode (find-mode-descriptor 'oo-varying-holes)))
    (fiveam:is (= 1 (%mode-hole-count mode)))
    (multiple-value-bind (lo hi) (%mode-hole-count-range mode)
      (fiveam:is (= 1 lo))
      (fiveam:is (= 2 hi)))))

(defmode oo-three-hole expr "," expr "," expr)

(defmode oo-two-varying (one-of oo-reg oo-two-hole) "|" (one-of oo-reg oo-three-hole))

(fiveam:test multiple-varying-elements-expand-independently
  (let ((tuples (%mode-hole-tuples (find-mode-descriptor 'oo-two-varying))))
    (fiveam:is (= 4 (length tuples)))
    (fiveam:is (equal '(2 4 3 5)
                      (mapcar (lambda (tuple) (length (mode-hole-tuple-hole-alternatives tuple))) tuples)))))

(defmode nv-vary (one-of oo-reg oo-two-hole))
(defmode nv-plain "#" expr)
(defmode nv-outer (one-of (nv-slot nv-vary nv-plain)))
(defmode nv-unnamed (one-of nv-vary nv-plain))
(defmode nv-wrapped "(" (one-of oo-reg oo-two-hole) ")")
(defmode nv-wrapped-outer (one-of nv-wrapped nv-plain))
(defmode nv-deep-mid "<" expr "," (one-of oo-reg oo-two-hole) ">")
(defmode nv-deep (one-of oo-reg nv-deep-mid))
(defmode nv-deep-outer (one-of nv-deep nv-plain))
(defmode nv-signed-tail (one-of oo-reg oo-two-hole) "," (expr :signed t))
(defmode nv-signed-outer (one-of nv-signed-tail nv-plain))

(defun nv-keys (mode)
  (mapcar #'car (%one-of-element-options (first (mode-descriptor-pattern mode)))))

(defun nv-choices (text mode)
  (multiple-value-bind (asts okp choices) (try-match-operand-mode (%tokens-for text) mode)
    (declare (ignore asts))
    (fiveam:is-true okp)
    (mapcar (lambda (entry) (and entry (%choice-entry-key entry))) choices)))

(fiveam:test nested-varying-alternative-is-accepted-and-varying
  (fiveam:is-true (mode-descriptor-varyingp (find-mode-descriptor 'nv-outer)))
  (fiveam:is-true (mode-descriptor-varyingp (find-mode-descriptor 'nv-wrapped-outer))))

(fiveam:test nested-varying-option-keys-and-counts
  (fiveam:is (equal '((nv-vary oo-reg) (nv-vary oo-two-hole) nv-plain)
                    (nv-keys (find-mode-descriptor 'nv-outer))))
  (fiveam:is (equal '(1 2 1)
                    (mapcar #'cdr (%one-of-element-options
                                   (first (mode-descriptor-pattern (find-mode-descriptor 'nv-outer)))))))
  (fiveam:is (equal '((nv-deep oo-reg) (nv-deep (nv-deep-mid oo-reg)) (nv-deep (nv-deep-mid oo-two-hole)) nv-plain)
                    (nv-keys (find-mode-descriptor 'nv-deep-outer)))))

(fiveam:test nested-varying-named-slot-tuples
  (let ((tuples (%mode-hole-tuples (find-mode-descriptor 'nv-outer))))
    (fiveam:is (equal '(1 1 2)
                      (mapcar (lambda (tuple) (length (mode-hole-tuple-hole-alternatives tuple)))
                              tuples)))
    (fiveam:is (equal '((nv-vary oo-reg) nv-plain (nv-vary oo-two-hole))
                      (mapcar (lambda (tuple)
                                (mode-hole-group-alt-name (first (mode-hole-tuple-groups tuple))))
                              tuples)))))

(fiveam:test nested-varying-unnamed-tuples
  (let ((tuples (%mode-hole-tuples (find-mode-descriptor 'nv-unnamed))))
    (fiveam:is (equal '(1 2)
                      (mapcar (lambda (tuple) (length (mode-hole-tuple-hole-alternatives tuple)))
                              tuples)))
    (fiveam:is (equal '((nv-vary oo-reg) (nv-vary oo-two-hole) nv-plain)
                      (first (mode-hole-tuple-hole-alternatives (first tuples)))))))

(fiveam:test nested-varying-choices-report-the-path
  (fiveam:is (equal '((nv-vary oo-reg)) (nv-choices "5" 'nv-outer)))
  (fiveam:is (equal '((nv-vary oo-two-hole) (nv-vary oo-two-hole))
                    (nv-choices "1, 2" 'nv-outer)))
  (fiveam:is (equal '(nv-plain) (nv-choices "#5" 'nv-outer))))

(fiveam:test nested-varying-non-wrapper-alternative
  (fiveam:is (equal '((nv-wrapped oo-reg)) (nv-choices "(5)" 'nv-wrapped-outer)))
  (fiveam:is (equal '((nv-wrapped oo-two-hole) (nv-wrapped oo-two-hole))
                    (nv-choices "(1, 2)" 'nv-wrapped-outer))))

(fiveam:test nested-varying-three-levels
  (fiveam:is (equal '((nv-deep oo-reg)) (nv-choices "5" 'nv-deep-outer)))
  (fiveam:is (equal '((nv-deep (nv-deep-mid oo-reg)) (nv-deep (nv-deep-mid oo-reg)))
                    (nv-choices "<1, 2>" 'nv-deep-outer)))
  (fiveam:is (equal '((nv-deep (nv-deep-mid oo-two-hole)) (nv-deep (nv-deep-mid oo-two-hole))
                      (nv-deep (nv-deep-mid oo-two-hole)))
                    (nv-choices "<1, 2, 3>" 'nv-deep-outer)))
  (fiveam:is (= 3 (%option-hole-count '(nv-deep (nv-deep-mid oo-two-hole))))))

(fiveam:test nested-varying-hole-attributes-follow-the-path
  (let ((mode (find-mode-descriptor 'nv-signed-outer)))
    (fiveam:is (equal '(nil t)
                      (%option-hole-attributes '(nv-signed-tail oo-reg) :signed)))
    (fiveam:is (equal '(nil nil t)
                      (%option-hole-attributes '(nv-signed-tail oo-two-hole) :signed)))
    (fiveam:is (= 3 (length (mode-hole-tuple-hole-sources
                             (third (%mode-hole-tuples mode))))))))

(fiveam:test nested-varying-propagates-varyingp
  (eval '(defmode nv-pair-a (one-of oo-reg oo-two-hole)))
  (eval '(defmode nv-pair-b "@" (one-of oo-reg oo-two-hole)))
  (eval '(defmode nv-pair-outer (one-of nv-pair-a nv-pair-b)))
  (fiveam:is-true (mode-descriptor-varyingp (find-mode-descriptor 'nv-pair-outer))))

(fiveam:test redefining-an-inner-mode-updates-its-dependents-varyingp
  (eval '(defmode rv-inner "(" expr ")"))
  (eval '(defmode rv-plain "#" expr))
  (eval '(defmode rv-outer (one-of rv-inner rv-plain)))
  (flet ((counts ()
           (mapcar #'cdr (%one-of-element-options
                          (first (mode-descriptor-pattern (find-mode-descriptor 'rv-outer)))))))
    (fiveam:is-false (mode-descriptor-varyingp (find-mode-descriptor 'rv-outer)))
    (eval '(defmode rv-inner "(" expr "," expr ")"))
    (fiveam:is-true (mode-descriptor-varyingp (find-mode-descriptor 'rv-outer)))
    (fiveam:is (equal '(2 1) (counts)))
    (eval '(defmode rv-inner "(" expr ")"))
    (fiveam:is-false (mode-descriptor-varyingp (find-mode-descriptor 'rv-outer)))
    (fiveam:is (equal '(1 1) (counts)))))

(fiveam:test redefining-an-inner-one-of-updates-nested-varyingp
  (eval '(defmode rv-leaf-a "a" expr))
  (eval '(defmode rv-leaf-b "b" expr))
  (eval '(defmode rv-mid (one-of rv-leaf-a rv-leaf-b)))
  (eval '(defmode rv-top (one-of rv-mid rv-leaf-a)))
  (flet ((top-varying-p () (mode-descriptor-varyingp (find-mode-descriptor 'rv-top))))
    (fiveam:is-false (top-varying-p))
    (eval '(defmode rv-leaf-b "b" expr "," expr))
    (fiveam:is-true (mode-descriptor-varyingp (find-mode-descriptor 'rv-mid)))
    (fiveam:is-true (top-varying-p))
    (fiveam:is (equal '((rv-mid rv-leaf-a) (rv-mid rv-leaf-b) rv-leaf-a)
                      (mapcar #'car (%one-of-element-options
                                     (first (mode-descriptor-pattern (find-mode-descriptor 'rv-top)))))))
    (fiveam:is (equal '((rv-mid rv-leaf-b) (rv-mid rv-leaf-b)) (nv-choices "b 1, 2" 'rv-top)))
    (eval '(defmode rv-leaf-b "b" expr))
    (fiveam:is-false (top-varying-p))))

(defun nv-error-text (form)
  (handler-case (progn (eval form) nil)
    (error (c) (princ-to-string c))))

(fiveam:test nested-varying-rejections-do-not-cite-tickets
  (dolist (form '((defmode nv-bad-zero (one-of nv-zero-hole nv-plain))
                  (defmode nv-bad-width (one-of nv-wide nv-plain))
                  (defmode nv-bad-signed (one-of nv-signed-wrap nv-plain))
                  (defmode nv-bad-relative (one-of nv-relative-wrap nv-plain))
                  (defmode nv-bad-suffix (one-of nv-suffix-wrap nv-plain))
                  (defmode nv-bad-strict (one-of nv-strict-wrap nv-plain))))
    (let ((text (nv-error-text form)))
      (fiveam:is-true text)
      (fiveam:is (null (search "#1" text))))))

(defmode nv-two-varying (one-of oo-reg oo-two-hole) "|" (one-of oo-reg oo-three-hole))
(defmode nv-none "NONE")
(defmode nv-zero-hole (one-of nv-none oo-two-hole))
(defmode nv-wide (one-of oo-reg oo-two-hole) :width 1)
(defmode nv-signed-wrap (one-of oo-reg oo-two-hole) :signed t)
(defmode nv-relative-wrap (one-of oo-reg oo-two-hole) :relative t)
(defmode nv-suffix-wrap (one-of oo-reg oo-two-hole) :suffix "nvs")
(defmode nv-strict-wrap (one-of oo-reg oo-two-hole) :strict t)

(defmode oo-signed "@" expr :signed t)
(defmode oo-relative expr :relative t)
(defmode oo-widthed "%" expr :width 1)
(defmode oo-strict expr :strict t)
(defmode oo-suffixed expr :suffix "oo")

(fiveam:test one-of-alternative-with-signed-is-accepted
  ;; #124/#127: :SIGNED is exempt from ONE-OF's whole-mode-attribute
  ;; restriction, like :STRICT (#115) -- honored per hole once a
  ;; DEFINSTRUCTION site gives that hole a decode-time discriminator
  ;; (instruction.lisp's %CHECK-ONE-OF-SIGNED), which this DEFMODE-time
  ;; check cannot know about, so it must accept :SIGNED unconditionally.
  (fiveam:finishes (eval '(defmode oo-ok-signed (one-of oo-reg oo-signed))))
  (fiveam:is (mode-descriptor-signedp (find-mode-descriptor 'oo-signed))))

(fiveam:test one-of-alternative-with-relative-is-accepted
  ;; #130: :RELATIVE is exempt from ONE-OF's whole-mode-attribute
  ;; restriction, like :STRICT (#115), :SIGNED (#124/#127), and :WIDTH
  ;; (#129) -- honored per hole once a DEFINSTRUCTION site gives that hole a
  ;; decode-time discriminator (instruction.lisp's %CHECK-BYTE-ONE-OF-
  ;; RELATIVE), which this DEFMODE-time check cannot know about, so it must
  ;; accept :RELATIVE unconditionally.
  (fiveam:finishes (eval '(defmode oo-ok-relative (one-of oo-ind oo-relative))))
  (fiveam:is (mode-descriptor-relativep (find-mode-descriptor 'oo-relative))))

(fiveam:test one-of-alternative-with-width-is-accepted
  ;; #129: :WIDTH is exempt from ONE-OF's whole-mode-attribute restriction,
  ;; like :STRICT (#115) and :SIGNED (#124/#127) -- honored per hole once a
  ;; DEFINSTRUCTION site gives that hole a decode-time discriminator
  ;; (instruction.lisp's %CHECK-BYTE-ONE-OF-WIDTH), which this DEFMODE-time
  ;; check cannot know about, so it must accept :WIDTH unconditionally.
  (fiveam:finishes (eval '(defmode oo-ok-width (one-of oo-reg oo-widthed))))
  (fiveam:is (= 1 (mode-descriptor-width (find-mode-descriptor 'oo-widthed)))))

(fiveam:test one-of-alternative-with-strict-is-accepted
  ;; #115: :STRICT is exempt from ONE-OF's whole-mode-attribute restriction
  ;; -- it is a pure encode-time range check with no size, value, or decode
  ;; consequence, so it is meaningful (and honored) per hole regardless of
  ;; encoding scheme, unlike :WIDTH/:SIGNED/:RELATIVE/:SUFFIX above.
  (fiveam:finishes (eval '(defmode oo-ok-strict (one-of oo-ind oo-strict))))
  (fiveam:is (mode-descriptor-strictp (find-mode-descriptor 'oo-strict))))

(fiveam:test one-of-alternative-with-suffix-signals-error
  (fiveam:signals mode-definition-error
    (eval '(defmode oo-bad-suffix (one-of oo-reg oo-suffixed)))))

(defmode oo-reg-2 expr)
(defmode oo-indexed-x-lower expr "," "x")

(fiveam:test one-of-duplicate-alternative-patterns-signal-error
  (fiveam:signals mode-definition-error
    (eval '(defmode oo-bad-dup (one-of oo-reg oo-reg-2)))))

(fiveam:test one-of-duplicate-check-is-case-insensitive
  ;; A :LITERAL element matches case-insensitively at match time
  ;; (STRING-EQUAL), so two alternatives differing only in a literal's case
  ;; are the same syntax and must be rejected the same way -- EQUALP, not
  ;; EQUAL, on the pattern comparison.
  (fiveam:signals mode-definition-error
    (eval '(defmode oo-bad-dup-case (one-of test-indexed-x oo-indexed-x-lower)))))

;;; Hole-aligned CHOICES (#104) -- ONE-OF's CHOICES value grows one entry per
;;; hole, NIL for a hole not governed by any ONE-OF, rather than one entry
;;; per ONE-OF pattern element.

(defmode oo-mixed (one-of oo-reg oo-ind) "," expr)

(fiveam:test one-of-choices-hole-aligned-with-a-plain-expr-hole
  ;; OO-MIXED has two holes: the ONE-OF's own, and a plain EXPR after the
  ;; comma. CHOICES must carry one entry per hole -- the ONE-OF's chosen
  ;; alternative, then NIL for the plain EXPR hole -- not just one entry for
  ;; the ONE-OF element as the pre-#104 shape did.
  (multiple-value-bind (asts okp choices) (try-match-operand-mode (%tokens-for "5,10") 'oo-mixed)
    (fiveam:is (eq t okp))
    (fiveam:is (equal '(5 10) (mapcar #'expr-number-value asts)))
    (fiveam:is (= 2 (length choices)))
    (fiveam:is (eq 'oo-reg (mode-descriptor-name (first choices))))
    (fiveam:is (null (second choices)))))

(fiveam:test one-of-choices-length-matches-asts-length
  ;; A general invariant this ticket introduces: CHOICES is always the same
  ;; length as ASTS, for any pattern.
  (multiple-value-bind (asts okp choices) (try-match-operand-mode (%tokens-for "5,10") 'oo-two)
    (declare (ignore okp))
    (fiveam:is (= (length asts) (length choices))))
  (multiple-value-bind (asts okp choices) (try-match-operand-mode (%tokens-for "5,10") 'oo-mixed)
    (declare (ignore okp))
    (fiveam:is (= (length asts) (length choices)))))

;;; %MODE-HOLE-ALTERNATIVES (#104) -- the pattern-only, DEFINSTRUCTION-time
;;; counterpart of hole-aligned CHOICES: per hole, the ONE-OF alternative
;;; names available there, or NIL for a plain EXPR hole.

(fiveam:test mode-hole-alternatives-plain-expr-mode
  (fiveam:is (equal '(nil) (%mode-hole-alternatives (find-mode-descriptor 'oo-reg)))))

(fiveam:test mode-hole-alternatives-one-of-mode
  (fiveam:is (equal '((oo-reg oo-ind oo-lit) (oo-reg oo-ind oo-lit))
                     (%mode-hole-alternatives (find-mode-descriptor 'oo-two)))))

(fiveam:test mode-hole-alternatives-mixed-mode
  (fiveam:is (equal '((oo-reg oo-ind) nil)
                     (%mode-hole-alternatives (find-mode-descriptor 'oo-mixed)))))

;;; Nested ONE-OF (#115) -- one ONE-OF alternative's own pattern is itself
;;; another ONE-OF's whole pattern. BUILD-MODE-DESCRIPTOR's hole-count check
;;; treats a ONE-OF element like a plain EXPR (one hole, taken from its first
;;; alternative), so this is legal; %MATCH-MODE-ELEMENTS' own docstring
;;; already commits to "outermost wins" for CHOICES -- these are the tests
;;; that invariant was left owing.

(defmode no-inner-a expr)
(defmode no-inner-b "[" expr "]")
(defmode no-inner (one-of no-inner-a no-inner-b))
(defmode no-other expr)
(defmode no-outer (one-of no-inner no-other))

(fiveam:test nested-one-of-hole-count-is-one
  (fiveam:is (= 1 (%mode-hole-count (find-mode-descriptor 'no-outer)))))

(fiveam:test nested-one-of-hole-alternatives-names-the-outer-element
  (fiveam:is (equal '((no-inner no-other))
                     (%mode-hole-alternatives (find-mode-descriptor 'no-outer)))))

(fiveam:test nested-one-of-choices-report-the-outermost-alternative
  ;; "5" matches via NO-INNER's own NO-INNER-A -- but CHOICES must report
  ;; NO-OUTER's own chosen alternative, NO-INNER, discarding what the nested
  ;; match itself found.
  (multiple-value-bind (asts okp choices) (try-match-operand-mode (%tokens-for "5") 'no-outer)
    (fiveam:is (eq t okp))
    (fiveam:is (equal '(5) (mapcar #'expr-number-value asts)))
    (fiveam:is (equal '(no-inner) (mapcar #'mode-descriptor-name choices)))))

(fiveam:test nested-one-of-choices-report-the-outermost-alternative-via-inner-b
  ;; "[5]" matches via NO-INNER's own NO-INNER-B -- CHOICES still reports
  ;; NO-INNER, not NO-INNER-B: the nested match's own choice is discarded
  ;; regardless of which inner alternative actually matched.
  (multiple-value-bind (asts okp choices) (try-match-operand-mode (%tokens-for "[5]") 'no-outer)
    (fiveam:is (eq t okp))
    (fiveam:is (equal '(5) (mapcar #'expr-number-value asts)))
    (fiveam:is (equal '(no-inner) (mapcar #'mode-descriptor-name choices)))))

;;; Nested ONE-OF with a :SIGNED alternative (#124/#127) -- %MATCH-MODE-
;;; ELEMENTS' outermost-ONE-OF-wins rule means a nested alternative's own
;;; :SIGNED would never reach any hole's CHOICES entry, so %CHECK-ONE-OF-
;;; ELEMENTS! rejects it at DEFMODE time rather than silently not honoring
;;; it once a DEFINSTRUCTION site tries to use it.

(defmode no-signed-inner-a expr :signed t)
(defmode no-signed-inner-b "[" expr "]")
(defmode no-signed-inner (one-of no-signed-inner-a no-signed-inner-b))

(fiveam:test nested-one-of-with-signed-alternative-is-keyed
  (eval '(defmode no-signed-outer (one-of no-signed-inner no-other)))
  (fiveam:is (equal '((no-signed-inner no-signed-inner-a) (no-signed-inner no-signed-inner-b) no-other)
                    (%outer-options 'no-signed-outer)))
  (fiveam:is (equal '(t) (%option-hole-attributes '(no-signed-inner no-signed-inner-a) :signed)))
  (fiveam:is (equal '(nil) (%option-hole-attributes '(no-signed-inner no-signed-inner-b) :signed))))

;;; Nested ONE-OF with a :WIDTH alternative (#129) -- the same
;;; outermost-ONE-OF-wins rule means a nested alternative's own :WIDTH would
;;; never reach any hole's CHOICES entry either, so %CHECK-ONE-OF-ELEMENTS!
;;; rejects it here too.

(defmode no-widthed-inner-a expr :width 2)
(defmode no-widthed-inner-b "[" expr "]")
(defmode no-widthed-inner (one-of no-widthed-inner-a no-widthed-inner-b))

(fiveam:test nested-one-of-with-width-alternative-is-keyed
  (eval '(defmode no-widthed-outer (one-of no-widthed-inner no-other)))
  (fiveam:is (equal '(2) (%option-hole-attributes '(no-widthed-inner no-widthed-inner-a) :width)))
  (fiveam:is (equal '(nil) (%option-hole-attributes '(no-widthed-inner no-widthed-inner-b) :width)))
  (fiveam:is (equal '((no-widthed-inner no-widthed-inner-a) (no-widthed-inner no-widthed-inner-b) no-other)
                    (%outer-options 'no-widthed-outer))))

;;; Nested ONE-OF with a :RELATIVE alternative (#130) -- MODE-DESCRIPTOR-
;;; SIGNEDP is (OR RELATIVE SIGNED), so %PATTERN-NESTED-ONE-OF-SIGNED-P
;;; (mode.lisp) already catches a nested :RELATIVE alternative with no new
;;; predicate of its own; this only needed its error message widened to
;;; name :RELATIVE alongside :SIGNED, not a new check.

(defmode no-relative-inner-a expr :relative t)
(defmode no-relative-inner-b "[" expr "]")
(defmode no-relative-inner (one-of no-relative-inner-a no-relative-inner-b))

(fiveam:test nested-one-of-with-relative-alternative-is-keyed
  (eval '(defmode no-relative-outer (one-of no-relative-inner no-other)))
  (fiveam:is (equal '(t) (%option-hole-attributes '(no-relative-inner no-relative-inner-a) :relative)))
  (fiveam:is (equal '(t) (%option-hole-attributes '(no-relative-inner no-relative-inner-a) :signed)))
  (fiveam:is (equal '(nil) (%option-hole-attributes '(no-relative-inner no-relative-inner-b) :relative))))

;;; DEFMODE cycle guard (#115) -- redefining a mode some ONE-OF already
;;; references so the reference loops back to it must signal, not recurse
;;; forever. A plain file reload can't create one (it replays the same
;;; patterns in the same order); this exercises the hand-written case.

(defmode cyc-base-1 expr)
(defmode cyc-base-2 "[" expr "]")
(defmode cyc-mode-a (one-of cyc-base-1 cyc-base-2))
(defmode cyc-mode-b (one-of cyc-mode-a cyc-base-2))

(fiveam:test one-of-cycle-signals-rather-than-recursing-forever
  ;; Redefining CYC-BASE-1 to route back through CYC-MODE-B -> CYC-MODE-A ->
  ;; CYC-BASE-1 doesn't create a *live* cycle until something recomputes a
  ;; hole count through it -- REGISTER time only validates the new mode's own
  ;; alternatives (CYC-MODE-B, CYC-BASE-2), neither of which is cyclic yet at
  ;; that instant, since *MODES* isn't updated until DEFMODE's SETF returns.
  (eval '(defmode cyc-base-1 (one-of cyc-mode-b cyc-base-2)))
  (fiveam:signals error (%mode-hole-count (find-mode-descriptor 'cyc-mode-a)))
  ;; Restore CYC-BASE-1 for any test run after this one in the same image.
  (eval '(defmode cyc-base-1 expr)))

;;; Hole forcing prefixes

(defmode hp-plain expr)
(defmode hp-plain-copy expr)
(defmode hp-twin-a expr :suffix "twa")
(defmode hp-twin-b expr :suffix "twb")
(defmode hp-twins (one-of hp-twin-a hp-twin-b))

(defun %prefixed-tokens (string)
  (%collapse-hole-prefixes (%tokens-for string) 0 ":"))

(fiveam:test try-match-reports-hole-prefixes
  (multiple-value-bind (asts okp choices selections prefixes)
      (try-match-operand-mode (%prefixed-tokens "w:5") 'hp-plain)
    (declare (ignore choices selections))
    (fiveam:is-true okp)
    (fiveam:is (= 1 (length asts)))
    (fiveam:is (equal '("w") prefixes)))
  (fiveam:is (equal '(nil) (nth-value 4 (try-match-operand-mode (%prefixed-tokens "5") 'hp-plain)))))

(fiveam:test one-of-prefix-restricts-alternative
  (fiveam:is (eq 'hp-twin-b (mode-descriptor-name
                             (first (nth-value 2 (try-match-operand-mode (%prefixed-tokens "twb:5") 'hp-twins))))))
  (fiveam:is (eq 'hp-twin-a (mode-descriptor-name
                             (first (nth-value 2 (try-match-operand-mode (%prefixed-tokens "5") 'hp-twins)))))))

(fiveam:test identical-syntax-alternatives-need-suffixes
  (fiveam:signals mode-definition-error
    (eval '(defmode hp-bad-twins (one-of hp-plain hp-plain-copy))))
  (fiveam:finishes (eval '(defmode hp-ok-twins (one-of hp-twin-a hp-twin-b)))))

(defmode mz-pop "POP")
(defmode mz-idx "[" expr "," expr "]")
(defmode mz-stk (one-of mz-pop mz-idx))
(defmode mz-outer (one-of (mz-slot oo-reg mz-stk)))

(fiveam:test nested-hole-less-option-selection-carries-the-path
  (flet ((selections (text)
           (nth-value 3 (try-match-operand-mode (%tokens-for text) 'mz-outer))))
    (fiveam:is (equal '((mz-slot mz-stk mz-pop)) (selections "POP")))
    (fiveam:is (equal '((mz-slot mz-stk mz-idx)) (selections "[1, 2]")))
    (fiveam:is (equal '((mz-slot . oo-reg)) (selections "5")))))

(fiveam:test nested-hole-less-option-needs-a-named-outer-one-of
  (fiveam:is (null (nv-error-text '(defmode mz-ok (one-of (mz-ok-slot oo-reg mz-stk))))))
  (let ((text (nv-error-text '(defmode mz-bad (one-of oo-reg mz-stk)))))
    (fiveam:is-true (search "name the outer ONE-OF" text))))

;;; #221 -- attributes on the inner alternatives of a varying nested ONE-OF
;;; resolve through the path, so DEFMODE accepts them.

(defmode nv-attr-near expr :width 1 :signed t)
(defmode nv-attr-far "[" expr "," expr "]" :width 2 :strict t)
(defmode nv-attr-ind (one-of nv-attr-near nv-attr-far))

(fiveam:test nested-varying-inner-attributes-are-accepted
  (fiveam:finishes (eval '(defmode nv-attr-outer (one-of nv-attr-ind nv-plain)))))

(fiveam:test nested-varying-inner-attributes-follow-the-path
  (fiveam:is (equal '(1) (%option-hole-attributes '(nv-attr-ind nv-attr-near) :width)))
  (fiveam:is (equal '(2 2) (%option-hole-attributes '(nv-attr-ind nv-attr-far) :width)))
  (fiveam:is (equal '(t) (%option-hole-attributes '(nv-attr-ind nv-attr-near) :signed)))
  (fiveam:is (equal '(nil) (%option-hole-attributes '(nv-attr-ind nv-attr-near) :strict)))
  (fiveam:is (equal '(t t) (%option-hole-attributes '(nv-attr-ind nv-attr-far) :strict))))

;; A non-varying ONE-OF beside the varying one is keyed when its alternatives
;; differ in an attribute.
(defmode nv-attr-flat-a expr :strict t)
(defmode nv-attr-flat-b "[" expr "]")
(defmode nv-attr-flat (one-of nv-attr-flat-a nv-attr-flat-b))
(defmode nv-attr-mixed (one-of nv-attr-near nv-attr-far) "," (one-of nv-attr-flat-a nv-attr-flat-b))

(fiveam:test nested-non-varying-one-of-with-strict-alternative-is-keyed
  (eval '(defmode nv-attr-mixed-outer (one-of nv-attr-mixed nv-plain)))
  (eval '(defmode nv-attr-flat-outer (one-of nv-attr-flat nv-plain)))
  (fiveam:is (equal '((nv-attr-flat nv-attr-flat-a) (nv-attr-flat nv-attr-flat-b) nv-plain)
                    (%outer-options 'nv-attr-flat-outer)))
  (fiveam:is (= 5 (length (%outer-options 'nv-attr-mixed-outer))))
  (fiveam:is (equal '(t) (%option-hole-attributes '(nv-attr-flat nv-attr-flat-a) :strict)))
  (fiveam:is (equal '(nil) (%option-hole-attributes '(nv-attr-flat nv-attr-flat-b) :strict))))

;;; #220 -- a nested varying alternative may have several varying ONE-OFs; its
;;; options are trees with one subkey per ONE-OF.

(fiveam:test nested-several-varying-elements-are-accepted
  (fiveam:finishes (eval '(defmode nv-two-outer (one-of nv-two-varying nv-plain)))))

(fiveam:test nested-several-varying-elements-have-tree-options
  (let ((element (first (mode-descriptor-pattern (find-mode-descriptor 'nv-two-outer)))))
    (fiveam:is (equal '((nv-two-varying oo-reg oo-reg) (nv-two-varying oo-reg oo-three-hole)
                        (nv-two-varying oo-two-hole oo-reg) (nv-two-varying oo-two-hole oo-three-hole)
                        nv-plain)
                      (mapcar #'car (%one-of-element-options element))))
    (fiveam:is (equal '(2 4 3 5 1) (mapcar #'cdr (%one-of-element-options element))))))

(defmode nv-tree-pair (one-of oo-reg oo-two-hole) "TO" (one-of oo-reg oo-three-hole))
(defmode nv-tree-outer (one-of nv-tree-pair nv-plain))

(fiveam:test nested-several-varying-elements-report-the-tree
  (fiveam:is (equal '((nv-tree-pair oo-two-hole oo-reg) (nv-tree-pair oo-two-hole oo-reg)
                      (nv-tree-pair oo-two-hole oo-reg))
                    (nv-choices "1, 2 TO 3" 'nv-tree-outer)))
  (fiveam:is (equal '((nv-tree-pair oo-reg oo-three-hole) (nv-tree-pair oo-reg oo-three-hole)
                      (nv-tree-pair oo-reg oo-three-hole) (nv-tree-pair oo-reg oo-three-hole))
                    (nv-choices "1 TO 2, 3, 4" 'nv-tree-outer))))

(fiveam:test nested-several-varying-elements-count-holes-per-subkey
  (fiveam:is (= 5 (%option-hole-count '(nv-two-varying oo-two-hole oo-three-hole))))
  (fiveam:is (= 2 (%option-hole-count '(nv-two-varying oo-reg oo-reg))))
  (fiveam:is (equal '(nil nil nil nil nil)
                    (%option-hole-attributes '(nv-two-varying oo-two-hole oo-three-hole) :signed))))

(fiveam:test nested-several-varying-elements-name-slots-through-keys
  (eval '(defmode nv-slotted-pair (one-of (lhs oo-reg oo-two-hole)) "|" (one-of (rhs oo-reg oo-three-hole))))
  (let ((varying (%pattern-varying-one-of-elements
                  (mode-descriptor-pattern (find-mode-descriptor 'nv-slotted-pair)))))
    (fiveam:is (equal '(lhs rhs) (mapcar #'%one-of-slot varying))))
  (fiveam:is (eq 'oo-three-hole
                 (%key-component '(nv-slotted-pair oo-reg oo-three-hole) '(nv-slotted-pair rhs))))
  (fiveam:is (eq 'oo-reg (%key-component '(nv-slotted-pair oo-reg oo-three-hole) '(nv-slotted-pair lhs))))
  (fiveam:is (null (%key-component '(nv-slotted-pair oo-reg oo-three-hole) '(nv-slotted-pair)))))

;;; Redefinition warnings (#277)

(defun rs-stale-warnings (form)
  (let (warnings)
    (handler-bind ((stale-mode (lambda (c) (cl:push c warnings) (muffle-warning c))))
      (eval form))
    (nreverse warnings)))

(defun rs-define-modes ()
  (eval '(defmode rs-x "a" expr))
  (eval '(defmode rs-y "b" expr))
  (eval '(defmode rs-mid (one-of rs-x rs-y)))
  (eval '(defmode rs-outer (one-of rs-mid rs-x))))

(fiveam:test redefining-a-mode-with-the-same-shape-does-not-warn
  (rs-define-modes)
  (fiveam:is (null (rs-stale-warnings '(defmode rs-y "b" expr)))))

(fiveam:test redefining-a-mode-warns-about-invalidated-dependents
  (rs-define-modes)
  (let ((warnings (rs-stale-warnings '(defmode rs-y "b"))))
    (fiveam:is (equal '((rs-outer)) (mapcar #'stale-mode-dependents warnings)))
    (fiveam:is (search "RS-OUTER" (princ-to-string (first warnings)))))
  (rs-define-modes))

(fiveam:test stale-mode-warnings-name-dependents-innermost-first
  (rs-define-modes)
  (eval '(defmode rs-top (one-of rs-outer rs-x)))
  (eval '(defmode rs-y "b"))
  (fiveam:is (equal '(rs-mid rs-outer rs-top) (%mode-dependents 'rs-y)))
  (rs-define-modes))

(defmachine restale-machine
  (register pc :width 16)
  (memory ram :width 8 :addr-width 16))

(fiveam:test redefining-a-mode-warns-about-compiled-instructions
  (eval '(defmode rs-inst "(" expr ")"))
  (eval '(definstruction restale-machine rsop (modes rs-inst) (encoding (opcode 1) (operand v :width 1)) (semantics)))
  (let ((warnings (rs-stale-warnings '(defmode rs-inst "[" expr "]"))))
    (fiveam:is (= 1 (length warnings)))
    (fiveam:is (equal '((restale-machine . "RSOP")) (stale-mode-instructions (first warnings))))
    (fiveam:is (search "RSOP" (princ-to-string (first warnings))))))

;;; #275 -- a nested ONE-OF whose alternatives differ in a per-hole attribute is
;;; keyed: its pick is a subkey of the option tree, recorded by the matcher.

(defmode ka-near expr :width 1 :signed t)
(defmode ka-abs "abs" expr :width 2)
(defmode ka-ind "(" (one-of ka-near ka-abs) ")")
(defmode ka-lit "#" expr)
(defmode ka-mode (one-of ka-ind ka-lit))

(fiveam:test keyed-nested-one-of-is-not-varying
  (fiveam:is-true (mode-descriptor-keyedp (find-mode-descriptor 'ka-ind)))
  (fiveam:is-false (mode-descriptor-varyingp (find-mode-descriptor 'ka-ind)))
  (fiveam:is-false (mode-descriptor-keyedp (find-mode-descriptor 'ka-lit))))

(fiveam:test keyed-nested-one-of-options-are-trees
  (fiveam:is (equal '((ka-ind ka-near) (ka-ind ka-abs) ka-lit) (%outer-options 'ka-mode)))
  (fiveam:is (equal '(1 1 1) (mapcar #'cdr (%one-of-element-options
                                            (first (mode-descriptor-pattern
                                                    (find-mode-descriptor 'ka-mode))))))))

(fiveam:test keyed-nested-one-of-attributes-follow-the-pick
  (fiveam:is (equal '(1) (%option-hole-attributes '(ka-ind ka-near) :width)))
  (fiveam:is (equal '(2) (%option-hole-attributes '(ka-ind ka-abs) :width)))
  (fiveam:is (equal '(t) (%option-hole-attributes '(ka-ind ka-near) :signed)))
  (fiveam:is (equal '(nil) (%option-hole-attributes '(ka-ind ka-abs) :signed))))

(fiveam:test keyed-nested-one-of-pick-is-recorded-by-the-matcher
  (flet ((choices (text)
           (mapcar #'%choice-entry-key
                   (nth-value 2 (try-match-operand-mode (%tokens-for text) 'ka-mode)))))
    (fiveam:is (equal '((ka-ind ka-abs)) (choices "(abs 5)")))
    (fiveam:is (equal '((ka-ind ka-near)) (choices "(5)")))
    (fiveam:is (equal '(ka-lit) (choices "#5")))))

(defmode ag-a "a" expr :width 1)
(defmode ag-b "b" expr :width 1)
(defmode ag-inner "(" (one-of ag-a ag-b) ")")
(defmode ag-outer (one-of ag-inner ka-lit))

(fiveam:test nested-one-of-with-agreeing-attributes-is-not-keyed
  (fiveam:is-false (mode-descriptor-keyedp (find-mode-descriptor 'ag-inner)))
  (fiveam:is (equal '(ag-inner ka-lit) (%outer-options 'ag-outer))))

(defmode wf-x "x" expr)
(defmode wf-y "y" expr)
(defmode wf-wrap "[" (one-of wf-x wf-y) "]" :width 2)
(defmode wf-outer (one-of wf-wrap ka-lit))

(fiveam:test wrapper-width-applies-to-a-nested-one-ofs-holes
  (fiveam:is-false (mode-descriptor-keyedp (find-mode-descriptor 'wf-wrap)))
  (fiveam:is (equal '(2) (%option-hole-attributes 'wf-wrap :width)))
  (fiveam:is (equal '(nil) (%option-hole-attributes 'ka-lit :width))))

(fiveam:test alternative-selected-by-a-tree-cannot-declare-attributes
  (eval '(defmode kb-wrap "(" (one-of ka-near ka-abs) ")" :width 1))
  (fiveam:is (search "selected by a tree" (nv-error-text '(defmode kb-outer (one-of kb-wrap ka-lit))))))

(fiveam:test keyedp-follows-a-redefined-inner-mode
  (eval '(defmode kc-a "a" expr))
  (eval '(defmode kc-b "b" expr))
  (eval '(defmode kc-mid "(" (one-of kc-a kc-b) ")"))
  (eval '(defmode kc-top (one-of kc-mid ka-lit)))
  (fiveam:is-false (mode-descriptor-keyedp (find-mode-descriptor 'kc-mid)))
  (eval '(defmode kc-b "b" expr :signed t))
  (fiveam:is-true (mode-descriptor-keyedp (find-mode-descriptor 'kc-mid)))
  (fiveam:is (equal '((kc-mid kc-a) (kc-mid kc-b) ka-lit) (%outer-options 'kc-top)))
  (eval '(defmode kc-b "b" expr))
  (fiveam:is-false (mode-descriptor-keyedp (find-mode-descriptor 'kc-mid)))
  (fiveam:is (equal '(kc-mid ka-lit) (%outer-options 'kc-top))))

;;; #29 -- machine-local modes: (defmode (NAME (:machine M)) ...) shadows a
;;; global of the same name for M and its descendants.

(defmachine lm-a (register a :width 8) (register pc :width 16) (memory ram :width 8 :addr-width 16))
(defmachine lm-b (register a :width 8) (register pc :width 16) (memory ram :width 8 :addr-width 16))
(defmachine (lm-b-child (:extends lm-b)) (clock-speed 2))

(defmode lm-shared "<" expr ">" :width 1)
(defmode (lm-shared (:machine lm-a)) "(" expr ")" :width 1)
(defmode (lm-shared (:machine lm-b)) "[" expr "]" :width 1)

(definstruction lm-a ldv (modes lm-shared) (encoding (opcode 1) (operand :mode)) (semantics (set! a operand)))
(definstruction lm-b ldv (modes lm-shared) (encoding (opcode 2) (operand :mode)) (semantics (set! a operand)))

(fiveam:test machines-share-a-mode-name-with-different-syntax
  (fiveam:is (equalp #(1 5) (assembly-cells (assemble "ldv (5)" :machine 'lm-a))))
  (fiveam:is (equalp #(2 5) (assembly-cells (assemble "ldv [5]" :machine 'lm-b))))
  (fiveam:signals lasm-error (assemble "ldv [5]" :machine 'lm-a))
  (fiveam:signals lasm-error (assemble "ldv (5)" :machine 'lm-b)))

(fiveam:test machine-local-modes-disassemble-in-their-own-syntax
  (fiveam:is (string= "ldv ($5)" (disassembly-line-text
                                 (first (disassemble-cells #(1 5) :machine 'lm-a)))))
  (fiveam:is (string= "ldv [$5]" (disassembly-line-text
                                 (first (disassemble-cells #(2 5) :machine 'lm-b))))))

(fiveam:test machine-local-mode-leaves-the-global-mode-alone
  (fiveam:is (equal '("<" ">") (mapcar #'second (remove :expr (mode-descriptor-pattern (find-mode-descriptor 'lm-shared))
                                                        :key #'first))))
  (fiveam:is (null (mode-descriptor-machine (find-mode-descriptor 'lm-shared))))
  (fiveam:is (eq 'lm-a (mode-descriptor-machine (find-mode-descriptor 'lm-shared 'lm-a)))))

(fiveam:test machine-local-modes-are-visible-to-descendants
  (fiveam:is (eq (find-mode-descriptor 'lm-shared 'lm-b)
                 (find-mode-descriptor 'lm-shared 'lm-b-child)))
  (fiveam:is (eq (find-mode-descriptor 'lm-shared 'lm-b-child)
                 (let ((*mode-scope* 'lm-b-child)) (find-mode-descriptor 'lm-shared)))))

(defmode (lm-only-a (:machine lm-a)) "@" expr)

(fiveam:test machine-local-only-mode-is-invisible-elsewhere
  (fiveam:is (find-mode-descriptor 'lm-only-a 'lm-a))
  (fiveam:signals unknown-mode (find-mode-descriptor 'lm-only-a 'lm-b))
  (fiveam:signals unknown-mode (find-mode-descriptor 'lm-only-a)))

(defmode (lm-sfx (:machine lm-a)) expr :suffix "lmq")

(fiveam:test machine-local-suffix-is-scoped
  (fiveam:is (eq 'lm-sfx (mode-descriptor-name (find-mode-by-suffix "lmq" 'lm-a))))
  (fiveam:is (null (find-mode-by-suffix "lmq" 'lm-b)))
  (fiveam:is (null (find-mode-by-suffix "lmq" nil)))
  (fiveam:signals mode-definition-error (eval '(defmode (lm-sfx2 (:machine lm-a)) expr :suffix "lmq")))
  (eval '(defmode (lm-sfx2 (:machine lm-b)) expr :suffix "lmq"))
  (fiveam:signals mode-definition-error (eval '(defmode lm-sfx3 expr :suffix "lmq"))))

(fiveam:test defmode-machine-head-is-validated
  (fiveam:signals mode-definition-error (eval '(defmode (lm-bad (:machine no-such-machine)) "a" expr)))
  (fiveam:signals mode-definition-error (eval '(defmode (lm-bad (:machine)) "a" expr)))
  (fiveam:signals mode-definition-error (eval '(defmode (lm-bad (:machin lm-a)) "a" expr)))
  (fiveam:signals mode-definition-error (eval '(defmode (lm-bad) "a" expr))))

(defmode lm-inner "i" expr)
(defmode lm-sel "s" expr)
(defmode lm-wrap (one-of lm-inner lm-sel))
(defmode (lm-inner (:machine lm-b)) "j" expr)

(fiveam:test one-of-alternative-resolves-through-the-machine-scope
  (flet ((alternative-literal (scope)
           (let ((*mode-scope* scope))
             (second (first (mode-descriptor-pattern
                             (find-mode-descriptor (first (%one-of-alternatives
                                                           (first (mode-descriptor-pattern
                                                                   (find-mode-descriptor 'lm-wrap))))))))))))
    (fiveam:is (string= "i" (alternative-literal nil)))
    (fiveam:is (string= "j" (alternative-literal 'lm-b)))
    (fiveam:is (string= "j" (alternative-literal 'lm-b-child)))
    (fiveam:is (string= "i" (alternative-literal 'lm-a)))))

(defmachine lm-stale (register pc :width 16) (memory ram :width 8 :addr-width 16))
(defmachine lm-other (register pc :width 16) (memory ram :width 8 :addr-width 16))
(defmode (lm-st (:machine lm-stale)) "(" expr ")")
(defmode lm-st2 "(" expr ")")
(definstruction lm-stale lmst (modes lm-st) (encoding (opcode 1) (operand v :width 1)) (semantics))
(definstruction lm-other lmst (modes lm-st2) (encoding (opcode 1) (operand v :width 1)) (semantics))

(fiveam:test redefining-a-local-mode-warns-only-for-its-machines-instructions
  (let ((warnings (rs-stale-warnings '(defmode (lm-st (:machine lm-stale)) "[" expr "]"))))
    (fiveam:is (= 1 (length warnings)))
    (fiveam:is (equal '((lm-stale . "LMST")) (stale-mode-instructions (first warnings))))))

(fiveam:test shadowing-a-global-warns-about-instructions-compiled-against-it
  (let ((warnings (rs-stale-warnings '(defmode (lm-st2 (:machine lm-other)) "{" expr "}"))))
    (fiveam:is (= 1 (length warnings)))
    (fiveam:is (equal '((lm-other . "LMST")) (stale-mode-instructions (first warnings))))))
