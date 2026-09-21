;;;; tests/mode.lisp
;;;; fiveam tests for DEFMODE and addressing-mode pattern matching (mode.lisp).

(in-package #:lasm)

(fiveam:def-suite mode :in lasm)
(fiveam:in-suite mode)

(defmode test-indexed-x expr "," "X")
(defmode test-indirect-y "(" expr ")" "," "Y")
(defmode test-no-width expr)
(defmode test-signed-imm "#" expr :width 1 :signed t)
(defmode test-plus "[" expr "+" expr "]")
(defmode test-bracket "[" expr "]")
(defmode test-register "[" (expr :register bank) "]")
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
  (fiveam:signals error
    (eval '(defmode bogus-relative-unsigned expr :relative t :signed nil))))

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
  (fiveam:signals error
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

(fiveam:test one-of-nested-varying-alternative-signals-error
  (fiveam:signals error
    (eval '(defmode oo-nest-varying (one-of oo-reg oo-varying-holes)))))

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

(fiveam:test nested-one-of-with-signed-alternative-signals-error
  (fiveam:signals error
    (eval '(defmode no-signed-outer (one-of no-signed-inner no-other)))))

;;; Nested ONE-OF with a :WIDTH alternative (#129) -- the same
;;; outermost-ONE-OF-wins rule means a nested alternative's own :WIDTH would
;;; never reach any hole's CHOICES entry either, so %CHECK-ONE-OF-ELEMENTS!
;;; rejects it here too.

(defmode no-widthed-inner-a expr :width 2)
(defmode no-widthed-inner-b "[" expr "]")
(defmode no-widthed-inner (one-of no-widthed-inner-a no-widthed-inner-b))

(fiveam:test nested-one-of-with-width-alternative-signals-error
  (fiveam:signals error
    (eval '(defmode no-widthed-outer (one-of no-widthed-inner no-other)))))

;;; Nested ONE-OF with a :RELATIVE alternative (#130) -- MODE-DESCRIPTOR-
;;; SIGNEDP is (OR RELATIVE SIGNED), so %PATTERN-NESTED-ONE-OF-SIGNED-P
;;; (mode.lisp) already catches a nested :RELATIVE alternative with no new
;;; predicate of its own; this only needed its error message widened to
;;; name :RELATIVE alongside :SIGNED, not a new check.

(defmode no-relative-inner-a expr :relative t)
(defmode no-relative-inner-b "[" expr "]")
(defmode no-relative-inner (one-of no-relative-inner-a no-relative-inner-b))

(fiveam:test nested-one-of-with-relative-alternative-signals-error
  (fiveam:signals error
    (eval '(defmode no-relative-outer (one-of no-relative-inner no-other)))))

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
