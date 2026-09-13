;;;; macro.lisp
;;;; .macro/.endm statement expansion (#33). A directive (directive.lisp, #14)
;;;; is a per-statement action with a size the assembler can compute without
;;;; evaluating anything; a macro instead captures a *range* of statements and
;;;; substitutes parameter tokens into them at each invocation site -- there
;;;; is no DEFDIRECTIVE action form that could express "collect everything up
;;;; to the matching .endm", so this lives in its own file/pass instead.
;;;;
;;;; Two phases, not one interleaved walk: %COLLECT-MACROS first strips every
;;;; .macro...endm block out of the statement list into a MACRO-DESCRIPTOR
;;;; table, then EXPAND-MACROS rewrites invocations against that table to a
;;;; fixpoint. This is what buys forward references (a macro invoked before
;;;; its own .macro...endm block, later in the same file) for free, and makes
;;;; a macro invoking another macro (nesting) fall out of the same loop
;;;; instead of needing separate handling.
;;;;
;;;; A macro body's own statements are ordinary parser.lisp STATEMENTs --
;;;; %SUBSTITUTE-STATEMENT replaces any identifier token whose text names a
;;;; parameter with that argument's token run and rebuilds both
;;;; STATEMENT-OPERAND-TOKENS (the whole uncommitted run instruction-mode
;;;; matching reads, mode.lisp) and STATEMENT-OPERANDS (the comma-split list
;;;; a directive reads, directive.lisp) from the result -- see its docstring.
;;;; Token objects are shared, not copied, across every expansion of one
;;;; macro: %QUALIFY-LOCALS! (assembler.lisp, #16) mutates ASTs, not tokens,
;;;; and %LAYOUT re-parses every statement's operands from its tokens on each
;;;; relaxation pass, so nothing downstream depends on token identity.
;;;;
;;;; No label hygiene: a body-defined label collides (the existing
;;;; "Duplicate label" ASSEMBLY-ERROR) if the macro is invoked more than once
;;;; under the same enclosing global label, or invoked more than once at
;;;; top level with a global label of its own inside the body. The documented
;;;; pattern (docs/macros.md) is a distinct global label before each
;;;; invocation, with the body using local labels scoped to it. A follow-up
;;;; ticket tracks auto-uniquifying body-defined local labels instead.

(in-package #:lasm)

;;; Conditions

(define-condition macro-error (lasm-syntax-error) ()
  (:documentation "Signalled by EXPAND-MACROS on a malformed .macro/.endm
block (unterminated .macro, orphan .endm, a nested .macro, a label on a
.macro/.endm line, a malformed parameter, a duplicate macro name, or a name
colliding with a registered directive) or a malformed invocation (wrong
argument count, or expansion that doesn't converge within
*MAX-MACRO-EXPANSION-ROUNDS*)."))

(defun %macro-error (line fmt &rest args)
  (error 'macro-error :message (apply #'format nil fmt args) :line line))

;;; Macro descriptor

(defstruct macro-descriptor
  name    ; string, upcased -- matched case-insensitively, like a directive
  params  ; list of parameter name strings, in declaration order, exact case
  body    ; list of STATEMENTs (parser.lisp) between .macro and .endm
  line)   ; source line of the .macro statement, for diagnostics

(defparameter *max-macro-expansion-rounds* 32
  "Safety cap on the number of rewrite rounds EXPAND-MACROS will run while
replacing macro-invocation statements with their substituted bodies. Each
round can only ever grow the statement list (a macro body is never empty of
effect once it contains a nested invocation), so a genuinely recursive macro
-- one that (directly or through another macro) invokes itself -- never
reaches a fixpoint and would otherwise grow the statement list without
bound; this cap turns that into a MACRO-ERROR instead of exhausting memory.
An ordinary, non-recursive nest of macros converges in at most as many
rounds as the deepest invocation chain, far below this cap.")

;;; Phase 1: collect .macro...endm blocks, leaving the rest of the program

(defun %macro-directive-p (mnemonic)
  (and mnemonic (string-equal mnemonic ".macro")))

(defun %endm-directive-p (mnemonic)
  (and mnemonic (string-equal mnemonic ".endm")))

(defun %parse-macro-header (statement)
  "STATEMENT is a .macro statement. Returns (VALUES name params), NAME the
first operand's identifier text and PARAMS the rest. Reads the raw
STATEMENT-OPERAND-TOKENS run directly, not the comma-split STATEMENT-OPERANDS
list -- \".macro loadx n\" has no comma between the name and its one
parameter, so a comma-split would lump both into a single operand; a
\".macro\" header is instead a bare whitespace- (and optionally comma-)
separated run of identifiers, a comma between two of them treated as an
optional separator and skipped. Any non-identifier, non-comma token is a
MACRO-ERROR -- as is a local-label-prefixed identifier (TOKEN-LOCALP,
lexer.lisp, #16): a macro name or parameter spelled like a local label
(e.g. \".loop\") would otherwise be indistinguishable from one at every
later lookup (FIND-DIRECTIVE-DESCRIPTOR does not see it, and a parameter
named this way would shadow a genuine local-label reference of the same
spelling in the body during substitution instead of leaving it alone)."
  (let ((tokens (statement-operand-tokens statement))
        (line (statement-line statement))
        names)
    (loop for tok across tokens
          do (cond
               ((and (eq (token-type tok) :identifier) (token-localp tok))
                (%macro-error line ".macro: name/parameter ~S cannot start with the ~
local-label prefix" (token-value tok)))
               ((eq (token-type tok) :identifier) (cl:push (token-value tok) names))
               ((eq (%punct-value tok) :comma))  ; optional separator, skipped
               (t (%macro-error line ".macro: expected a plain name, not an expression"))))
    (setf names (nreverse names))
    (when (null names)
      (%macro-error line ".macro: expected a name"))
    (values (first names) (rest names))))

(defun %collect-macros (statements)
  "Split STATEMENTS (parser.lisp) into (VALUES macros remaining), MACROS a
string -> MACRO-DESCRIPTOR hash table (name upcased) and REMAINING the
statement list with every .macro...endm block removed, in order, everything
else preserved. Signals MACRO-ERROR on an unterminated .macro (EOF before a
matching .endm), an orphan .endm, a nested .macro, a label on a .macro or
.endm line, a malformed header (%PARSE-MACRO-HEADER), a duplicate macro
name, or a name already registered as a directive (DIRECTIVE.LISP) --
.macro and .endm themselves are recognized by mnemonic text, not through
FIND-DIRECTIVE-DESCRIPTOR, so they need no entry there."
  (let ((macros (make-hash-table :test 'equal))
        remaining
        in-macro-p header-name header-key header-params header-line body)
    (dolist (statement statements)
      (let ((mnemonic (statement-mnemonic statement)))
        (cond
          ((%macro-directive-p mnemonic)
           (when in-macro-p
             (%macro-error (statement-line statement) ".macro: nested inside another .macro"))
           (when (statement-label statement)
             (%macro-error (statement-line statement) ".macro: cannot itself carry a label"))
           (multiple-value-bind (name params) (%parse-macro-header statement)
             (let ((key (string-upcase name)))
               (when (nth-value 1 (gethash key macros))
                 (%macro-error (statement-line statement) "Duplicate macro ~S" name))
               (when (find-directive-descriptor name)
                 (%macro-error (statement-line statement)
                                "Macro name ~S collides with a directive" name))
               (setf in-macro-p t header-name name header-key key header-params params
                     header-line (statement-line statement) body nil))))
          ((%endm-directive-p mnemonic)
           (unless in-macro-p
             (%macro-error (statement-line statement) ".endm without a matching .macro"))
           (when (statement-label statement)
             (%macro-error (statement-line statement) ".endm: cannot itself carry a label"))
           (setf (gethash header-key macros)
                 (make-macro-descriptor :name header-key :params header-params
                                         :body (nreverse body) :line header-line))
           (setf in-macro-p nil header-key nil))
          (in-macro-p (cl:push statement body))
          (t (cl:push statement remaining)))))
    (when in-macro-p
      (%macro-error header-line ".macro ~A has no matching .endm" header-name))
    (values macros (nreverse remaining))))

;;; Phase 2: expand invocations against the collected macro table

(defun %substitute-tokens (tokens bindings)
  "Return a fresh list of tokens: every :IDENTIFIER token in the (list of)
TOKENS whose TOKEN-VALUE is a key in BINDINGS (an alist, param name string ->
argument token list) is replaced by that argument's own token run; every
other token passes through unchanged (and shared, not copied -- see this
file's header comment)."
  (loop for tok in tokens
        for binding = (and (eq (token-type tok) :identifier)
                            (assoc (token-value tok) bindings :test #'string=))
        if binding append (copy-list (cdr binding))
        else collect tok))

(defun %substitute-statement (statement bindings)
  "Return a fresh STATEMENT (parser.lisp) with every parameter reference in
STATEMENT's operand tokens replaced per BINDINGS (%SUBSTITUTE-TOKENS).
STATEMENT's own LABEL/LABEL-LOCALP/MNEMONIC/LINE pass through unchanged --
only operand tokens are ever a parameter reference. Rebuilds both
OPERAND-TOKENS (the whole uncommitted run instruction-mode matching reads,
mode.lisp) and OPERANDS (the comma-split list a directive reads,
directive.lisp) from the substituted run, via the same %SPLIT-OPERANDS
(parser.lisp) %PARSE-LINE itself uses, so the two fields can't drift apart
the way hand-splicing each independently could."
  (let* ((substituted (%substitute-tokens (coerce (statement-operand-tokens statement) 'list)
                                           bindings))
         (operand-tokens (coerce substituted 'simple-vector)))
    (make-statement
     :label (statement-label statement) :label-localp (statement-label-localp statement)
     :mnemonic (statement-mnemonic statement)
     :operand-tokens operand-tokens
     :operands (if (plusp (length operand-tokens))
                   (mapcar (lambda (group) (make-operand :tokens (coerce group 'simple-vector)))
                           (%split-operands substituted))
                   nil)
     :line (statement-line statement))))

(defun %macro-invocation-p (statement macros)
  "Returns the MACRO-DESCRIPTOR STATEMENT invokes, or NIL -- a statement
invokes a macro when its mnemonic (case-insensitively) names one registered
in MACROS (%COLLECT-MACROS)."
  (and (statement-mnemonic statement)
       (gethash (string-upcase (statement-mnemonic statement)) macros)))

(defun %expand-invocation (statement descriptor)
  "Expand one invocation STATEMENT of macro DESCRIPTOR into a list of
statements: a leading label-only statement carrying STATEMENT's own label
(if any) -- correct even for an empty-bodied macro, and it keeps %LAYOUT's
(assembler.lisp, #16) scope threading intact exactly as an ordinary
label-bearing line would -- followed by DESCRIPTOR's body with each
statement's parameter references substituted for STATEMENT's actual
argument token runs. Signals MACRO-ERROR if STATEMENT's operand count
doesn't match DESCRIPTOR's parameter count."
  (let* ((params (macro-descriptor-params descriptor))
         (args (statement-operands statement))
         (line (statement-line statement)))
    (unless (= (length args) (length params))
      (%macro-error line "~A: expected ~D argument~:P, got ~D"
                    (macro-descriptor-name descriptor) (length params) (length args)))
    (let ((bindings (mapcar (lambda (p a) (cons p (coerce (operand-tokens a) 'list)))
                             params args))
          (label-statement
            (when (statement-label statement)
              (make-statement :label (statement-label statement)
                               :label-localp (statement-label-localp statement)
                               :line line))))
      (append (and label-statement (list label-statement))
              (mapcar (lambda (body-statement) (%substitute-statement body-statement bindings))
                      (macro-descriptor-body descriptor))))))

(defun expand-macros (statements)
  "Collect every .macro...endm block in STATEMENTS (%COLLECT-MACROS) and
replace each remaining invocation statement with its substituted body
(%EXPAND-INVOCATION), to a fixpoint -- a macro invoking another macro is
handled by the next round seeing the newly-spliced-in invocation, so nesting
needs no special handling here. Signals MACRO-ERROR (%COLLECT-MACROS,
%EXPAND-INVOCATION) on a malformed block or invocation, or if expansion
hasn't converged within *MAX-MACRO-EXPANSION-ROUNDS* rounds -- the only way
that happens is a macro invoking itself, directly or through another macro,
which would otherwise grow the statement list without bound."
  (multiple-value-bind (macros remaining) (%collect-macros statements)
    (if (zerop (hash-table-count macros))
        remaining
        (let ((result remaining))
          (dotimes (round *max-macro-expansion-rounds*)
            (if (some (lambda (s) (%macro-invocation-p s macros)) result)
                (setf result
                      (loop for statement in result
                            for descriptor = (%macro-invocation-p statement macros)
                            if descriptor
                              append (%expand-invocation statement descriptor)
                            else collect statement))
                (return-from expand-macros result)))
          (%macro-error nil "macro expansion did not converge after ~D rounds ~
(a macro invoking itself, directly or indirectly?)"
                        *max-macro-expansion-rounds*)))))
