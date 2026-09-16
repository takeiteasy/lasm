;;;; mode.lisp
;;;; DEFMODE: a declarative addressing-mode grammar (LASM-plan.md sec. 3.4).
;;;; Each mode is a literal/token pattern with one or more `expr` holes and an
;;;; optional default operand :WIDTH; matching a mode against a statement's
;;;; operand tokens consumes its literal tokens and parses each `expr` hole
;;;; with the shared Pratt parser (parser.lisp).
;;;;
;;;; This replaces M1's *BUILTIN-MODE-PREFIXES* table (formerly in
;;;; instruction.lisp) -- IMMEDIATE and ABSOLUTE are now ordinary DEFMODE
;;;; forms defined at the bottom of this file, rather than a special case
;;;; MATCH-OPERAND-MODE has to know about.
;;;;
;;;; Registration happens inside an EVAL-WHEN, like DEFMACHINE (machine.lisp):
;;;; DEFINSTRUCTION resolves mode names against this registry at
;;;; macroexpansion time, so a mode must be visible as soon as its DEFMODE
;;;; form is compiled, not only after the file loads.
;;;;
;;;; Modes are a global registry (mirroring *LEXERS*, lexer.lisp), not scoped
;;;; per machine -- a mode is a syntax concept, like a lexer's surface syntax,
;;;; not part of any one machine's storage model. Two machines wanting the
;;;; same mode name with different syntax is a follow-up concern.

(in-package #:lasm)

;;; Mode descriptor
;;;
;;; Wrapped in an EVAL-WHEN, like the DEFVAR below, so MAKE-MODE-DESCRIPTOR
;;; is callable at :COMPILE-TOPLEVEL time from this same file's built-in
;;; DEFMODE forms.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct mode-descriptor
    name       ; symbol, upcased on lookup like instruction mnemonics
    pattern    ; list of (:literal "text") | (:expr), in match order
    width      ; default operand byte width, or nil (caller/machine decides)
    relativep  ; T if this mode's operand is a PC-relative offset (#23), not
               ; an absolute value -- the assembler computes the offset from
               ; the branch's own address at encode time (assembler.lisp).
               ; Implies SIGNEDP (below); a RELATIVE mode is always signed,
               ; since a branch offset can go either direction.
    signedp    ; T if this mode's operand is a signed quantity (#30, split off
               ; RELATIVE): the emulator sign-extends the fetched operand
               ; (emulator.lisp) before handing it to semantics, and the
               ; assembler's mode selector range-checks candidate values
               ; against the signed range rather than the unsigned one
               ; (assembler.lisp's %CHOOSE-VARIANT).
    suffix     ; string, or nil -- a gas-style mnemonic suffix (e.g. "w" for
               ; ABSOLUTE, "z" for ZERO-PAGE, #40) a program can append to a
               ; mnemonic (lda.w target) to force this mode regardless of
               ; what the operand's value folds to, bypassing relaxation's
               ; floor and value filters entirely (assembler.lisp's
               ; %CHOOSE-VARIANT). Not every mode needs one -- only modes
               ; that share operand syntax with another mode (so relaxation
               ; has an actual choice to override) benefit from a suffix;
               ; LASM's built-ins give one only to ZERO-PAGE/ABSOLUTE.
    strictp)   ; T if an operand encoded through this mode that doesn't fit
               ; its own width is an ASSEMBLY-ERROR rather than silently
               ; wrapping (#74, absorbing #28/#43) -- checked at encode time
               ; by %ENCODE's :INSTRUCTION branch (assembler.lisp), alongside
               ; the *STRICT-OPERAND-RANGE* global switch (diagnostic.lisp)
               ; that makes every mode strict, including a mode-less
               ; instruction's bare operand. Default NIL preserves #28/#43's
               ; original wrap-on-overflow behavior.
  )

;; Registry of defined addressing modes, keyed by name -- mirrors *LEXERS*
;; (lexer.lisp) and *MACHINES* (storage.lisp). Unlike those two, this needs
;; an EVAL-WHEN around its own DEFVAR: DEFMODE's registration runs at
;; :COMPILE-TOPLEVEL (below), and this file defines its own built-in modes
;; (IMMEDIATE, ABSOLUTE, ...) at the bottom of this same file -- a plain
;; top-level DEFVAR's initial value is only guaranteed set at load time, which
;; is too late for a DEFMODE compiled later in the same compilation unit.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *modes* (make-hash-table :test 'eq)))

(defun find-mode-descriptor (name)
  "Look up the MODE-DESCRIPTOR registered under NAME (a symbol) with DEFMODE.
Signals an error if none is registered."
  (or (gethash name *modes*)
      (error "No addressing mode named ~S has been defined with DEFMODE" name)))

;; Both wrapped in an EVAL-WHEN, like BUILD-MODE-DESCRIPTOR itself (below) --
;; BUILD-MODE-DESCRIPTOR calls %CHECK-SUFFIX-COLLISION, which calls
;; FIND-MODE-BY-SUFFIX, at :COMPILE-TOPLEVEL time for this same file's own
;; built-in DEFMODE forms (bottom of this file), so a plain DEFUN (only
;; guaranteed callable at load time) would be too late.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun find-mode-by-suffix (suffix)
    "Look up the MODE-DESCRIPTOR whose :SUFFIX (a string, #40) equals SUFFIX
case-insensitively, or NIL if none declares one. A linear scan over *MODES*
rather than a second suffix -> descriptor table -- there are only a handful
of modes registered at once, and a parallel table would need its own
invalidation whenever a DEFMODE is redefined dropping (or changing) its
suffix. Used by the assembler's forced-mode operand syntax (assembler.lisp's
%CHOOSE-VARIANT) to resolve e.g. \"w\" in \"lda.w\" to ABSOLUTE."
    (loop for mode being the hash-values of *modes*
          when (and (mode-descriptor-suffix mode)
                    (string-equal (mode-descriptor-suffix mode) suffix))
            return mode))

  (defun %check-suffix-collision (name suffix)
    "Signal an error if SUFFIX (non-NIL) is already claimed by a mode other
than NAME -- e.g. two DEFMODE forms both declaring :SUFFIX \"w\" would make
FIND-MODE-BY-SUFFIX's lookup ambiguous. Compares by MODE-DESCRIPTOR-NAME,
not object identity: DEFMODE re-registering the same NAME (a plain file
reload, e.g. under ASDF) builds a fresh MODE-DESCRIPTOR struct each time, so
an EQ check would spuriously reject a mode reclaiming its own suffix."
    (when suffix
      (let ((existing (find-mode-by-suffix suffix)))
        (when (and existing (not (eq (mode-descriptor-name existing) name)))
          (error "DEFMODE ~S: suffix ~S is already used by mode ~S"
                 name suffix (mode-descriptor-name existing)))))))

;;; DEFMODE pattern parsing
;;;
;;; These run inside an EVAL-WHEN, not just plain DEFUNs, because this same
;;; file's built-in DEFMODE forms (bottom of this file) call BUILD-MODE-
;;; DESCRIPTOR at :COMPILE-TOPLEVEL time -- a plain DEFUN's body is only
;;; guaranteed callable at load time, which is too late within one
;;; compilation unit (see the DEFVAR comment above for the same issue).
;;;
;;; %MODE-HOLE-COUNT and %PATTERN-HOLE-COUNT (below) also live in this
;;; EVAL-WHEN, not just at top level like a plain accessor would -- ONE-OF
;;; validation (%CHECK-ONE-OF-ELEMENTS!, #103) calls %MODE-HOLE-COUNT at
;;; DEFMODE's own :COMPILE-TOPLEVEL time, the same reason everything else
;;; here needs one.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %one-of-element-p (el)
    "T if EL is a raw DEFMODE pattern element spelled (ONE-OF mode...) (#103)
-- a list headed by a symbol named ONE-OF, matched case-insensitively like
EXPR below."
    (and (consp el) (symbolp (first el)) (string-equal (symbol-name (first el)) "ONE-OF")))

  (defun %parse-mode-pattern (elements)
    "ELEMENTS is DEFMODE's pattern spine: a mix of string literals, the
symbol EXPR, and (ONE-OF mode...) alternations (#103). Returns a list of
(:literal string) | (:expr) | (:one-of mode-name...) pattern elements, plus
any trailing keyword options untouched by this function (the caller splits
those off first)."
    (mapcar (lambda (el)
              (cond
                ((stringp el) (list :literal el))
                ((and (symbolp el) (string-equal (symbol-name el) "EXPR")) (list :expr))
                ((%one-of-element-p el)
                 (unless (and (>= (length (rest el)) 2) (every #'symbolp (rest el)))
                   (error "Malformed DEFMODE pattern element ~S -- (ONE-OF ...) needs at ~
least two mode-name symbols" el))
                 (list* :one-of (rest el)))
                (t (error "Malformed DEFMODE pattern element ~S -- expected a string ~
literal, the symbol EXPR, or (ONE-OF mode...)" el))))
            elements))

  (defun %split-mode-clause (body)
    "Split DEFMODE's BODY into (VALUES pattern-elements keyword-plist). Keyword
options start at the first keyword symbol; everything before it is pattern."
    (let ((pos (position-if #'keywordp body)))
      (if pos
          (values (subseq body 0 pos) (subseq body pos))
          (values body nil))))

  (defun %pattern-hole-count (pattern &optional seen)
    "Total :EXPR holes in PATTERN (a MODE-DESCRIPTOR's own pattern list, or a
DEFMODE-in-progress's), recursing into any :ONE-OF element via its first
alternative -- %CHECK-ONE-OF-ELEMENTS! (below) validates every alternative of
one :ONE-OF shares the same hole count, so any one of them stands for the
element as a whole. SEEN is the list of mode names already on this recursion
path -- signals an error rather than recursing forever if a :ONE-OF element
names a mode already being walked (a hand-written DEFMODE cycle: redefining
a mode some :ONE-OF already references so the reference loops back to it).
A plain file reload can't create a cycle (it replays the same patterns in
the same order), so this only ever fires on a genuinely circular
redefinition, caught here at DEFMODE time rather than as an unbounded
recursion at some later, unrelated call."
    (loop for element in pattern
          sum (ecase (first element)
                (:literal 0)
                (:expr 1)
                (:one-of (%mode-hole-count (find-mode-descriptor (second element)) seen)))))

  (defun %mode-hole-count (mode &optional seen)
    (let ((name (mode-descriptor-name mode)))
      (when (member name seen)
        (error "DEFMODE ~S: ONE-OF cycle -- ~{~S~^ -> ~} -> ~S references itself"
               name (reverse seen) name))
      (%pattern-hole-count (mode-descriptor-pattern mode) (cons name seen))))

  (defun %pattern-hole-alternatives (pattern &optional seen)
    "One entry per hole in PATTERN, in hole order -- NIL for a plain :EXPR
hole, or the list of :ONE-OF alternative mode-name symbols governing a
:ONE-OF-produced hole (#104). A multi-hole :ONE-OF element repeats its own
alt-names list once per hole it contributes -- the whole element's choice of
alternative governs each of its holes alike, mirroring the hole-alignment
%MATCH-MODE-ELEMENTS' CHOICES return value now uses. A nested :ONE-OF (inside
one alternative of an outer one) is walked via that alternative's own
pattern -- see %MODE-HOLE-ALTERNATIVES; this is the pattern-only half, mirroring
%PATTERN-HOLE-COUNT/%MODE-HOLE-COUNT's own split. Used by DEFINSTRUCTION's
word-encoded (CHOICE M) selector validation (instruction.lisp) to check M
against the actual alternatives available at a given hole. SEEN guards
against a DEFMODE cycle, same as %PATTERN-HOLE-COUNT/%MODE-HOLE-COUNT."
    (loop for element in pattern
          append (ecase (first element)
                   (:literal nil)
                   (:expr (list nil))
                   (:one-of (let* ((alt-names (rest element))
                                    (holes (%mode-hole-count (find-mode-descriptor (first alt-names)) seen)))
                              (make-list holes :initial-element alt-names))))))

  (defun %mode-hole-alternatives (mode &optional seen)
    (%pattern-hole-alternatives (mode-descriptor-pattern mode) seen))

  (defun %pattern-nested-one-of-signed-p (pattern &optional seen)
    "T if any :ONE-OF element nested anywhere in PATTERN -- at any depth, not
just PATTERN's own top-level elements -- has an alternative declaring
:SIGNED T. Used by %CHECK-ONE-OF-ELEMENTS! to reject a nested :ONE-OF's
:SIGNED alternative: %MATCH-MODE-ELEMENTS' outermost-ONE-OF-wins rule (this
file) means only the *outermost* :ONE-OF a hole belongs to ever gets that
hole's CHOICES entry, so a :SIGNED declared on some inner alternative -- two
:ONE-OF levels down from the hole a DEFINSTRUCTION site actually sees -- has
no decode-time record anywhere that could recover it; letting it through here
would silently not honor it later instead of erroring where the mistake is
made. SEEN guards the same hand-written-redefinition-cycle case
%MODE-HOLE-COUNT does, for the same reason."
    (loop for element in pattern
          thereis (when (eq (first element) :one-of)
                    (let ((alts (mapcar #'find-mode-descriptor (rest element))))
                      (or (some #'mode-descriptor-signedp alts)
                          (some (lambda (alt)
                                  (let ((alt-name (mode-descriptor-name alt)))
                                    (unless (member alt-name seen)
                                      (%pattern-nested-one-of-signed-p
                                       (mode-descriptor-pattern alt) (cons alt-name seen)))))
                                alts))))))

  (defun %pattern-nested-one-of-width-p (pattern &optional seen)
    "T if any :ONE-OF element nested anywhere in PATTERN -- at any depth, not
just PATTERN's own top-level elements -- has an alternative declaring
:WIDTH. Used by %CHECK-ONE-OF-ELEMENTS! to reject a nested :ONE-OF's :WIDTH
alternative, for the same reason %PATTERN-NESTED-ONE-OF-SIGNED-P (above)
rejects a nested :SIGNED one: %MATCH-MODE-ELEMENTS' outermost-ONE-OF-wins
rule means only the outermost :ONE-OF a hole belongs to ever gets that
hole's CHOICES entry, so a :WIDTH declared on some inner alternative has no
decode-time record anywhere that could recover it -- error where the mistake
is made rather than silently not honoring it later. SEEN guards the same
hand-written-redefinition-cycle case %MODE-HOLE-COUNT does, for the same
reason."
    (loop for element in pattern
          thereis (when (eq (first element) :one-of)
                    (let ((alts (mapcar #'find-mode-descriptor (rest element))))
                      (or (some #'mode-descriptor-width alts)
                          (some (lambda (alt)
                                  (let ((alt-name (mode-descriptor-name alt)))
                                    (unless (member alt-name seen)
                                      (%pattern-nested-one-of-width-p
                                       (mode-descriptor-pattern alt) (cons alt-name seen)))))
                                alts))))))

  (defun %check-one-of-elements! (name pattern)
    "Validate every (:ONE-OF ...) element of PATTERN, the DEFMODE NAME is
building: at least two alternatives; each must already be a registered mode
(FIND-MODE-DESCRIPTOR signals if not); none may declare a whole-mode
:SUFFIX attribute -- honoring that per hole rather than per statement needs
a byte-encoded machine to have some way to decode which alternative was
actually written, which is not designed yet, so it stays a follow-up.
:STRICT (#115), :SIGNED (#124/#127), :WIDTH (#129), and :RELATIVE (#130) are
exempt from this restriction: :STRICT is a pure encode-time range check with
no size, value, or decode consequence, so it is meaningful and honored per
hole regardless of encoding scheme (see %CHECK-STRICT-OPERAND-RANGE!,
assembler.lisp); :SIGNED, :WIDTH, and :RELATIVE are each honored per hole
when a decode-time record of which alternative matched exists for that hole
-- a byte-encoded hole carrying a (variant (choice m) (sub s)) selector, or
a word-encoded hole whose every field variant is (choice m)-selected --
checked at DEFINSTRUCTION time (instruction.lisp's %CHECK-BYTE-ONE-OF-SIGNED,
%CHECK-BYTE-ONE-OF-WIDTH, and %CHECK-BYTE-ONE-OF-RELATIVE), not here, since
this function has no machine or encoding scheme in scope. :WIDTH is honored
only on a byte-encoded machine; a word-encoded one rejects a
width-disagreeing hole at DEFINSTRUCTION time too, since a word-encoded
operand's size always comes from its own field width, never from
OPERAND-WIDTHS (permanently empty there) -- a per-hole :WIDTH has nothing to
mean on that scheme. :RELATIVE is likewise byte-machine-only -- a
word-encoded hole whose alternatives declare it at all is rejected at
DEFINSTRUCTION time (%CHECK-WORD-ONE-OF-RELATIVE) -- and, beyond the
selector requirement it shares with :SIGNED/:WIDTH, carries its own
positional rule (at most one hole per expanded descriptor may resolve
relative), also checked at DEFINSTRUCTION time since it spans more than one
hole. Every alternative must have the same hole count as every other, since
the positional hole <-> operand-field parallel the rest of the pipeline
depends on (instruction.lisp, decoder.lisp, disassembler.lisp) has no room
for a :ONE-OF that yields a different field count depending which
alternative matched; and no two alternatives may share identical (EQUALP,
since a :LITERAL matches case-insensitively) syntax, since nothing could
ever disambiguate between them."
    (dolist (element pattern)
      (when (eq (first element) :one-of)
        (let* ((alt-names (rest element))
               (alts (mapcar #'find-mode-descriptor alt-names)))
          (when (< (length alts) 2)
            (error "DEFMODE ~S: ONE-OF needs at least two alternative modes, got ~S"
                   name alt-names))
          (dolist (alt alts)
            (when (mode-descriptor-suffix alt)
              (error "DEFMODE ~S: ONE-OF alternative ~S declares a whole-mode attribute ~
(:SUFFIX) -- not yet supported per-hole inside ONE-OF"
                     name (mode-descriptor-name alt)))
            (when (%pattern-nested-one-of-signed-p (mode-descriptor-pattern alt))
              (error "DEFMODE ~S: ONE-OF alternative ~S has a nested ONE-OF whose own ~
alternative declares :SIGNED T or :RELATIVE T -- only the outermost ONE-OF a hole belongs ~
to keeps its CHOICES entry, so a nested :SIGNED/:RELATIVE can never be recovered at decode ~
time; give ~S itself :SIGNED T or :RELATIVE T instead, or move the alternative up to this ~
ONE-OF directly"
                     name (mode-descriptor-name alt) (mode-descriptor-name alt)))
            (when (%pattern-nested-one-of-width-p (mode-descriptor-pattern alt))
              (error "DEFMODE ~S: ONE-OF alternative ~S has a nested ONE-OF whose own ~
alternative declares :WIDTH -- only the outermost ONE-OF a hole belongs to keeps its ~
CHOICES entry, so a nested :WIDTH can never be recovered at decode time; give ~S itself ~
:WIDTH instead, or move the :WIDTH alternative up to this ONE-OF directly"
                     name (mode-descriptor-name alt) (mode-descriptor-name alt))))
          (let ((counts (remove-duplicates (mapcar #'%mode-hole-count alts))))
            (when (> (length counts) 1)
              (error "DEFMODE ~S: ONE-OF alternatives ~S have differing hole counts ~S -- ~
every alternative must bind the same number of operand fields"
                     name alt-names counts)))
          (loop for (alt . later) on alts
                do (dolist (other later)
                     (when (equalp (mode-descriptor-pattern alt) (mode-descriptor-pattern other))
                       (error "DEFMODE ~S: ONE-OF alternatives ~S and ~S have identical syntax ~
-- nothing could ever choose between them"
                              name (mode-descriptor-name alt) (mode-descriptor-name other)))))))))

  (defun build-mode-descriptor (name body)
    (multiple-value-bind (pattern-elements options) (%split-mode-clause body)
      (when (null pattern-elements)
        (error "DEFMODE ~S: pattern must include at least one EXPR hole" name))
      (let ((pattern (%parse-mode-pattern pattern-elements)))
        (%check-one-of-elements! name pattern)
        (unless (plusp (%pattern-hole-count pattern))
          (error "DEFMODE ~S: pattern must include at least one EXPR hole" name))
        (destructuring-bind (&key width relative signed suffix strict) options
          (when (and relative (not (eq signed t)) (member :signed options))
            (error "DEFMODE ~S: :RELATIVE T implies :SIGNED T -- do not pass ~
:SIGNED NIL alongside it" name))
          (%check-suffix-collision name suffix)
          (make-mode-descriptor :name name :pattern pattern :width width
                                 :relativep relative
                                 :signedp (or relative signed)
                                 :suffix suffix
                                 :strictp strict))))))

(defmacro defmode (name &body pattern)
  "Define an addressing mode named NAME matching PATTERN, a sequence of
string literals, the symbol EXPR (one per operand hole), and/or
(ONE-OF mode...) alternations (#103, at least two already-registered modes;
see \"Per-operand modes\" in docs/modes.md) -- each hole an addressing mode
declares may independently pick its own syntax from a set of other modes,
e.g. a bare register or \"[\" expr \"]\" indirection at the same operand
position. Every ONE-OF alternative must have the same hole count as every
other and may declare neither :SUFFIX itself (a follow-up extends per-hole
attributes further) nor -- on a word-encoded machine -- :RELATIVE, which
stays byte-machine-only; :STRICT, :SIGNED, :WIDTH, and :RELATIVE (on a
byte-encoded machine) are the exceptions -- an alternative may declare any
of the four on its own, honored per hole rather than per statement (see
docs/modes.md's \"Per-hole :strict\"/\"Per-hole :signed\"/\"Per-hole
:width\"/\"PC-relative modes\" sections). A per-hole :SIGNED, :WIDTH, or
:RELATIVE still needs a way to recover, at decode time, which alternative a
hole actually matched when its siblings disagree -- DEFINSTRUCTION
(instruction.lisp) enforces that, not this macro; :RELATIVE additionally
needs to know, per expanded descriptor, which hole of a multi-hole pattern
is the relative one, also enforced there. Optionally
followed by
:WIDTH n (a default operand byte width instructions using this
mode may omit from their own encoding), :SIGNED t (this mode's operand is a
signed quantity -- the emulator sign-extends it and the assembler
range-checks candidate values against the signed range; #30), :RELATIVE t
(this mode's operand is a PC-relative offset rather than an absolute value
-- see RELATIVE below and #23; implies :SIGNED t, so passing :SIGNED NIL
alongside :RELATIVE T is an error), and/or :SUFFIX \"s\" (a gas-style
mnemonic suffix, e.g. \"w\"/\"z\" -- a program can append SEPARATOR ++ s to
a mnemonic, e.g. \"lda.w\", to force this mode regardless of what the
operand's value folds to, bypassing relaxation entirely; #40, see
docs/modes.md), and/or :STRICT t (an operand that doesn't fit this mode's
own width is an ASSEMBLY-ERROR at encode time rather than silently wrapping
via WRAP-VALUE; #74, see docs/diagnostics.md -- *STRICT-OPERAND-RANGE*
makes every mode behave this way without marking any one of them). Signals
an error if SUFFIX is already claimed by a different mode. E.g.:

  (defmode immediate  \"#\" expr        :width 1)
  (defmode zero-page  expr            :width 1)
  (defmode absolute   expr)
  (defmode indexed-x  expr \",\" \"X\")
  (defmode indirect-y \"(\" expr \")\" \",\" \"Y\")
  (defmode signed-imm \"#\" expr        :width 1 :signed t)
  (defmode relative   expr            :width 1 :relative t)

Registers the resulting MODE-DESCRIPTOR under NAME in *MODES*, inside an
EVAL-WHEN so it is available at macroexpansion time like DEFMACHINE
(machine.lisp) -- DEFINSTRUCTION resolves mode names against this registry
when its own form is compiled."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ',name *modes*) (build-mode-descriptor ',name ',pattern))
     ',name))

;;; Pattern matching

(defun %match-mode-elements (tokens elements i end)
  "Match ELEMENTS (a suffix of some mode's pattern) against TOKENS from
position I (bounded by END). Returns (VALUES asts choices next-i okp
failure-token message): on success ASTS is the list of EXPR-* ASTs parsed
from each :EXPR hole and CHOICES the parallel, HOLE-ALIGNED list -- one entry
per hole in ASTS, NIL for a hole not governed by any :ONE-OF, or the chosen
MODE-DESCRIPTOR for a hole that came from one (#104) -- both in pattern
order, and NEXT-I the token position just past the match; on failure OKP is
NIL and FAILURE-TOKEN/MESSAGE describe why.

CHOICES is always the same length as ASTS: a multi-hole :ONE-OF alternative
contributes its own chosen MODE-DESCRIPTOR to *every* hole it produces, not
just one entry for the element as a whole -- this is what lets a caller
(DEFINSTRUCTION's word-encoded (CHOICE M) selector, instruction.lisp) key
directly off hole position, the same position (OPERAND ...) subclauses
already use. When an alternative's own pattern nests another :ONE-OF, the
*outer* element's chosen alternative overwrites whatever the nested match
would have reported for those holes -- the outermost :ONE-OF a hole belongs
to always wins its CHOICES entry, preserving \"CHOICES[i] is one of the
alternatives named by the pattern element that produced hole i\" as an
invariant callers can validate against (mirrored by mode.lisp's
%MODE-HOLE-ALTERNATIVES, the pattern-only version of this same walk). Nested
:ONE-OF is legal (BUILD-MODE-DESCRIPTOR's hole-count check treats it like a
plain :EXPR) -- see tests/mode.lisp for coverage of both the hole-counting
and this outermost-wins CHOICES behavior.

A hand-written DEFMODE cycle -- redefining a mode that some :ONE-OF already
references so the reference loops back to it -- is guarded against
elsewhere: %MODE-HOLE-COUNT (#115) signals rather than recursing forever
when a mode name reappears on its own recursion path. A plain file reload
can't create a cycle, since it replays the same patterns in the same order.

Recursive over ELEMENTS -- not a linear scan -- so a :ONE-OF element can
backtrack (#103): each alternative is tried by recursively matching *the
rest of the pattern* after it, not just the alternative's own tokens, so an
alternative that matches locally but leaves the remaining elements unable to
match is rejected in favor of a later alternative, rather than committing to
the first token-level match. ASTS and CHOICES are built by consing each
element's own contribution onto the *return value* of the recursive call for
everything after it, never onto a shared accumulator -- an abandoned
alternative's partial match is simply never included in what gets returned,
with no separate undo step needed."
  (if (null elements)
      (values nil nil i t nil nil)
      (let ((element (first elements)) (rest-elements (rest elements)))
        (ecase (first element)
          (:literal
           (let ((tok (%tok tokens i end)))
             (if (and tok (string-equal (token-text tok) (second element)))
                 (multiple-value-bind (asts choices next-i okp failure-token message)
                     (%match-mode-elements tokens rest-elements (1+ i) end)
                   (if okp
                       (values asts choices next-i t nil nil)
                       (values nil nil nil nil failure-token message)))
                 (values nil nil nil nil tok
                         (format nil "Operand does not match addressing mode ~
(expected ~S~@[, found ~S~])"
                                 (second element) (and tok (token-text tok)))))))
          (:expr
           (handler-case
               (multiple-value-bind (ast next-i-hole) (parse-expression tokens :start i :end end)
                 (multiple-value-bind (asts choices next-i okp failure-token message)
                     (%match-mode-elements tokens rest-elements next-i-hole end)
                   (if okp
                       (values (cons ast asts) (cons nil choices) next-i t nil nil)
                       (values nil nil nil nil failure-token message))))
             ;; #74: keep the inner PARSE-FAILURE's own line/column instead of
             ;; only its message -- %TOK below can't reconstruct a token from
             ;; a bare string, so the failure token this returns is a
             ;; synthetic one carrying just enough (line/column) for
             ;; MATCH-OPERAND-MODE's %PARSE-ERROR to preserve position.
             (parse-failure (c)
               (values nil nil nil nil
                       (make-token :line (lasm-syntax-error-line c)
                                   :column (lasm-syntax-error-column c))
                       (lasm-syntax-error-message c)))))
          (:one-of
           (let (last-failure-token last-message)
             (dolist (alt-name (rest element)
                      (values nil nil nil nil last-failure-token last-message))
               (let ((alt (find-mode-descriptor alt-name)))
                 (multiple-value-bind (alt-asts alt-choices alt-next-i alt-okp alt-failure-token alt-message)
                     (%match-mode-elements tokens (mode-descriptor-pattern alt) i end)
                   (declare (ignore alt-choices))
                   (if alt-okp
                       (multiple-value-bind (asts choices next-i okp failure-token message)
                           (%match-mode-elements tokens rest-elements alt-next-i end)
                         (if okp
                             ;; #104: every hole ALT-ASTS contributes gets ALT
                             ;; itself as its CHOICES entry -- not ALT-CHOICES
                             ;; (the nested match's own, discarded above) --
                             ;; so the outermost :ONE-OF a hole belongs to
                             ;; always wins that hole's entry.
                             (return-from %match-mode-elements
                               (values (append alt-asts asts)
                                       (append (make-list (length alt-asts) :initial-element alt) choices)
                                       next-i t nil nil))
                             (setf last-failure-token failure-token last-message message)))
                       (setf last-failure-token alt-failure-token last-message alt-message)))))))))))

(defun %match-mode-pattern (tokens mode)
  "Match the SIMPLE-VECTOR TOKENS against MODE's pattern from the start.
Returns (VALUES asts okp failure-token message choices): on success ASTS is
the list of EXPR-* ASTs parsed from each :EXPR hole (in pattern order) and
OKP is T; on failure OKP is NIL and FAILURE-TOKEN/MESSAGE describe why --
FAILURE-TOKEN is NIL only when the underlying PARSE-FAILURE (an :EXPR hole's
own malformed expression) itself carried no token to point at (e.g. an empty
expression at end of input), never as a way of discarding a position that
was available. CHOICES (#103, hole-aligned per #104) is the list of chosen
MODE-DESCRIPTORs, one per hole in ASTS, NIL for a hole not governed by any
:ONE-OF -- see %MATCH-MODE-ELEMENTS -- a trailing value existing callers
that only bind the first four are unaffected by."
  (let ((end (length tokens)))
    (multiple-value-bind (asts choices next-i okp failure-token message)
        (%match-mode-elements tokens (mode-descriptor-pattern mode) 0 end)
      (cond
        ((not okp) (values nil nil failure-token message nil))
        ((< next-i end) (values nil nil (%tok tokens next-i end) "Unexpected trailing token in operand" nil))
        (t (values asts t nil nil choices))))))

(defun try-match-operand-mode (tokens mode)
  "Like MATCH-OPERAND-MODE, but returns (VALUES asts T choices) on a match or
(VALUES NIL NIL NIL) on a mismatch instead of signalling -- the assembler's
mode candidate filter (assembler.lisp) uses this to try several modes in
turn. MODE, like MATCH-OPERAND-MODE's, may be a MODE-DESCRIPTOR or a symbol
naming one. CHOICES (#103, hole-aligned per #104) is the list of chosen
MODE-DESCRIPTORs, one per hole, NIL for a hole not governed by any :ONE-OF --
see %MATCH-MODE-ELEMENTS."
  (let ((mode (if (mode-descriptor-p mode) mode (find-mode-descriptor mode))))
    (multiple-value-bind (asts okp failure-token message choices) (%match-mode-pattern tokens mode)
      (declare (ignore failure-token message))
      (if okp (values asts t choices) (values nil nil nil)))))

(defun match-operand-mode (tokens mode)
  "Match TOKENS (a SIMPLE-VECTOR of raw tokens, e.g. an OPERAND's TOKENS or a
STATEMENT's OPERAND-TOKENS) against addressing MODE's pattern (a
MODE-DESCRIPTOR, or a symbol naming one): consume MODE's literal tokens in
order and parse each :EXPR hole as an expression. Returns (VALUES first-ast
all-asts choices) -- FIRST-AST alone is what every current single-hole mode
needs; CHOICES (#103, hole-aligned per #104) is the list of chosen
MODE-DESCRIPTORs, one per hole, NIL for a hole not governed by any :ONE-OF --
see %MATCH-MODE-ELEMENTS. Signals PARSE-FAILURE
if TOKENS don't match MODE or leave a trailing token unconsumed -- with the
failing token's own line/column (#74), not just its message, even when the
failure came from a nested :EXPR hole's own PARSE-FAILURE rather than a
literal mismatch."
  (let ((mode (if (mode-descriptor-p mode) mode (find-mode-descriptor mode))))
    (multiple-value-bind (asts okp failure-token message choices) (%match-mode-pattern tokens mode)
      (unless okp
        (%parse-error failure-token message))
      (values (first asts) asts choices))))

;;; Built-in modes -- M1's *BUILTIN-MODE-PREFIXES* table, expressed as
;;; ordinary DEFMODE forms. "#" already lexes to :HASH (lexer.lisp) for
;;; exactly this purpose.

(defmode immediate "#" expr :width 1)
;; ZERO-PAGE/ABSOLUTE (#40): the only two built-in modes that share operand
;; syntax (a bare expr) and so are the only pair relaxation ever has to pick
;; between -- each gets a suffix ("z"/"w") so a program can force one over
;; the other. IMMEDIATE/INDEXED-X/INDIRECT-Y/RELATIVE below are already
;; syntactically unambiguous, so a suffix would buy them nothing.
(defmode zero-page expr :width 1 :suffix "z")
(defmode absolute expr :suffix "w")
(defmode indexed-x expr "," "X")
(defmode indirect-y "(" expr ")" "," "Y")

;; RELATIVE (#23): syntactically identical to ABSOLUTE (a bare expr), but its
;; operand is a signed offset from the address of the *next* instruction, not
;; an absolute target -- computed by the assembler once layout has placed
;; both the branch and its target (assembler.lisp's %ENCODE). :RELATIVE T
;; implies :SIGNED T (#30): the emulator sign-extends it on fetch
;; (emulator.lisp's STEP-MACHINE) so semantics can write (set! pc (+ pc
;; operand)) with no width of its own to worry about. :WIDTH 1 is only this
;; mode's default -- a machine with wider branches overrides it per
;; instruction via the existing (operand :width n).
(defmode relative expr :width 1 :relative t)

;; STACK-RELATIVE (#50): syntactically "n,S" -- an expr hole followed by the
;; literal ",S", 6502/65816-flavoured like INDEXED-X/INDIRECT-Y above. Like
;; every mode, this is pure operand *syntax*: the parsed value is just an
;; offset, and it says nothing about which stack it indexes into or what that
;; offset means -- an instruction's semantics resolves it against a named
;; stack via STACK-REF (storage.lisp), with STACK-REF's own top-relative,
;; unsigned convention (offset 0 = the top). No :SUFFIX -- unlike ZERO-PAGE/
;; ABSOLUTE, this mode shares its syntax with no other built-in mode, so
;; there is nothing for a forced suffix to disambiguate.
(defmode stack-relative expr "," "S" :width 1)
