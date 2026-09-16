;;;; assembler.lisp
;;;; The M2 assembler pass: turns a STATEMENT list (parser.lisp) into encoded
;;;; bytes, resolving labels and selecting an addressing mode along the way.
;;;;
;;;; Layout (%LAYOUT below) binds every label to an address and chooses each
;;;; instruction's addressing-mode variant (mode.lisp/instruction.lisp) so it
;;;; can size the statement; encode (%ENCODE) then evaluates operands against
;;;; the completed symbol table and emits bytes. This is what buys forward
;;;; references (`jmp end` ... `end:`) for free.
;;;;
;;;; M1 sized every statement without looking at its operand at all, since it
;;;; allowed only one mode per instruction. M2 modes can overlap in syntax
;;;; (zero-page and absolute both match a bare `expr`) and differ in size, so
;;;; layout now also selects a mode per statement -- see %CHOOSE-VARIANT.
;;;; Since a label's value isn't known on the first attempt, layout iterates:
;;;; every label-bearing (or relative-mode) operand starts at its narrowest
;;;; legal variant, and each subsequent pass re-chooses against the previous
;;;; pass's provisional symbol table, widening whatever no longer fits.
;;;; Widening is sticky -- a statement's chosen width, once committed, is a
;;;; floor for every later pass -- which bounds the loop by the number of
;;;; relaxable statements and makes it impossible to oscillate. %LAYOUT drives
;;;; %LAYOUT-PASS to a fixpoint on the per-statement width vector, then runs
;;;; one final pass whose output feeds %ENCODE.
;;;;
;;;; A statement whose mnemonic carries a forced addressing-mode suffix
;;;; (#40, e.g. "lda.w") skips mode selection entirely -- %CHOOSE-VARIANT
;;;; hands off to %CHOOSE-FORCED-VARIANT, which resolves the suffix to its
;;;; mode and returns it unconditionally, bypassing both the floor and value
;;;; filters (an out-of-range value silently wraps at encode time, like a
;;;; single-mode M1 instruction always did -- one more instance of the class
;;;; #28 tracks unifying). This doesn't threaten the fixpoint argument above:
;;;; a forced statement's chosen variant depends only on its own suffix and
;;;; operand syntax, never on the symbol table, so it picks the same
;;;; (constant) width on every pass -- trivially monotone.
;;;;
;;;; Local labels (an identifier starting with the lexer's LOCAL-LABEL-PREFIX,
;;;; e.g. ".loop") are scoped to their nearest preceding non-local ("global")
;;;; label (#16): %LAYOUT threads a SCOPE variable, updated by every global
;;;; label definition, and qualifies each local name -- both a definition and
;;;; a reference -- to SCOPE ++ NAME (e.g. "loop" ++ ".next" -> "loop.next")
;;;; before it ever reaches the symbol table, so SYMBOLS itself stays a flat
;;;; string -> address table and EVAL-EXPR needs no scope of its own. A local
;;;; label with no enclosing global label is an ASSEMBLY-ERROR. See
;;;; %QUALIFY-LOCAL/%QUALIFY-LOCALS! below.
;;;;
;;;; The location-counter symbol ("*" in an operand, #15) folds to the
;;;; address of the statement (or, for a multi-value .BYTE/.WORD, the value)
;;;; it appears in -- see EVAL-EXPR's :PC argument (instruction.lisp) and
;;;; %CHOOSE-VARIANT/%ENCODE below, both of which now pass an ADDRESS/PC
;;;; through even though pass 1 has no symbol table yet, since a statement's
;;;; own address is already known at that point.
;;;;
;;;; ASSEMBLE-STATEMENTS runs EXPAND-MACROS (macro.lisp, #33) before %LAYOUT
;;;; ever sees the statement list -- a .macro/.endm block is collected and
;;;; every invocation replaced by its substituted body first, so neither
;;;; %LAYOUT nor %ENCODE below has any notion of a macro at all.
;;;;
;;;; Directives (directive.lisp, #14) are dispatched in %LAYOUT before a
;;;; mnemonic reaches FIND-INSTRUCTION-VARIANTS (which signals on an
;;;; unregistered name, so directive lookup can't be a fallback after that
;;;; call). .ORG (a DIRECTIVE-DESCRIPTOR with ACTION :SET-ORIGIN) moves the
;;;; address counter -- and, if no statement has occupied any address yet,
;;;; the assembly's own ORIGIN -- rather than sizing a statement; .BYTE/.WORD
;;;; (:EMIT) and .RES (:RESERVE) size a statement like an instruction does,
;;;; so pass 1 still sizes everything and pass 2 still only evaluates
;;;; operands against the completed symbol table. Because a directive
;;;; statement can leave a gap (a forward .ORG, or plain non-contiguous
;;;; layout) or contribute no bytes at all (.ORG itself), %ENCODE below
;;;; writes each entry at its own address into a growable byte vector
;;;; instead of accumulating a flat list.
;;;;
;;;; .EQU (a DIRECTIVE-DESCRIPTOR with ACTION :ASSIGN, #35) binds a name to a
;;;; computed value in the symbol table without occupying any address --
;;;; distinct from a label, which always binds to the current address. Its
;;;; value must fold during the pass that reaches it, against that pass's
;;;; symbol table as built *so far* -- so an .EQU sees every label and .EQU
;;;; above it, never one below (a forward reference is an ASSEMBLY-ERROR,
;;;; same rule as .ORG/.RES's own operand). This makes the symbol table a
;;;; string -> value map, not string -> address: an ordinary instruction
;;;; operand or .BYTE/.WORD value can reference either kind of symbol
;;;; interchangeably at encode time, since EVAL-EXPR doesn't distinguish
;;;; them. Because a later layout pass's addresses can change an
;;;; address-dependent .EQU's value (e.g. \"size = * - start\"), while
;;;; .ORG/.RES must fold *before* any address is final, those two directives
;;;; may only reference a *pure* .EQU -- one whose value contains no label
;;;; and no \"*\" -- kept in a separate CONSTANTS table (%LAYOUT-PASS) built
;;;; alongside the main one; an address-dependent .EQU used there is an
;;;; ASSEMBLY-ERROR pointing at #41, which tracks lifting the restriction.
;;;; Once %LAYOUT's widths have converged, every address is fixed, so every
;;;; .EQU's value is too -- the existing width-vector fixpoint check already
;;;; implies an .EQU fixpoint, and nothing new needs to converge.
;;;;
;;;; %LAYOUT's SIZED-ENTRIES already *is* the address<->statement mapping a
;;;; listing/source map needs (#25) -- ASSEMBLE-STATEMENTS used to let it
;;;; fall on the floor once %ENCODE had consumed it. %BUILD-LISTING (below,
;;;; near the entry points) instead keeps a LISTING-LINE per entry (address,
;;;; size, source line, kind) as ASSEMBLY-LISTING, without duplicating the
;;;; encoded cells themselves -- see listing.lisp, which renders it and
;;;; answers address<->line lookups.

(in-package #:lasm)

;;; Conditions

(define-condition assembly-error (lasm-syntax-error) ()
  (:documentation "Signalled by ASSEMBLE-STATEMENTS on a malformed program:
a duplicate label, or an operand that matches no addressing mode any variant
of the instruction declares. Undefined labels are not this condition -- they
surface as UNRESOLVED-LABEL from EVAL-EXPR, since that condition already
names exactly this failure."))

(defun %signal-assembly-error (line column fmt args)
  (error 'assembly-error :message (apply #'format nil fmt args) :line line :column column))

(defun %assembly-error (line fmt &rest args)
  (%signal-assembly-error line nil fmt args))

(defun %assembly-error-at (token fmt &rest args)
  "Like %ASSEMBLY-ERROR, but anchored at TOKEN (a token, or NIL) rather than
a bare line number -- gives the resulting ASSEMBLY-ERROR a COLUMN (#74), so
DIAGNOSTIC-TEXT can point a caret at the offending operand instead of only
naming its line."
  (%signal-assembly-error (and token (token-line token)) (and token (token-column token)) fmt args))

;;; Operand/mode diagnostic text (#74)

(defun %operand-text (tokens)
  "Join TOKENS' (a simple-vector, e.g. a STATEMENT's OPERAND-TOKENS) verbatim
TOKEN-TEXT back into one string, for echoing an operand into a diagnostic --
e.g. \"(#5),Y\". Not a re-lexing round-trip (no whitespace is reinserted
between tokens), just enough to name what was given."
  (format nil "~{~A~}" (map 'list #'token-text tokens)))

(defun %mode-syntax-text (mode)
  "MODE's own pattern (mode.lisp), rendered back to the syntax a program
would write to select it, e.g. IMMEDIATE -> \"#expr\", INDIRECT-Y ->
\"(expr),Y\" -- an :EXPR hole prints as the literal word \"expr\", and a
:ONE-OF element (#103) as its alternatives' own syntax joined with \"|\",
e.g. \"expr|[expr]\"."
  (format nil "~{~A~}"
          (mapcar (lambda (el)
                    (ecase (first el)
                      (:literal (second el))
                      (:expr "expr")
                      (:one-of (format nil "~{~A~^|~}"
                                        (mapcar (lambda (name) (%mode-syntax-text (find-mode-descriptor name)))
                                                (rest el))))))
                  (mode-descriptor-pattern mode))))

(defun %accepted-modes-text (variants)
  "VARIANTS' (an instruction's list of INSTRUCTION-DESCRIPTOR) addressing
modes, rendered as a comma-separated \"name (syntax)\" list for a mode-
mismatch diagnostic -- a no-operand variant (MODE nil) renders as \"no
operand\" instead."
  (format nil "~{~A~^, ~}"
          (mapcar (lambda (v)
                    (let ((mode (instruction-descriptor-mode v)))
                      (if mode
                          (format nil "~(~A~) (~A)" (mode-descriptor-name mode) (%mode-syntax-text mode))
                          "no operand")))
                  variants)))

;;; Result

(defstruct listing-line
  "One address-occupying statement's entry in an ASSEMBLY's LISTING (#25) --
enough to look a statement back up by ADDRESS (LISTING-LINE-AT) or by LINE
(LISTING-LINES-FOR-SOURCE-LINE, listing.lisp) and to slice its own encoded
cells out of ASSEMBLY-CELLS at render time, without duplicating them here.
KIND is :INSTRUCTION, :EMIT (.byte/.word), or :RESERVE (.res) -- the same
three tags %LAYOUT-PASS's SIZED-ENTRIES already carries; DESCRIPTOR is only
ever non-NIL for :INSTRUCTION."
  (address 0 :type (integer 0))
  (size 0 :type (integer 0))
  (line 0 :type (integer 0))
  (kind :instruction :type keyword)
  (descriptor nil :type (or null instruction-descriptor)))

(defstruct symbol-info
  "One ASSEMBLY-SYMBOL-INFO entry (#37) -- the scope and kind metadata
ASSEMBLY-SYMBOLS itself cannot carry, since that table must stay a flat
string -> value map (EVAL-EXPR's documented contract, and every caller that
folds an expr-label against it). Captured at bind time (%BIND-SYMBOL!) rather
than recovered later by splitting QUALIFIED-NAME on LOCAL-LABEL-PREFIX -- a
global literally spelled \"loop.next\" is indistinguishable from local
\".next\" under scope \"loop\" by string-splitting alone (#36), but not by
this struct, since SCOPE is recorded, not inferred.
NAME is the unqualified spelling as written (e.g. \".next\", or \"loop\" for
a global); QUALIFIED-NAME is NAME's ASSEMBLY-SYMBOLS key (e.g. \"loop.next\",
or just \"loop\" for a global -- global names are never qualified). SCOPE is
the enclosing global label's name, or NIL for a global (or a top-level
.EQU). KIND is :LABEL (bound to an address, %BIND-LABEL!) or :EQU (bound to
a computed value with no address meaning, %APPLY-ASSIGN-DIRECTIVE, #35).
LOCALP mirrors STATEMENT-LABEL-LOCALP/EXPR-LABEL-LOCALP. VALUE duplicates
the ASSEMBLY-SYMBOLS entry so a caller need not look twice. LINE is the
statement's line; inherits #89's caveat that a macro-expanded statement's
line is the body's own definition line, not the call site's."
  (name "" :type string)
  (qualified-name "" :type string)
  (scope nil :type (or null string))
  (kind :label :type keyword)
  (localp nil :type boolean)
  (value 0 :type integer)
  (line 0 :type (integer 0)))

(defstruct assembly
  (cells nil :type (or null vector))  ; (unsigned-byte cell-width), the
                                       ; machine's own code cell width (#53)
                                       ; -- not declared (vector (unsigned-byte
                                       ; n)) here: under SBCL a specialized
                                       ; array type is not a subtype of
                                       ; another by element width, so a fixed
                                       ; element-type declaration would make
                                       ; every non-8-bit MAKE-ASSEMBLY a type
                                       ; error
  (cell-width 8 :type (integer 1))
  (origin 0 :type (integer 0))
  (symbols nil :type (or null hash-table))  ; string -> value (a label's
                                             ; address, or an .EQU's folded
                                             ; value, #35)
  (symbol-info nil :type (or null hash-table))  ; qualified name -> SYMBOL-INFO
                                                 ; (#37) -- scope/kind metadata
                                                 ; for every ASSEMBLY-SYMBOLS
                                                 ; entry, built alongside it
                                                 ; and keyed the same way
  (listing nil :type list)            ; LISTING-LINE list, ascending by
                                       ; address (#25) -- see listing.lisp
  (source nil :type (or null string)))  ; the original source text, or NIL
                                         ; when ASSEMBLE-STATEMENTS was
                                         ; called directly with no :SOURCE
                                         ; (#25) -- LISTING-TEXT degrades to
                                         ; an entry-ordered listing with no
                                         ; source column in that case

;;; Pass 1: layout -- size every statement, bind every label, choose modes

(defun %fits-width-p (value width cell-width)
  "T if VALUE (a folded constant) fits in WIDTH cells of CELL-WIDTH bits
each, either as an unsigned or a two's-complement signed value -- e.g. both
255 and -1 fit one 8-bit cell, so an operand that hasn't declared itself
SIGNED (mode.lisp, #30) isn't rejected just because it folds negative.
Accepts the full unsigned range too, so this is NOT the right predicate for
a SIGNED mode's operand (a RELATIVE branch offset, #23, included) -- see
%FITS-SIGNED-WIDTH-P."
  (and (>= value (- (ash 1 (1- (* cell-width width)))))
       (< value (ash 1 (* cell-width width)))))

(defun %fits-signed-width-p (value width cell-width)
  "T if VALUE fits as a two's-complement signed WIDTH-cell integer at
CELL-WIDTH bits per cell, i.e. -(2^(cell-width*width-1)) <= VALUE <
2^(cell-width*width-1). Unlike %FITS-WIDTH-P, this rejects the unsigned-only
range (e.g. +200 does not fit one 8-bit cell) -- used to range-check any
SIGNED mode's operand (mode.lisp, #30), a RELATIVE mode's offset (#23)
included, where wrapping silently instead of erroring would run the wrong
(or a wrapped) value."
  (let ((bound (ash 1 (1- (* cell-width width)))))
    (and (>= value (- bound)) (< value bound))))

(defun %relative-fits-p (value address descriptor cell-width)
  "T if VALUE -- the absolute target a RELATIVE candidate's hole folds to --
encodes as an offset that fits DESCRIPTOR's operand width, computed the same
way %RELATIVE-OFFSET (below) will at encode time: relative to the address of
the *next* instruction, not this one's own. Used by %CHOOSE-VARIANT's value
filter so a RELATIVE candidate can compete on width like any other once an
address is available to compute its offset from, rather than always winning
by default as the widest candidate. :RELATIVE is rejected on a word-encoded
machine (instruction.lisp's %CHECK-WORD-RELATIVE, #20), so DESCRIPTOR here
is always cell-encoded and WIDTH is its operand cell width."
  (let* ((width (instruction-descriptor-total-operand-width descriptor))
         (next-address (+ address (instruction-descriptor-size descriptor))))
    (%fits-signed-width-p (- value next-address) width cell-width)))

(defun %word-variant-fits-p (values descriptor)
  "T if VALUES -- one already-evaluated operand value per DESCRIPTOR's
WORD-FIELDS entry, in order (#20) -- fits this word-field combo: an
:EXTRA-WORD field always fits (any value there is spilled into its own
word); an :INLINE field fits only when VALUE falls in that field's declared
(pre-bias) RANGE. Parallel to %FITS-WIDTH-P/%FITS-SIGNED-WIDTH-P for the
byte-encoded case, used by %CHOOSE-VARIANT's value filter to pick the
narrowest (fewest extra words) combo a value actually fits."
  (every (lambda (value choice)
           (ecase (word-field-choice-kind choice)
             (:extra-word t)
             (:inline (destructuring-bind (lo . hi) (word-field-choice-range choice)
                        (<= lo value hi)))))
         values (instruction-descriptor-word-fields descriptor)))

(defun %choices-eligible-p (descriptor choices)
  "T if DESCRIPTOR is eligible given CHOICES -- mode.lisp's hole-aligned
per-hole list of the ONE-OF alternative each operand hole actually matched,
NIL for a hole not governed by any ONE-OF (#104). Covers both encoding
schemes with one predicate (formerly %WORD-CHOICES-ELIGIBLE-P, word-only,
before #126 gave a byte-encoded descriptor's operand holes their own
selector to filter by): a word-encoded DESCRIPTOR's per-hole selector is its
WORD-FIELDS' own WORD-FIELD-CHOICE-CHOICE; a byte-encoded one's is its
SUB-CHOICES (#126) entry, already a bare mode-name symbol or NIL, hole-
aligned the same way. For each hole, paired positionally with CHOICES: a
hole whose own selector is non-NIL is eligible only when that hole's CHOICES
entry is the same mode M actually matched; a hole with no selector of its
own at all is always eligible regardless of CHOICES. #118: a word-encoded
field mixing CHOICE-selected (CHOICE M) variants with value-selected
(RANGE/:ELSE) ones has every variant's own WORD-FIELD-CHOICE-CHOICE stamped
by %CHECK-WORD-VARIANT-CHOICES! (instruction.lisp) -- the value-selected
ones with the one ONE-OF alternative no CHOICE-selected variant already
claims -- so this function sees a uniformly non-NIL selector across a mixed
field's whole menu and needs no separate mixed-field case. #126's byte path
has no such mixed case at all: %CHECK-BYTE-SUB-VARIANTS! (instruction.lisp)
requires every alternative of a sub-selected hole to be claimed, so a
byte-encoded hole's selector is either NIL (no selector on this hole) or
non-NIL for every descriptor expanded from it, never a mix within one hole.

A byte-encoded DESCRIPTOR with no SUB-CHOICES entry anywhere, and a
word-encoded one with no CHOICE-selected field anywhere, are both vacuously
eligible for any CHOICES, including the all-NIL CHOICES of a program using no
ONE-OF at all -- neither #104 nor #126 changes selection for a DEFINSTRUCTION
that doesn't use them."
  ;; WORD-FIELDS and SUB-CHOICES are mutually exclusive by construction (a
  ;; descriptor is word-encoded or byte-encoded, never both), so this OR
  ;; picks whichever one DESCRIPTOR actually has -- an empty/NIL WORD-FIELDS
  ;; (a byte-encoded descriptor, or a no-operand word-encoded one) correctly
  ;; falls through to SUB-CHOICES, and a non-empty WORD-FIELDS list (even one
  ;; whose every entry is NIL, i.e. no CHOICE-selected field at all) is
  ;; itself a non-NIL list and so is kept, never masked by SUB-CHOICES.
  (let ((selectors (or (mapcar #'word-field-choice-choice (instruction-descriptor-word-fields descriptor))
                        (instruction-descriptor-sub-choices descriptor))))
    (loop for wanted in selectors
          for hole-choice in choices
          always (or (null wanted)
                     (and hole-choice (eq wanted (mode-descriptor-name hole-choice)))))))

(defun %choose-forced-variant (statement variants)
  "STATEMENT carries a forced addressing-mode suffix (#40, e.g. \"w\" from
\"lda.w\"). Resolve it to the one VARIANTS entry using that mode and match
STATEMENT's operand tokens against it, bypassing %CHOOSE-VARIANT's floor and
value filters entirely -- the whole point of a forced suffix is that the
caller, not relaxation, decides the mode. An out-of-range value for that
mode is not an error here: ENCODE-INSTRUCTION's existing WRAP-VALUE
truncates it silently, exactly as a single-mode M1 instruction always did
(this is one more instance of the class #28 tracks unifying). The one
exception is a forced RELATIVE mode: %RELATIVE-OFFSET (below) still
range-checks unconditionally at encode time and signals ASSEMBLY-ERROR on
overflow, since that check isn't part of this filter at all.

Whose forced MODE contains a ONE-OF, several VARIANTS entries can share that
one mode name -- one sibling combo per %EXPAND-WORD-COMBOS variant
combination on a word-encoded machine (#20, instruction.lisp), or one per
claimed ONE-OF alternative on a byte-encoded machine whose sub-opcode is
hole-selected (#126, %BYTE-DESCRIPTOR-FORMS), same as an unforced candidate
set either way. %CHOICES-ELIGIBLE-P still applies here, after matching: the
plain (FIND ... :KEY #'MODE-DESCRIPTOR-NAME) below picks some same-mode
sibling just to size the \"does this mnemonic have this mode at all\" check,
but the actual return value is re-selected among every same-mode sibling for
the one whose own selector, if any, agrees with the operand's own matched
alternative -- skipping this would let an arbitrary sibling's field code (or
sub-opcode) win regardless of which ONE-OF alternative was actually written,
silently encoding the wrong addressing form exactly the way an unfiltered
candidate set would (see %CHOOSE-VARIANT's own point 1.5). A mode with no
CHOICE-selected/sub-selected field at all has only one such sibling (or
several vacuously all-eligible ones), so this changes nothing for that case.

Returns (VALUES chosen-descriptor hole-asts choices), like %CHOOSE-VARIANT
(#115) -- CHOICES is TRY-MATCH-OPERAND-MODE's own hole-aligned match result
for MODE, already computed below to check syntax, simply threaded out
instead of discarded."
  (let* ((suffix (statement-mode-suffix statement))
         (mode (find-mode-by-suffix suffix))
         (operand-tokens (statement-operand-tokens statement))
         (anchor (and (plusp (length operand-tokens)) (aref operand-tokens 0))))
    (unless mode
      (%assembly-error (statement-line statement)
                        "~A: no addressing mode has suffix ~S"
                        (statement-mnemonic statement) suffix))
    (let ((variant (find (mode-descriptor-name mode) variants
                          :key (lambda (v) (and (instruction-descriptor-mode v)
                                                 (mode-descriptor-name (instruction-descriptor-mode v)))))))
      (unless variant
        (%assembly-error (statement-line statement)
                          "~A: has no addressing-mode variant using .~A -- this instruction ~
accepts ~A"
                          (statement-mnemonic statement) suffix (%accepted-modes-text variants)))
      (multiple-value-bind (asts okp choices) (try-match-operand-mode operand-tokens mode)
        (unless okp
          (%assembly-error-at anchor
                               "~A: operand ~S does not match the forced .~A ~
(~(~A~), syntax ~A) addressing mode"
                               (statement-mnemonic statement) (%operand-text operand-tokens)
                               suffix (mode-descriptor-name mode) (%mode-syntax-text mode)))
        (values (or (find-if (lambda (v) (and (instruction-descriptor-mode v)
                                               (eq (mode-descriptor-name (instruction-descriptor-mode v))
                                                   (mode-descriptor-name mode))
                                               (%choices-eligible-p v choices)))
                              variants)
                    variant)
                asts
                choices)))))

(defun %choose-variant (statement variants address &key symbols (floor 0) (cell-width 8) finalp)
  "Pick which of a mnemonic's VARIANTS (instruction-descriptor list,
instruction.lisp) STATEMENT's operand tokens select, and the parsed hole ASTs
for that variant's mode. ADDRESS is this statement's own address; SYMBOLS,
when given, is the provisional (or, on the final layout pass, complete)
symbol table built so far -- passed to EVAL-EXPR alongside ADDRESS as :PC so
both a label reference and a location-counter hole (\"*\", #15) can fold to a
real value and take part in the value filter below. FLOOR is the narrowest
total operand width this statement is still allowed to choose -- relaxation
only ever widens a statement across layout passes (see %LAYOUT), so a
candidate narrower than FLOOR is dropped before the value filter even runs.
Filters, applied in VARIANTS' declaration order -- so an author should
declare narrower/more specific modes before wider ones that also match their
syntax (e.g. zero-page before absolute):

1. Syntax -- keep variants whose mode's pattern matches the operand tokens
   (a no-operand variant's \"pattern\" is simply an empty token run). No
   match at all is an ASSEMBLY-ERROR.
2. Floor -- drop any variant narrower than FLOOR.
3. Value -- for a variant whose mode holes fold (against SYMBOLS), keep it
   only if every value fits its own hole's operand width; if none of the
   syntax-and-floor-matching variants fit, fall back to the widest one (by
   total operand width) and let ENCODE-INSTRUCTION's existing WRAP-VALUE mask
   each value, exactly as a single-mode M1 instruction always did. If any
   hole doesn't fold (a label absent from SYMBOLS, or no SYMBOLS at all), that
   variant is excluded from the value filter and only considered as the
   *narrowest* eligible fallback instead of the widest -- an operand whose
   value isn't known yet should be given the chance to fit once it is,
   rather than committing to the widest mode up front. A RELATIVE candidate's
   hole folds to an absolute target, not the offset actually encoded, so its
   fit test goes through %RELATIVE-FITS-P instead of %FITS-WIDTH-P. A
   non-RELATIVE SIGNED candidate (mode.lisp, #30) fits against
   %FITS-SIGNED-WIDTH-P instead of %FITS-WIDTH-P, so e.g. #200 no longer
   fits a signed byte and the filter moves on to a wider candidate; every
   other mode is unaffected. Ties, in every branch, keep declaration order.
   Resolvedness is checked per
   candidate, not once for all of them: two variants of one mnemonic can
   have different hole counts (e.g. a two-register mode alongside a
   one-immediate mode), so whether their holes resolve is not the same
   question for each.

1.5 (#104/#126) Mode-choice -- between the syntax and floor filters above: on
   a word-encoded candidate (instruction.lisp, #20) whose fields include a
   (CHOICE M) variant, or a byte-encoded candidate (#126) expanded from a
   hole-selected (variant (choice m) (sub s)) selector, drop it unless M is
   the alternative each such hole actually matched (mode.lisp's hole-aligned
   MATCH-OPERAND-MODE CHOICES, #103/#104 -- see %CHOICES-ELIGIBLE-P). A
   candidate with no such selector anywhere is always eligible, so a program
   using no ONE-OF at all is unaffected. On the word path, a CHOICE-narrowed
   field's matched-but-out-of-range value has no wider CHOICE-selected
   sibling to relax into -- the ambiguity would be silent (ENCODE-
   INSTRUCTION's WRAP-VALUE would emit bits that decode as a *different*
   addressing form, not merely a truncated one) -- so once relaxation has
   converged (FINALP), an eligible-but-unfitting CHOICE-narrowed set signals
   ASSEMBLY-ERROR instead of falling back to WIDEST (see the value filter,
   point 3, and %SIGNAL-WORD-CHOICE-OVERFLOW below). The byte path's
   sub-opcode selection has no value component to overflow this way at all
   -- which ONE-OF alternative matched is purely syntactic, so eligibility
   alone always narrows a sub-selected hole to exactly one surviving
   sibling, with nothing left for the value filter or FINALP's overflow
   check to do. Deferred until FINALP for the same reason %MAYBE-WARN-
   AMBIGUOUS-MODE is: mode selection is syntax-determined and constant
   across passes (see the forced-suffix argument below), so eligibility
   itself never oscillates, but a label's *value* can still be provisional
   on a trial pass, and erroring off a value that has not settled yet would
   be a false positive.

If STATEMENT carries a forced addressing-mode suffix (#40, e.g. \"w\" from
\"lda.w\"), none of the above runs -- %CHOOSE-FORCED-VARIANT resolves the
suffix to its mode, matches syntax against that one variant only, and
returns it unconditionally, without the floor or value filter. This is
still safe for %LAYOUT's fixpoint argument: a forced statement's chosen
variant depends only on its own suffix and operand syntax, never on
SYMBOLS, so it picks the exact same (constant) width on every pass --
trivially monotone, same as sticky widening's floor, so it can never be the
statement that keeps relaxation from converging.

FINALP (#74), like %LAYOUT-PASS's own, defers a check that only makes sense
once relaxation has converged: when two or more syntax-matching candidates
tie on total operand width, declaration order alone decides between them --
genuinely ambiguous mode selection, unlike e.g. ZERO-PAGE/ABSOLUTE sharing
syntax at *different* widths, which relaxation resolves on its own and never
warns about. %MAYBE-WARN-AMBIGUOUS-MODE below only runs when FINALP is T, so
a mid-relaxation trial pass (whose candidate set can still change) never
produces a spurious or duplicate warning -- see %LAYOUT-PASS's own FINALP
for the parallel deferral.

Returns (VALUES chosen-descriptor hole-asts choices) -- CHOICES (#115) is
the hole-aligned MODE-DESCRIPTOR list TRY-MATCH-OPERAND-MODE reported for
CHOSEN's own match (NIL entries for a hole not governed by any ONE-OF, NIL
throughout for a no-operand statement), carried through so
%CHECK-STRICT-OPERAND-RANGE! (below) can read a hole's own matched
alternative's :STRICT once a value exists to check it against."
  (when (statement-mode-suffix statement)
    (return-from %choose-variant (%choose-forced-variant statement variants)))
  (let* ((tokens (statement-operand-tokens statement))
         (anchor (and (plusp (length tokens)) (aref tokens 0)))
         (candidates
           (loop for v in variants
                 for mode = (instruction-descriptor-mode v)
                 for (asts okp choices) = (multiple-value-list
                                            (if mode
                                                (try-match-operand-mode tokens mode)
                                                (values nil (zerop (length tokens)) nil)))
                 when (and okp (>= (instruction-descriptor-size v) floor)
                           ;; #104/#126: drop a candidate whose CHOICE-
                           ;; selected word field(s) or hole-selected
                           ;; sub-opcode don't match what this operand's
                           ;; ONE-OF hole(s) actually chose -- vacuously T
                           ;; for a candidate with no such selector at all.
                           (%choices-eligible-p v choices))
                   ;; #115: CHOICES rides along with each candidate (not just
                   ;; used to filter, above) so %CHECK-STRICT-OPERAND-RANGE!
                   ;; can read a hole's own matched ONE-OF alternative's
                   ;; :STRICT once ENCODE has a value to check it against.
                   collect (list v asts choices))))
    (when (null candidates)
      (%assembly-error-at anchor
                           "~A: operand ~S matches no addressing mode -- this instruction ~
accepts ~A"
                           (statement-mnemonic statement) (%operand-text tokens)
                           (%accepted-modes-text variants)))
    ;; STABLE-SORT, not SORT: ties (equal total size) must keep declaration
    ;; order.
    (let* ((width-key (lambda (c) (instruction-descriptor-size (first c))))
           ;; STABLE-SORT twice, not once-and-REVERSE: reversing a stable
           ;; descending sort breaks ties in the *wrong* order (last
           ;; declared, not first), which would silently contradict the
           ;; declaration-order tiebreak promised above and in
           ;; docs/assembler.md.
           (widest (first (stable-sort (copy-list candidates) #'> :key width-key)))
           (narrowest (first (stable-sort (copy-list candidates) #'< :key width-key)))
           (resolvedp (lambda (c)
                        (let* ((descriptor (first c))
                               (mode (instruction-descriptor-mode descriptor))
                               (word-fields (instruction-descriptor-word-fields descriptor)))
                          (handler-case
                              (let ((widths (instruction-descriptor-operand-widths descriptor))
                                    (vals (mapcar (lambda (ast)
                                                    (eval-expr ast :symbols symbols :pc address))
                                                  (second c))))
                                (cond
                                  ;; #20: a word-encoded descriptor's fit test
                                  ;; is per-field range membership, not a
                                  ;; byte width -- OPERAND-WIDTHS is NIL for
                                  ;; these, so none of the byte-encoded
                                  ;; branches below apply.
                                  (word-fields (%word-variant-fits-p vals descriptor))
                                  ((and mode (mode-descriptor-relativep mode))
                                   (%relative-fits-p (first vals) address descriptor cell-width))
                                  ;; #124/#127: per hole, not per whole mode --
                                  ;; DESCRIPTOR's own OPERAND-SIGNEDNESS
                                  ;; (instruction.lisp) already folds in
                                  ;; MODE-DESCRIPTOR-SIGNEDP for an ungoverned
                                  ;; hole, so this one branch replaces what
                                  ;; used to be two (a whole-mode :SIGNED
                                  ;; branch and a plain unsigned fallback).
                                  (t (let ((signedness (or (instruction-descriptor-operand-signedness descriptor)
                                                            (make-list (length widths)))))
                                       (every (lambda (v w signedp)
                                                (if signedp
                                                    (%fits-signed-width-p v w cell-width)
                                                    (%fits-width-p v w cell-width)))
                                              vals widths signedness)))))
                            (unresolved-label () :unresolved)))))
           (fitting (find-if (lambda (c) (eq t (funcall resolvedp c))) candidates))
           (any-unresolvedp (some (lambda (c) (eq :unresolved (funcall resolvedp c))) candidates))
           ;; #104: the first remaining CANDIDATE (if any) whose word-fields
           ;; include a CHOICE-selected one -- i.e. the eligibility filter
           ;; above actually narrowed by syntax for this statement, so a
           ;; value that doesn't fit has no wider CHOICE-selected sibling to
           ;; relax into (see %CHOOSE-VARIANT's own docstring, point 1.5).
           ;; Used both as the "narrowed at all?" test and, when so, as the
           ;; specific candidate %SIGNAL-WORD-CHOICE-OVERFLOW reports on --
           ;; on a multi-mode mnemonic mixing CHOICE- and value-selected
           ;; modes, CANDIDATES' first entry need not be this one.
           (choice-narrowed
             (find-if (lambda (c) (some #'word-field-choice-choice
                                         (instruction-descriptor-word-fields (first c))))
                      candidates))
           (chosen (cond
                     (fitting fitting)
                     (any-unresolvedp narrowest)
                     ((and finalp choice-narrowed)
                      (%signal-word-choice-overflow statement choice-narrowed symbols address anchor))
                     (t widest))))
      (when finalp
        (%maybe-warn-ambiguous-mode statement candidates chosen))
      (values-list chosen))))

(defun %word-choice-overflow-values (candidate symbols address)
  "CANDIDATE is (descriptor asts), a word-encoded %CHOOSE-VARIANT candidate
(#104) none of whose CHOICE-selected variants fit. Evaluates ASTS and finds
the first :INLINE field whose value falls outside its own (biased) RANGE.
Returns (VALUES hole-index value lo hi choice-name), or NIL if every field
does fit (not reachable from %CHOOSE-VARIANT's own call site, which only
calls this once %WORD-VARIANT-FITS-P has already said no)."
  (let* ((descriptor (first candidate))
         (vals (mapcar (lambda (ast) (eval-expr ast :symbols symbols :pc address)) (second candidate))))
    (loop for value in vals
          for field-choice in (instruction-descriptor-word-fields descriptor)
          for i from 0
          when (and (eq (word-field-choice-kind field-choice) :inline)
                    (destructuring-bind (lo . hi) (word-field-choice-range field-choice)
                      (not (<= lo value hi))))
            return (destructuring-bind (lo . hi) (word-field-choice-range field-choice)
                     (values i value lo hi (word-field-choice-choice field-choice))))))

(defun %signal-word-choice-overflow (statement candidate symbols address anchor)
  "Signal ASSEMBLY-ERROR (#104) for CANDIDATE (a word-encoded %CHOOSE-VARIANT
candidate, instruction.lisp's #20), whose matched CHOICE-selected addressing
form's own operand value doesn't fit that form's declared :RANGE -- see
%CHOOSE-VARIANT's docstring, point 1.5, for why this is an error rather than
the value filter's usual silent WRAP-VALUE fallback. ANCHOR anchors the
diagnostic at the whole operand's first token (#74), same as every other
mode-mismatch error in this file -- there is no per-hole token position kept
this far from parsing to point at just the offending hole."
  (multiple-value-bind (hole value lo hi choice-name)
      (%word-choice-overflow-values candidate symbols address)
    (let* ((descriptor (first candidate))
           (operand-name (nth hole (instruction-descriptor-operand-names descriptor))))
      (%assembly-error-at anchor
                           "~A: operand value ~D out of range ~D..~D for addressing form ~
~(~A~)~@[ (operand ~(~A~))~]"
                           (statement-mnemonic statement) value lo hi
                           choice-name operand-name))))

(defun %maybe-warn-ambiguous-mode (statement candidates chosen)
  "WARN with an AMBIGUOUS-MODE condition (#74) if CANDIDATES (the full
syntax-and-floor-matching list %CHOOSE-VARIANT built, one (descriptor asts)
pair per entry) has another candidate tied with CHOSEN on total operand
width but naming a different mode -- the one case width-based relaxation
cannot break, so declaration order alone decided. No-op when CHOSEN's own
mode is NIL (a no-operand variant, which nothing can tie against)."
  (let ((chosen-mode (instruction-descriptor-mode (first chosen))))
    (when chosen-mode
      (let* ((chosen-width (instruction-descriptor-size (first chosen)))
             (ties (remove-duplicates
                    (loop for c in candidates
                          for mode = (instruction-descriptor-mode (first c))
                          when (and mode
                                    (not (eq (mode-descriptor-name mode) (mode-descriptor-name chosen-mode)))
                                    (= (instruction-descriptor-size (first c)) chosen-width))
                            collect mode)
                    :key #'mode-descriptor-name)))
        (when ties
          (warn 'ambiguous-mode
                :mnemonic (statement-mnemonic statement)
                :chosen chosen-mode
                :alternatives ties
                :line (statement-line statement)
                :message (format nil "~A: operand matches ~D addressing modes of equal ~
width (~(~A~)~{, ~(~A~)~}) -- picked ~(~A~) by declaration order"
                                  (statement-mnemonic statement) (1+ (length ties))
                                  (mode-descriptor-name chosen-mode)
                                  (mapcar #'mode-descriptor-name ties)
                                  (mode-descriptor-name chosen-mode))))))))

;;; Directives (directive.lisp, #14) -- operand parsing and argument folding

(defun %directive-operand-ast (operand)
  "Parse OPERAND's (parser.lisp) TOKENS as a single expression, signalling
PARSE-FAILURE if they don't consume the whole operand -- a directive
operand is always one bare expression, never an addressing-mode pattern."
  (let ((tokens (operand-tokens operand)))
    (multiple-value-bind (ast next-i) (parse-expression tokens)
      (unless (= next-i (length tokens))
        (%parse-error (aref tokens next-i) "Unexpected trailing token in directive operand"))
      ast)))

(defun %directive-args (statement directive)
  "Parse STATEMENT's operands (parser.lisp) into a list of EXPR-* ASTs, one
per operand, after checking their count against DIRECTIVE's arity: exactly
one for a (:FIXED 1) directive (e.g. .ORG, .RES), any number (zero
included) for a :VARIADIC one (e.g. .BYTE, .WORD)."
  (let ((operands (statement-operands statement)))
    (let ((arity (directive-descriptor-arity directive)))
      (unless (eq arity :variadic)
        (destructuring-bind (kind n) arity
          (declare (ignore kind))
          (unless (= (length operands) n)
            (%assembly-error (statement-line statement)
                              "~A: expected ~D operand~:P, got ~D"
                              (statement-mnemonic statement) n (length operands))))))
    (mapcar #'%directive-operand-ast operands)))

(defun %directive-constant-arg (statement directive address scope constants)
  "Fold DIRECTIVE's single argument (STATEMENT's one operand) to a constant
-- used for .ORG and .RES, whose size/address effect must be known in pass
1, before any label has resolved. ADDRESS is this statement's own address
(already known in pass 1), resolving a location-counter reference (\"*\",
#15) in the operand -- e.g. \".org *+16\" pads 16 bytes forward from here.
SCOPE qualifies a local-label reference (#16) before it's folded, so the
error below names the qualified form. CONSTANTS (#35) is this pass's table
of *pure* .EQU bindings -- ones whose own value contains no label and no
\"*\", so they're already known in pass 1 -- letting e.g. \".equ bufsize, 16\"
feed \".res bufsize\"; an .EQU that isn't pure is absent from CONSTANTS, so a
reference to one still falls into the UNRESOLVED-LABEL branch below (#41
tracks lifting this restriction). Signals ASSEMBLY-ERROR (not the bare
UNRESOLVED-LABEL EVAL-EXPR itself signals) naming the offending directive,
since a plain \"unresolved label\" report wouldn't say why this one operand
can't wait for pass 2."
  (let ((ast (%qualify-locals! (first (%directive-args statement directive))
                                scope (statement-line statement))))
    (handler-case (eval-expr ast :symbols constants :pc address)
      (unresolved-label (c)
        (%assembly-error (statement-line statement)
                          "~A: operand must be a constant expression -- ~S ~
is not resolvable here (pass 1 has no symbol table yet, and only a ~
label-and-*-free .equ can be used here -- see #41)"
                          (statement-mnemonic statement) (unresolved-label-name c))))))

(defun %apply-origin-directive (statement directive address asm-origin emitted-p scope finalp constants)
  "Apply a :SET-ORIGIN directive (.ORG) at layout time. Returns (VALUES
new-address new-asm-origin): before any statement has occupied an address
(EMITTED-P NIL), .ORG moves both the address counter and the assembly's own
ORIGIN (so a leading .ORG places the whole program, and LOAD-PROGRAM's
ASSEMBLY-ORIGIN default, emulator.lisp, lands it there) -- unconditionally,
regardless of FINALP, since there is no earlier address for this branch to
move backward from. Afterwards .ORG only pads forward -- a backward move is
ambiguous (overwrite? truncate?) so it signals ASSEMBLY-ERROR instead of
guessing, but only when FINALP: an earlier statement growing on a later
layout pass (see %LAYOUT) can turn what was a legal forward pad into an
apparent backward move, and that must not fail until the widths have
actually converged -- a trial pass instead clamps forward (MAX VALUE ADDRESS)
so layout can keep iterating. A \"*\" in the operand (#15) resolves against
ADDRESS -- the counter's value *before* this .ORG moves it. CONSTANTS is
%DIRECTIVE-CONSTANT-ARG's pure-.EQU table (#35)."
  (let ((value (%directive-constant-arg statement directive address scope constants)))
    (cond
      ((not emitted-p) (values value value))
      ((>= value address) (values value asm-origin))
      (finalp (%assembly-error (statement-line statement)
                                ".org cannot move the address counter backward (from ~D to ~D)"
                                address value))
      (t (values address asm-origin)))))

;;; Local-label scoping (#16) -- qualify a local name against its nearest
;;; preceding global label before it ever reaches the (flat) symbol table.

(defun %qualify-local (scope name line)
  "Qualify local label NAME (its LOCAL-LABEL-PREFIX included, e.g. \".next\")
against SCOPE, the nearest preceding global label's name -- e.g. SCOPE
\"loop\" and NAME \".next\" qualify to \"loop.next\". Signals ASSEMBLY-ERROR
if SCOPE is NIL (a local label with no enclosing global label)."
  (unless scope
    (%assembly-error line "Local label ~S has no enclosing global label" name))
  (concatenate 'string scope name))

(defun %qualify-locals! (ast scope line)
  "Destructively rewrite every local EXPR-LABEL node (LOCALP true) reachable
from AST to its SCOPE-qualified name (%QUALIFY-LOCAL), leaving every other
node untouched. Safe to call on any AST since each statement's operand ASTs
(mode.lisp/parser.lisp) are freshly parsed and not shared -- it does not
clear LOCALP after qualifying, so calling it twice on the same node
double-qualifies the name (e.g. \"loop.next\" becomes \"looploop.next\"). This
is why %LAYOUT re-parses every statement's operands on every relaxation
pass instead of reusing one pass's ASTs on the next -- a follow-up ticket
tracks caching them across passes, which would need this cleared or the
call made idempotent some other way."
  (etypecase ast
    ((or expr-number expr-location))
    (expr-label
     (when (expr-label-localp ast)
       (setf (expr-label-name ast) (%qualify-local scope (expr-label-name ast) line))))
    (expr-unary (%qualify-locals! (expr-unary-operand ast) scope line))
    (expr-binary (%qualify-locals! (expr-binary-left ast) scope line)
                 (%qualify-locals! (expr-binary-right ast) scope line)))
  ast)

(defun %qualify-locals-in-asts! (asts scope line)
  (dolist (ast asts) (%qualify-locals! ast scope line))
  asts)

(defun %bind-symbol! (symbols info qualified-name name value line kind scope localp)
  "Bind QUALIFIED-NAME to VALUE in SYMBOLS, signalling ASSEMBLY-ERROR if it
is already bound -- the one duplicate-symbol check shared by a label
definition (%BIND-LABEL!) and an .EQU assignment (%APPLY-ASSIGN-DIRECTIVE,
#35), so \"foo: nop\" followed by \".equ foo, 5\" (or the reverse order)
signals identically either way: both a label and an .EQU claim a name in
the same flat table. Also records a SYMBOL-INFO (#37) under the same key in
INFO, capturing NAME (the unqualified spelling), KIND (:LABEL or :EQU),
SCOPE (the enclosing global, or NIL), and LOCALP -- at bind time, so this
metadata never has to be recovered later by splitting QUALIFIED-NAME (#36)."
  (when (nth-value 1 (gethash qualified-name symbols))
    (%assembly-error line "Duplicate symbol ~S" qualified-name))
  (setf (gethash qualified-name symbols) value)
  (setf (gethash qualified-name info)
        (make-symbol-info :name name :qualified-name qualified-name :scope scope
                           :kind kind :localp localp :value value :line line)))

(defun %bind-label! (statement symbols info address scope)
  "Bind STATEMENT's own label (if any) to ADDRESS in SYMBOLS (and its
SYMBOL-INFO in INFO, #37), qualifying it against SCOPE first if it's local
(#16). Returns the SCOPE in effect for any later statement: a global label
definition becomes the new scope; a local one, or no label at all, leaves
SCOPE unchanged."
  (let ((label (statement-label statement))
        (line (statement-line statement)))
    (cond
      ((null label) scope)
      ((statement-label-localp statement)
       (%bind-symbol! symbols info (%qualify-local scope label line) label address line
                       :label scope t)
       scope)
      (t
       (%bind-symbol! symbols info label label address line :label nil nil)
       label))))

;;; .EQU / symbol assignment (#35) -- a layout-time binding that occupies no
;;; address, distinct from a label (which always binds to the current
;;; address, %BIND-LABEL! above).

(defun %purep (ast)
  "T if AST (an .EQU value, already local-qualified) contains no EXPR-LABEL
and no EXPR-LOCATION node -- i.e. its value doesn't depend on any address,
so it's known even before layout has placed anything. Used to decide
whether an .EQU belongs in %LAYOUT-PASS's CONSTANTS table, the only symbols
a .ORG/.RES operand may reference (%DIRECTIVE-CONSTANT-ARG) -- an
address-dependent .EQU's value can change across relaxation passes as
labels move, which .RES's count (not sticky like an addressing-mode width)
has no mechanism to accommodate without risking non-convergence (#41 tracks
lifting this restriction with one)."
  (etypecase ast
    (expr-number t)
    ((or expr-label expr-location) nil)
    (expr-unary (%purep (expr-unary-operand ast)))
    (expr-binary (and (%purep (expr-binary-left ast)) (%purep (expr-binary-right ast))))))

(defun %apply-assign-directive (statement directive address symbols info constants scope)
  "Apply an :ASSIGN directive (.EQU, #35) at layout time. STATEMENT's first
operand must be a bare identifier (the name being bound, qualified against
SCOPE first if local, #16) and its second the value expression, folded
against ADDRESS and SYMBOLS -- the flat, incrementally-built table this
pass has bound so far, so an .EQU sees every label and .EQU defined above
it and signals ASSEMBLY-ERROR (via the UNRESOLVED-LABEL it converts) on a
forward reference, exactly like every other directive whose effect must be
known during layout. Binds NAME in both SYMBOLS and INFO (via %BIND-SYMBOL!,
tagged :KIND :EQU, #37, so it shares one duplicate check with a label) and,
when the value is address-independent (%PUREP), CONSTANTS -- see
%DIRECTIVE-CONSTANT-ARG. Does not change SCOPE: unlike a global label, an
.EQU never becomes the enclosing scope for a later local label."
  (let* ((line (statement-line statement))
         (asts (%directive-args statement directive))
         (name-ast (first asts))
         (value-ast (%qualify-locals! (second asts) scope line)))
    (unless (expr-label-p name-ast)
      (%assembly-error line "~A: first operand must be a symbol name"
                        (statement-mnemonic statement)))
    (let* ((localp (expr-label-localp name-ast))
           (unqualified-name (expr-label-name name-ast))
           (name (if localp
                     (%qualify-local scope unqualified-name line)
                     unqualified-name))
           (value (handler-case (eval-expr value-ast :symbols symbols :pc address)
                    (unresolved-label (c)
                      (%assembly-error line
                                        "~A: operand must be resolvable here -- ~
label ~S is not yet defined (an .equ can only reference a label or .equ ~
defined above it)"
                                        (statement-mnemonic statement) (unresolved-label-name c))))))
      (%bind-symbol! symbols info name unqualified-name value line :equ
                      (and localp scope) localp)
      (when (%purep value-ast)
        (setf (gethash name constants) value)))))

(defparameter *max-layout-iterations* 8
  "Safety cap on the number of trial passes %LAYOUT will run while relaxing
addressing-mode choices before giving up. Sticky widening (%LAYOUT-PASS's
FLOORS) makes the per-statement width vector monotone non-decreasing and
bounded above by each statement's widest declared variant, so it always
reaches a fixpoint in at most (length statements) passes -- exceeding this
cap without converging means that invariant has been broken elsewhere, not
that the input program is unusual. Treated as an assertion: it has no test,
since sticky widening makes it unreachable by construction.")

(defun %layout-pass (statements machine origin prev-symbols floors finalp cell-width)
  "Run one layout pass over STATEMENTS. Returns (VALUES symbols sized-entries
final-address asm-origin new-floors widths info). SYMBOLS is a fresh string ->
value hash table built by this pass alone (a label's address, or an .EQU's
folded value, #35) -- never reused across passes, since %BIND-SYMBOL!
signals on a rebind. INFO is a fresh, parallel qualified-name -> SYMBOL-INFO
table (#37), built and keyed the same way, carrying the scope/kind metadata
SYMBOLS itself cannot -- returned last so existing positional callers of the
other five values are unaffected. SIZED-ENTRIES is, in order, one tagged
entry per
mnemonic-bearing statement that occupies address space:
  (:instruction address descriptor asts line choices)
  (:emit        address width asts line)
  (:reserve     address count line)
CHOICES (#115) is :INSTRUCTION's own trailing element -- %CHOOSE-VARIANT's
hole-aligned matched-alternative list for the chosen descriptor, threaded
through so %ENCODE can read a hole's own matched ONE-OF alternative's
:STRICT once a value exists to check it against.
-- %ENCODE dispatches on the leading keyword. A .ORG statement (directive.lisp)
contributes no entry -- it only moves the address counter (and, before
anything else has been laid out, ASM-ORIGIN -- see %APPLY-ORIGIN-DIRECTIVE).
FINAL-ADDRESS is the address counter's value after the last statement;
ASM-ORIGIN is ORIGIN unless a leading .ORG moved it.

PREV-SYMBOLS is the previous pass's completed symbol table (NIL on the very
first pass, when no addresses are known yet at all) -- %CHOOSE-VARIANT folds
operand holes against it, so a label's value (forward or backward) can take
part in mode selection once a prior pass has placed it, not just a
statement's own address. FLOORS is a vector, one entry per element of
STATEMENTS (by position -- a plain index, not anything address-derived),
giving each instruction statement's narrowest still-eligible addressing-mode
width; NEW-FLOORS is a copy updated to each statement's width as chosen by
this pass. Because a candidate narrower than its floor is never considered
(%CHOOSE-VARIANT), and a chosen width becomes the next floor, floors -- and
so WIDTHS, the parallel list of chosen widths in statement order that
%LAYOUT compares between passes to detect a fixpoint -- only ever widen.

FINALP defers two checks that only make sense once relaxation has converged:
an .ORG backward move (%APPLY-ORIGIN-DIRECTIVE) can be a false positive
mid-relaxation, when an earlier statement hasn't finished widening yet.

SCOPE (the nearest preceding global label's name, #16) is threaded statement
to statement so %BIND-LABEL! can qualify a local label definition and so a
statement's own operands (its own label bound first -- \"loop: bne .x\"'s .x
is scoped to LOOP, not whatever preceded it) can be qualified via
%QUALIFY-LOCALS!.

CONSTANTS (#35) is a second, sparser table -- built alongside SYMBOLS -- of
only the *pure* .EQU bindings seen so far (%PUREP); it's what
%DIRECTIVE-CONSTANT-ARG passes to .ORG/.RES, since those must fold before
addresses are final.

CELL-WIDTH is MACHINE's own code cell width (#53, %MACHINE-CELL-WIDTH) --
every operand-width fit check below (%CHOOSE-VARIANT) is counted in cells of
this width, resolved once by %LAYOUT rather than per pass or per statement."
  (let ((symbols (make-hash-table :test 'equal))
        (info (make-hash-table :test 'equal))
        (constants (make-hash-table :test 'equal))
        (new-floors (copy-seq floors))
        (address origin)
        (asm-origin origin)
        (emitted-p nil)
        (scope nil)
        sized
        widths)
    (loop for statement in statements
          for i from 0
          do (let* ((mnemonic (statement-mnemonic statement))
                     (directive (and mnemonic (find-directive-descriptor mnemonic)))
                     (line (statement-line statement)))
               ;; A forced addressing-mode suffix (#40) names an addressing
               ;; mode, which only means something for an instruction
               ;; statement -- a directive has no addressing mode to force.
               (when (and directive (statement-mode-suffix statement))
                 (%assembly-error line "~A: a mode suffix is not valid on a directive" mnemonic))
               (cond
                 ;; .ORG binds this statement's own label (if any) to the
                 ;; address it moves *to*, not the address before the move --
                 ;; so "foo: .org $8000" binds FOO to $8000. Its own operand
                 ;; is qualified against the scope in effect *before* that
                 ;; bind (a label on a .ORG line has no bearing on its own
                 ;; operand).
                 ((and directive (eq (directive-descriptor-action directive) :set-origin))
                  (multiple-value-bind (new-address new-origin)
                      (%apply-origin-directive statement directive address asm-origin
                                                emitted-p scope finalp constants)
                    (setf address new-address asm-origin new-origin))
                  (setf scope (%bind-label! statement symbols info address scope)))
                 ;; .EQU (#35) binds a name to a computed value instead of an
                 ;; address -- it contributes no SIZED entry and does not
                 ;; advance ADDRESS or set EMITTED-P, and (unlike a global
                 ;; label) never becomes SCOPE. Still binds its own line's
                 ;; label (if any) first, same as every other statement.
                 ((and directive (eq (directive-descriptor-action directive) :assign))
                  (setf scope (%bind-label! statement symbols info address scope))
                  (%apply-assign-directive statement directive address symbols info constants scope))
                 (t
                  (setf scope (%bind-label! statement symbols info address scope))
                  (when mnemonic
                    (cond
                      (directive
                       (ecase (directive-descriptor-action directive)
                         (:reserve
                          (let ((count (%directive-constant-arg statement directive address
                                                                   scope constants)))
                            (when (minusp count)
                              (%assembly-error (statement-line statement)
                                               "~A: count must not be negative" mnemonic))
                            (cl:push (list :reserve address count (statement-line statement)) sized)
                            (incf address count)
                            (setf emitted-p t)))
                         (:emit
                          (let* ((asts (%qualify-locals-in-asts!
                                        (%directive-args statement directive) scope line))
                                 (width (directive-descriptor-width directive)))
                            (cl:push (list :emit address width asts (statement-line statement)) sized)
                            (incf address (* width (length asts)))
                            (setf emitted-p t)))))
                      (t
                       (let ((variants (find-instruction-variants machine mnemonic)))
                         (multiple-value-bind (descriptor asts choices)
                             (%choose-variant statement variants address
                                               :symbols prev-symbols :floor (aref floors i)
                                               :cell-width cell-width :finalp finalp)
                           (%qualify-locals-in-asts! asts scope line)
                           ;; #115: CHOICES rides along in the sized entry so
                           ;; %ENCODE can read a hole's own matched ONE-OF
                           ;; alternative's :STRICT (%CHECK-STRICT-OPERAND-
                           ;; RANGE!) once a value exists to check it against.
                           (cl:push (list :instruction address descriptor asts
                                          (statement-line statement) choices)
                                    sized)
                           (let ((size (instruction-descriptor-size descriptor)))
                             (setf (aref new-floors i) size)
                             (cl:push size widths)
                             (incf address size))
                           (setf emitted-p t))))))))))
    (values symbols (nreverse sized) address asm-origin new-floors (nreverse widths) info)))

(defun %layout (statements machine origin cell-width)
  "Returns (VALUES symbols sized-entries final-address asm-origin info) --
see %LAYOUT-PASS for the shape of SYMBOLS/SIZED-ENTRIES/INFO (#37). A
label-bearing (or RELATIVE-mode) operand's addressing-mode width can't be
decided in one walk over STATEMENTS, since it depends on an address that
isn't known until layout has placed it -- so %LAYOUT-PASS runs repeatedly,
re-choosing every statement's variant against the previous pass's complete
symbol table, each pass only ever widening (never re-narrowing) a statement
that no longer fits, until the vector of chosen widths stops changing. Once
two consecutive passes agree, one more pass runs with FINALP T -- surfacing
the two checks %LAYOUT-PASS defers until relaxation has settled -- and its
result, checked against the same width vector as an assertion, is returned.
CELL-WIDTH is MACHINE's own code cell width (#53), resolved once here and
threaded through every pass."
  (let ((floors (make-array (length statements) :initial-element 0))
        (widths :none)
        (symbols nil))
    (dotimes (iteration *max-layout-iterations*)
      (declare (ignore iteration))
      (multiple-value-bind (new-symbols sized final-address asm-origin new-floors new-widths)
          (%layout-pass statements machine origin symbols floors nil cell-width)
        (declare (ignore sized final-address asm-origin))
        (when (equal new-widths widths)
          (return-from %layout
            (multiple-value-bind (final-symbols final-sized final-address final-asm-origin
                                   final-floors final-widths final-info)
                (%layout-pass statements machine origin new-symbols new-floors t cell-width)
              (declare (ignore final-floors))
              (unless (equal final-widths new-widths)
                (%assembly-error nil "addressing-mode layout did not converge -- the final ~
pass chose different widths than the trial pass it followed"))
              (values final-symbols final-sized final-address final-asm-origin final-info))))
        (setf symbols new-symbols floors new-floors widths new-widths)))
    (%assembly-error nil "addressing-mode layout failed to converge after ~D iterations"
                      *max-layout-iterations*)))

;;; Pass 2: encode -- evaluate operands against the completed symbol table

(defun %relative-offset (address descriptor value line cell-width)
  "VALUE is the absolute target address a RELATIVE-mode operand (mode.lisp)
folded to; ADDRESS is this instruction's own address and DESCRIPTOR its
chosen INSTRUCTION-DESCRIPTOR. Returns the signed offset to encode, computed
from the address of the *next* instruction -- STEP-MACHINE (emulator.lisp)
advances PC past the whole instruction before running its semantics, so that
is the base a branch's own (set! pc (+ pc operand)) actually adds to.
Signals ASSEMBLY-ERROR if the offset doesn't fit the operand's width, rather
than silently wrapping to a branch at the wrong address (#23)."
  (let* ((width (instruction-descriptor-total-operand-width descriptor))
         (next-address (+ address (instruction-descriptor-size descriptor)))
         (offset (- value next-address)))
    (unless (%fits-signed-width-p offset width cell-width)
      (%assembly-error line
                        "~A: relative branch offset ~D out of range for ~D-cell operand ~
(must be between ~D and ~D)"
                        (instruction-descriptor-name descriptor) offset width
                        (- (ash 1 (1- (* cell-width width)))) (1- (ash 1 (1- (* cell-width width))))))
    offset))

(defun %operand-range (width cell-width signedp)
  "(VALUES lo hi), the inclusive range of values WIDTH cells of CELL-WIDTH
bits each can hold without WRAP-VALUE truncating -- the signed two's-
complement range when SIGNEDP, else the wider unsigned-or-signed range
%FITS-WIDTH-P itself accepts (mirrors that function's and %FITS-SIGNED-
WIDTH-P's own bounds exactly, so a strict range check and the ordinary
value filter never disagree about what \"fits\")."
  (let ((bits (* cell-width width)))
    (if signedp
        (let ((bound (ash 1 (1- bits)))) (values (- bound) (1- bound)))
        (values (- (ash 1 (1- bits))) (1- (ash 1 bits))))))

(defun %check-strict-operand-range! (descriptor mode values line cell-width choices)
  "Signal ASSEMBLY-ERROR if any of VALUES (DESCRIPTOR's already-folded
operand values, in encoding order) doesn't fit its own operand width, when
strict range-checking is in effect for *that hole* (#74, absorbing #28 and
#43's out-of-range-operand-silently-wraps reports; #115 makes the decision
per hole rather than once for the whole statement). A hole is strict when
*STRICT-OPERAND-RANGE* (diagnostic.lisp) is bound to T -- the only way to
cover a mode-less instruction's bare (operand :width n) M1-style encoding,
since :STRICT otherwise lives on a MODE-DESCRIPTOR -- or MODE itself
declares :STRICT T, or, when CHOICES (#115, %CHOOSE-VARIANT's hole-aligned
matched-ONE-OF-alternative list, assembler.lisp) names one for this hole,
*that alternative's own* :STRICT is T -- a ONE-OF alternative may declare
:STRICT independently of its siblings and of MODE's own (mode.lisp's
%CHECK-ONE-OF-ELEMENTS! is what lets :STRICT, alone among the whole-mode
attributes, appear on a ONE-OF alternative at all: it is a pure encode-time
check with no size, value, or decode consequence, so a per-hole difference
never disturbs %LAYOUT's monotone floor fixpoint). CHOICES may be NIL (a
statement with no ONE-OF hole at all, or the forced-suffix path when the
forced mode itself has none) -- every hole is then governed by MODE/*STRICT-
OPERAND-RANGE* alone, as before #115.

A no-op by design for a word-encoded DESCRIPTOR (WORD-FIELDS non-NIL -- an
:INLINE field's own RANGE is already a hard boundary chosen at
DEFINSTRUCTION time, not a WRAP-VALUE truncation, and per-hole :WIDTH
remains unsupported there regardless of :STRICT) and for a RELATIVE mode
(%RELATIVE-OFFSET below already range-checks it unconditionally, strict or
not, since a wrapped branch is a correctness bug regardless).

#124/#127: the SIGNEDP passed to %OPERAND-RANGE is DESCRIPTOR's own
per-hole OPERAND-SIGNEDNESS (instruction.lisp), the same source
%CHOOSE-VARIANT's value filter reads (assembler.lisp, above) -- not CHOICES,
even though CHOICES is already threaded through this loop for :STRICT.
%OPERAND-RANGE's own docstring promises it mirrors %FITS-WIDTH-P/%FITS-
SIGNED-WIDTH-P's bounds exactly so a strict range check and the ordinary
value filter never disagree about what \"fits\" -- reading a second,
possibly NIL (CHOICES is NIL on the forced-suffix path) source here would
risk exactly that disagreement."
  (when (and (not (instruction-descriptor-word-fields descriptor))
             (not (and mode (mode-descriptor-relativep mode))))
    (loop for value in values
          for width in (instruction-descriptor-operand-widths descriptor)
          for choice in (or choices (make-list (length values)))
          for signedp in (or (instruction-descriptor-operand-signedness descriptor)
                              (make-list (length values)))
          for hole-strictp = (or *strict-operand-range*
                                  (and mode (mode-descriptor-strictp mode))
                                  (and choice (mode-descriptor-strictp choice)))
          when hole-strictp
            do (multiple-value-bind (lo hi)
                   (%operand-range width cell-width signedp)
                 (unless (<= lo value hi)
                   (%assembly-error line
                                     "~A: operand value ~D out of range for ~D-cell operand ~
(must be between ~D and ~D)"
                                     (instruction-descriptor-name descriptor) value width lo hi))))))

(defun %make-growable-cells (size cell-width)
  (make-array size :element-type `(unsigned-byte ,cell-width) :adjustable t :fill-pointer size
                    :initial-element 0))

(defun %ensure-cells-length (cells n)
  "Grow the adjustable vector CELLS (%MAKE-GROWABLE-CELLS) to at least N
elements, zero-filling the new tail -- a directive statement can leave a
gap (a forward .ORG, #14) that no earlier entry ever writes, so the
accumulator can't be a flat push-then-reverse list the way M1/M2's
contiguous instruction stream could."
  (when (> n (length cells))
    (adjust-array cells n :fill-pointer n :initial-element 0))
  cells)

(defun %encode (sized-entries symbols origin final-address cell-width)
  "Evaluate SIZED-ENTRIES (%LAYOUT's tagged output) against the completed
symbol table SYMBOLS and write each entry's cells at its own address (minus
ORIGIN) into a cell vector, CELL-WIDTH bits per element (#53), sized to
FINAL-ADDRESS - ORIGIN. A gap between entries -- a forward .ORG, or a
.RESERVE's run -- is left zero-filled by %ENSURE-CELLS-LENGTH's growth
rather than written explicitly. A location-counter reference (\"*\", #15) in
an operand resolves against the address of the entry it's *in* -- for
:INSTRUCTION that's the whole statement's address (further adjusted by
%RELATIVE-OFFSET for a RELATIVE mode, same as gas's \"bne *\" branching to
itself); for :EMIT (e.g. \".byte 1, *, 3\") each value gets *its own*
element address, not the directive statement's address, so \".word *, *\"
emits two different words."
  (let ((cells (%make-growable-cells (max 0 (- final-address origin)) cell-width)))
    (dolist (entry sized-entries)
      (ecase (first entry)
        (:instruction
         (destructuring-bind (kind address descriptor asts line choices) entry
           (declare (ignore kind))
           (let* ((mode (instruction-descriptor-mode descriptor))
                  (values (mapcar (lambda (ast) (eval-expr ast :symbols symbols :pc address)) asts)))
             (if (and mode (mode-descriptor-relativep mode))
                 ;; %CHECK-RELATIVE-MODE-HOLES (instruction.lisp) guarantees a
                 ;; RELATIVE mode has exactly one hole, so VALUES here is
                 ;; always a single-element list.
                 (setf values (list (%relative-offset address descriptor (first values) line cell-width)))
                 ;; #74: strict range-checking runs on every other mode --
                 ;; RELATIVE's own unconditional check above already covers
                 ;; it, and running both would double-report the same value.
                 (%check-strict-operand-range! descriptor mode values line cell-width choices))
             (loop with i = (- address origin)
                   for cell in (encode-instruction descriptor values)
                   do (setf (aref cells i) cell) (incf i)))))
        (:emit
         (destructuring-bind (kind address width asts line) entry
           (declare (ignore kind line))
           (loop with i = (- address origin)
                 for ast in asts
                 do (dolist (cell (%encode-value-cells
                                    (eval-expr ast :symbols symbols :pc (+ origin i)) width cell-width))
                      (setf (aref cells i) cell) (incf i)))))
        (:reserve
         ;; Zero-filled -- %MAKE-GROWABLE-CELLS/%ENSURE-CELLS-LENGTH already
         ;; zero-initialize every element, so there is nothing to write here
         ;; beyond making sure the run is covered (relevant when a .RESERVE
         ;; is the very last statement, so no later write grows the vector
         ;; past it).
         (destructuring-bind (kind address count line) entry
           (declare (ignore kind line))
           (%ensure-cells-length cells (- (+ address count) origin))))))
    (make-array (length cells) :element-type `(unsigned-byte ,cell-width) :initial-contents cells)))

;;; Listing (#25) -- retain %LAYOUT's address<->statement mapping instead of
;;; discarding it once %ENCODE has run. See listing.lisp for the rendering
;;; and lookup entry points built on this.

(defun %sized-entry-listing-line (entry)
  "Convert one of %LAYOUT-PASS's tagged SIZED-ENTRIES to a LISTING-LINE.
Mirrors %ENCODE's own ECASE dispatch on ENTRY's leading keyword -- kept
separate from it (rather than folded into the same walk) since %ENCODE
needs SYMBOLS to evaluate operand values and this doesn't, only sizes."
  (ecase (first entry)
    (:instruction
     (destructuring-bind (kind address descriptor asts line choices) entry
       (declare (ignore asts choices))
       (make-listing-line :address address :size (instruction-descriptor-size descriptor)
                            :line line :kind kind :descriptor descriptor)))
    (:emit
     (destructuring-bind (kind address width asts line) entry
       (make-listing-line :address address :size (* width (length asts)) :line line :kind kind)))
    (:reserve
     (destructuring-bind (kind address count line) entry
       (make-listing-line :address address :size count :line line :kind kind)))))

(defun %build-listing (sized-entries)
  "SIZED-ENTRIES in address order in, LISTING-LINE list in address order out
-- see %SIZED-ENTRY-LISTING-LINE."
  (mapcar #'%sized-entry-listing-line sized-entries))

;;; Entry points

(defun assemble-statements (statements &key machine (origin 0) memory source)
  "Assemble a STATEMENT list (parser.lisp) targeting MACHINE into an
ASSEMBLY. Runs EXPAND-MACROS (macro.lisp, #33) first, so both this entry
point and ASSEMBLE (which reaches here after parsing) see .macro/.endm
blocks collected and every invocation replaced by its substituted body
before layout ever looks at the statement list. Signals ASSEMBLY-ERROR on a
duplicate symbol (a label or .EQU name bound twice, #35), an operand
matching no addressing mode, a malformed or backward-moving directive (#14,
e.g. .ORG with a label operand or one that moves the address counter
backward; #35's .EQU has the same label-free/forward-reference-free
restriction, plus the .ORG/.RES-may-only-reference-a-pure-.EQU restriction
-- see this file's header comment), MACRO-ERROR on a malformed .macro/.endm
block or invocation, UNKNOWN-INSTRUCTION on an unregistered mnemonic, and
UNRESOLVED-LABEL (via EVAL-EXPR) on a reference to a label that is never
defined anywhere in STATEMENTS. ORIGIN is the assembly's starting address
unless a leading .ORG (before any other statement occupies an address)
moves it -- see ASSEMBLY-ORIGIN. MEMORY names which of MACHINE's memory
elements this assembly is targeting, resolving its CELL-WIDTH (#53) -- the
bit width of the assembled ASSEMBLY-CELLS vector's own elements; defaults
per %MACHINE-CELL-WIDTH (MACHINE's sole memory element, or its shared
cell-width across several), same rule as LOAD-PROGRAM's own :MEMORY. A
machine with no memory element at all cannot be assembled -- its code cell
width is undefined -- and signals the same error %MACHINE-CELL-WIDTH gives
any other caller in that position. SOURCE (#25), when given, is the
original source text this STATEMENTS list came from -- ASSEMBLE passes its
own SOURCE argument through automatically; a caller building STATEMENTS by
hand (e.g. from PARSE directly, or synthesizing them) may pass it too, or
leave it NIL, in which case the returned ASSEMBLY's LISTING (below) is
still complete but ASSEMBLY-SOURCE is NIL and LISTING-TEXT (listing.lisp)
renders without a source column. Retains the address<->statement mapping
%LAYOUT computes -- discarded before #25 -- as ASSEMBLY-LISTING, a
LISTING-LINE list in address order; see listing.lisp for how it's rendered
and looked up. Also retains %LAYOUT's scope/kind metadata (#37) as
ASSEMBLY-SYMBOL-INFO, alongside ASSEMBLY-SYMBOLS itself."
  (with-source-context source
    (let ((cell-width (%machine-cell-width machine memory)))
      (multiple-value-bind (symbols sized final-address asm-origin info)
          (%layout (expand-macros statements) machine origin cell-width)
        (make-assembly :cells (%encode sized symbols asm-origin final-address cell-width)
                       :cell-width cell-width
                       :origin asm-origin :symbols symbols :symbol-info info
                       :listing (%build-listing sized) :source source)))))

(defun assemble (source &key machine (lexer 'default) (origin 0) memory)
  "Tokenize and parse SOURCE with LEXER (lexer.lisp/parser.lisp), then
ASSEMBLE-STATEMENTS the result targeting MACHINE. See ASSEMBLE-STATEMENTS
for the conditions this can signal, plus LEX-ERROR/PARSE-FAILURE from the
front end, for what MEMORY selects, and for how SOURCE (passed through
automatically here) is retained as ASSEMBLY-SOURCE (#25) and, via WITH-
SOURCE-CONTEXT (#74), on any LASM-SYNTAX-ERROR either stage signals."
  (with-source-context source
    (assemble-statements (parse source :lexer lexer) :machine machine :origin origin :memory memory
                          :source source)))
