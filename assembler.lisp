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
;;;; floor for every later pass -- which bounds the loop by possible width
;;;; increases and prevents oscillation. %LAYOUT drives
;;;; %LAYOUT-PASS to a fixpoint on widths, directive effects, and symbols,
;;;; then runs one final pass whose output feeds %ENCODE.
;;;;
;;;; A statement whose mnemonic carries a forced addressing-mode suffix
;;;; (e.g. "lda.w") has its variants narrowed to that suffix's mode by
;;;; %NARROW-TO-FORCED-MODE before %CHOOSE-VARIANT's filters run. A per-hole
;;;; forcing prefix ("seta #w:5") instead filters candidates by the word
;;;; variant :SUFFIX it names (%HOLE-PREFIXES-ELIGIBLE-P). Both depend only
;;;; on the statement's own syntax, never the symbol table, so neither
;;;; disturbs the fixpoint argument above.
;;;;
;;;; Local labels (an identifier starting with the lexer's LOCAL-LABEL-PREFIX,
;;;; e.g. ".loop") are scoped to their nearest preceding non-local ("global")
;;;; label: %LAYOUT threads a SCOPE variable, updated by every global
;;;; label definition, and qualifies each local name -- both a definition and
;;;; a reference -- to SCOPE ++ NUL ++ NAME
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
;;;; .EQU and .SET bind computed values without occupying an address. Both
;;;; fold against preceding bindings during layout. .SET may replace an
;;;; assignment, so mode selection uses its current value and emitted operands
;;;; capture it before encode. .ORG/.RES operands use current assignments and
;;;; provisional forward labels while layout settles.
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

(define-condition assertion-error (assembly-error) ()
  (:documentation "Signalled by a failed .assert or a reached .error."))

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
                                                (%one-of-alternatives el))))))
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
  (file nil :type (or null string))
  source-unit
  (definition-line nil :type (or null (integer 0)))
  (kind :instruction :type keyword)
  (descriptor nil :type (or null instruction-descriptor))
  (region nil :type (or null symbol))   ; banked region and bank the entry
  (bank nil :type (or null (integer 0)))) ; is placed in; NIL for the main image

(defstruct symbol-info
  "Metadata for a bound symbol. QUALIFIED-NAME is readable; local hash keys
use a reserved separator instead. SCOPE distinguishes equal readable names.
VALUE is the symbol's final value, LINE and FILE its invocation site (FILE
is NIL for string input), and DEFINITION-LINE and DEFINITION-FILE its macro
body site when applicable. ORDER is the binding sequence across all files."
  (name "" :type string)
  (qualified-name "" :type string)
  (scope nil :type (or null string))
  (kind :label :type keyword)
  (localp nil :type boolean)
  (value 0 :type integer)
  (line 0 :type (integer 0))
  (file nil :type (or null string))
  (definition-line nil :type (or null (integer 0)))
  (definition-file nil :type (or null string))
  (order 0 :type (integer 0))
  (region nil :type (or null symbol))   ; banked region and bank a label is
  (bank nil :type (or null (integer 0)))) ; placed in; NIL for the main image

(defvar *current-definition-line* nil)
(defvar *current-invocation-line* nil)
(defvar *current-source-unit* nil)
(defvar *current-definition-unit* nil)
(defvar *symbol-order* 0
  "Next SYMBOL-INFO-ORDER; bound per layout pass.")

;; The output a .BANK section places in one bank of a banked region: CELLS
;; spans the whole region window, starting at address ORIGIN.
(defstruct bank-image
  (region nil :type symbol)
  (bank 0 :type (integer 0))
  (origin 0 :type (integer 0))
  (cells nil :type (or null vector)))

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
  (banks nil :type list)                ; BANK-IMAGEs, by region then bank
  (symbols nil :type (or null hash-table))  ; string -> final value
  (symbol-info nil :type (or null hash-table))  ; internal key -> SYMBOL-INFO
                                                 ; (#37) -- scope/kind metadata
                                                 ; for every ASSEMBLY-SYMBOLS
                                                 ; entry, built alongside it
                                                 ; and keyed the same way
  (listing nil :type list)            ; LISTING-LINE list, ascending by
                                       ; address (#25) -- see listing.lisp
  source-unit
  (source nil :type (or null string)))  ; the original source text, or NIL
                                         ; when ASSEMBLE-STATEMENTS was
                                         ; called directly with no :SOURCE
                                         ; (#25) -- LISTING-TEXT degrades to
                                         ; an entry-ordered listing with no
                                         ; source column in that case

;;; Bank placement (.BANK) state lives in instruction.lisp, beside bank().

(defun %entry-bank-region (address size line finalp)
  "The banked region an entry of SIZE cells at ADDRESS lands in, or NIL for
the main image. Once layout has converged, signals if the entry straddles
the region's edge or the selected bank does not exist."
  (let ((region (%bank-region-at address)))
    (when (and *layout-bank* (plusp size) (not region))
      (setf region (%bank-region-at (+ address size -1))))
    (when (and region finalp)
      (when (or (< address (memory-region-start region))
                (> (+ address size -1) (memory-region-end region)))
        (%assembly-error line "output crosses the boundary of banked region ~(~A~)"
                         (memory-region-name region)))
      (when (>= *layout-bank* (memory-region-banks region))
        (%assembly-error line ".bank ~D is out of range for region ~(~A~) (~D bank~:P)"
                         *layout-bank* (memory-region-name region) (memory-region-banks region))))
    region))

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

(defun %relative-fits-p (value address descriptor width cell-width)
  "T if VALUE -- the absolute target a RELATIVE hole's value folds to --
encodes as an offset that fits WIDTH cells, computed the same way
%RELATIVE-OFFSET (below) will at encode time: relative to the address of
the *next* instruction, not this one's own. Used by %CHOOSE-VARIANT's value
filter so a RELATIVE candidate can compete on width like any other once an
address is available to compute its offset from, rather than always winning
by default as the widest candidate. This is the byte/cell-encoded branch of
that filter only -- %CHOOSE-VARIANT's word-encoded branch (#20) reaches this
function's DESCRIPTOR only when WORD-FIELDS is NIL, since a word-encoded
relative hole is range-checked against its own WORD-FIELD-CHOICE instead
(%WORD-RELATIVE-OFFSET-FITS-P, #62) -- so DESCRIPTOR here is always
cell-encoded, by construction of the caller, not because a word-encoded
:RELATIVE is unsupported.

WIDTH is deliberately the relative hole's *own* OPERAND-WIDTHS entry, not
INSTRUCTION-DESCRIPTOR-TOTAL-OPERAND-WIDTH (the sum across every hole, #130):
a multi-hole descriptor with, say, a 1-cell relative hole beside a 2-cell
plain one would otherwise let a 3-cell-wide offset silently pass this filter
and then fail %RELATIVE-OFFSET's own (correctly 1-cell) check at encode
time -- or worse, pick this candidate over a narrower one during relaxation
on the strength of a fit test that was never really testing this hole's
width. ADDRESS and DESCRIPTOR's own SIZE, by contrast, describe the whole
encoded instruction and stay as-is regardless of which hole is relative --
see %RELATIVE-OFFSET's docstring for why that half was never per-hole to
begin with."
  (let ((next-address (+ address (instruction-descriptor-size descriptor))))
    (%fits-signed-width-p (- value next-address) width cell-width)))

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

(defun %word-field-bounds (choice cell-width)
  "(VALUES lo hi), the inclusive range of pre-bias values the word field
CHOICE encodes without truncation: an :INLINE field's declared RANGE, else
its extra word's own width (signed when the field is SIGNEDP)."
  (ecase (word-field-choice-kind choice)
    (:inline (values (car (word-field-choice-range choice))
                     (cdr (word-field-choice-range choice))))
    ((:extra-word :trailing-word)
     (%operand-range (word-field-choice-extra-cells choice) cell-width
                     (word-field-choice-signedp choice)))))

(defun %word-variant-fits-p (values descriptor cell-width)
  "T if VALUES -- one already-evaluated operand value per DESCRIPTOR's
WORD-FIELDS entry, in order -- fits this word-field combo: each value must
fall within its field's %WORD-FIELD-BOUNDS. Parallel to %FITS-WIDTH-P/
%FITS-SIGNED-WIDTH-P for the byte-encoded case, used by %CHOOSE-VARIANT's
value filter to pick the narrowest (fewest extra cells) combo a value
actually fits."
  (every (lambda (value choice)
           (multiple-value-bind (lo hi) (%word-field-bounds choice cell-width)
             (<= lo value hi)))
         values (instruction-descriptor-word-fields descriptor)))

(defun %choices-eligible-p (descriptor choices &optional selections)
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
that doesn't use them.

#120: a word-encoded mode whose ONE-OF alternatives disagree on hole count
now registers sibling descriptors of *different* WORD-FIELDS lengths at one
opcode -- SELECTORS and CHOICES can then legitimately differ in length, and
the LOOP below pairs them positionally only as far as the shorter one, which
would silently judge a shorter descriptor eligible for a longer match (its
own selector list is a prefix of the longer CHOICES) rather than rejecting
it outright. Guarded first: a word-encoded DESCRIPTOR is eligible only when
its own hole count actually matches CHOICES' length."
  (when (and (instruction-descriptor-word-layout descriptor)
              (/= (length (instruction-descriptor-word-fields descriptor)) (length choices)))
    (return-from %choices-eligible-p nil))
  (unless (every (lambda (selection)
                   (equal (cdr selection) (cdr (assoc (car selection) selections))))
                 (instruction-descriptor-choice-selections descriptor))
    (return-from %choices-eligible-p nil))
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
                     (and hole-choice (equal wanted (%choice-entry-key hole-choice)))))))

(defvar *operand-cache* nil
  "Bound by %LAYOUT to an EQ table so each statement's operands parse once
across every layout pass (#39); NIL elsewhere, where nothing is cached.")

(defun %cached-operands (statement key compute)
  "Return COMPUTE's values, memoised under KEY (an EQ-comparable tag) for
STATEMENT while *OPERAND-CACHE* is bound. A COMPUTE that signals is not cached."
  (if (null *operand-cache*)
      (funcall compute)
      (let ((hit (assoc key (gethash statement *operand-cache*) :test #'eq)))
        (if hit
            (values-list (cdr hit))
            (let ((values (multiple-value-list (funcall compute))))
              (cl:push (cons key values) (gethash statement *operand-cache*))
              (values-list values))))))

(defun %narrow-to-forced-mode (statement variants)
  "STATEMENT carries a mnemonic mode suffix (e.g. \"w\" from \"lda.w\"). Return
(VALUES narrowed-variants mode): the VARIANTS using the suffix's mode -- more
than one when the mode has sibling word combos or hole-selected sub-opcodes,
which %CHOOSE-VARIANT's ordinary filters then choose between. Signals
ASSEMBLY-ERROR if no mode has the suffix or the mnemonic has no variant using
it."
  (let* ((suffix (statement-mode-suffix statement))
         (mode (find-mode-by-suffix suffix))
         (narrowed (and mode
                        (remove-if-not (lambda (v)
                                         (and (instruction-descriptor-mode v)
                                              (eq (mode-descriptor-name (instruction-descriptor-mode v))
                                                  (mode-descriptor-name mode))))
                                       variants))))
    (unless mode
      (%assembly-error (statement-line statement)
                       "~A: no addressing mode has suffix ~S"
                       (statement-mnemonic statement) suffix))
    (unless narrowed
      (%assembly-error (statement-line statement)
                       "~A: has no addressing-mode variant using .~A -- this instruction accepts ~A"
                       (statement-mnemonic statement) suffix (%accepted-modes-text variants)))
    (values narrowed mode)))

(defun %hole-prefixes-eligible-p (descriptor hole-prefixes)
  "T if every non-NIL entry of HOLE-PREFIXES (the forcing prefix written
before each hole, or NIL) names the :SUFFIX of DESCRIPTOR's word-field choice
for that hole. A descriptor without word fields has no such choice, so any
prefix makes it ineligible."
  (let ((fields (instruction-descriptor-word-fields descriptor)))
    (loop for prefix in hole-prefixes
          for i from 0
          always (or (null prefix)
                     (let ((suffix (and (< i (length fields))
                                        (word-field-choice-suffix (nth i fields)))))
                       (and suffix (string-equal suffix prefix)))))))

(defun %hole-prefix-error (statement tokens variants prefixes anchor)
  "Signal ASSEMBLY-ERROR for a forcing prefix in PREFIXES that no variant of
STATEMENT's mnemonic declares as a :SUFFIX for that hole."
  (let* ((prefix (find-if #'identity prefixes))
         (accepted (remove-duplicates
                    (loop for v in variants
                          append (loop for f in (instruction-descriptor-word-fields v)
                                       when (word-field-choice-suffix f)
                                         collect (word-field-choice-suffix f)))
                    :test #'string-equal)))
    (%assembly-error-at anchor
                        "~A: operand ~S: no variant has forcing prefix ~S -- ~:[byte-encoded ~
operands are forced with a mnemonic suffix~;accepted prefixes: ~:*~{~A~^, ~}~]"
                        (statement-mnemonic statement) (%operand-text tokens) prefix accepted)))

(defun %choose-variant (statement variants address &key symbols scope (floor 0) (cell-width 8) finalp)
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
Only the best-matching syntax is considered: candidates matching with fewer
literal tokens, or with equally many but fewer register-qualified holes, are
dropped before the filters below. The rest are applied in VARIANTS'
declaration order -- so an author should declare narrower modes before wider
ones that also match their syntax (e.g. zero-page before absolute):

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

1.5 (#104/#126) Mode-choice -- after the syntax and floor filters above (the
   candidate loop below applies it last of the three, though as a pure
   predicate with no side effects its position relative to floor is
   immaterial to the result): on
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

If STATEMENT carries a forced addressing-mode suffix (e.g. \"w\" from
\"lda.w\"), VARIANTS is first narrowed to that suffix's mode
(%NARROW-TO-FORCED-MODE) and the filters above run over what is left. A
hole forcing prefix (\"#w:5\") additionally keeps only candidates whose word
field for that hole declares the named :SUFFIX. Neither reads SYMBOLS, so
both keep the choice constant across passes, trivially monotone.

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
  (let* ((forced-mode (and (statement-mode-suffix statement)
                           (multiple-value-bind (narrowed mode) (%narrow-to-forced-mode statement variants)
                             (setf variants narrowed)
                             mode)))
         (tokens (statement-operand-tokens statement))
         (anchor (and (plusp (length tokens)) (aref tokens 0)))
         (candidates
           (loop for v in variants
                 for mode = (instruction-descriptor-mode v)
                 for (asts okp choices selections hole-prefixes ties score)
                   = (multiple-value-list
                      (if mode
                          (%cached-operands statement mode
                                            (lambda () (try-match-operand-mode tokens mode)))
                          (values nil (zerop (length tokens)) nil nil nil nil (cons 0 0))))
                 when (and okp (>= (instruction-descriptor-size v) floor)
                           ;; #104/#126: drop a candidate whose CHOICE-
                           ;; selected word field(s) or hole-selected
                           ;; sub-opcode don't match what this operand's
                           ;; ONE-OF hole(s) actually chose -- vacuously T
                           ;; for a candidate with no such selector at all.
                           (%choices-eligible-p v choices selections)
                           (%hole-prefixes-eligible-p v hole-prefixes))
                   ;; #115: CHOICES rides along with each candidate (not just
                   ;; used to filter, above) so %CHECK-STRICT-OPERAND-RANGE!
                   ;; can read a hole's own matched ONE-OF alternative's
                   ;; :STRICT once ENCODE has a value to check it against.
                   collect (list v (%qualify-locals-in-asts! asts scope (statement-line statement))
                                 choices selections hole-prefixes ties score))))
    (when (null candidates)
      (let ((prefixes (loop for v in variants
                            for mode = (instruction-descriptor-mode v)
                            for prefixes = (and mode (nth-value 4 (%cached-operands
                                                                   statement mode
                                                                   (lambda () (try-match-operand-mode tokens mode)))))
                            when (some #'identity prefixes) return prefixes)))
        (when prefixes
          (%hole-prefix-error statement tokens variants prefixes anchor)))
      (when forced-mode
        (%assembly-error-at anchor
                            "~A: operand ~S does not match the forced .~A (~(~A~), syntax ~A) addressing mode"
                            (statement-mnemonic statement) (%operand-text tokens)
                            (statement-mode-suffix statement) (mode-descriptor-name forced-mode)
                            (%mode-syntax-text forced-mode)))
      (%assembly-error-at anchor
                           "~A: operand ~S matches no addressing mode -- this instruction ~
accepts ~A"
                           (statement-mnemonic statement) (%operand-text tokens)
                           (%accepted-modes-text variants)))
    (let ((best (reduce (lambda (a b) (if (%score> b a) b a)) candidates :key #'seventh)))
      (setf candidates (remove-if-not (lambda (c) (equal (seventh c) best)) candidates)))
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
                               (word-fields (instruction-descriptor-word-fields descriptor)))
                          (handler-case
                              (let ((widths (instruction-descriptor-operand-widths descriptor))
                                    (vals (mapcar (lambda (ast)
                                                    (eval-expr ast :symbols symbols :pc address))
                                                  (second c))))
                                (cond
                                  ;; Word fields check their own ranges after
                                  ;; relative targets become offsets.
                                  (word-fields (%word-variant-fits-p
                                                (%relative-adjusted-values address descriptor vals)
                                                descriptor cell-width))
                                  ;; Byte fields each use their resolved attributes.
                                  (t (let ((signedness (or (instruction-descriptor-operand-signedness descriptor)
                                                            (make-list (length widths))))
                                           (relative-holes (instruction-descriptor-relative-holes descriptor)))
                                       (loop for v in vals
                                             for w in widths
                                             for signedp in signedness
                                             for i from 0
                                             always (if (nth i relative-holes)
                                                        (%relative-fits-p v address descriptor w cell-width)
                                                        (if signedp
                                                            (%fits-signed-width-p v w cell-width)
                                                            (%fits-width-p v w cell-width))))))))
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
             (find-if (lambda (c) (or (some #'word-field-choice-choice
                                            (instruction-descriptor-word-fields (first c)))
                                      (some #'identity (fifth c))))
                      candidates))
           (chosen (cond
                     (fitting fitting)
                     (any-unresolvedp narrowest)
                     ((and finalp choice-narrowed)
                      (%signal-word-choice-overflow statement choice-narrowed symbols address anchor cell-width))
                     (t widest))))
      (when finalp
        (%maybe-warn-ambiguous-mode statement candidates chosen)
        (%maybe-warn-ambiguous-alternative statement chosen))
      (values-list chosen))))

(defun %word-choice-overflow-values (candidate symbols address cell-width)
  "CANDIDATE is (descriptor asts), a word-encoded %CHOOSE-VARIANT candidate
(#104) none of whose CHOICE-selected variants fit. Evaluates ASTS and finds
the first field whose value falls outside its own bound -- an :INLINE
field's (biased) RANGE, or, #135, an :EXTRA-WORD field's own EXTRA-CELLS
width (via %OPERAND-RANGE, signed when the field is SIGNEDP). Returns
(VALUES hole-index value lo hi choice-name), or NIL if every field does fit
(not reachable from %CHOOSE-VARIANT's own call site, which only calls this
once %WORD-VARIANT-FITS-P has already said no). #62: %RELATIVE-ADJUSTED-
VALUES folds relative targets down to raw offsets first (a
no-op when DESCRIPTOR has none), so a relative hole's own overflow, if
that's the one that doesn't fit, is reported as the offset it actually
tried to encode, not the absolute branch target."
  (let* ((descriptor (first candidate))
         (vals (%relative-adjusted-values
                address descriptor
                (mapcar (lambda (ast) (eval-expr ast :symbols symbols :pc address)) (second candidate)))))
    (loop for value in vals
          for field-choice in (instruction-descriptor-word-fields descriptor)
          for i from 0
          do (multiple-value-bind (lo hi) (%word-field-bounds field-choice cell-width)
               (unless (<= lo value hi)
                 (return (values i value lo hi (word-field-choice-choice field-choice))))))))

(defun %signal-word-choice-overflow (statement candidate symbols address anchor cell-width)
  "Signal ASSEMBLY-ERROR (#104) for CANDIDATE (a word-encoded %CHOOSE-VARIANT
candidate, instruction.lisp's #20), whose matched CHOICE-selected addressing
form's own operand value doesn't fit that form's declared :RANGE -- see
%CHOOSE-VARIANT's docstring, point 1.5, for why this is an error rather than
the value filter's usual silent WRAP-VALUE fallback. ANCHOR anchors the
diagnostic at the whole operand's first token (#74), same as every other
mode-mismatch error in this file -- there is no per-hole token position kept
this far from parsing to point at just the offending hole."
  (multiple-value-bind (hole value lo hi choice-name)
      (%word-choice-overflow-values candidate symbols address cell-width)
    (let* ((descriptor (first candidate))
           (operand-name (nth hole (instruction-descriptor-operand-names descriptor))))
      (%assembly-error-at anchor
                           "~A: operand value ~D out of range ~D..~D~@[ for addressing form ~
~(~A~)~]~@[ (operand ~(~A~))~]"
                           (statement-mnemonic statement) value lo hi
                           choice-name operand-name))))

(defun %maybe-warn-ambiguous-mode (statement candidates chosen)
  "WARN with an AMBIGUOUS-MODE condition (#74) if CANDIDATES (the full
syntax-and-floor-matching list %CHOOSE-VARIANT built, one (descriptor asts)
pair per entry) has another candidate tied with CHOSEN on total operand
width but naming a different mode -- the one case neither relaxation nor
specificity can break, so declaration order alone decided. No-op when
CHOSEN's own mode is NIL (a no-operand variant, which nothing can tie against)."
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

(defun %maybe-warn-ambiguous-alternative (statement chosen)
  "WARN with an AMBIGUOUS-ALTERNATIVE condition for each ONE-OF pick in
CHOSEN's match (a %CHOOSE-VARIANT candidate) that declaration order alone
decided -- see TRY-MATCH-OPERAND-MODE's tie records."
  (loop for (hole slot alt . runners-up) in (sixth chosen)
        do (warn 'ambiguous-alternative
                 :mnemonic (statement-mnemonic statement)
                 :chosen alt
                 :alternatives runners-up
                 :hole hole
                 :slot slot
                 :line (statement-line statement)
                 :message (format nil "~A: operand hole ~D matches ~D alternatives equally ~
(~(~A~)~{, ~(~A~)~}) -- picked ~(~A~) by declaration order"
                                  (statement-mnemonic statement) hole (1+ (length runners-up))
                                  (mode-descriptor-name alt)
                                  (mapcar #'mode-descriptor-name runners-up)
                                  (mode-descriptor-name alt)))))

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

(defun %assert-statement-p (statement)
  (let ((mnemonic (statement-mnemonic statement)))
    (and mnemonic (string-equal mnemonic ".assert"))))

(defun %error-statement-p (statement)
  (let ((mnemonic (statement-mnemonic statement)))
    (and mnemonic (string-equal mnemonic ".error"))))

(defun %message-operand (operand statement)
  "The string OPERAND of STATEMENT's .assert or .error."
  (let ((tokens (operand-tokens operand)))
    (unless (and (= 1 (length tokens)) (eq (token-type (aref tokens 0)) :string))
      (%assembly-error (statement-line statement) "~A: expected a quoted message"
                       (statement-mnemonic statement)))
    (token-value (aref tokens 0))))

(defun %assert-parts (statement)
  "Returns (VALUES condition-ast message) for a .assert STATEMENT."
  (let ((operands (statement-operands statement))
        (line (statement-line statement)))
    (when (statement-mode-suffix statement)
      (%assembly-error line ".assert: a mode suffix is not valid here"))
    (unless (<= 1 (length operands) 2)
      (%assembly-error line ".assert: expected a condition and an optional message"))
    (%cached-operands statement :assert
                      (lambda ()
                        (values (%directive-operand-ast (first operands))
                                (and (second operands) (%message-operand (second operands) statement)))))))

(defun %signal-user-error (statement)
  "Signal ASSERTION-ERROR with the message of a reached .error STATEMENT."
  (let ((operands (statement-operands statement))
        (line (statement-line statement)))
    (when (statement-mode-suffix statement)
      (%assembly-error line ".error: a mode suffix is not valid here"))
    (unless (= 1 (length operands))
      (%assembly-error line ".error: expected a quoted message"))
    (error 'assertion-error :message (%message-operand (first operands) statement) :line line)))

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
    (%cached-operands statement :args
                      (lambda () (mapcar #'%directive-operand-ast operands)))))

(defun %expand-string-operands (asts terminator element-bits line mnemonic)
  "Replace each top-level EXPR-STRING in ASTS with one EXPR-NUMBER per
character, followed by TERMINATOR when non-NIL. Other ASTs pass through."
  (loop for ast in asts
        if (expr-string-p ast)
          append (append
                  (loop for char across (expr-string-value ast)
                        for code = (char-code char)
                        do (unless (< code (ash 1 element-bits))
                             (%assembly-error line "~A: character ~S does not fit a ~D-bit element"
                                              mnemonic char element-bits))
                        collect (make-expr-number :value code))
                  (and terminator (list (make-expr-number :value terminator))))
        else collect ast))

(defun %directive-constant-arg (statement directive address scope directive-symbols labels previous)
  "Fold .ORG/.RES's operand against current symbols and previous-pass forward
labels. Return NIL when a known forward label has no provisional value yet."
  (let ((ast (%qualify-locals! (first (%directive-args statement directive))
                                scope (statement-line statement))))
    (handler-case (eval-expr ast :symbols directive-symbols :pc address)
      (unresolved-label (c)
        (if (and (null previous) (gethash (unresolved-label-name c) labels))
            nil
            (%assembly-error (statement-line statement)
                             "~A: symbol ~S is not resolvable here"
                             (statement-mnemonic statement)
                             (%display-symbol-key (unresolved-label-name c))))))))

(defun %apply-origin-directive (statement directive address asm-origin emitted-p scope finalp directive-symbols labels previous main-end)
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
ADDRESS -- the counter's value *before* this .ORG moves it. EMITTED-P and
MAIN-END describe the main image only: a .ORG into a banked region (see
%BANK-REGION-AT) may move anywhere and never moves the assembly's origin."
  (let ((value (%directive-constant-arg statement directive address scope directive-symbols labels previous)))
    (cond
      ((null value) (values address asm-origin))
      ((%bank-region-at value) (values value asm-origin))
      ((not emitted-p) (values value value))
      ((>= value main-end) (values value asm-origin))
      (finalp (%assembly-error (statement-line statement)
                                ".org cannot move the address counter backward (from ~D to ~D)"
                                main-end value))
      (t (values main-end asm-origin)))))

;;; Local-label scoping (#16) -- qualify a local name against its nearest
;;; preceding global label before it ever reaches the (flat) symbol table.

(defun %qualify-local (scope name line)
  "Return the internal key for local NAME under SCOPE."
  (unless scope
    (%assembly-error line "Local label ~S has no enclosing global label" name))
  (concatenate 'string scope (string #\Null) name))

(defun %display-qualified-local (scope name)
  (concatenate 'string scope name))

(defun %qualify-locals! (ast scope line)
  "Destructively rewrite every local EXPR-LABEL node (LOCALP true) reachable
from AST to its SCOPE-qualified name (%QUALIFY-LOCAL), leaving every other
node untouched. Clears LOCALP afterwards, so qualifying an AST again (a
cached operand AST, #39) is a no-op: the first scope wins."
  (etypecase ast
    ((or expr-number expr-string expr-location))
    (expr-label
     (when (expr-label-localp ast)
       (setf (expr-label-name ast) (%qualify-local scope (expr-label-name ast) line)
             (expr-label-localp ast) nil)))
    (expr-unary (%qualify-locals! (expr-unary-operand ast) scope line))
    (expr-binary (%qualify-locals! (expr-binary-left ast) scope line)
                 (%qualify-locals! (expr-binary-right ast) scope line)))
  ast)

(defun %qualify-locals-in-asts! (asts scope line)
  (dolist (ast asts) (%qualify-locals! ast scope line))
  asts)

(defun %bank-region-name (address)
  (let ((region (%bank-region-at address)))
    (and region (memory-region-name region))))

(defun %bind-symbol! (symbols info qualified-name name value line kind scope localp &optional rebindp directive-symbols)
  "Bind a symbol and its metadata. REBINDP permits replacing assignments only."
  (when (and *register-aliases* (nth-value 1 (gethash name *register-aliases*)))
    (%assembly-error line "~A ~S collides with a register alias of the same name"
                      (case kind (:equ ".equ") (:set ".set") (otherwise "Label")) name))
  (when (nth-value 1 (gethash qualified-name symbols))
    (unless (and rebindp (member (symbol-info-kind (gethash qualified-name info))
                                 '(:equ :set)))
      (%assembly-error line "Duplicate symbol ~S" (%display-symbol-key qualified-name))))
  (setf (gethash qualified-name symbols) value)
  (when (and *label-banks* (eq kind :label))
    (setf (gethash qualified-name *label-banks*) (and (%bank-region-name value) *layout-bank*)))
  (when directive-symbols
    (setf (gethash qualified-name directive-symbols) value))
  (setf (gethash qualified-name info)
        (make-symbol-info :name name
                           :qualified-name (if localp
                                               (%display-qualified-local scope name)
                                               qualified-name)
                           :scope scope
                           :kind kind :localp localp :value value :line line
                           :file (and *current-source-unit* (source-unit-file *current-source-unit*))
                           :definition-line *current-definition-line*
                           :definition-file (and *current-definition-line* *current-definition-unit*
                                                 (source-unit-file *current-definition-unit*))
                           :order (prog1 *symbol-order* (incf *symbol-order*))
                           :region (and (eq kind :label) (%bank-region-name value))
                           :bank (and (eq kind :label) (%bank-region-name value) *layout-bank*))))

(defun %bind-label! (statement symbols info address scope directive-symbols)
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
                       :label scope t nil directive-symbols)
       scope)
      (t
       (%bind-symbol! symbols info label label address line :label nil nil nil directive-symbols)
       label))))

;;; Assignments bind values at layout time without occupying an address.

(defun %apply-assign-directive (statement directive address symbols info scope directive-symbols)
  "Bind .EQU or .SET at layout time and return its name and folded value."
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
symbol ~S is not yet defined"
                                        (statement-mnemonic statement)
                                        (%display-symbol-key (unresolved-label-name c)))))))
      (let ((kind (if (eq (directive-descriptor-action directive) :reassign)
                      :set :equ)))
        (%bind-symbol! symbols info name unqualified-name value line kind
                        (and localp scope) localp (eq kind :set) directive-symbols)
        (values name value)))))

(defun %capture-set-values (ast symbols set-names line)
  "Return AST with references to reassignable names replaced by their value at
this statement. AST itself is never modified (it may be a cached operand AST,
#39); unchanged subtrees are shared."
  (etypecase ast
    ((or expr-number expr-string expr-location) ast)
    (expr-label
     (if (gethash (expr-label-name ast) set-names)
         (multiple-value-bind (value presentp) (gethash (expr-label-name ast) symbols)
           (unless presentp
             (%assembly-error line "Symbol ~S is not yet assigned"
                              (%display-symbol-key (expr-label-name ast))))
           (make-expr-number :value value))
         ast))
    (expr-unary
     (if (eq (expr-unary-op ast) :defined)
         ast
         (let ((operand (%capture-set-values (expr-unary-operand ast) symbols set-names line)))
           (if (eq operand (expr-unary-operand ast))
               ast
               (let ((copy (copy-expr-unary ast)))
                 (setf (expr-unary-operand copy) operand)
                 copy)))))
    (expr-binary
     (let ((left (%capture-set-values (expr-binary-left ast) symbols set-names line))
           (right (%capture-set-values (expr-binary-right ast) symbols set-names line)))
       (if (and (eq left (expr-binary-left ast)) (eq right (expr-binary-right ast)))
           ast
           (let ((copy (copy-expr-binary ast)))
             (setf (expr-binary-left copy) left
                   (expr-binary-right copy) right)
             copy))))))

(defun %mode-symbols (previous set-names)
  "Keep forward labels from the previous pass while excluding future .set values."
  (if (or (null previous) (zerop (hash-table-count set-names)))
      previous
      (let ((symbols (make-hash-table :test 'equal)))
        (maphash (lambda (name value) (setf (gethash name symbols) value)) previous)
        (maphash (lambda (name presentp)
                   (declare (ignore presentp))
                   (remhash name symbols))
                 set-names)
        symbols)))

(defun %layout-labels (statements)
  "Collect label definitions before resolving forward directive references."
  (let ((labels (make-hash-table :test 'equal))
        (assignments (make-hash-table :test 'equal))
        (scopes (make-array (length statements)))
        (scope nil))
    (loop for statement in statements for i from 0
          for directive = (and (statement-mnemonic statement)
                               (find-directive-descriptor (statement-mnemonic statement)))
          for action = (and directive (directive-descriptor-action directive))
          do (setf (aref scopes i) scope)
             (let ((label (statement-label statement)))
               (when label
                 (setf (gethash (if (statement-label-localp statement)
                                    (%qualify-local scope label (statement-line statement))
                                    label)
                                labels)
                       (cons i action))
                 (unless (statement-label-localp statement)
                   (setf scope label))))
             (unless (eq action :set-origin)
               (setf (aref scopes i) scope))
             (when (member action '(:assign :reassign))
               (let* ((name-ast (first (%directive-args statement directive)))
                      (name (and (expr-label-p name-ast)
                                 (if (expr-label-localp name-ast)
                                     (%qualify-local scope (expr-label-name name-ast)
                                                     (statement-line statement))
                                     (expr-label-name name-ast)))))
                 (when name (cl:push i (gethash name assignments))))))
    (values labels assignments scopes)))

(defun %first-expr-label (ast)
  (etypecase ast
    ((or expr-number expr-string expr-location) nil)
    (expr-label (expr-label-name ast))
    (expr-unary (%first-expr-label (expr-unary-operand ast)))
    (expr-binary (or (%first-expr-label (expr-binary-left ast))
                     (%first-expr-label (expr-binary-right ast))))))

(defun %check-layout-dependencies (statements labels assignments scopes)
  "Reject directive operands whose address effects depend on themselves."
  (let* ((n (length statements))
         (edges (make-array (* 3 n) :initial-element nil))
         (directives (make-array n :initial-element nil))
         (emitted-p nil))
    (labels ((before (i) (* 2 i))
             (after (i) (1+ (* 2 i)))
             (assignment (i) (+ (* 2 n) i))
             (add (from to) (cl:pushnew to (aref edges from)))
             (walk-expr (ast from i)
               (etypecase ast
                 ((or expr-number expr-string) nil)
                 (expr-location (add from (before i)))
                 (expr-label
                  (let* ((name (expr-label-name ast))
                         (definition (gethash name labels))
                         (prior (find-if (lambda (j) (< j i))
                                         (gethash name assignments))))
                    (cond
                      ((and prior (or (null definition) (< prior (car definition))))
                       (add from (assignment prior)))
                      (definition
                       (add from (if (eq (cdr definition) :set-origin)
                                     (after (car definition))
                                     (before (car definition))))))))
                 (expr-unary (walk-expr (expr-unary-operand ast) from i))
                 (expr-binary (walk-expr (expr-binary-left ast) from i)
                              (walk-expr (expr-binary-right ast) from i))))
             (operand (statement directive i from)
               (walk-expr (%qualify-locals!
                           (first (%directive-args statement directive))
                           (aref scopes i) (statement-line statement))
                          from i)))
      (loop for statement in statements for i from 0
            for directive = (and (statement-mnemonic statement)
                                 (find-directive-descriptor (statement-mnemonic statement)))
            for action = (and directive (directive-descriptor-action directive))
            do (when (plusp i) (add (before i) (after (1- i))))
               (case action
                 (:set-origin
                  (setf (aref directives i) statement)
                  (operand statement directive i (after i))
                  (when emitted-p (add (after i) (before i))))
                 (:reserve
                  (setf (aref directives i) statement)
                  (add (after i) (before i))
                  (operand statement directive i (after i))
                  (setf emitted-p t))
                 (:select-bank
                  (setf (aref directives i) statement)
                  (add (after i) (before i))
                  (operand statement directive i (after i)))
                 ((:assign :reassign)
                  (add (after i) (before i))
                  (walk-expr (%qualify-locals!
                              (second (%directive-args statement directive))
                              (aref scopes i) (statement-line statement))
                             (assignment i) i))
                 (otherwise
                  (add (after i) (before i))
                  (when (statement-mnemonic statement) (setf emitted-p t)))))
      (let ((active (make-hash-table))
            (seen (make-hash-table)))
        (loop for i below n when (aref directives i)
              do (labels ((visit (node)
                           (when (gethash node active)
                             (let* ((statement (aref directives i))
                                    (directive (find-directive-descriptor
                                                (statement-mnemonic statement)))
                                    (name (%first-expr-label
                                           (first (%directive-args statement directive)))))
                               (%assembly-error (statement-line statement)
                                                "~A: cyclic layout dependency through ~S"
                                                (statement-mnemonic statement) name)))
                           (unless (gethash node seen)
                             (setf (gethash node active) t)
                             (dolist (next (aref edges node)) (visit next))
                             (remhash node active)
                             (setf (gethash node seen) t))))
                   (visit (after i))))))
    labels))

(defun %layout-iteration-bound (statements machine)
  "Allow one full symbol-propagation sweep between instruction widenings."
  (let ((widenings
          (loop for statement in statements
                for mnemonic = (statement-mnemonic statement)
                when (and mnemonic (not (find-directive-descriptor mnemonic))
                          (not (%assert-statement-p statement)))
                  sum (max 0 (1- (length (remove-duplicates
                                         (mapcar #'instruction-descriptor-size
                                                 (find-instruction-variants machine mnemonic)))))))))
    (+ 2 (* (1+ widenings) (1+ (length statements))))))

(defun %same-symbols-p (left right)
  (and left (= (hash-table-count left) (hash-table-count right))
       (loop for name being the hash-keys of left using (hash-value value)
             always (multiple-value-bind (other foundp) (gethash name right)
                      (and foundp (eql value other))))))

(defun %layout-pass (statements machine origin prev-symbols set-names floors finalp cell-width labels prev-banks)
  "Run one layout pass over STATEMENTS. Returns (VALUES symbols sized-entries
final-address asm-origin new-floors widths info effects banks). SYMBOLS is a fresh string ->
value hash table built by this pass alone (a label's address or an
assignment's current value). INFO is a fresh, parallel qualified-name -> SYMBOL-INFO
table (#37), built and keyed the same way, carrying the scope/kind metadata
SYMBOLS itself cannot. SIZED-ENTRIES is, in order, one tagged
entry per
mnemonic-bearing statement that occupies address space:
  (:instruction address descriptor asts line choices definition-line unit definition-unit)
  (:emit        address width endian asts line definition-line unit definition-unit)
  (:reserve     address count line definition-line unit definition-unit)
CHOICES (#115) is :INSTRUCTION's own trailing element -- %CHOOSE-VARIANT's
hole-aligned matched-alternative list for the chosen descriptor, threaded
through so %ENCODE can read a hole's own matched ONE-OF alternative's
:STRICT once a value exists to check it against.
-- %ENCODE dispatches on the leading keyword. A .ORG statement (directive.lisp)
contributes no entry -- it only moves the address counter (and, before
anything else has been laid out, ASM-ORIGIN -- see %APPLY-ORIGIN-DIRECTIVE).
FINAL-ADDRESS is the end of the main image (banked output excluded);
ASM-ORIGIN is ORIGIN unless a leading .ORG moved it.

PREV-SYMBOLS is the previous pass's completed symbol table (NIL on the very
first pass, when no addresses are known yet at all). %CHOOSE-VARIANT uses
its label values and the current pass's .SET values for mode selection.
SET-NAMES identifies assignments whose operands capture source-order values.
FLOORS is a vector, one entry per element of
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

LABELS identifies forward label names for directive operands. EFFECTS records
the resulting .ORG addresses and .RES counts for convergence checks.

CELL-WIDTH is MACHINE's own code cell width (#53, %MACHINE-CELL-WIDTH) --
every operand-width fit check below (%CHOOSE-VARIANT) is counted in cells of
this width, resolved once by %LAYOUT rather than per pass or per statement."
  (let ((symbols (make-hash-table :test 'equal))
        (directive-symbols (make-hash-table :test 'equal))
        (info (make-hash-table :test 'equal))
        (mode-symbols (%mode-symbols prev-symbols set-names))
        (effects (make-array (length statements) :initial-element nil))
        (new-floors (copy-seq floors))
        (address origin)
        (asm-origin origin)
        (main-end origin)
        (*symbol-order* 0)
        (*layout-bank* nil)
        (*label-banks* (make-hash-table :test 'equal))
        (emitted-p nil)
        (scope nil)
        sized
        widths)
    (when prev-banks
      (maphash (lambda (name bank) (setf (gethash name *label-banks*) bank)) prev-banks))
    (when prev-symbols
      (maphash (lambda (name value)
                 (when (gethash name labels)
                   (setf (gethash name directive-symbols) value)))
               prev-symbols))
    (loop for statement in statements
          for i from 0
          do (let* ((mnemonic (statement-mnemonic statement))
                     (directive (and mnemonic (find-directive-descriptor mnemonic)))
                     (line (statement-line statement))
                     (*current-invocation-line* (and (statement-definition-line statement) line))
                     (*current-definition-line* (statement-definition-line statement))
                     (*current-source-unit* (statement-source-unit statement))
                     (*current-definition-unit* (statement-definition-unit statement)))
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
                                                emitted-p scope finalp directive-symbols labels prev-symbols
                                                main-end)
                    (setf address new-address asm-origin new-origin)
                    (unless (%bank-region-at address) (setf main-end address)))
                  (setf (aref effects i) address)
                  (setf scope (%bind-label! statement symbols info address scope directive-symbols)))
                 ((and directive (eq (directive-descriptor-action directive) :select-bank))
                  (setf scope (%bind-label! statement symbols info address scope directive-symbols))
                  (unless *banked-regions*
                    (%assembly-error line ".bank: the target memory has no banked region"))
                  (let ((bank (%directive-constant-arg statement directive address
                                                       scope directive-symbols labels prev-symbols)))
                    (setf (aref effects i) bank)
                    (when bank
                      (unless (>= bank 0)
                        (%assembly-error line ".bank: bank must not be negative"))
                      (setf *layout-bank* bank)
                      (cl:push (list :bank bank line *current-definition-line*
                                     *current-source-unit* *current-definition-unit*)
                               sized))))
                 ;; Assignments contribute no SIZED entry or address space.
                 ;; A label on the same line binds first and sets local scope.
                 ((and directive (member (directive-descriptor-action directive)
                                         '(:assign :reassign)))
                  (setf scope (%bind-label! statement symbols info address scope directive-symbols))
                  (multiple-value-bind (name value)
                      (%apply-assign-directive statement directive address symbols info scope directive-symbols)
                    (when (and mode-symbols (gethash name set-names))
                      (setf (gethash name mode-symbols) value))))
                 ((%assert-statement-p statement)
                  (setf scope (%bind-label! statement symbols info address scope directive-symbols))
                  (multiple-value-bind (ast message) (%assert-parts statement)
                    (cl:push (list :assert address
                                   (%capture-set-values (%qualify-locals! ast scope line)
                                                        symbols set-names line)
                                   message line *current-definition-line*
                                   *current-source-unit* *current-definition-unit*)
                             sized)))
                 (t
                  (setf scope (%bind-label! statement symbols info address scope directive-symbols))
                  (when mnemonic
                    (cond
                      (directive
                       (ecase (directive-descriptor-action directive)
                         (:reserve
                          (let ((count (%directive-constant-arg statement directive address
                                                                   scope directive-symbols labels prev-symbols)))
                            (setf (aref effects i) count)
                            (setf count (or count 0))
                            (when (minusp count)
                              (%assembly-error (statement-line statement)
                                               "~A: count must not be negative" mnemonic))
                            (cl:push (list :reserve address count line *current-definition-line*
                                           *current-source-unit* *current-definition-unit*) sized)
                            (let ((region (%entry-bank-region address count line finalp)))
                              (incf address count)
                              (unless region (setf emitted-p t main-end address)))))
                         (:emit
                          (let* ((width (directive-descriptor-width directive))
                                 (asts (mapcar (lambda (ast)
                                                 (%capture-set-values ast symbols set-names line))
                                               (%qualify-locals-in-asts!
                                                (%expand-string-operands
                                                 (%directive-args statement directive)
                                                 (directive-descriptor-terminator directive)
                                                 (* width cell-width) line mnemonic)
                                                scope line))))
                            (cl:push (list :emit address width (directive-descriptor-endian directive)
                                           asts line *current-definition-line*
                                           *current-source-unit* *current-definition-unit*) sized)
                            (let ((region (%entry-bank-region address (* width (length asts)) line finalp)))
                              (incf address (* width (length asts)))
                              (unless region (setf emitted-p t main-end address)))))))
                      (t
                       (let ((variants (find-instruction-variants machine mnemonic)))
                         (multiple-value-bind (descriptor asts choices)
                             (%choose-variant statement variants address
                                               :symbols mode-symbols :scope scope :floor (aref floors i)
                                               :cell-width cell-width :finalp finalp)
                           (setf asts (mapcar (lambda (ast)
                                                (%capture-set-values ast symbols set-names line)) asts))
                           ;; #115: CHOICES rides along in the sized entry so
                           ;; %ENCODE can read a hole's own matched ONE-OF
                           ;; alternative's :STRICT (%CHECK-STRICT-OPERAND-
                           ;; RANGE!) once a value exists to check it against.
                           (cl:push (list :instruction address descriptor asts
                                          line choices *current-definition-line*
                                          *current-source-unit* *current-definition-unit*)
                                    sized)
                           (let* ((size (instruction-descriptor-size descriptor))
                                  (region (%entry-bank-region address size line finalp)))
                             (setf (aref new-floors i) size)
                             (cl:push size widths)
                             (incf address size)
                             (unless region (setf emitted-p t main-end address))))))))))))
    (values symbols (nreverse sized) main-end asm-origin new-floors (nreverse widths) info effects
            *label-banks*)))

(defun %layout (statements machine origin cell-width)
  "Returns (VALUES symbols sized-entries final-address asm-origin info) --
see %LAYOUT-PASS for the shape of SYMBOLS/SIZED-ENTRIES/INFO (#37). A
label-bearing (or RELATIVE-mode) operand's addressing-mode width can't be
decided in one walk over STATEMENTS, since it depends on an address that
isn't known until layout has placed it -- so %LAYOUT-PASS runs repeatedly,
re-choosing every statement's variant against the previous pass's complete
symbol table, each pass only ever widening a statement that no longer fits.
The loop stops when widths, directive effects, and symbols all agree across
passes. One final pass checks that layout still agrees.
CELL-WIDTH is MACHINE's own code cell width (#53), resolved once here and
threaded through every pass. Operands parse once, into *OPERAND-CACHE* (#39)."
  (let ((*operand-cache* (make-hash-table :test 'eq)))
    (%layout-passes statements machine origin cell-width)))

(defun %layout-passes (statements machine origin cell-width)
  "The relaxation loop of %LAYOUT."
  (multiple-value-bind (labels assignments scopes) (%layout-labels statements)
    (%check-layout-dependencies statements labels assignments scopes)
    (let ((floors (make-array (length statements) :initial-element 0))
          (widths :none)
          (effects nil)
          (banks nil)
          (symbols nil)
          (set-names (make-hash-table :test 'equal))
          (iteration-bound (%layout-iteration-bound statements machine)))
    (loop repeat iteration-bound do
      (multiple-value-bind (new-symbols sized final-address asm-origin new-floors new-widths new-info new-effects new-banks)
          (%layout-pass statements machine origin symbols set-names floors nil cell-width labels banks)
        (declare (ignore sized final-address asm-origin))
        (maphash (lambda (name info)
                   (when (eq :set (symbol-info-kind info))
                     (setf (gethash name set-names) t)))
                 new-info)
        (when (and (equal new-widths widths)
                   (equalp new-effects effects)
                   (%same-symbols-p new-symbols symbols)
                   (%same-symbols-p new-banks banks))
          (return-from %layout-passes
            (multiple-value-bind (final-symbols final-sized final-address final-asm-origin
                                   final-floors final-widths final-info final-effects final-banks)
                (%layout-pass statements machine origin new-symbols set-names new-floors t cell-width labels new-banks)
              (declare (ignore final-floors))
              (unless (and (equal final-widths new-widths)
                           (equalp final-effects new-effects)
                           (%same-symbols-p final-symbols new-symbols)
                           (%same-symbols-p final-banks new-banks))
                (%assembly-error nil "layout did not converge in the final pass"))
              (values final-symbols final-sized final-address final-asm-origin final-info final-banks))))
        (setf symbols new-symbols floors new-floors widths new-widths effects new-effects
              banks new-banks)))
    (%assembly-error nil "addressing-mode layout failed to converge after ~D iterations"
                      iteration-bound))))

;;; Pass 2: encode -- evaluate operands against the completed symbol table

(defun %relative-adjusted-values (address descriptor values)
  "Return VALUES with each relative target converted to its unchecked offset from the end of DESCRIPTOR."
  (let ((relative-holes (instruction-descriptor-relative-holes descriptor)))
    (if (some #'identity relative-holes)
        (let ((next-address (+ address (instruction-descriptor-size descriptor))))
          (loop for v in values
                for i from 0
                collect (if (nth i relative-holes) (- v next-address) v)))
        values)))

(defun %word-relative-offset-fits-p (offset choice cell-width)
  "T if OFFSET -- a word-encoded RELATIVE hole's already-computed signed
offset (#62) -- fits CHOICE, the relative hole's own WORD-FIELD-CHOICE:
membership in its own (pre-bias) RANGE for an :INLINE choice, exactly
%WORD-VARIANT-FITS-P's own :INLINE test (above) applied to this one field in
isolation; signed fit within CHOICE's own extra-word width (#135; CHOICE's
own EXTRA-CELLS * CELL-WIDTH bits, not always the instruction word's own
WIDTH-CELLS) for an :EXTRA-WORD choice, mirroring %RELATIVE-FITS-P's
byte-path bound but sized to the chosen extra word rather than an
OPERAND-WIDTHS cell count, since that's what an escaped extra-word actually
spills into (ENCODE-INSTRUCTION, instruction.lisp)."
  (ecase (word-field-choice-kind choice)
    (:inline (destructuring-bind (lo . hi) (word-field-choice-range choice)
               (<= lo offset hi)))
    (:extra-word (%fits-signed-width-p offset (word-field-choice-extra-cells choice) cell-width))))

(defun %relative-offset (address descriptor index value line cell-width)
  "Return the checked signed offset for field INDEX from the end of DESCRIPTOR."
  (let* ((word-fields (instruction-descriptor-word-fields descriptor))
         (next-address (+ address (instruction-descriptor-size descriptor)))
         (offset (- value next-address)))
    (if word-fields
        (let ((choice (nth index word-fields)))
          (unless (%word-relative-offset-fits-p offset choice cell-width)
            (%assembly-error line
                              "~A: relative branch offset ~D out of range for its instruction-word ~
field" (instruction-descriptor-name descriptor) offset)))
        (let ((width (nth index (instruction-descriptor-operand-widths descriptor))))
          (unless (%fits-signed-width-p offset width cell-width)
            (%assembly-error line
                              "~A: relative branch offset ~D out of range for ~D-cell operand ~
(must be between ~D and ~D)"
                              (instruction-descriptor-name descriptor) offset width
                              (- (ash 1 (1- (* cell-width width)))) (1- (ash 1 (1- (* cell-width width))))))))
    offset))

(defun %entry-strict-p (entry)
  "T if a hole the matcher CHOICES ENTRY covers can be strict: a nested ONE-OF
whose pick no entry records may still hold a strict alternative."
  (and entry (%mode-strict-reachable-p (if (consp entry) (first entry) entry))))

(defun %hole-choice-strict-p (choices i)
  "T if the alternative CHOICES records for hole I declares :STRICT for it.
Every hole of a matched alternative carries the same entry, so a run of equal
entries splits into whole alternatives of %OPTION-HOLE-COUNT holes each."
  (let ((entry (nth i choices)))
    (when (and entry (%entry-strict-p entry))
      (let* ((key (%choice-entry-key entry))
             (start (loop with j = i
                          while (and (plusp j) (equal (nth (1- j) choices) entry))
                          do (decf j)
                          finally (return j))))
        (or (nth (mod (- i start) (%option-hole-count key)) (%option-hole-attributes key :strict))
            (mode-descriptor-strictp (%choice-key-descriptor key)))))))

(defun %check-strict-operand-range! (descriptor mode values line cell-width choices)
  "Check ordinary strict fields against the bounds of the field each is
encoded into. Relative fields are checked by %RELATIVE-OFFSET."
  (let ((relative-holes (instruction-descriptor-relative-holes descriptor))
        (word-fields (instruction-descriptor-word-fields descriptor)))
    (loop for value in values
          for i from 0
          when (and (not (nth i relative-holes))
                    (or *strict-operand-range*
                        (and mode (mode-descriptor-strictp mode))
                        (%hole-choice-strict-p choices i)))
            do (if word-fields
                   (multiple-value-bind (lo hi) (%word-field-bounds (nth i word-fields) cell-width)
                     (unless (<= lo value hi)
                       (%assembly-error line
                                         "~A: operand value ~D out of range for its instruction-word ~
field (must be between ~D and ~D)"
                                         (instruction-descriptor-name descriptor) value lo hi)))
                   (let ((width (nth i (instruction-descriptor-operand-widths descriptor)))
                         (signedp (nth i (instruction-descriptor-operand-signedness descriptor))))
                     (multiple-value-bind (lo hi) (%operand-range width cell-width signedp)
                       (unless (<= lo value hi)
                         (%assembly-error line
                                           "~A: operand value ~D out of range for ~D-cell operand ~
(must be between ~D and ~D)"
                                           (instruction-descriptor-name descriptor) value width lo hi))))))))

(defun %check-register-operand-range! (descriptor values line)
  "Signal an assembly error for a :REGISTER hole whose value indexes outside its bank."
  (let ((registers (instruction-descriptor-operand-registers descriptor)))
    (when (some #'identity registers)
      (let ((table (machine-descriptor-table
                    (find-machine-descriptor (instruction-descriptor-machine descriptor)))))
        (loop for register in registers
              for value in values
              when register
                do (let ((count (storage-element-count (gethash register table))))
                     (unless (< -1 value count)
                       (%assembly-error line
                                        "~A: register index ~D out of range for ~A (must be between 0 and ~D)"
                                        (instruction-descriptor-name descriptor) value register
                                        (1- count)))))))))

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

(defun %encode (sized-entries symbols origin final-address cell-width endian)
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
emits two different words. ENDIAN (#66) governs :EMIT's own
%ENCODE-VALUE-CELLS call and :INSTRUCTION's encoding.

A :BANK entry (.BANK) selects the bank for the entries after it. An entry
landing in a banked region while a bank is selected is written to that bank's
BANK-IMAGE instead. Returns (VALUES main-cells bank-images), the images
ordered by region then bank; overlapping output in one bank is an error."
  (let ((cells (%make-growable-cells (max 0 (- final-address origin)) cell-width))
        (banks (make-hash-table :test 'equal))
        (*layout-bank* nil))
    (labels ((bank-target (region address size line)
               (let* ((key (cons (memory-region-name region) *layout-bank*))
                      (start (memory-region-start region))
                      (entry (or (gethash key banks)
                                 (let ((length (1+ (- (memory-region-end region) start))))
                                   (setf (gethash key banks)
                                         (cons (make-bank-image
                                                :region (memory-region-name region)
                                                :bank *layout-bank* :origin start
                                                :cells (make-array length
                                                                   :element-type `(unsigned-byte ,cell-width)
                                                                   :initial-element 0))
                                               (make-array length :element-type 'bit
                                                                  :initial-element 0)))))))
                 (loop for i from (- address start) repeat size
                       do (when (= 1 (sbit (cdr entry) i))
                            (%assembly-error line "output overlaps earlier output at ~D in bank ~D of ~(~A~)"
                                             (+ start i) *layout-bank* (memory-region-name region)))
                          (setf (sbit (cdr entry) i) 1))
                 (values (bank-image-cells (car entry)) (- address start))))
             (target (address size line)
               (let ((region (%bank-region-at address)))
                 (cond (region (bank-target region address size line))
                       ((< address origin)
                        (%assembly-error line "output at ~D lies below the assembly's origin ~D"
                                         address origin))
                       (t (values cells (- address origin)))))))
      (dolist (entry sized-entries)
        (let ((*current-definition-line* (nth (- (length entry) 3) entry))
              (*current-source-unit* (nth (- (length entry) 2) entry))
              (*current-definition-unit* (car (last entry)))
              (*current-invocation-line* (and (nth (- (length entry) 3) entry)
                                              (ecase (first entry)
                                                (:instruction (fifth entry))
                                                (:emit (sixth entry))
                                                (:reserve (fourth entry))
                                                (:assert (fifth entry))
                                                (:bank (third entry))))))
          (ecase (first entry)
            (:bank (setf *layout-bank* (second entry)))
            (:assert
             (destructuring-bind (kind address ast message line definition-line unit definition-unit) entry
               (declare (ignore kind definition-line unit definition-unit))
               (when (zerop (eval-expr ast :symbols symbols :pc address))
                 (error 'assertion-error :message (or message "assertion failed") :line line))))
            (:instruction
             (destructuring-bind (kind address descriptor asts line choices definition-line unit definition-unit) entry
               (declare (ignore kind definition-line unit definition-unit))
               (let* ((mode (instruction-descriptor-mode descriptor))
                      (relative-holes (instruction-descriptor-relative-holes descriptor))
                      (values (mapcar (lambda (ast) (eval-expr ast :symbols symbols :pc address)) asts)))
                 (loop for value in values
                       for relativep in relative-holes
                       for i from 0
                       when relativep
                         do (setf (nth i values)
                                  (%relative-offset address descriptor i value line cell-width)))
                 (%check-register-operand-range! descriptor values line)
                 (%check-strict-operand-range! descriptor mode values line cell-width choices)
                 (let* ((encoded (%encode-instruction-resolved descriptor values cell-width endian))
                        (shadow nil)
                        (removedp nil))
                   (multiple-value-setq (shadow removedp) (%shadowing-descriptor descriptor encoded))
                   (when shadow
                     (%assembly-error line (if removedp
                                               "~A: this encoding is the removed instruction ~A"
                                               "~A: this encoding decodes as ~A")
                                      (instruction-descriptor-name descriptor)
                                      (instruction-descriptor-name shadow)))
                   (multiple-value-bind (target-cells start) (target address (length encoded) line)
                     (loop with i = start
                           for cell in encoded
                           do (setf (aref target-cells i) cell) (incf i)))))))
            (:emit
             (destructuring-bind (kind address width entry-endian asts line definition-line unit definition-unit) entry
               (declare (ignore kind definition-line unit definition-unit))
               (multiple-value-bind (target-cells start) (target address (* width (length asts)) line)
                 (loop with i = start
                       for ast in asts
                       do (dolist (cell (%encode-value-cells
                                          (eval-expr ast :symbols symbols :pc (+ address (- i start)))
                                          width cell-width (or entry-endian endian)))
                            (setf (aref target-cells i) cell) (incf i))))))
            (:reserve
             ;; Zero-filled -- the arrays are zero-initialized, so nothing is
             ;; written beyond covering the run (relevant when a .RESERVE is
             ;; the very last statement, so no later write grows the main
             ;; vector past it).
             (destructuring-bind (kind address count line definition-line unit definition-unit) entry
               (declare (ignore kind definition-line unit definition-unit))
               (if (%bank-region-at address)
                   (target address count line)
                   (%ensure-cells-length cells (- (+ address count) origin)))))))))
    (values (make-array (length cells) :element-type `(unsigned-byte ,cell-width) :initial-contents cells)
            (loop for region in *banked-regions*
                  append (sort (loop for key being the hash-keys of banks using (hash-value entry)
                                     when (eq (car key) (memory-region-name region))
                                       collect (car entry))
                               #'< :key #'bank-image-bank)))))

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
     (destructuring-bind (kind address descriptor asts line choices definition-line unit definition-unit) entry
       (declare (ignore asts choices definition-unit))
       (make-listing-line :address address :size (instruction-descriptor-size descriptor)
                            :line line :definition-line definition-line
                            :source-unit unit :file (and unit (source-unit-file unit))
                            :kind kind :descriptor descriptor)))
    (:emit
     (destructuring-bind (kind address width endian asts line definition-line unit definition-unit) entry
       (declare (ignore definition-unit endian))
       (make-listing-line :address address :size (* width (length asts))
                          :line line :definition-line definition-line
                          :source-unit unit :file (and unit (source-unit-file unit)) :kind kind)))
    (:reserve
     (destructuring-bind (kind address count line definition-line unit definition-unit) entry
       (declare (ignore definition-unit))
       (make-listing-line :address address :size count :line line
                          :definition-line definition-line :source-unit unit
                          :file (and unit (source-unit-file unit)) :kind kind)))))

(defun %build-listing (sized-entries)
  "SIZED-ENTRIES in address order in, LISTING-LINE list in address order out
-- see %SIZED-ENTRY-LISTING-LINE. :ASSERT entries produce no line; :BANK
entries produce none either, but set the bank the following lines are tagged
with."
  (let ((*layout-bank* nil) lines)
    (dolist (entry sized-entries)
      (case (first entry)
        (:bank (setf *layout-bank* (second entry)))
        (:assert)
        (t
         (let* ((line (%sized-entry-listing-line entry))
                (region (%bank-region-at (listing-line-address line))))
           (when region
             (setf (listing-line-region line) (memory-region-name region)
                   (listing-line-bank line) *layout-bank*))
           (cl:push line lines)))))
    (nreverse lines)))

;;; Entry points

(defun assemble-statements (statements &key machine (lexer 'default) (origin 0) memory source source-unit)
  "Assemble a STATEMENT list (parser.lisp) targeting MACHINE into an
ASSEMBLY. Runs PREPROCESS (preprocess.lisp) first, so both this entry
point and ASSEMBLE (which reaches here after parsing) see .include, .macro/.endm
and .if blocks resolved before layout ever looks at the statement list;
LEXER parses any included file. Signals ASSEMBLY-ERROR on a
duplicate symbol, an operand matching no addressing mode, a malformed or
backward-moving directive, a forward assignment reference, or a cyclic
.ORG/.RES address dependency. Signals MACRO-ERROR on a malformed .macro/.endm
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
any other caller in that position. MEMORY's declared :ENDIAN (#66) governs
.BYTE/.WORD data the same way it governs instruction operands -- resolved
via %MACHINE-ENDIAN, same rule. SOURCE (#25), when given, is the
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
    ;; #72: *REGISTER-ALIASES* (instruction.lisp) in scope for both EVAL-EXPR
    ;; (operand/assignment folding) and %BIND-SYMBOL!'s alias-collision
    ;; check, for the whole of this assembly.
    (let* ((cell-width (%machine-cell-width machine memory))
           (*cell-width* cell-width)
           (endian (%machine-endian machine memory))
           (*banked-regions*
             (let ((element (descriptor-element (find-machine-descriptor machine)
                                                (%resolve-memory machine memory))))
               (remove-if-not #'memory-region-banks (storage-element-regions element))))
            (*register-aliases* (machine-descriptor-register-aliases (find-machine-descriptor machine)))
            (*register-alias-elements* (machine-descriptor-register-alias-elements (find-machine-descriptor machine))))
      (handler-bind ((lasm-syntax-error
                       (lambda (condition)
                         (when *current-source-unit*
                           (unless (lasm-syntax-error-source condition)
                             (setf (lasm-syntax-error-source condition)
                                   (source-unit-text *current-source-unit*)))
                           (unless (lasm-syntax-error-file condition)
                             (setf (lasm-syntax-error-file condition)
                                   (source-unit-file *current-source-unit*))))
                         (when *current-invocation-line*
                           (setf (lasm-syntax-error-line condition)
                                 *current-invocation-line*
                                 (lasm-syntax-error-column condition) nil
                                 (lasm-syntax-error-definition-line condition)
                                 *current-definition-line*)
                           (when *current-definition-unit*
                             (setf (lasm-syntax-error-definition-file condition)
                                   (source-unit-file *current-definition-unit*)
                                   (lasm-syntax-error-definition-source condition)
                                   (source-unit-text *current-definition-unit*)))))))
        (multiple-value-bind (symbols sized final-address asm-origin info label-banks)
            (%layout (preprocess statements :machine machine :lexer lexer)
                     machine origin cell-width)
          (multiple-value-bind (cells bank-images)
              (let ((*label-banks* label-banks))
                (%encode sized symbols asm-origin final-address cell-width endian))
           (make-assembly :cells cells :banks bank-images
                         :cell-width cell-width
                         :origin asm-origin :symbols symbols :symbol-info info
                         :listing (%build-listing sized) :source source
                         :source-unit source-unit)))))))

(defun assemble (source &key machine (lexer 'default) (origin 0) memory file)
  "Tokenize and parse SOURCE with LEXER (lexer.lisp/parser.lisp), then
ASSEMBLE-STATEMENTS the result targeting MACHINE. See ASSEMBLE-STATEMENTS
for the conditions this can signal, plus LEX-ERROR/PARSE-FAILURE from the
front end, for what MEMORY selects, and for how SOURCE is retained as
ASSEMBLY-SOURCE and on positioned conditions. FILE names SOURCE in
diagnostics and listings when supplied."
  (multiple-value-bind (statements unit) (parse source :lexer lexer :file file)
    (with-source-unit unit
      (assemble-statements statements
                           :machine machine :lexer lexer :origin origin :memory memory
                           :source source :source-unit unit))))

(defun assemble-file (path &key machine (lexer 'default) (origin 0) memory)
  "Read the source file at PATH (conventionally .asm or .s) and ASSEMBLE its
text; see ASSEMBLE for the keys and conditions. A missing or unreadable file
signals the ordinary CL FILE-ERROR."
  (let* ((text (%read-source-file path))
         (file (truename path))
         (*include-directory* (%file-directory file))
         (*include-chain* (list file)))
    (assemble text :machine machine :lexer lexer :origin origin :memory memory
                   :file path)))
