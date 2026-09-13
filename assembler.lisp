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

(in-package #:lasm)

;;; Conditions

(define-condition assembly-error (lasm-syntax-error) ()
  (:documentation "Signalled by ASSEMBLE-STATEMENTS on a malformed program:
a duplicate label, or an operand that matches no addressing mode any variant
of the instruction declares. Undefined labels are not this condition -- they
surface as UNRESOLVED-LABEL from EVAL-EXPR, since that condition already
names exactly this failure."))

(defun %assembly-error (line fmt &rest args)
  (error 'assembly-error :message (apply #'format nil fmt args) :line line))

;;; Result

(defstruct assembly
  (bytes nil :type (or null (vector (unsigned-byte 8))))
  (origin 0 :type (integer 0))
  (symbols nil :type (or null hash-table)))  ; string -> address

;;; Pass 1: layout -- size every statement, bind every label, choose modes

(defun %fits-width-p (value width)
  "T if VALUE (a folded constant) fits in WIDTH bytes, either as an unsigned
or a two's-complement signed value -- e.g. both 255 and -1 fit one byte, so an
operand that hasn't declared itself SIGNED (mode.lisp, #30) isn't rejected
just because it folds negative. Accepts the full unsigned range too, so this
is NOT the right predicate for a SIGNED mode's operand (a RELATIVE branch
offset, #23, included) -- see %FITS-SIGNED-WIDTH-P."
  (and (>= value (- (ash 1 (1- (* 8 width)))))
       (< value (ash 1 (* 8 width)))))

(defun %fits-signed-width-p (value width)
  "T if VALUE fits as a two's-complement signed WIDTH-byte integer, i.e.
-(2^(8*width-1)) <= VALUE < 2^(8*width-1). Unlike %FITS-WIDTH-P, this
rejects the unsigned-only range (e.g. +200 does not fit one byte) -- used to
range-check any SIGNED mode's operand (mode.lisp, #30), a RELATIVE mode's
offset (#23) included, where wrapping silently instead of erroring would run
the wrong (or a wrapped) value."
  (let ((bound (ash 1 (1- (* 8 width)))))
    (and (>= value (- bound)) (< value bound))))

(defun %relative-fits-p (value address descriptor)
  "T if VALUE -- the absolute target a RELATIVE candidate's hole folds to --
encodes as an offset that fits DESCRIPTOR's operand width, computed the same
way %RELATIVE-OFFSET (below) will at encode time: relative to the address of
the *next* instruction, not this one's own. Used by %CHOOSE-VARIANT's value
filter so a RELATIVE candidate can compete on width like any other once an
address is available to compute its offset from, rather than always winning
by default as the widest candidate."
  (let* ((width (instruction-descriptor-total-operand-width descriptor))
         (next-address (+ address 1 width)))
    (%fits-signed-width-p (- value next-address) width)))

(defun %choose-variant (statement variants address &key symbols (floor 0))
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

Returns (VALUES chosen-descriptor hole-asts)."
  (let* ((tokens (statement-operand-tokens statement))
         (candidates
           (loop for v in variants
                 for mode = (instruction-descriptor-mode v)
                 for (asts okp) = (multiple-value-list
                                    (if mode
                                        (try-match-operand-mode tokens mode)
                                        (values nil (zerop (length tokens)))))
                 when (and okp (>= (instruction-descriptor-total-operand-width v) floor))
                   collect (list v asts))))
    (when (null candidates)
      (%assembly-error (statement-line statement)
                        "~A: no addressing mode matches this operand"
                        (statement-mnemonic statement)))
    ;; STABLE-SORT, not SORT: ties (equal total width) must keep declaration
    ;; order.
    (let* ((width-key (lambda (c) (instruction-descriptor-total-operand-width (first c))))
           ;; STABLE-SORT twice, not once-and-REVERSE: reversing a stable
           ;; descending sort breaks ties in the *wrong* order (last
           ;; declared, not first), which would silently contradict the
           ;; declaration-order tiebreak promised above and in
           ;; docs/assembler.md.
           (widest (first (stable-sort (copy-list candidates) #'> :key width-key)))
           (narrowest (first (stable-sort (copy-list candidates) #'< :key width-key)))
           (resolvedp (lambda (c)
                        (let ((descriptor (first c))
                              (mode (instruction-descriptor-mode (first c))))
                          (handler-case
                              (let ((widths (instruction-descriptor-operand-widths descriptor))
                                    (vals (mapcar (lambda (ast)
                                                    (eval-expr ast :symbols symbols :pc address))
                                                  (second c))))
                                (cond
                                  ((and mode (mode-descriptor-relativep mode))
                                   (%relative-fits-p (first vals) address descriptor))
                                  ((and mode (mode-descriptor-signedp mode))
                                   (every #'%fits-signed-width-p vals widths))
                                  (t (every #'%fits-width-p vals widths))))
                            (unresolved-label () :unresolved)))))
           (fitting (find-if (lambda (c) (eq t (funcall resolvedp c))) candidates))
           (any-unresolvedp (some (lambda (c) (eq :unresolved (funcall resolvedp c))) candidates)))
      (values-list (or fitting (if any-unresolvedp narrowest widest))))))

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

(defun %directive-constant-arg (statement directive address scope)
  "Fold DIRECTIVE's single argument (STATEMENT's one operand) to a
label-free constant -- used for .ORG and .RES, whose size/address effect
must be known in pass 1, before any label has resolved. ADDRESS is this
statement's own address (already known in pass 1), resolving a
location-counter reference (\"*\", #15) in the operand -- e.g. \".org *+16\"
pads 16 bytes forward from here. SCOPE qualifies a local-label reference
(#16) before it's folded, so the error below names the qualified form.
Signals ASSEMBLY-ERROR (not the bare UNRESOLVED-LABEL EVAL-EXPR-CONSTANT
itself signals) naming the offending directive, since a plain \"unresolved
label\" report wouldn't say why this one operand can't wait for pass 2."
  (let ((ast (%qualify-locals! (first (%directive-args statement directive))
                                scope (statement-line statement))))
    (handler-case (eval-expr-constant ast :pc address)
      (unresolved-label (c)
        (%assembly-error (statement-line statement)
                          "~A: operand must be a constant expression -- label ~S ~
is not resolvable here (pass 1 has no symbol table yet)"
                          (statement-mnemonic statement) (unresolved-label-name c))))))

(defun %apply-origin-directive (statement directive address asm-origin emitted-p scope finalp)
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
ADDRESS -- the counter's value *before* this .ORG moves it."
  (let ((value (%directive-constant-arg statement directive address scope)))
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

(defun %bind-label! (statement symbols address scope)
  "Bind STATEMENT's own label (if any) to ADDRESS in SYMBOLS, qualifying it
against SCOPE first if it's local (#16). Returns the SCOPE in effect for any
later statement: a global label definition becomes the new scope; a local
one, or no label at all, leaves SCOPE unchanged."
  (let ((label (statement-label statement)))
    (cond
      ((null label) scope)
      ((statement-label-localp statement)
       (let ((qualified (%qualify-local scope label (statement-line statement))))
         (when (nth-value 1 (gethash qualified symbols))
           (%assembly-error (statement-line statement) "Duplicate label ~S" qualified))
         (setf (gethash qualified symbols) address))
       scope)
      (t
       (when (nth-value 1 (gethash label symbols))
         (%assembly-error (statement-line statement) "Duplicate label ~S" label))
       (setf (gethash label symbols) address)
       label))))

(defparameter *max-layout-iterations* 8
  "Safety cap on the number of trial passes %LAYOUT will run while relaxing
addressing-mode choices before giving up. Sticky widening (%LAYOUT-PASS's
FLOORS) makes the per-statement width vector monotone non-decreasing and
bounded above by each statement's widest declared variant, so it always
reaches a fixpoint in at most (length statements) passes -- exceeding this
cap without converging means that invariant has been broken elsewhere, not
that the input program is unusual. Treated as an assertion: it has no test,
since sticky widening makes it unreachable by construction.")

(defun %layout-pass (statements machine origin prev-symbols floors finalp)
  "Run one layout pass over STATEMENTS. Returns (VALUES symbols sized-entries
final-address asm-origin new-floors widths). SYMBOLS is a fresh string ->
address hash table built by this pass alone -- never reused across passes,
since %BIND-LABEL! signals on a rebind. SIZED-ENTRIES is, in order, one
tagged entry per mnemonic-bearing statement that occupies address space:
  (:instruction address descriptor asts line)
  (:emit        address width asts line)
  (:reserve     address count line)
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
%QUALIFY-LOCALS!."
  (let ((symbols (make-hash-table :test 'equal))
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
                                                emitted-p scope finalp)
                    (setf address new-address asm-origin new-origin))
                  (setf scope (%bind-label! statement symbols address scope)))
                 (t
                  (setf scope (%bind-label! statement symbols address scope))
                  (when mnemonic
                    (cond
                      (directive
                       (ecase (directive-descriptor-action directive)
                         (:reserve
                          (let ((count (%directive-constant-arg statement directive address scope)))
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
                         (multiple-value-bind (descriptor asts)
                             (%choose-variant statement variants address
                                               :symbols prev-symbols :floor (aref floors i))
                           (%qualify-locals-in-asts! asts scope line)
                           (cl:push (list :instruction address descriptor asts (statement-line statement))
                                    sized)
                           (let ((width (instruction-descriptor-total-operand-width descriptor)))
                             (setf (aref new-floors i) width)
                             (cl:push width widths)
                             (incf address (1+ width)))
                           (setf emitted-p t))))))))))
    (values symbols (nreverse sized) address asm-origin new-floors (nreverse widths))))

(defun %layout (statements machine origin)
  "Returns (VALUES symbols sized-entries final-address asm-origin) -- see
%LAYOUT-PASS for the shape of SYMBOLS/SIZED-ENTRIES. A label-bearing (or
RELATIVE-mode) operand's addressing-mode width can't be decided in one walk
over STATEMENTS, since it depends on an address that isn't known until
layout has placed it -- so %LAYOUT-PASS runs repeatedly, re-choosing every
statement's variant against the previous pass's complete symbol table, each
pass only ever widening (never re-narrowing) a statement that no longer
fits, until the vector of chosen widths stops changing. Once two consecutive
passes agree, one more pass runs with FINALP T -- surfacing the two checks
%LAYOUT-PASS defers until relaxation has settled -- and its result, checked
against the same width vector as an assertion, is returned."
  (let ((floors (make-array (length statements) :initial-element 0))
        (widths :none)
        (symbols nil))
    (dotimes (iteration *max-layout-iterations*)
      (declare (ignore iteration))
      (multiple-value-bind (new-symbols sized final-address asm-origin new-floors new-widths)
          (%layout-pass statements machine origin symbols floors nil)
        (declare (ignore sized final-address asm-origin))
        (when (equal new-widths widths)
          (return-from %layout
            (multiple-value-bind (final-symbols final-sized final-address final-asm-origin
                                   final-floors final-widths)
                (%layout-pass statements machine origin new-symbols new-floors t)
              (declare (ignore final-floors))
              (unless (equal final-widths new-widths)
                (%assembly-error nil "addressing-mode layout did not converge -- the final ~
pass chose different widths than the trial pass it followed"))
              (values final-symbols final-sized final-address final-asm-origin))))
        (setf symbols new-symbols floors new-floors widths new-widths)))
    (%assembly-error nil "addressing-mode layout failed to converge after ~D iterations"
                      *max-layout-iterations*)))

;;; Pass 2: encode -- evaluate operands against the completed symbol table

(defun %relative-offset (address descriptor value line)
  "VALUE is the absolute target address a RELATIVE-mode operand (mode.lisp)
folded to; ADDRESS is this instruction's own address and DESCRIPTOR its
chosen INSTRUCTION-DESCRIPTOR. Returns the signed offset to encode, computed
from the address of the *next* instruction -- STEP-MACHINE (emulator.lisp)
advances PC past the whole instruction before running its semantics, so that
is the base a branch's own (set! pc (+ pc operand)) actually adds to.
Signals ASSEMBLY-ERROR if the offset doesn't fit the operand's width, rather
than silently wrapping to a branch at the wrong address (#23)."
  (let* ((width (instruction-descriptor-total-operand-width descriptor))
         (next-address (+ address 1 width))
         (offset (- value next-address)))
    (unless (%fits-signed-width-p offset width)
      (%assembly-error line
                        "~A: relative branch offset ~D out of range for ~D-byte operand ~
(must be between ~D and ~D)"
                        (instruction-descriptor-name descriptor) offset width
                        (- (ash 1 (1- (* 8 width)))) (1- (ash 1 (1- (* 8 width))))))
    offset))

(defun %make-growable-bytes (size)
  (make-array size :element-type '(unsigned-byte 8) :adjustable t :fill-pointer size
                    :initial-element 0))

(defun %ensure-bytes-length (bytes n)
  "Grow the adjustable vector BYTES (%MAKE-GROWABLE-BYTES) to at least N
elements, zero-filling the new tail -- a directive statement can leave a
gap (a forward .ORG, #14) that no earlier entry ever writes, so the
accumulator can't be a flat push-then-reverse list the way M1/M2's
contiguous instruction stream could."
  (when (> n (length bytes))
    (adjust-array bytes n :fill-pointer n :initial-element 0))
  bytes)

(defun %encode (sized-entries symbols origin final-address)
  "Evaluate SIZED-ENTRIES (%LAYOUT's tagged output) against the completed
symbol table SYMBOLS and write each entry's bytes at its own address (minus
ORIGIN) into a byte vector sized to FINAL-ADDRESS - ORIGIN. A gap between
entries -- a forward .ORG, or a .RESERVE's run -- is left zero-filled by
%ENSURE-BYTES-LENGTH's growth rather than written explicitly. A
location-counter reference (\"*\", #15) in an operand resolves against the
address of the entry it's *in* -- for :INSTRUCTION that's the whole
statement's address (further adjusted by %RELATIVE-OFFSET for a RELATIVE
mode, same as gas's \"bne *\" branching to itself); for :EMIT (e.g.
\".byte 1, *, 3\") each value gets *its own* element address, not the
directive statement's address, so \".word *, *\" emits two different words."
  (let ((bytes (%make-growable-bytes (max 0 (- final-address origin)))))
    (dolist (entry sized-entries)
      (ecase (first entry)
        (:instruction
         (destructuring-bind (kind address descriptor asts line) entry
           (declare (ignore kind))
           (let* ((mode (instruction-descriptor-mode descriptor))
                  (values (mapcar (lambda (ast) (eval-expr ast :symbols symbols :pc address)) asts)))
             (when (and mode (mode-descriptor-relativep mode))
               ;; %CHECK-RELATIVE-MODE-HOLES (instruction.lisp) guarantees a
               ;; RELATIVE mode has exactly one hole, so VALUES here is
               ;; always a single-element list.
               (setf values (list (%relative-offset address descriptor (first values) line))))
             (loop with i = (- address origin)
                   for byte in (encode-instruction descriptor values)
                   do (setf (aref bytes i) byte) (incf i)))))
        (:emit
         (destructuring-bind (kind address width asts line) entry
           (declare (ignore kind line))
           (loop with i = (- address origin)
                 for ast in asts
                 do (dolist (byte (%encode-value-bytes
                                    (eval-expr ast :symbols symbols :pc (+ origin i)) width))
                      (setf (aref bytes i) byte) (incf i)))))
        (:reserve
         ;; Zero-filled -- %MAKE-GROWABLE-BYTES/%ENSURE-BYTES-LENGTH already
         ;; zero-initialize every element, so there is nothing to write here
         ;; beyond making sure the run is covered (relevant when a .RESERVE
         ;; is the very last statement, so no later write grows the vector
         ;; past it).
         (destructuring-bind (kind address count line) entry
           (declare (ignore kind line))
           (%ensure-bytes-length bytes (- (+ address count) origin))))))
    (make-array (length bytes) :element-type '(unsigned-byte 8) :initial-contents bytes)))

;;; Entry points

(defun assemble-statements (statements &key machine (origin 0))
  "Assemble a STATEMENT list (parser.lisp) targeting MACHINE into an
ASSEMBLY. Runs EXPAND-MACROS (macro.lisp, #33) first, so both this entry
point and ASSEMBLE (which reaches here after parsing) see .macro/.endm
blocks collected and every invocation replaced by its substituted body
before layout ever looks at the statement list. Signals ASSEMBLY-ERROR on a
duplicate label, an operand matching no addressing mode, a malformed or
backward-moving directive (#14, e.g. .ORG with a label operand or one that
moves the address counter backward), MACRO-ERROR on a malformed
.macro/.endm block or invocation, UNKNOWN-INSTRUCTION on an unregistered
mnemonic, and UNRESOLVED-LABEL (via EVAL-EXPR) on a reference to a label
that is never defined anywhere in STATEMENTS. ORIGIN is the assembly's
starting address unless a leading .ORG (before any other statement occupies
an address) moves it -- see ASSEMBLY-ORIGIN."
  (multiple-value-bind (symbols sized final-address asm-origin)
      (%layout (expand-macros statements) machine origin)
    (make-assembly :bytes (%encode sized symbols asm-origin final-address)
                   :origin asm-origin :symbols symbols)))

(defun assemble (source &key machine (lexer 'default) (origin 0))
  "Tokenize and parse SOURCE with LEXER (lexer.lisp/parser.lisp), then
ASSEMBLE-STATEMENTS the result targeting MACHINE. See ASSEMBLE-STATEMENTS
for the conditions this can signal, plus LEX-ERROR/PARSE-FAILURE from the
front end."
  (assemble-statements (parse source :lexer lexer) :machine machine :origin origin))
