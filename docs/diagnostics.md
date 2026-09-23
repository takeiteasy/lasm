# Diagnostics

LASM's lexer, parser, and assembler report positioned program errors through
`lasm-syntax-error`. `diagnostic.lisp` renders those positions against source
text and supports the mode-selection warning and strict operand range check;
[Lexer](lexer.md), [Statement grammar & expression parser](parser.md), and
[Assembler](assembler.md) each still document their own conditions in full —
this page is about the rendering and the two opt-in behaviors (the
ambiguity warning, strict operand range), not a duplicate condition list —
see [Conditions](conditions.md) for every condition type and its readers.

## `diagnostic-text`

```lisp
(diagnostic-text condition &key source)
```

Renders a `lasm-syntax-error` (or any subtype — `lex-error`, `parse-
failure`, `assembly-error`, `macro-error`, `unresolved-label`) as a report: its position and
message, followed — when source text is available at the condition's own
line — by that source line and a caret under the offending column:

```
line 3, column 5: ldx: operand "(#5),Y" matches no addressing mode -- this
instruction accepts immediate (#expr)
3 | ldx (#5),Y
  |     ^
```

File-backed input uses `path:line:column: message`, with the source excerpt
from that file. In-memory input uses `line N, column C: message`.
`source`, when given, overrides the condition's stored source text. A
condition without a line renders its message alone; one without available
source text omits the excerpt and caret.

`unresolved-label` retains `unresolved-label-name` and points to the label
token, including when it appears inside a larger expression. A standalone
`eval-expr` call has a position when its AST came from tokens, but no source
excerpt unless the caller supplies source context.

`lasm-syntax-error`'s own `:report` calls `diagnostic-text`, so printing
any of these conditions the ordinary way (an uncaught error at the REPL,
`format nil "~A" condition`) already gets the excerpt — there's nothing
extra to call at the point a condition is handled.

## Source propagation: `with-source-context`

A condition's `source` slot is filled by a surrounding source context.
Parsed statements retain their source text and file, so errors during include
expansion, macro expansion, layout, and encoding use the containing file.
For in-memory source, entry points use:

```lisp
(with-source-context source
  ...)
```

which installs a `handler-bind` around the body: on any `lasm-syntax-error`
that doesn't already carry a `source`, it fills the slot in place and
**declines** — returns normally from the handler rather than transferring
control — so the condition keeps propagating with its original type,
restarts, and dynamic state untouched. This is what lets a condition caught
well outside `assemble`'s own call frame — stored, logged, or re-signalled
by a caller several stack frames up — still render with a source excerpt:

```lisp
(let (caught)
  (handler-case (assemble source :machine 'sixtyfoo)
    (error (c) (setf caught c)))
  ;; still renders with the source excerpt, even here:
  (format t "~A~%" caught))
```

A condition that already carries a `source` (e.g. one that escaped a nested
`assemble-statements` call with its own `:source`) keeps that one — an
outer `with-source-context` never overwrites it.

File-backed conditions also expose `lasm-syntax-error-file`. A macro error
uses the invocation file and line as its main position; its body file and
line are available through `lasm-syntax-error-definition-file` and
`lasm-syntax-error-definition-line`.

## Mode-mismatch diagnostics

An operand matching none of a mnemonic's declared addressing modes now
names the mnemonic, echoes the operand text given, and lists every mode the
instruction accepts with its syntax:

```
ldx: operand "(#5),Y" matches no addressing mode -- this instruction
accepts immediate (#expr)
```

The same treatment applies to a forced mnemonic suffix (`lda.z`, see
[Addressing modes, "Forcing a mode with a mnemonic
suffix"](modes.md#forcing-a-mode-with-a-mnemonic-suffix)) that names a real
mode the instruction just doesn't declare, or whose syntax the operand
doesn't match.

A mode containing a `(one-of ...)` element ([Addressing modes, "Per-operand
modes"](modes.md#per-operand-modes)) renders that element as its
alternatives' own syntax joined with `|`:

```
moo: operand "$10,X" matches no addressing mode -- this instruction accepts
expr|[expr]
```

Per-hole ambiguity — two `one-of` alternatives both matching the same
operand text — is not detected: "Mode-selection ambiguity" below compares
whole modes by name, and a mode with a `one-of` element has only the one
name regardless of which alternative each hole picked (see the tracker for
this follow-up).

## Mode-selection ambiguity

Two modes sharing identical operand syntax (`zero-page`/`absolute`, a bare
`expr`) is the *designed* case — see [Addressing modes, "Declare narrower
modes before wider ones"](modes.md#declare-narrower-modes-before-wider-ones)
— and relaxation resolves it on width alone, without any warning, as long
as the two differ in size. The case that's genuinely ambiguous is two
syntax-matching candidates that **also tie on total operand width**: width
can't break the tie either, so declaration order alone decides, silently,
which mode a value gets.

```lisp
(defmode mode-a expr :width 1)
(defmode mode-b expr :width 1)

(definstruction some-machine ambi
  (modes
    (mode-a (opcode #x01) (semantics ...))
    (mode-b (opcode #x02) (semantics ...))))
```

```lisp
(assemble "ambi $10" :machine 'some-machine)
;; WARNING: ambi: operand matches 2 addressing modes of equal width
;; (mode-a, mode-b) -- picked mode-a by declaration order
```

This is a `warn`, not an error — an `ambiguous-mode` condition (a subtype
of `lasm-warning`, itself a subtype of `cl:warning`) — so assembly
completes normally; the default handler prints and resumes, same as any
other Lisp warning. `ambiguous-mode-mnemonic`, `-chosen` (the
`mode-descriptor` declaration order picked), and `-alternatives` (the other
tied candidates, in declaration order) are readable off the condition for a
caller that wants to act on it (e.g. `handler-bind` with `muffle-warning`
to suppress it once acknowledged, or to promote it to an error via
`handler-bind` binding `warning` to `error`).

The check only runs on `%choose-variant`'s *final* pass — layout relaxes a
label-bearing operand's mode across several trial passes before it settles
(see [Assembler, "Convergence"](assembler.md#convergence)), and a
mid-relaxation candidate set can still change, so warning there would
either over-report or report a tie that resolves itself by the final pass.
A forced mnemonic suffix skips mode selection entirely and so never
produces this warning either — the whole point of a forced suffix is that
the program, not relaxation, picked the mode.

## Strict operand range

An operand that doesn't fit its addressing mode's own width has always
wrapped silently via `wrap-value` rather than erroring — `ldx #300` on a
one-byte `immediate` mode truncates to `ldx #44` rather than complaining.
Some fantasy CPUs want that as defined behavior (intentional wraparound);
others want it caught. Both are now available, opt-in, default off:

```lisp
(defmode strict-imm "#" expr :width 1 :strict t)   ; this mode only

(let ((*strict-operand-range* t))                  ; every mode, globally
  (assemble ...))
```

`:strict t` on `defmode` (see [Addressing modes](modes.md)) makes any
operand encoded through that mode a hard `assembly-error` when it doesn't
fit, instead of wrapping. `*strict-operand-range*`, bound to `t`, does the
same for **every** operand regardless of mode — the only way to cover a
mode-less, M1-style single-mode instruction's bare `(operand :width n)`
encoding, since `:strict` itself lives on a `mode-descriptor`. Both check
against exactly the range `%choose-variant`'s own value filter uses
(unsigned-or-signed for an ordinary mode, signed-only for a `:signed` one),
so a strict check and the ordinary fit test never disagree about what
"fits":

```lisp
(assemble "sti #300" :machine 'some-machine)
;; assembly-error: sti: operand value 300 out of range for 1-cell operand
;; (must be between -128 and 255)
```

`:strict t` may also be declared on one alternative of a `one-of` element
(see [Addressing modes, "Per-hole `:strict`"](modes.md#per-hole-strict)),
independently of its siblings and of the mode as a whole — a hole is
strict when `*strict-operand-range*` is set, the whole mode is `:strict`,
*or* the specific alternative that hole matched is, so the same instruction
can error on one written syntax and silently wrap the identical value
written another way. The *bound* a strict hole is checked against is also
per hole when `:signed` is (see [Addressing modes, "Per-hole
`:signed`"](modes.md#per-hole-signed)) — a strict `one-of` hole whose
matched alternative is signed reports the signed range, a sibling
alternative's own unsigned range otherwise, read from the same
`operand-signedness` the ordinary (non-strict) fit test uses, so the two
never disagree about what "fits" any more than the whole-mode case above
does.

A `relative` mode is unaffected by either switch — it already range-checks
unconditionally and errors on overflow (see [Addressing modes, "PC-relative
modes"](modes.md#pc-relative-modes)), strict or not, since a wrapped branch
is a correctness bug regardless of whether wraparound is otherwise wanted.
A word-encoded instruction (see [Assembler, "Word-encoded
instructions"](assembler.md#word-encoded-instructions-20)) is likewise
unaffected — an inline field's own declared `(range lo hi)` is already a
hard boundary chosen at `definstruction` time, not a `wrap-value`
truncation.

**A `(choice mode)`-selected word field (#104) also errors unconditionally,
like `relative`**, and for the same reason: once a hole's matched
alternative has narrowed a field to a `choice`-selected variant, there is no
wider `choice`-selected sibling to relax into the way a value-selected
field's `:else` escape provides — the value either fits that one matched
alternative's declared `:range`, or the program is wrong.

```lisp
(assemble "ld 1, 9" :machine 'anima16foo)
;; assembly-error: ld: operand value 9 out of range 0..7 for addressing
;; form a-reg (operand src)
```

See [Instructions, "CHOICE-selected word
fields"](instructions.md#choice-selected-word-fields) and
[`examples/anima16.lisp`](../examples/anima16.lisp).

**One asymmetry worth knowing:** because the check runs where `%encode`
already knows the *chosen* descriptor, a per-mode `:strict` only fires when
that strict mode is the one `%choose-variant` actually picked. On a
multi-variant mnemonic where no candidate's value fits, the fallback picks
the *widest* syntax-matching candidate (see [Assembler, "Choosing a
mode"](assembler.md#choosing-a-mode)) — so an out-of-range value errors if
that widest fallback happens to be the strict mode, and wraps silently if
it isn't. `*strict-operand-range*` doesn't have this gap, since it applies
regardless of which mode was chosen.

## Opcode conflicts

`opcode-conflict` — signalled by `definstruction`, not a runtime assembly
diagnostic — covers three shapes: an unrelated mnemonic already claiming an
opcode; a word-encoded machine's own mode-distinguished variants sharing an
opcode without any operand field's raw bits actually telling them apart
("indistinguishable"); and, on a byte-encoded machine, any second descriptor
at all landing on an already-claimed opcode ("undecodable — byte-encoded"),
since a byte encoding has no per-field discriminator for decode to key off
regardless of how the two modes' syntax differs. See [Instructions, "Opcode
to descriptor decode"](instructions.md#opcode-to-descriptor-decode) for the
full picture and a worked example.

An assembly diagnostic from a macro expansion reports the outermost
invocation line and includes the emitted statement's body line as a secondary
location.

Machine-definition forms signal ordinary Lisp errors; this page covers
program-source diagnostics.
