# Diagnostics

Every LASM pipeline stage that can reject a malformed program — the lexer,
the parser, and the assembler — already carried a line and column on its
condition (`lasm-syntax-error`). What was missing was rendering that
position against the actual source line, naming what mode selection
actually found when it failed, and a way to opt into an error instead of a
silent wraparound for an out-of-range operand. `diagnostic.lisp` (loaded
right after `storage.lisp`) is the shared mechanism this document covers;
[Lexer](lexer.md), [Statement grammar & expression parser](parser.md), and
[Assembler](assembler.md) each still document their own conditions in full —
this page is about the rendering and the two opt-in behaviors (the
ambiguity warning, strict operand range), not a duplicate condition list.

## `diagnostic-text`

```lisp
(diagnostic-text condition &key source)
```

Renders a `lasm-syntax-error` (or any subtype — `lex-error`, `parse-
failure`, `assembly-error`, `macro-error`) as a report: its position and
message, followed — when source text is available at the condition's own
line — by that source line and a caret under the offending column:

```
line 3, column 5: ldx: operand "(#5),Y" matches no addressing mode -- this
instruction accepts immediate (#expr)
3 | ldx (#5),Y
  |     ^
```

`source`, when given, overrides the condition's own `lasm-syntax-error-
source` slot — useful for re-rendering a condition against different or
additional text. Degrades gracefully: a condition with no line at all
renders as a bare message; one with a line but no source (or no source at
that line) renders as the older one-line `message (line N, column C)`
form; one with a line and column but no caret-worthy source omits the caret
row but keeps the source line.

`lasm-syntax-error`'s own `:report` calls `diagnostic-text`, so printing
any of these conditions the ordinary way (an uncaught error at the REPL,
`format nil "~A" condition`) already gets the excerpt — there's nothing
extra to call at the point a condition is handled.

## Source propagation: `with-source-context`

A condition's `source` slot isn't filled at the point it's signalled — the
lexer only has the string it's tokenizing, the parser only the token
stream, and neither necessarily has the *original* source text a caller
started from (e.g. after `.include` composes several files, a follow-up
concern — see [`LASM-plan.md`](../LASM-plan.md) §M7). Instead, `tokenize`,
`parse`, and `assemble` each wrap their own body in:

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

A `relative` mode is unaffected by either switch — it already range-checks
unconditionally and errors on overflow (see [Addressing modes, "PC-relative
modes"](modes.md#pc-relative-modes)), strict or not, since a wrapped branch
is a correctness bug regardless of whether wraparound is otherwise wanted.
A word-encoded instruction (see [Assembler, "Word-encoded
instructions"](assembler.md#word-encoded-instructions-20)) is likewise
unaffected — an inline field's own declared `(range lo hi)` is already a
hard boundary chosen at `definstruction` time, not a `wrap-value`
truncation.

**One asymmetry worth knowing:** because the check runs where `%encode`
already knows the *chosen* descriptor, a per-mode `:strict` only fires when
that strict mode is the one `%choose-variant` actually picked. On a
multi-variant mnemonic where no candidate's value fits, the fallback picks
the *widest* syntax-matching candidate (see [Assembler, "Choosing a
mode"](assembler.md#choosing-a-mode)) — so an out-of-range value errors if
that widest fallback happens to be the strict mode, and wraps silently if
it isn't. `*strict-operand-range*` doesn't have this gap, since it applies
regardless of which mode was chosen.

## Follow-ups not covered here

- **Macro-expansion context.** A diagnostic inside a `.macro` expansion
  currently points at the macro's own body line, with no indication of
  which invocation produced it (see [Macros](macros.md)).
- **Source file name.** `diagnostic-text` renders `line N` with no file
  name; once file-based source loading (`assemble-file`, `.include`) lands,
  a diagnostic spanning files needs one.
- **`unresolved-label`.** The most common real assembly error — an
  undefined label — doesn't subtype `lasm-syntax-error` and carries no
  position at all; it prints as a bare message while everything documented
  above renders with a caret.
- **DSL-author diagnostics.** A malformed `definstruction`/`defmode`/
  `defmachine`/`defdirective` form signals a plain Lisp `error`, not a
  structured LASM condition — this page is about diagnosing a *program's*
  source, not a machine definition's.
