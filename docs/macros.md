# Macros

`.macro`/`.endm` captures a range of statements as a reusable template and
substitutes each invocation's actual arguments into it — something no
`defdirective` action form can express, since a directive is a per-statement
action with a statically-known size, while a macro spans a range of
statements and its expansion isn't known until it's substituted (see
[Directives, "Scope: `.macro` is not a
directive"](directives.md#scope-macro-is-not-a-directive)). It's implemented
as its own statement-expansion pass, `(expand-macros statements machine)`
(`macro.lisp`), which
`assemble-statements` ([Assembler](assembler.md)) runs before layout ever
sees the statement list — so both of `assemble-statements`'s entry points
(`assemble`, and a caller already holding a parsed `statement` list, see
[Statement grammar & expression parser](parser.md)) get macro expansion for
free.

```lisp
.macro addconst dst, k     ; dst = *dst + k
    lda dst
    adc #k
    sta dst
.endm

    addconst cell, 5       ; expands to: lda cell / adc #5 / sta cell
```

See [`examples/macros.lisp`](../examples/macros.lisp) for a runnable version.

## Syntax

A macro definition is `.macro name param...` — a name followed by zero or
more plain identifier parameters. Commas between parameters are optional when
there are no defaults. A trailing parameter may declare a default token run:

```lisp
.macro addconst dst, k=1 + 2
    lda dst
    adc #k
    sta dst
.endm

    addconst cell       ; k expands to 1 + 2
    addconst cell, 5    ; k expands to 5
```

Headers with defaults require commas between parameters. Required parameters
precede optional ones. A default is inserted as written; it does not expand
references to other parameters. Everything between `.macro` and `.endm` is
stored until invocation. A macro can be invoked before its definition because
expansion collects all definitions first.

An invocation is an ordinary statement whose mnemonic names a macro:
`statement-operands` (comma-split, one expression each — [Statement grammar &
expression parser](parser.md)) supplies arguments for all required parameters
and optionally any trailing defaults. A label on the invocation line is emitted as its
own label-only statement immediately ahead of the substituted body, so it
binds to the address the expansion starts at — including for an
empty-bodied macro, where it binds to whatever statement follows. This is
not quite the same as writing that label directly on the body's own first
statement: if the invocation's label is local and the body's first statement
defines a *global* label, the local one is qualified against the scope in
effect at the call site, not against the global label the body is about to
define.

## Substitution

Any identifier token in the body whose text matches a parameter name is
replaced, token-for-token, by that argument's own token run; every other
token in the body — including a literal `#` or any other addressing-mode
syntax that isn't itself a parameter reference — passes through unchanged.
This means a parameter substitutes into *expression position*, not into
operand syntax: if the body writes `adc #k`, the caller passes the bare value
(`addconst cell, 5`, not `addconst cell, #5`) — the `#` already belongs to
the body, so an argument that included its own `#` would produce `##5`, not
match any addressing mode, and signal `assembly-error`.

A macro invoking another macro (nesting) needs no special handling: each
round of expansion rewrites every remaining invocation it finds, so a body
statement that turns out to invoke another macro is itself expanded on the
next round. Expansion is capped at `*max-macro-expansion-rounds*` (32 by
default) rounds — a macro that (directly or through another macro) invokes
itself never reaches a fixpoint and signals `macro-error` instead of growing
the statement list without bound.

Expanded statements use the outermost source invocation line for listings,
symbols and diagnostics. Their macro body definition line is retained
separately. Assembly errors show the invocation first, followed by the
body line.

## Body-defined symbols

Each invocation gives its body-defined labels and `.equ` names unique names,
including global and local names. References written in that body use the same
unique names. Caller supplied argument tokens retain their original spelling.
The names are private to the expansion: code outside it cannot refer to them
by their source spelling. Local names still use the current global scope (see
[Assembler, "Local-label scoping"](assembler.md#local-label-scoping-16)).

```lisp
.macro countdown
.loop:  dex
        bne .loop
.endm

start:  countdown
        countdown   ; each .loop is distinct
```

## Forced addressing-mode suffix

A mode suffix (see [Addressing modes, "Forcing a mode with a mnemonic
suffix"](modes.md#forcing-a-mode-with-a-mnemonic-suffix)) is rejected on a
macro **invocation** — `macro-error`, since a mode name has nothing coherent
to force against a statement that expands to zero or more statements of its
own:

```lisp
.macro loadx n
    ldx #n
.endm
    loadx.w 10   ; macro-error: a mode suffix is not meaningful here
```

Written literally inside a macro **body**, a suffix survives substitution
and forces its mode after expansion exactly as it would in ordinary code:

```lisp
.macro loada n
    lda.w n      ; always ABSOLUTE, whatever the caller's argument resolves to
.endm
```

## Conditionals

A macro body may contain balanced `.if`/`.endif` blocks, which see the
substituted arguments; `.macro` cannot be defined inside `.if`. See
[Conditional assembly](conditionals.md).

## Conditions

`macro-error` (a subtype of `lasm-syntax-error`) covers every
`.macro`/`.endm`-specific failure:

- An unterminated `.macro` (end of input before a matching `.endm`).
- An `.endm` with no open `.macro`.
- A `.macro` nested inside another macro's body.
- A label on the `.macro` or `.endm` line itself.
- A malformed header — a parameter that isn't a plain identifier, or no name
  at all.
- A duplicate macro name, or one registered as a directive or instruction on
  the target machine.
- A malformed default, required parameter after an optional one, or an
  invocation with too few or too many arguments.
- A parameter name also used for a body-defined symbol.
- Expansion that doesn't converge within `*max-macro-expansion-rounds*`
  rounds (a directly or indirectly recursive macro).
