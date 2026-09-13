# Macros

`.macro`/`.endm` captures a range of statements as a reusable template and
substitutes each invocation's actual arguments into it — something no
`defdirective` action form can express, since a directive is a per-statement
action with a statically-known size, while a macro spans a range of
statements and its expansion isn't known until it's substituted (see
[Directives, "Scope: `.macro` is not a
directive"](directives.md#scope-macro-is-not-a-directive)). It's implemented
as its own statement-expansion pass, `expand-macros` (`macro.lisp`), which
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
more parameter names, each a plain identifier. Unlike an ordinary statement's
operands, a comma between the name and a parameter, or between two
parameters, is optional and purely cosmetic (`.macro foo a, b` and `.macro
foo a b` mean the same thing) — a `.macro` header has no expression syntax to
disambiguate, so there's nothing a comma needs to separate. Everything
between `.macro` and the matching `.endm` is the macro's body, stored
unevaluated (not walked for labels, addressing modes, or anything else) until
an invocation substitutes into it. A macro can be invoked before its own
`.macro`...`.endm` block appears later in the same program — expansion
collects every macro first, then rewrites invocations, so definition order
doesn't matter.

An invocation is an ordinary statement whose mnemonic names a macro:
`statement-operands` (comma-split, one expression each — [Statement grammar &
expression parser](parser.md)) supplies one argument per parameter, checked
for an exact count match. A label on the invocation line is emitted as its
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

## Labels and hygiene

A macro body can define its own labels, but **there is no hygiene**: a label
defined inside the body binds at ordinary layout time, in whatever scope was
in effect at that expansion site, exactly like any other label. Invoking the
same macro twice under the same enclosing global label — or twice at top
level, if the body defines a global label of its own — collides, signalling
the ordinary duplicate-label `assembly-error`, not something specific to
macros:

```lisp
.macro tagged
tag: nop        ; a global label inside the body
.endm

    tagged      ; binds "tag"
    tagged      ; assembly-error: duplicate label "tag"
```

The documented pattern is to give each invocation its own global label and
let the body use local labels (scoped to whichever global label precedes the
invocation, see [Assembler, "Local-label scoping"](assembler.md#local-label-scoping-16)):

```lisp
.macro countdown
.loop:  dex
        bne .loop   ; scoped to whichever global label precedes this invocation
.endm

first:  countdown   ; .loop -> "first.loop"
second: countdown   ; .loop -> "second.loop" -- no collision
```

Auto-uniquifying a macro body's own local labels (so repeat invocations never
collide even under one enclosing global label) is tracked as a follow-up
ticket.

## Conditions

`macro-error` (a subtype of `lasm-syntax-error`) covers every
`.macro`/`.endm`-specific failure:

- An unterminated `.macro` (end of input before a matching `.endm`).
- An `.endm` with no open `.macro`.
- A `.macro` nested inside another macro's body.
- A label on the `.macro` or `.endm` line itself.
- A malformed header — a parameter that isn't a plain identifier, or no name
  at all.
- A duplicate macro name, or a name already registered as a directive
  ([Directives](directives.md)).
- An invocation whose argument count doesn't match the macro's declared
  parameter count.
- Expansion that doesn't converge within `*max-macro-expansion-rounds*`
  rounds (a directly or indirectly recursive macro).

A collision between a macro name and a machine instruction's mnemonic is
*not* checked here (a follow-up ticket) — it surfaces however the assembler
would otherwise treat that statement once expansion is done.

## Follow-ups

- Auto-uniquified macro-body local labels (hygiene, above).
- Parameter defaults / optional parameters.
- Checking a macro name against the target machine's registered instruction
  mnemonics.
- Macro expansion can grow a program's statement count past
  `*max-layout-iterations*`'s convergence budget (see [Assembler,
  "Convergence"](assembler.md#convergence)).

See the tracker for these.
