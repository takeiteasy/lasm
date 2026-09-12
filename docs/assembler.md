# Assembler

`assemble` turns a program's source text (or an already-parsed statement
list) into encoded bytes, resolving labels and choosing an addressing mode
along the way.

```lisp
(assemble "        ldx #10
.loop:  dex
        bne .loop
        sta $1000
        hlt" :lexer 'sixtyfoo-syntax :machine 'sixtyfoo)
```

See [`examples/counter.lisp`](../examples/counter.lisp) for a runnable
single-mode version, [`examples/modes.lisp`](../examples/modes.lisp) for a
multi-mode one, [`examples/directives.lisp`](../examples/directives.lisp) for
one using `.org`/`.byte`/`.res` (see [Directives](directives.md)), and
[Emulator](emulator.md) for running the result.

## `assemble` / `assemble-statements`

```lisp
(assemble SOURCE &key machine (lexer 'default) (origin 0))
(assemble-statements STATEMENTS &key machine (origin 0))
```

`assemble` is `parse` (see [Statement grammar & expression
parser](parser.md)) followed by `assemble-statements` — a caller already
holding a `statement` list (e.g. from its own preprocessing) can call the
latter directly. Both return an `assembly`:

```lisp
(defstruct assembly bytes origin symbols)
```

- `bytes` — a `(vector (unsigned-byte 8))` of the encoded program. A gap left
  by a forward `.org` or a `.res` run (see [Directives](directives.md)) is
  zero-filled.
- `origin` — the address the first byte was placed at: the `:origin` key
  below, unless a leading `.org` moved it first (see "`:origin`" below).
- `symbols` — a hash table (label name string → address) of every label
  bound while assembling, forward or backward.

## Two passes

1. **Layout.** Walk the statements with an address counter starting at
   `:origin`. Each `statement-label` binds to the current address; each
   `statement-mnemonic` is looked up first as a directive (`directive.lisp`,
   see [Directives](directives.md)) and, if it isn't one, as its mnemonic's
   addressing-mode variants (`find-instruction-variants`,
   [Instructions](instructions.md)), which **chooses one** (see "Choosing a
   mode" below) and advances the counter by `1 +` the sum of that variant's
   operand field widths (its `instruction-descriptor-total-operand-width` —
   a variant may wire up more than one field, one per hole of its mode; see
   "Multi-operand instructions" below). Directive lookup has to come first:
   `find-instruction-variants` signals on an unregistered name, so it can't
   be tried first and fallen back from. This is what resolves *forward*
   references (`jmp end` before `end:` appears) — a caller doesn't have to
   write labels before their uses.
2. **Encode.** Walk again, now with the complete symbol table: evaluate each
   statement's already-chosen variant's operand AST (`eval-expr`, below)
   against the symbol table, and encode (`encode-instruction`, or a
   directive's own byte-laying — [Directives](directives.md)). No re-parsing
   or re-matching happens here — pass 1 already committed to a mode (or
   directive) and its parsed operand ASTs.

### Choosing a mode

Two modes can share identical operand syntax and differ only in width — the
6502-shaped `zero-page`/`absolute` pair being the standard example (see
[Addressing modes](modes.md)). Sizing an instruction therefore isn't always
independent of its operand's *value*, and when that operand is a label, the
value isn't known until layout has already placed every address. `%choose-
variant` (in `assembler.lisp`) breaks this with two filters, applied to a
mnemonic's variants in the order they were declared in `(modes ...)`:

1. **Syntax.** Keep the variants whose mode's pattern matches the
   statement's operand tokens (`try-match-operand-mode`,
   [Addressing modes](modes.md)) — a no-operand variant's "pattern" is
   simply an empty token run. No match at all is an `assembly-error`.
2. **Value**, checked independently for each syntax-matching candidate —
   candidates of one mnemonic can have different hole counts (e.g. a
   two-register mode alongside a one-immediate mode), so whether a
   candidate's values are even known yet is not one shared question:
   - If **every** hole folds to a **constant** (no label reference) and each
     one fits its own field's width (`%fits-width-p`, checked value-by-value
     against the candidate's own `operand-widths` — it accepts both the
     unsigned and the two's-complement signed range, e.g. both `255` and
     `-1` fit one byte), the candidate is a **fit**. The **first** fit in
     declaration order is kept — which is why [Addressing
     modes](modes.md#declare-narrower-modes-before-wider-ones) says to
     declare narrower/cheaper modes before wider ones that also match their
     syntax: declaring them in the other order still "works", it just always
     picks the wider one.
   - If **no** candidate fits — `ldx #300` on a one-byte `immediate`, or any
     hole is a **label** reference whose value isn't known yet — fall back
     to the **widest** syntax-matching candidate (by total operand width)
     and let `encode-instruction`'s existing `wrap-value` mask each value,
     exactly as a single-mode instruction has always done. A label-bearing
     candidate never has to shrink once the label resolves later, so one
     layout pass suffices.
   - A **`relative`** mode candidate ([Addressing modes](modes.md#pc-relative-modes))
     is excluded from the fit check even when its hole folds to a constant:
     its parsed value is an absolute target, not the offset that actually
     gets encoded, so checking it against an operand width here would
     compare the wrong quantity — it only ever wins as the widest fallback,
     and `%encode` (below) does the real range check once it has an address
     to compute the offset from.
   - The widest-fallback case also keeps declaration order on a tie (equal
     total width).

```lisp
lda $10       ; constant, fits zero-page -> zero-page (first declared fit)
lda $1000     ; constant, doesn't fit zero-page -> absolute
lda target    ; label -> absolute, even if target turns out to be $0010
```

**Deviation from the ticket/plan wording:** both the tracker ticket that
added multi-mode resolution and `LASM-plan.md` §2 describe M2's two-pass as
"label resolution before final mode/encoding selection." This does the
reverse — mode selection happens in pass 1, before labels resolve in pass 2
— because doing it in the stated order needs a relaxation loop: assume the
narrowest mode everywhere, lay out, widen whatever doesn't fit, repeat until
addresses stop moving. Picking the widest matching variant for any
label-bearing operand sidesteps that loop entirely, at the cost of never
choosing zero-page for a label operand even when its resolved address would
have fit. A follow-up ticket tracks narrowing a label-bearing operand's mode
once layout has converged.

## Multi-operand instructions

An instruction whose mode declares more than one `expr` hole
([Addressing modes](modes.md)) wires up one operand encoding field per hole
([Instructions](instructions.md)), evaluated and encoded in hole order. Pass
1 sizes the statement by the sum of those fields' widths
(`instruction-descriptor-total-operand-width`); pass 2 evaluates *every*
hole's AST against the completed symbol table — including a label bound in
a later hole, not just the first — and hands the whole list of values to
`encode-instruction`:

```lisp
mov $20, target   ; DST ($20) and SRC (target's resolved address) both
target: nop       ; encode, little-endian, one after the other
```

## PC-relative offsets

A `relative`-mode operand ([Addressing modes](modes.md#pc-relative-modes))
folds to an absolute target address like `absolute` does, but that's not
what gets encoded. Once pass 2 evaluates the target against the completed
symbol table, `%encode` computes the signed offset from the address of the
*next* instruction:

```lisp
offset = target-address - (branch-address + 1 + operand-width)
```

`branch-address + 1 + operand-width` is deliberately the same arithmetic
`step-machine` ([Emulator](emulator.md#step-machine)) uses to advance `pc`
past the branch *before* running its semantics — so an instruction's own
`(set! pc (+ pc operand))` adds the offset to exactly the base it was
computed from, at any `:origin`. `encode-instruction` itself needs no
special case: `wrap-value` already renders a negative offset as its
two's-complement byte (e.g. `-3` as `#xFD`).

If the offset doesn't fit the operand's width, this signals `assembly-error`
naming the mnemonic, the offset, and the legal range, rather than silently
wrapping to a branch at the wrong address.

## `eval-expr`

`instruction.lisp`'s `eval-expr-constant` folds an expression AST with no
label support at all (any `expr-label` signals `unresolved-label`). The
assembler needs the same folding logic once labels *are* known, so
`eval-expr-constant` is defined in terms of a more general function:

```lisp
(eval-expr AST &key symbols pc)  ; symbols: string -> address hash table
                                  ; pc: this statement's address, or NIL
(eval-expr-constant AST &key pc) = (eval-expr AST :symbols nil :pc pc)
```

`eval-expr` looks an `expr-label`'s name up in `symbols` and signals
`unresolved-label` only on a miss (or when `symbols` is `nil`, matching the
old no-labels-ever behavior). An `expr-location` node (the `"*"`
location-counter symbol, below) resolves from `pc` instead, independently of
`symbols` — `eval-expr-constant` still folds no labels, but a caller can
still supply `:pc` to fold a location-counter reference.

## Location counter

```lisp
lda *+2      ; the byte after this instruction's operand
bne *        ; branch to self
.org *+16    ; pad 16 bytes forward from here
.word *      ; this .word entry's own address
```

`"*"` in operand position parses to `expr-location` ([Statement grammar &
expression parser](parser.md)) rather than the multiply operator — see that
doc for why the two never actually collide. Resolving it just needs an
address, which pass 1 already has for every statement (unlike a label, whose
address may not be known until pass 1 finishes), so both passes pass one
through as `eval-expr`'s `:pc`:

- **An instruction operand** resolves against the statement's own address —
  the same address `%choose-variant` uses to pick a mode in pass 1, and the
  same one `%encode` passes to `eval-expr` in pass 2. A `relative`-mode
  operand ("PC-relative offsets" above) still runs through the usual
  next-instruction adjustment afterward, so `bne *` branches to itself exactly
  like `here: bne here` does.
- **A `.byte`/`.word` element** ([Directives](directives.md)) resolves
  against *its own* address, not the directive statement's — `.byte *, *` at
  address `$10` emits `$10` then `$11`, matching how each element already
  gets its own address for a label reference.
- **A `.org`/`.res` operand** ([Directives](directives.md#org)) resolves
  against the address counter's value *before* this directive moves or
  reserves anything — `.org *+16` pads 16 bytes forward from here, and a
  label on the same `.org` line still binds to the address it moves *to*, not
  this one.

With no address available at all — `eval-expr-constant` called with no
`:pc`, e.g. by a standalone caller outside the assembler — an `expr-location`
signals `unresolved-location`.

## Local-label scoping (#16)

A local label (an identifier starting with the lexer's `local-label-prefix`,
e.g. `.loop` for the default lexer — `token-localp`/`expr-label-localp`, see
[Lexer](lexer.md) and [Statement grammar & expression
parser](parser.md#ast-nodes)) is scoped to its nearest preceding non-local
("global") label, so the same local name can repeat once per routine:

```lisp
a:
  ldx #0
.loop:      ; -> bound as "a.loop"
  bne .loop ; -> resolves against "a.loop"

b:
  ldx #0
.loop:      ; -> bound as "b.loop"; does not collide with "a.loop"
  bne .loop
```

`%layout` threads a `scope` variable (the nearest preceding global label's own
name) through the statement list; every local label definition and reference
is qualified to `scope ++ name` (`%qualify-local`/`%qualify-locals!` in
`assembler.lisp`) before it ever reaches the symbol table or an operand
AST — so `symbols` itself stays the same flat string → address table, and
`eval-expr` needs no scope argument of its own. **A statement's own label is
bound (and, if global, becomes the new scope) before its own operands are
qualified** — so in `loop: bne .x`, `.x` is scoped to `loop`, the label on
that same line, not whatever preceded it.

A local label with no enclosing global label — at its definition or at a
reference — signals `assembly-error`. A qualified name can collide with an
identically-spelled global (a global literally named `loop.next` alongside a
`.next:` under `loop:`) — this surfaces loudly as the ordinary duplicate-label
`assembly-error`, never as silent aliasing; a follow-up ticket tracks a
reserved separator that rules this out entirely.

## `:origin`

`(assemble source :origin #x200)` starts layout at `#x200` instead of `0` —
every label and byte address is computed against that base. The resulting
`assembly`'s `origin` slot carries this forward so `load-program` (see
[Emulator](emulator.md)) places the bytes at the same address the labels
were computed against; passing mismatched origins to `assemble` and
`load-program` is how labels silently end up pointing at the wrong place, so
`load-program` defaults to the assembly's own origin rather than requiring
it be repeated.

A leading `.org` (before any statement has occupied an address) moves this
same `origin` — see [Directives](directives.md#org) — so `(assemble
".org $8000 ...")` needs no `:origin` key at all; the two ways of setting it
compose in the obvious way (`:origin` picks the starting point layout begins
at, `.org` can still move it further before the first byte).

## Conditions

- `assembly-error` (a subtype of `lasm-syntax-error`) — a duplicate label, a
  local label with no enclosing global label (see "Local-label scoping"
  above), an operand whose syntax matches none of the mnemonic's declared
  addressing-mode variants, a `relative`-mode offset that doesn't fit its
  operand's width (see "PC-relative offsets" above), or a malformed
  directive use (wrong operand count, a non-constant `.org`/`.res` operand,
  or a backward-moving `.org` — see [Directives](directives.md)).
- `unknown-instruction` — an unregistered mnemonic (from
  `find-instruction-variants`, see [Instructions](instructions.md)).
- `unresolved-label` — an operand references a label never bound anywhere in
  the program (from `eval-expr`).
- `unresolved-location` — a `"*"` location-counter reference folded with no
  `:pc` given (see "Location counter" above); does not occur during ordinary
  assembly, only from a standalone `eval-expr`/`eval-expr-constant` call.
- `lex-error` / `parse-failure` — from the front end (`assemble` only).

## Scope

This produces bytes and a symbol table from a statement list, including
directives (`.org`, `.byte`/`.word`, `.res` — see [Directives](directives.md)).
It does not cover `.macro` (a statement-expansion pass, not a per-statement
directive — see [Directives](directives.md#scope-macro-is-not-a-directive))
or a listing / source-map output tying addresses back to source lines (both
separate, follow-up tickets).
