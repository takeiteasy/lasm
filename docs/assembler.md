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
multi-mode one, and [Emulator](emulator.md) for running the result.

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

- `bytes` — a `(vector (unsigned-byte 8))` of the encoded program.
- `origin` — the address the first byte was placed at (see `:origin` below).
- `symbols` — a hash table (label name string → address) of every label
  bound while assembling, forward or backward.

## Two passes

1. **Layout.** Walk the statements with an address counter starting at
   `:origin`. Each `statement-label` binds to the current address; each
   `statement-mnemonic` looks up its mnemonic's addressing-mode variants
   (`find-instruction-variants`, [Instructions](instructions.md)), **chooses
   one** (see "Choosing a mode" below), and advances the counter by `1 +`
   the sum of that variant's operand field widths (its
   `instruction-descriptor-total-operand-width` — a variant may wire up
   more than one field, one per hole of its mode; see "Multi-operand
   instructions" below). This is what resolves *forward* references (`jmp
   end` before `end:` appears) — a caller doesn't have to write labels
   before their uses.
2. **Encode.** Walk again, now with the complete symbol table: evaluate each
   statement's already-chosen variant's operand AST (`eval-expr`, below)
   against the symbol table, and encode (`encode-instruction`). No re-parsing
   or re-matching happens here — pass 1 already committed to a mode and its
   parsed hole ASTs.

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
(eval-expr AST &key symbols)     ; symbols: string -> address hash table
(eval-expr-constant AST) = (eval-expr AST :symbols nil)
```

`eval-expr` looks an `expr-label`'s name up in `symbols` and signals
`unresolved-label` only on a miss (or when `symbols` is `nil`, matching the
old no-labels-ever behavior).

## `:origin`

`(assemble source :origin #x200)` starts layout at `#x200` instead of `0` —
every label and byte address is computed against that base. The resulting
`assembly`'s `origin` slot carries this forward so `load-program` (see
[Emulator](emulator.md)) places the bytes at the same address the labels
were computed against; passing mismatched origins to `assemble` and
`load-program` is how labels silently end up pointing at the wrong place, so
`load-program` defaults to the assembly's own origin rather than requiring
it be repeated.

## Local labels are flat

A local label (lexer convention: a name starting with a non-alphanumeric
prefix, e.g. `.loop`) is **not** scoped to an enclosing global label here —
it shares one flat symbol table with every other name. Two different
routines both using `.loop` as their loop-back label will collide as a
duplicate-label error. Binding a local label to its nearest preceding global
label is a separate ticket (#16).

## Conditions

- `assembly-error` (a subtype of `lasm-syntax-error`) — a duplicate label,
  an operand whose syntax matches none of the mnemonic's declared
  addressing-mode variants, or a `relative`-mode offset that doesn't fit its
  operand's width (see "PC-relative offsets" above).
- `unknown-instruction` — an unregistered mnemonic (from
  `find-instruction-variants`, see [Instructions](instructions.md)).
- `unresolved-label` — an operand references a label never bound anywhere in
  the program (from `eval-expr`).
- `lex-error` / `parse-failure` — from the front end (`assemble` only).

## Scope

This produces bytes and a symbol table from a statement list. It does not
cover directives (`.org`, `.byte`/`.word`), macros, or a listing /
source-map output tying addresses back to source lines (all separate,
follow-up tickets).
