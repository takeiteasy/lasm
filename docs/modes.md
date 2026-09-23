# Addressing modes

`defmode` declares an addressing mode: a literal/token pattern with one or
more `expr` holes, matched against a statement's operand tokens by
`match-operand-mode`/`try-match-operand-mode`. `definstruction` (see
[Instructions](instructions.md)) wires one or more modes to encoding and
semantics; this document covers the pattern grammar and matching on their
own.

```lisp
(defmode immediate      "#" expr        :width 1)
(defmode zero-page      expr            :width 1 :suffix "z")
(defmode absolute       expr                      :suffix "w")
(defmode indexed-x      expr "," "X")
(defmode indirect-y     "(" expr ")" "," "Y")
(defmode relative       expr            :width 1 :relative t)
(defmode stack-relative expr "," "S"    :width 1)
```

`immediate`, `zero-page`, `absolute`, `indexed-x`, `indirect-y`, `relative`,
and `stack-relative` above are exactly the modes LASM ships built in (in
`mode.lisp`) — there is no special-cased "M1 mode" table any more; every
mode, built-in or user-declared, goes through the same `defmode`.

## `defmode`

```lisp
(defmode NAME pattern-element... [:width n] [:signed t] [:relative t] [:suffix "s"] [:strict t])
```

`pattern-element` is a string literal, `expr`, `(expr :signed t)`,
`(expr :relative t)`, or `(one-of mode...)`. A mode may be literal-only,
such as `(defmode stack-pointer "SP")`; it then contributes zero holes.

`NAME` is a symbol, registered globally (like a lexer — see below).
String literals match token text case-insensitively. `expr` parses one
expression with the shared parser.
`(expr :register NAME)` can combine `:register` with hole options. A mode
can also contain only literals. `:width`, if given, is this mode's default
operand byte width — see [`(encoding ...)`](instructions.md) and the
width-resolution note below. `:signed t`, if given, marks this mode's
operand as a signed quantity rather than an unsigned one — see
[Signed operands](#signed-operands) below. `:relative t`, if given, marks
this mode's operand as a PC-relative offset rather than an absolute value —
see [PC-relative modes](#pc-relative-modes) below. `:relative t` **implies**
`:signed t` (a branch offset can go either direction); passing `:signed nil`
alongside `:relative t` is a contradiction and `defmode` signals an error.
`:suffix "s"`, if given, lets a program force this mode on a per-statement
basis via a mnemonic suffix (e.g. `lda.w`) — see [Forcing a mode with a
mnemonic suffix](#forcing-a-mode-with-a-mnemonic-suffix) below. `:strict t`,
if given, turns an out-of-range operand value into an `assembly-error` at
encode time instead of silently wrapping — see [Diagnostics, "Strict operand
range"](diagnostics.md#strict-operand-range).

Registration happens inside an `eval-when`, like `defmachine` — a mode
must be resolvable by `definstruction` at macroexpansion time, not only
after the file loads.

### A mode is global, not per-machine

Modes are registered in one global table, the same way `deflexer` registers
a surface syntax rather than scoping it to a machine — a mode is a syntax
concept, not part of any one machine's storage model. Two machines wanting a
mode of the same name with different syntax is a follow-up concern (see the
tracker); for now, pick distinct names.

### Declare narrower modes before wider ones

`zero-page` and `absolute` above match *identical* operand syntax (a bare
`expr`) — they're disambiguated by value, not pattern, and only at assembly
time (see [Assembler](assembler.md#choosing-a-mode)). Declaration order in
an instruction's `(modes ...)` list is the tiebreak the assembler falls back
on, so **declare narrower/cheaper modes before wider ones that also match
their syntax**. Getting this backwards doesn't error — it just silently
gives every operand the wider mode, since the assembler tries candidates in
declaration order and a wider candidate placed first is found before a
narrower one that would also have fit. A label-bearing operand starts at
its narrowest candidate before any address is known (see
[Assembler, "Convergence"](assembler.md#convergence)), but is subject to
this same declaration-order tiebreak on every later pass, once a symbol
table exists for its value to fit against — so the advice above applies to
it too.

### Width

A mode's own `:width`, when given, is what `(operand :mode)` and a
multi-mode variant with no `(operand :width n)` of its own resolve to.
`immediate` and `zero-page` above are always one byte; `absolute` gives no
`:width`, so instructions using it fall back to the machine's address width
(see [Instructions, "`(encoding ...)`"](instructions.md)) — sized once per
machine rather than hard-coded into the mode.

A mode with more than one `expr` hole wires up one operand encoding field
per hole (see [Instructions](instructions.md#repeated-operand--subclauses--multi-operand-instructions)).
Each hole can declare `:signed` or `:relative` independently. A mode-wide
option supplies the default for every plain `expr` hole; an explicit hole
option overrides it. `:relative nil` leaves inherited signedness intact,
while `:relative t` always implies signedness. A `one-of` element contributes the holes
declared by the selected alternative. Alternatives may vary in count and, on
a byte-encoded machine, may disagree on their own `:width` (see
[Per-hole `:width`](#per-hole-width) below) or `:relative`.

## Per-operand modes

A `(one-of mode...)` pattern element lets a single operand hole pick its own
addressing-mode syntax independently of every other hole in the same
pattern — the shape a DCPU-16/ANIMA-16-style instruction set needs
throughout its operand table, where one operand might be a bare register,
another `[register]`, another `[register + next word]`, and so on, all in
the same instruction:

```lisp
(defmode a-reg expr)
(defmode a-ind "[" expr "]")
(defmode a-lit "#" expr)

(defmode ab (one-of a-reg a-ind a-lit) "," (one-of a-reg a-ind a-lit))
```

Use `(one-of (SLOT mode...))` to name a choice slot when an alternative has no
hole, or when a declaration has more than one independent choice set:

```lisp
(defmode fixed-or-register
  (one-of (fixed-kind stack-pointer program-counter) a-reg))
```

Legacy `(one-of mode...)` remains hole-aligned exactly as before. Matching APIs
return the existing hole-aligned `choices` value unchanged and add selection
metadata such as `((fixed-kind . stack-pointer))` as an additional value for
named slots. Each named element is selected independently, including when two
or more selected alternatives all have zero holes. This metadata is the
representation used by encoding extensions for zero-hole alternatives.

`ab` above matches `5, 10`, `[5], #10`, `#5, [10]`, and every other
combination of its two holes' three alternatives — each hole's choice has no
bearing on the other's.

Each `one-of` alternative names an already-registered mode (a plain symbol,
resolved the same way `definstruction`'s `(modes ...)` resolves a mode
name). At `defmode` time, every alternative:

- may have a *different* hole count than its siblings — see [Varying hole
  counts across alternatives](#varying-hole-counts-across-alternatives)
  below for the encoding declarations. Multiple elements may vary
  independently; their alternatives cannot themselves be varying modes;
- may declare `:strict` (see [Per-hole `:strict`](#per-hole-strict) below),
  `:signed` (see [Per-hole `:signed`](#per-hole-signed) below), `:width`
  (see [Per-hole `:width`](#per-hole-width) below), `:relative` (see
  [Per-hole `:relative`](#per-hole-relative) below), or `:suffix` (see
  [Forcing one hole with a prefix](#forcing-one-hole-with-a-prefix) below);
- must not share identical syntax with another alternative in the same
  `one-of` (checked case-insensitively, since a `:literal` element already
  matches that way) — nothing could ever choose between two alternatives
  that read the same — unless every alternative sharing that syntax declares
  a `:suffix`, which then selects between them. Without a prefix, the first
  declared alternative wins, whatever the value; it never relaxes to a later
  twin.

A `one-of` needs at least two alternatives; one alternative would just be
the same as writing that mode's pattern directly.

### Matching and backtracking

At match time, each alternative is tried against the rest of the pattern
and the complete input. Among successful matches, the path with the most
literal tokens wins; declaration order breaks ties. An alternative that
matches only a prefix cannot prevent a later complete match. Given

```lisp
(defmode bt-plain expr)
(defmode bt-marked expr "X")
(defmode bt (one-of bt-plain bt-marked) "," "Y")
```

matching `bt` against `5 X, Y`: `bt-plain` (a bare `expr`) matches `5`
locally and stops (`X` isn't part of an expression), but the pattern's
trailing `,` `Y` cannot match starting at `X`. `bt-marked` (`expr "X"`)
matches the full input. For overlapping complete matches such as `[0 + 4]`,
`"[" expr "+" expr "]"` wins over `"[" expr "]"` because it matches an
additional literal `+` instead of treating `0 + 4` as one expression.

`try-match-operand-mode`/`match-operand-mode` (see
[Matching](#matching) below) return which alternative each operand *hole*
matched, as a trailing `choices` value — one entry per hole, in hole order,
`nil` for a hole not governed by any `one-of` at all. A multi-hole `one-of`
alternative reports its own chosen `mode-descriptor` for *every* hole it
contributes, not just once for the element as a whole; a nested `one-of`
(one alternative's own pattern containing another `one-of`) reports the
*outer* element's chosen alternative for all of its holes, not the nested
match's own choice. On a word-encoded machine (see
[Instructions](instructions.md#choice-selected-word-fields)), a `(choice
mode)` variant selector reads this value to pick a field's own code, or an
unconditional extra word, by which alternative a hole actually matched; on a
byte-encoded machine, a `(variant (choice m) (sub s))` selector (see
[Instructions, "hole-selected
sub-opcode"](instructions.md#variant-choice-m-sub-s--hole-selected-sub-opcode))
reads it the same way to pick the sub-opcode cell's own value. A hole with
neither kind of selector leaves `choices` purely informational.

### Register-qualified holes

Use `(expr :register NAME)` for a hole that must name an alias of the
register bank `NAME`:

```lisp
(defmode indexed "[" (expr :register reg) "+" expr "]")
```

The assembler accepts only a direct register alias in the qualified hole.
Numbers, labels, and compound expressions remain available to an ordinary
`expr` hole, so register-plus-offset and absolute-plus-offset alternatives
remain distinguishable.

### What `one-of` does and does not do

Declaring a `one-of` only changes which *syntax* an operand hole accepts —
it says nothing, by itself, about the value each alternative parses to. On a
**byte-encoded** machine (a plain `(operand :mode)`/`(operand :width n)`
encoding) with no sub-opcode selector on that hole, this is still the whole
story: `a-ind` above (`"[" expr "]"`) parses to the same plain integer
`a-reg` (`expr`) would, and every alternative's value encodes into the same
field the same way, regardless of which one matched (see
[`examples/orthogonal.lisp`](../examples/orthogonal.lisp) for this made
explicit — three syntactically distinct operands assembling to identical
bytes, and staying that way, since there is no field, sub-opcode or
otherwise, for anything to apply to). A hole-selected `(variant (choice m)
(sub s))` (see [Instructions, "hole-selected
sub-opcode"](instructions.md#variant-choice-m-sub-s--hole-selected-sub-opcode))
is the byte-machine exception: it lets the matched alternative steer the
sub-opcode cell the same way a word-encoded field's `(choice mode)`,
described next, steers a bit field. A mode with more than one `one-of` hole
wanting a say in the cell uses a `(sub-opcode ...)` table instead (see
[Instructions, "multi-hole sub-opcode
selection"](instructions.md#sub-opcode--multi-hole-sub-opcode-selection)) —
the same mechanism, generalized from one carrying hole to several.

On a **word-encoded** machine (`(instruction-word ...)`, see
[Instructions](instructions.md#word-encoded-instructions-20-m4)), a `(choice
mode)` variant selector lets the matched alternative steer that hole's own
field code, or spend an unconditional extra word regardless of the value —
see [Instructions, "CHOICE-selected word
fields"](instructions.md#choice-selected-word-fields) and
[`examples/anima16.lisp`](../examples/anima16.lisp), where `reg`, `[reg]`,
and `(addr)` genuinely encode to different field codes for the identical
value 0. Two alternatives may also share one field code as spellings of the
same encoding — see [Instructions, "Aliased
escapes"](instructions.md#aliased-escapes). A `(semantics ...)` body can read this back too: `choice-case`
(see [Semantics vocabulary, "`choice-case`"](semantics.md#choice-case))
dispatches on which alternative a hole actually matched, so `[reg]` really
dereferences while a bare `reg` reads the value directly, on a word-encoded
machine — every sibling descriptor a `one-of`'s alternatives expand into
still shares one `semantics` body, but that body can now tell them apart at
runtime instead of treating every alternative identically. A byte-encoded
machine's hole-selected sub-opcode (above) gives `choice-case` the same
thing to read back, the first time that is reachable there at all; a hole
with neither a `(choice mode)` word field nor a hole-selected sub-opcode has
no encoded discriminator, so `choice-case` signals there unless given an
`otherwise` clause. `[register]` and `[register + offset]` can already mean
different things — [Varying hole counts across
alternatives](#varying-hole-counts-across-alternatives) above is exactly
this shape, one alternative's own extra hole holding the offset. Spelling
that second hole `"+"`-separated, so it reads as `[register + offset]`.
When a mode's next literal is `+`, matching retries expression boundaries at
that operator; ordinary expression parsing remains greedy elsewhere.

Disassembly mirrors this split: a byte-encoded machine's decoded word
renders a `one-of`'s first alternative unless its descriptor declares a
hole-selected sub-opcode, in which case the sub-opcode cell carries that
record forward from decode, same as a word-encoded machine's `(choice mode)`
field — either way, the disassembler renders the alternative that was
actually written when a record exists, and only falls back to the first
alternative when it doesn't — see [Disassembler](disassembler.md).

### Varying hole counts across alternatives

A `one-of`'s alternatives may declare *different* hole counts — `reg`
(one hole) and `[reg + off]` (two holes) can sit in the same `one-of` — on a
**word-encoded** machine, when the governing hole is `(choice mode)`-selected
on every alternative (or #118-mixed, with the one unclaimed alternative
sharing the other alternatives' hole count). `definstruction` expands one
fixed-arity `instruction-descriptor` per alternative-tuple rather than
assuming every alternative shares one shape, so nothing downstream of
`definstruction` — the decoder, the disassembler, `%choose-variant` — ever
sees a variable-arity descriptor.

An alternative with more holes than its siblings names its own extra
operand subclauses with a `(for-choice (operand alt) (operand ...)...)` subclause,
alongside the mode's ordinary `(operand ...)` subclauses. The first name
identifies a base operand in the owning `one-of`; the short form
`(for-choice alt ...)` works when that alternative identifies exactly one
varying element. Each element varies independently, producing the Cartesian
product of their shapes. See
[Instructions, "`for-choice` — extra holes for a varying
alternative"](instructions.md#for-choice--extra-holes-for-a-varying-alternative)
for the encoding-side mechanics and `(operand name :trailing-word)`, the
fieldless hole shape an extra operand typically takes. A shared
`(semantics ...)` body may read an extra hole's name directly, or through
`choice-case`; see [Semantics vocabulary,
"`choice-case`"](semantics.md#choice-case) for what a sibling descriptor
that lacks that hole sees instead.

Byte-encoded machines (#151) and a varying `one-of` nested inside another
alternative (#152) are both rejected outright at `definstruction` time —
neither is supported yet.

### Per-hole `:strict`

Like `:relative`, a `one-of` alternative may declare `:strict t`. Unlike
`:relative`, `:strict` needs no decode-time record of which alternative
matched, since it is a pure encode-time range check with no bearing on size,
value, or decode at all (see [Diagnostics, "Strict operand
range"](diagnostics.md#strict-operand-range)). A hole is strict when
`*strict-operand-range*` is set, when the mode as a whole declares `:strict
t`, or when the specific alternative that hole matched does — independently
of its siblings and of the mode's own setting:

```lisp
(defmode oo-strict expr :strict t)
(defmode oo-loose "[" expr "]")
(defmode oo-either (one-of oo-strict oo-loose))
```

An instruction using `oo-either` errors on an out-of-range bare value (it
matched `oo-strict`) but silently wraps the identical value written
bracketed (it matched `oo-loose`, which declares no `:strict` of its own) —
the same operand width, two different outcomes, decided purely by which
syntax was written.

### Per-hole `:signed`

Like `:strict`, a `one-of` alternative may also declare `:signed t` on its
own — but unlike `:strict`, `:signed` genuinely does need a decode-time
record of which alternative matched (see [Signed operands](#signed-operands)
below for what `:signed` affects): a fetched raw value is either
sign-extended or not, and that choice has to be recoverable from the
encoding itself, not just known at assemble time. A `one-of` hole only
needs that record when its alternatives actually *disagree*: `definstruction`
signals an error only when a `one-of` hole's alternatives declare different
`:signed` values *and* that hole has no decode-time discriminator — when
every alternative agrees, the hole's signedness is static regardless of
which one matched, and no discriminator is required at all.

The discriminator is scheme-specific:

- On a **byte-encoded** machine, the hole must carry a sub-opcode selector
  — either its own hole-selected `(variant (choice m) (sub s))`, or
  membership in a `(sub-opcode ...)` table's participating holes (see
  [Instructions, "hole-selected
  sub-opcode"](instructions.md#variant-choice-m-sub-s--hole-selected-sub-opcode)
  and ["multi-hole sub-opcode
  selection"](instructions.md#sub-opcode--multi-hole-sub-opcode-selection))
  — the same mechanism `choice-case` and the disassembler already use to
  recover which alternative was written. A table lets any number of a
  mode's holes have disagreeing-signedness alternatives at once, as long as
  each is one of the table's own participating holes — its full `one-of`
  hole set, or a narrower `(holes ...)` subset (see [Instructions,
  "`(holes ...)`"](instructions.md#holes--subsetting-the-table)).
- On a **word-encoded** machine, every field variant at that hole must be
  `(choice m)`-selected (see [Instructions, "CHOICE-selected word
  fields"](instructions.md#choice-selected-word-fields)) — a plain
  value-selected fallback variant (no `(choice ...)` of its own) has no
  alternative of its own to read `:signed` off. This restriction is only
  about a *disagreeing* `one-of` hole: a word-encoded hole with no `one-of`
  at all, or whose alternatives *agree* on `:signed`, needs no discriminator
  — see [Instructions, "Signed word
  fields"](instructions.md#signed-word-fields-63) for that plain case.

```lisp
(defmode oo-uval expr)
(defmode oo-sval "#" expr :signed t)
(defmode oo-signed (one-of oo-uval oo-sval))
```

Given a byte-encoded instruction whose sole operand hole uses `oo-signed`
and carries a `(variant (choice oo-uval) (sub 0)) (variant (choice oo-sval)
(sub 1))` selector, `ldb 200` decodes back as the plain unsigned `200`
(matched `oo-uval`) while `ldb #-100` decodes back as the signed `-100`
(matched `oo-sval`) — the identical field width, two different
reinterpretations, decided purely by which alternative was written (see
[`examples/subchoice.lisp`](../examples/subchoice.lisp) for this run end to
end, and [`examples/anima16.lisp`](../examples/anima16.lisp) for the
word-encoded equivalent).

A mode's *own* `:signed t` combined with a `one-of` as its single hole is
rejected outright, for the same reason `:relative` is below: which
alternative matched would silently override the mode's own declaration
whenever the alternatives happen to agree with each other, rather than the
two ever combining coherently — write `:signed t` on the `one-of`
alternative that needs it instead.

### Per-hole `:width`

Like `:signed`, a `one-of` alternative may also declare its own `:width n`
on a **byte-encoded** machine — and, also like `:signed`, this genuinely
does need a decode-time record of which alternative matched, since it
decides how many operand cells to read, both at encode and at decode. A
`one-of` hole only needs that record when its alternatives actually
*disagree* on width: `definstruction` signals an error only when a hole's
alternatives declare different `:width` values, that hole's own `(operand
...)` subclause is `(operand :mode)` (so the disagreement actually takes
effect — see below), *and* the hole has no decode-time discriminator; when
every alternative agrees, or the hole's `(operand ...)` subclause gives an
explicit `:width` of its own, the hole's width is static regardless of
which alternative matched, and no discriminator is required at all.

The discriminator is the same one `:signed` uses on a byte-encoded
machine: the hole must carry a sub-opcode selector — either its own
hole-selected `(variant (choice m) (sub s))`, or membership in a
`(sub-opcode ...)` table's participating holes (see [Instructions,
"hole-selected
sub-opcode"](instructions.md#variant-choice-m-sub-s--hole-selected-sub-opcode)
and ["multi-hole sub-opcode
selection"](instructions.md#sub-opcode--multi-hole-sub-opcode-selection)).
A table lets any number of a mode's holes have disagreeing-width
alternatives at once, exactly as it does for `:signed` — full hole set or
`(holes ...)` subset alike.

An explicit `(operand :width n)` on the hole's own `(operand ...)`
subclause always wins over a matched alternative's own `:width` — the
author naming a width directly overrides whatever the alternatives
themselves declare, so a hole given one needs no selector at all, even
when its alternatives disagree:

```lisp
(defmode oo-narrow expr :width 1)
(defmode oo-wide "#" expr :width 2)
(defmode oo-widths (one-of oo-narrow oo-wide))
```

Given a byte-encoded instruction whose sole operand hole uses `oo-widths`
with `(operand val :mode (variant (choice oo-narrow) (sub 0)) (variant
(choice oo-wide) (sub 1)))`, `ldw 200` encodes and decodes as a 1-cell
operand (matched `oo-narrow`) while `ldw #300` encodes and decodes as a
2-cell one (matched `oo-wide`) — the instruction's own total size differing
by which alternative was written (see
[`examples/subchoice.lisp`](../examples/subchoice.lisp) for this run end to
end, including a following label's address across the size difference, and
[`examples/subtable.lisp`](../examples/subtable.lisp) for the two-hole
`(sub-opcode ...)` case).

This does not disturb [the assembler's relaxation
pass](assembler.md#convergence): which sibling descriptor a statement uses
is decided purely by *syntax* — which alternative's own tokens were
written — not by a value that might still be provisional mid-relaxation,
so the chosen size is already constant from the first layout pass, with
nothing for `FLOOR`'s monotone widening to do. This is the opposite
situation from `zero-page`/`absolute` sharing identical syntax and being
told apart only by whether a value fits, which is exactly where relaxation
(narrow-then-widen-if-needed) is meaningful.

On a **word-encoded** machine, per-hole `:width` is permanently,
intentionally out of scope instead, since a word-encoded operand's size
always comes from its own field width, never from `operand-widths` (which
is always empty there) — there is nothing for a per-hole `:width` to mean
on that scheme, so `definstruction` rejects any `one-of` hole whose
alternatives declare `:width` at all (agreeing or not) rather than
silently ignoring it.

### Per-hole `:relative`

A plain `expr` hole can declare its own attributes:

```lisp
(defmode branch-and-immediate (expr :relative t) "," (expr :signed t))
(defmode two-targets (expr :relative t) "," (expr :relative t))
(defmode mixed-defaults (expr :relative nil :signed nil) "," expr :relative t)
```

Each relative hole contains an absolute target in source code and encodes a
signed offset from the address after the complete instruction. Multiple
holes may be relative. A hole's `:relative t` implies `:signed t`; an
explicit `:signed nil` on that hole is an error. `:relative nil` removes
offset computation while retaining signedness inherited from the mode unless
that hole also declares `:signed nil`.

A `one-of` alternative can also declare attributes for its own `expr` holes.
Mode-wide `:relative` and `:signed` options cannot be applied to an enclosing
`one-of` element; declare them on the alternatives instead.

As with `:signed`/`:width`, a hole only needs a decode-time discriminator
when its alternatives actually *disagree* on `:relative`; the discriminator
is the same sub-opcode selector mechanism (a hole-selected `(variant
(choice m) (sub s))`, or membership in a `(sub-opcode ...)` table):

```lisp
(defmode jmp-abs expr :width 1)
(defmode jmp-rel "#" expr :width 1 :relative t)
(defmode jmp-any (one-of jmp-abs jmp-rel))
```

Given a byte-encoded instruction whose sole operand hole uses `jmp-any` with
`(operand tgt :width 1 (variant (choice jmp-abs) (sub 0)) (variant (choice
jmp-rel) (sub 1)))`, `jmr 10` encodes `tgt` as the plain value `10`, while
`jmr #target` encodes it as the PC-relative offset to `target`, computed and
range-checked exactly as a whole-mode `relative` mode's operand is (see
[PC-relative modes](#pc-relative-modes) below) — and disassembly renders
`jmr #target`'s hole back as the resolved absolute target, while any
non-relative sibling hole of the same pattern renders plainly. See
[`examples/subchoice.lisp`](../examples/subchoice.lisp) for this run end to
end (the single-hole selector case), and
[`examples/subtable.lisp`](../examples/subtable.lisp) for a `(sub-opcode
...)` table where the relative hole's sibling independently disagrees on
`:width`.

On a word-encoded machine, each relative hole uses its selected field's
range. Alternatives that disagree on relativeness need a `(choice m)`
selector for that field.

## Signed operands

`:signed t` (#30) marks a mode's operand as a signed quantity rather than an
unsigned one. This affects two things:

- The emulator sign-extends each fetched hole back to a signed integer,
  by its own operand width, before running an instruction's `semantics`
  ([Emulator](emulator.md)) — so a signed-mode instruction body sees a plain
  negative Lisp integer rather than having to reinterpret an unsigned byte
  itself.
- The assembler's mode selector range-checks a signed candidate's value
  against the *signed* range (`-(2^(8w-1))` to `2^(8w-1)-1`) rather than the
  wider range an ordinary operand accepts, so e.g. `#200` no longer fits a
  signed byte and the selector moves on to a wider candidate instead of
  wrapping it (see [Assembler, "Choosing a
  mode"](assembler.md#choosing-a-mode)).

On a **word-encoded** machine (#63), a plain value-selected field (no
`(choice m)` of its own — see [Per-hole `:signed`](#per-hole-signed) above
for the `one-of` case) packs and decodes the same way: two's-complement
instead of unsigned, sign-extended back on decode, both for a value that
packs inline and one that escapes to its own extra word — see [Instructions,
"Signed word fields"](instructions.md#signed-word-fields-63).

`:relative` (below) **implies** `:signed` — a branch offset can go either
direction — but the two attributes are otherwise independent: a signed,
non-relative mode (e.g. a signed 8-bit immediate) declares `:signed t` on
its own, with no offset computation attached.

## PC-relative modes

`relative` matches the same bare `expr` syntax as `absolute`; its
mode-wide `:relative t` marks every plain hole. Hole options and selected
`one-of` alternatives can mark individual holes instead.

- An `absolute` operand's value **is** the address encoded, evaluated once
  the symbol table is complete ([Assembler](assembler.md)).
- A `relative` operand's value is a *target address*, but what gets encoded
  is the signed offset from the address of the instruction **after** the
  branch: `offset = target - (instruction-address + instruction-size)`. The
  assembler computes this once both the branch and its target have an
  address ([Assembler](assembler.md#pc-relative-offsets)), and signals
  `assembly-error` if the offset doesn't fit the operand's width rather than
  silently wrapping to a branch at the wrong address — unlike an ordinary
  signed operand (above), which falls back to a wider candidate, or wraps if
  none fits.
- The emulator sign-extends the fetched operand back to a signed integer
  before running an instruction's `semantics`, exactly as any other
  `:signed` mode's operand does (see [Signed operands](#signed-operands)
  above) — a relative-mode instruction body writes a plain `(set! pc (+ pc
  operand))` rather than tracking its own operand width.

A `relative` candidate's parsed value is the absolute target, not the
quantity that gets range-checked against its width -- so `%choose-variant`
(the assembler's mode selector) computes the same offset `%encode` will and
range-checks *that*, letting a `relative` candidate compete on width like
any other mode once an address is available to compute the offset from (see
[Assembler, "Choosing a mode"](assembler.md#choosing-a-mode)). This only
matters once a mnemonic declares `relative` alongside another mode on the
same syntax.

## Stack-relative addressing

`stack-relative` matches `expr "," "S"` — an operand followed by a literal
comma and `S` (case-insensitive, like every literal token, so `1,s` matches
too). Like every mode, its pattern says nothing about *which* stack `S`
names or what the parsed offset means — that's entirely up to the
instruction's semantics, exactly as `absolute`'s operand only becomes "a RAM
address" because some instruction's `semantics` passes it to `mref`. A
`stack-relative` instruction resolves its operand with
[`stack-ref`](machine-model.md) against the machine's sole fixed stack:

```lisp
(definstruction hybridfoo lda
  (modes stack-relative)          ; source: lda 1,S
  (encoding (opcode #xA3) (operand :mode))
  (semantics (set! a (stack-ref operand))))
```

`stack-ref`'s own offset convention is top-relative and unsigned: offset 0
is the top of the stack (the same entry a plain `pop` would return), 1 is
one below that, and so on — Forth `PICK`/`OVER`, or 65816 `n,S`. This is why
`stack-relative` declares no `:signed t` — unlike `relative`'s branch
offset, a stack-relative index has no direction to sign. See
[`examples/hybrid.lisp`](../examples/hybrid.lisp) for a subroutine reaching
an argument sitting just underneath its own return address on a shared call
stack.

`stack-relative` gets no `:suffix` (above) — unlike `zero-page`/`absolute`,
its syntax (`expr "," "S"`) is shared by no other built-in mode, so there is
nothing for a forced suffix to disambiguate.

## Forcing a mode with a mnemonic suffix

Relaxation ([Assembler, "Choosing a mode"](assembler.md#choosing-a-mode))
picks the narrowest addressing-mode variant a label-bearing operand fits
once its address is known — but sometimes a program wants a *specific* mode
regardless of what the operand's value folds to: reserving room for a value
that will grow, or matching a fixed layout another tool expects. A `:suffix`
on `defmode` (#40) enables a gas-style mnemonic suffix for exactly this:

```lisp
lda.z target   ; force ZERO-PAGE, whatever TARGET resolves to
lda.w target   ; force ABSOLUTE, whatever TARGET resolves to
```

Only `zero-page` (`"z"`) and `absolute` (`"w"`) ship with a suffix — they're
the only pair of LASM's built-in modes that share operand syntax (a bare
`expr`), and so the only pair relaxation ever has to choose between.
`immediate`/`indexed-x`/`indirect-y`/`relative` are already syntactically
unambiguous, so a suffix would buy them nothing; a user-declared mode that
does share syntax with another (like a custom signed-immediate alongside an
ordinary one) can declare its own via `:suffix`.

The separator between a mnemonic and its suffix (the `.` above) is a lexer
property, not hard-coded — see `mode-suffix-separator` in
[Lexer](lexer.md#mode-suffix-separator). The parser splits a dotted
mnemonic into a base mnemonic and a suffix at parse time, before the
assembler ever sees it (`statement-mode-suffix`; see [Statement grammar &
expression parser](parser.md)); `.byte`-style directive names, which begin
with the separator, are left alone (there is no base to their left to
split off).

A forced statement bypasses relaxation's floor *and* value filters entirely
— the chosen mode is exactly the one the suffix names, and an out-of-range
value silently wraps at encode time via the same `wrap-value` an ordinary
M1-style single-mode instruction always used (see [Assembler, "Choosing a
mode"](assembler.md#choosing-a-mode) for the one exception: a forced
`:relative` mode still range-checks and errors, since that check happens
unconditionally at encode time, not as part of the value filter). Signals
`assembly-error` if the suffix names no registered mode, if the mnemonic
has no variant using that mode, or if the operand doesn't match that mode's
syntax. A mode suffix is rejected on a directive statement and on a macro
invocation (it's meaningless on either); it survives substitution when
written literally inside a macro body.

See [`examples/complete.lisp`](../examples/complete.lisp) for a runnable
program using `sta.w`/`lda.z` to force a mode.

## Forcing one hole with a prefix

A `:suffix` also names an operand-level choice, written as a prefix before
the operand it forces (the separator is the lexer's `hole-prefix-separator`,
see [Lexer](lexer.md)):

```lisp
seta #w:5     ; force the extra-word encoding of a value that fits inline
two w:1, 2    ; force the first hole only
pick rb:3     ; force the alternative whose mode declares :suffix "rb"
```

- Before a `one-of` element, the name selects the alternative whose mode
  declares that `:suffix`, in place of the first-declared one.
- Before an `expr` hole, the name selects a word-encoded variant declaring
  the same `:suffix` (see [Instructions, "Variant
  suffixes"](instructions.md#variant-suffixes)). Both can be combined, the
  alternative name first: `rb:w:3`.
- A byte-encoded machine has no per-hole variants; it forces a whole mode
  with a mnemonic suffix instead (above).

`try-match-operand-mode` returns the written prefix names as a fifth value,
hole-aligned, `nil` for an unprefixed hole. `disassemble-*` renders them
under `:suffixes t`, so the text re-assembles to the same cells.

## Matching

- `(match-operand-mode tokens mode)` — `tokens` is a token run (e.g. a
  `statement`'s `operand-tokens`; see [Statement grammar & expression
  parser](parser.md)), `mode` a `mode-descriptor` or a symbol naming one.
  Consumes `mode`'s literal tokens in order, parses each `expr` hole, and
  picks a backtracking-matched alternative for each `one-of` element (see
  [Per-operand modes](#per-operand-modes) above). Returns `(values first-ast
  all-asts choices)` — `first-ast` alone is what every current single-hole
  mode needs; `choices` is one entry per hole, in hole order, `nil` for a
  hole not governed by any `one-of` (empty/all-`nil` for a mode with none at
  all); `selections` is an alist of named slot to selected alternative.
  Signals
  `parse-failure` if `tokens` don't match `mode`, or leave a trailing token
  unconsumed.
- `(try-match-operand-mode tokens mode)` — the non-signalling form: returns
  `(values asts t choices selections)` on a match or four `nil` values on a
  mismatch. This is what the assembler's mode-candidate filter uses to try
  several of a mnemonic's modes against one operand without a
  `handler-case` per candidate.

```lisp
(match-operand-mode (statement-operand-tokens some-statement) 'immediate)
;; => #10 parses to an EXPR-NUMBER AST, (values ast (list ast) nil)
```

## Why `statement-operand-tokens`, not `operands`

A `statement`'s `operands` (plural) are already split on top-level commas
(see [Statement grammar & expression parser](parser.md)) — a general
statement-grammar product, but wrong for mode matching: a pattern like
`indexed-x`'s `expr "," "X"` has its own literal comma, and matching it
against one comma-delimited fragment at a time would make it unmatchable.
This is also how a *multi*-operand instruction reaches its several operands
— through a multi-hole mode pattern with its own literal commas (see
[Instructions, "Repeated `(operand ...)` subclauses"](instructions.md)), not
through `operands`. Mode matching instead uses `statement-operand-tokens`,
the whole run of tokens after the mnemonic, uncommitted to any comma split.

## Scope

- Choosing which of a mnemonic's modes a given operand actually resolves
  to, given its syntax and (for a constant operand) its value — see
  [Assembler](assembler.md#choosing-a-mode).
- Wiring a mode to an opcode, operand width, and semantics —
  [Instructions](instructions.md).
- Directives (`.org`, `.byte`/`.word`, `defdirective`) — a separate,
  unrelated grammar; see [Directives](directives.md).

## Note on operand binding

A mode's pattern has no `-> tag` arrow — `defmode`'s pattern ends at the
last pattern element plus an optional `:width n`. `operand` is always bound
to the raw decoded integer regardless of mode, and semantics dereferences
explicitly (see [Instructions](instructions.md#note-on-operand-binding)).
