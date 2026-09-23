# Assembler

`assemble` turns a program's source text (or an already-parsed statement
list) into encoded cells (a byte on an ordinary byte-addressed machine, a
wider unit on a word-addressed one -- see "`assembly`'s cell width" below),
resolving labels and choosing an addressing mode
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
one using `.org`/`.byte`/`.res` (see [Directives](directives.md)),
[`examples/macros.lisp`](../examples/macros.lisp) for one using `.macro`/
`.endm` (see [Macros](macros.md)), and [Emulator](emulator.md) for running
the result.

## `assemble` / `assemble-statements`

```lisp
(assemble SOURCE &key machine (lexer 'default) (origin 0) memory file)
(assemble-statements STATEMENTS &key machine (origin 0) memory source)
```

`assemble` is `parse` (see [Statement grammar & expression
parser](parser.md)) and `expand-includes` ([Includes](includes.md)) followed by `assemble-statements` — a caller already
holding a `statement` list (e.g. from its own preprocessing) can call the
latter directly. `file` names in-memory source in diagnostics and listings;
`assemble-file` supplies its path automatically. Both return an `assembly`:

```lisp
(defstruct assembly cells cell-width origin symbols symbol-info listing source)
```

- `cells` — a `(vector (unsigned-byte cell-width))` of the encoded program,
  `cell-width` being the target memory element's own `:cell-width` (see
  "`assembly`'s cell width" below; 8 on every byte-addressed machine, same
  as every LASM machine before this). A gap left by a forward `.org` or a
  `.res` run (see [Directives](directives.md)) is zero-filled.
- `cell-width` — the bit width of one element of `cells`, resolved once at
  assemble time (see below); never larger than a `fixnum` in practice, but
  not otherwise constrained.
- `origin` — the address the first cell was placed at: the `:origin` key
  below, unless a leading `.org` moved it first (see "`:origin`" below).
- `symbols` — a hash table (name string → value) of every symbol bound while
  assembling: a label's address or an assignment's final folded value.
  Labels and assignments share one flat table; `.set` can update an
  assignment (see "`.set`" below). The table stays untagged (`eval-expr`
  reads it as a plain name → value map) — see `symbol-info` below for the
  scope/kind metadata this can't carry. Local keys contain a reserved NUL
  separator between the global and local names; use `assembly-symbol` with
  `:scope` to look them up.
- `symbol-info` — a hash table (qualified name string → `symbol-info`), built
  alongside `symbols` and keyed the same way, carrying what `symbols` alone
  cannot: whether an entry is a label, `.equ`, or `.set`, and its enclosing scope
  for a local name. See [Listing and source map](listing.md#symbol-table) for
  the query/render API built on it.
- `listing` / `source` (#25) — the retained address↔statement mapping
  layout computes, and the source text it came from (when known); see
  [Listing and source map](listing.md) for how it's rendered and looked up.
  `assemble` fills `source` in automatically from its own `SOURCE`
  argument; `assemble-statements`' `:source` key is for a caller building
  `statements` by hand.

### `assembly`'s cell width

Every address a program's labels and location counter resolve to is counted
in the target machine's own memory cells, not bytes — on a machine whose
memory declares `:cell-width 16` (word-addressed, e.g. a DCPU-16-shaped
design), a label two instructions in is address `2`, not `4`, and `assemble`
emits a `(vector (unsigned-byte 16))`, not a byte stream `load-program`
would need to re-pack. `MEMORY` names which of `machine`'s memory elements
this assembly targets, exactly like `load-program`'s own `:memory` — it
defaults to the machine's sole memory element, or (when several elements
share one cell width) that shared width; declaring more than one memory
element with *different* cell widths makes this ambiguous and `assemble`
requires `:memory` explicitly. `load-program` checks an `assembly`'s
`cell-width` against its target memory element and signals if they
disagree, rather than silently misplacing every cell (see
[Emulator](emulator.md#load-program)).

`assemble`/`assemble-statements` resolve the target memory element's
`:endian` (#66) the same way, alongside `cell-width` — there is no separate
`endian` slot on `assembly` itself, since decode always resolves endianness
from the machine descriptor at read time and an assembly is always produced
against one. Both properties are resolved once per assembly and used for
instructions and directive data; see [Machine model, "Cell width and the
assembler"](machine-model.md#cell-width-and-the-assembler) for the shared
resolution rule and [Directives, `.byte`/`.word`](directives.md) for how it
governs directive data.

## `assemble-file`

```lisp
(assemble-file PATH &key machine (lexer 'default) (origin 0) memory)
```

Reads the source file at `PATH` and `assemble`s its text; keys and conditions
are `assemble`'s. `.asm` or `.s` is the conventional extension for target
source (`.lasm` is reserved for machine definitions); it is not enforced. A
missing or unreadable file signals the ordinary CL `file-error`.

## Include expansion

`assemble` runs `expand-includes` ([Includes](includes.md)) right after
parsing, before macro expansion, so an included file's `.macro` and `.equ`
statements are visible to the includer. `assemble-statements` does not: it has
no lexer to parse an included file, so a statement list passed to it directly
must already have `.include` expanded, otherwise layout signals
`include-error`.

## Macro expansion

`assemble-statements` runs `expand-macros` ([Macros](macros.md)) before
layout ever sees the statement list — every `.macro`...`.endm` block is
collected and every invocation replaced by its substituted body first, so
neither layout nor encode below has any notion of a macro at all. Both entry
points (`assemble` and `assemble-statements`) get this, since `assemble`
reaches `assemble-statements` after parsing.

## Layout and encode

1. **Layout.** Walk the statements with an address counter starting at
   `:origin`. Each `statement-label` binds to the current address; each
   `statement-mnemonic` is looked up first as a directive (`directive.lisp`,
   see [Directives](directives.md)) and, if it isn't one, as its mnemonic's
   addressing-mode variants (`find-instruction-variants`,
   [Instructions](instructions.md)), which **chooses one** (see "Choosing a
   mode" below) and advances the counter by that variant's encoded size
   (`instruction-descriptor-size` — `1 +` the sum of its operand field
   widths, all counted in the machine's own memory cells (#53), on an
   ordinary cell-encoded machine, or a word-encoded one's own word size
   plus its total extra-word cells (#20, #135; see "Word-encoded
   instructions" below). A variant may wire up more than one field, one per
   hole of its mode; see "Multi-operand instructions" below. Directive lookup
   has to come first:
   `find-instruction-variants` signals on an unregistered name, so it can't
   be tried first and fallen back from. This is what resolves *forward*
   references (`jmp end` before `end:` appears) — a caller doesn't have to
   write labels before their uses.

   Layout is not a single walk, though: a label-bearing (or `relative`-mode)
   operand's chosen width can depend on an address that layout itself hasn't
   placed yet, so `%layout` runs `%layout-pass` repeatedly — each pass
   re-choosing every statement's variant against the *previous* pass's
   complete symbol table — until the vector of chosen widths across all
   statements stops changing. The first pass has no symbol table to fold
   against at all, so every label-bearing operand starts at its **narrowest**
   syntax-matching variant; each later pass only ever **widens** a statement
   whose chosen mode no longer fits. Directive address effects and symbol
   values also settle across passes. Cyclic directive address dependencies
   are rejected before layout; see "Convergence" below.
2. **Encode.** Walk again, now with the complete symbol table: evaluate each
   statement's already-chosen variant's operand AST (`eval-expr`, below)
   against the symbol table, and encode (`encode-instruction`, or a
   directive's own byte-laying — [Directives](directives.md)). No re-parsing
   or re-matching happens here — layout already committed to a mode (or
   directive) and its parsed operand ASTs. Layout captures `.set` values in
   those ASTs at each statement's source position, while ordinary forward
   labels still resolve against the completed table.

### Choosing a mode

Two modes can share identical operand syntax and differ only in width — the
6502-shaped `zero-page`/`absolute` pair being the standard example (see
[Addressing modes](modes.md)). Sizing an instruction therefore isn't always
independent of its operand's *value*, and when that operand is a label, the
value isn't known until an earlier layout pass has placed it. `%choose-
variant` (in `assembler.lisp`) picks a mnemonic's variant with a floor and
two filters, applied to a mnemonic's variants in the order they were
declared in `(modes ...)`:

1. **Syntax.** Keep the variants whose mode's pattern matches the
   statement's operand tokens (`try-match-operand-mode`,
   [Addressing modes](modes.md)) — a no-operand variant's "pattern" is
   simply an empty token run. No match at all is an `assembly-error`.
   - **CHOICE eligibility** (#104/#126/#128), applied right after: drop a
     candidate whose `word-fields` include a `(choice mode)`-selected field
     (see [Instructions, "CHOICE-selected word
     fields"](instructions.md#choice-selected-word-fields)), or whose own
     sub-opcode selector — a hole-selected `(variant (choice m) (sub s))`
     (see [Instructions, "hole-selected
     sub-opcode"](instructions.md#variant-choice-m-sub-s--hole-selected-sub-opcode)),
     or a `(sub-opcode ...)` table's entry at one or more holes (see
     ["multi-hole sub-opcode
     selection"](instructions.md#sub-opcode--multi-hole-sub-opcode-selection))
     — names a different alternative at any of its holes, unless `mode` is
     the alternative that hole actually matched — `try-match-operand-mode`'s
     own `choices` return value, hole-aligned. A candidate with no such
     selector on either encoding scheme is always eligible, so a machine
     using no `one-of` at all is unaffected.

     This is also what keeps per-hole `:width` ([Addressing modes, "Per-hole
     `:width`"](modes.md#per-hole-width)) from disturbing convergence
     (below), even though two sibling descriptors of one mode can now have
     genuinely different `instruction-descriptor-size`s: CHOICE eligibility
     narrows a sub-opcode-selected hole to its one matching sibling by
     *syntax* alone, and syntax doesn't change between relaxation passes the
     way a folded value can, so the chosen size is already constant on the
     first pass — nothing for the floor step below to widen. This is the
     opposite situation from `zero-page`/`absolute`, which share *identical*
     syntax and are told apart only by whether a value fits — exactly where
     relaxation is meaningful.
   - **Specificity.** Candidates whose match used more
     [register-qualified holes](modes.md#register-qualified-holes) move
     ahead of the rest; otherwise declaration order is kept. `[r1]` against
     `"[" expr "]"` and `"[" (expr :register r) "]"` variants picks the
     register variant even when the plain one is declared first or is
     wider, since a register alias is also a valid plain `expr` value.
2. **Floor.** Drop any variant smaller (by `instruction-descriptor-size`)
   than this statement's current floor — the size it committed to on an
   earlier pass (0 on the first pass, when nothing has committed to anything
   yet). This is the sticky-widening rule: once a statement has chosen a
   size, it is never offered a smaller one again, which is what keeps the
   pass-to-pass size vector monotone and bounds the number of passes.
3. **Value**, checked independently for each syntax-and-floor-matching
   candidate — candidates of one mnemonic can have different hole counts
   (e.g. a two-register mode alongside a one-immediate mode), so whether a
   candidate's values are even known yet is not one shared question:
   - If **every** hole folds — against the previous pass's symbol table, or
     no table at all on the first pass — to a value that fits its own
     field's width (`%fits-width-p`, checked value-by-value against the
     candidate's own `operand-widths`; it accepts both the unsigned and the
     two's-complement signed range, e.g. both `255` and `-1` fit one byte),
     the candidate is a **fit**. The **first** fit in that order is
     kept — which is why [Addressing
     modes](modes.md#declare-narrower-modes-before-wider-ones) says to
     declare narrower/cheaper modes before wider ones that also match their
     syntax: declaring them in the other order still "works", it just always
     picks the wider one.
   - A **`relative`** mode candidate ([Addressing modes](modes.md#pc-relative-modes))
     folds to an absolute target, not the offset that actually gets encoded,
     so its fit test computes that offset the same way `%encode` will (see
     "PC-relative offsets" below) and checks it against the signed range
     instead of comparing the raw target to an operand width. This is a
     *per-hole* check, same as `:signed` below: a candidate's
     `relative-holes` ([Instructions](instructions.md#relative-holes)) marks
     every relative field. Each offset is checked against that field's own
     range. Other fields use their signed or unsigned range.
   - A non-`relative` **`:signed`** mode candidate ([Addressing modes,
     "Signed operands"](modes.md#signed-operands)) fits against the signed
     range only (`%fits-signed-width-p`), not the wider unsigned-inclusive
     range `%fits-width-p` accepts — so e.g. `ldsi #200` on a one-byte
     `:signed` mode does not fit it and the filter moves on to a wider
     candidate, even though `200` would fit an ordinary (non-`:signed`) mode
     of the same width. Only `relative` errors instead of falling back at
     encode time (see "PC-relative offsets" below); a `:signed` operand that
     doesn't fit any candidate falls back and wraps like any other mode. This
     is a *per-hole*, not per-candidate, check: a candidate whose mode has a
     `one-of` hole with per-hole `:signed` ([Addressing modes, "Per-hole
     `:signed`"](modes.md#per-hole-signed)) fits each hole against
     `%fits-signed-width-p` or `%fits-width-p` individually, reading the
     descriptor's own `operand-signedness` (precomputed at `definstruction`
     time from which `one-of` alternative that hole was claimed for) rather
     than one signed/unsigned choice for the whole candidate.
   - A **word-encoded** candidate (#20, `instruction-descriptor-word-fields`
     non-`nil` — see "Word-encoded instructions" below) fits by a different
     rule entirely: each hole's value must fall inside that candidate's own
     declared `(range LO HI)` for an inline field, while an `extra-word`
     field fits when the value fits its own declared cell width (#135) —
     `operand-widths` is `nil` for these descriptors, so none of the
     byte-width branches above apply.
   - If **no** candidate fits — `ldx #300` on a one-byte `immediate` — fall
     back to the **widest** syntax-and-floor-matching candidate (by total
     operand width) and let `encode-instruction`'s existing `wrap-value`
     mask each value, exactly as a single-mode instruction has always done.
     **Unless** the eligible set was narrowed by CHOICE eligibility (step
     1.5) — a `choice`-selected field's matched-but-out-of-range value has
     no wider `choice`-selected sibling to relax into (widening there would
     just wrap to bits that decode as a *different* addressing form, not a
     truncated version of the same one) — in which case this is an
     `assembly-error` instead, deferred to the final pass like the ambiguity
     warning below. See [Diagnostics, "Strict operand
     range"](diagnostics.md#strict-operand-range).
   - If any hole doesn't fold at all (a label not yet in the symbol table —
     always true of a forward reference on the first pass) fall back to the
     **narrowest** eligible candidate instead of the widest, so an operand
     whose value isn't known yet gets a chance to fit once a later pass
     knows it, rather than committing to the widest mode immediately.
   - Both fallback cases keep that order on a tie (equal total width).

Once relaxation has converged (the final pass, not a mid-relaxation trial —
see "Convergence" below), `%choose-variant` also checks for **ambiguity**:
if the chosen candidate ties on total operand width and register-qualified
hole count with another syntax-matching candidate of a different mode, it
`warn`s with an
`ambiguous-mode` condition naming both, since nothing but declaration order
distinguished between them. This is *not* the `zero-page`/`absolute` case
above — those differ in width, so relaxation resolves the choice on its own
and never warns; the warning fires only when width alone can't break the
tie either. See [Diagnostics, "Mode-selection
ambiguity"](diagnostics.md#mode-selection-ambiguity). The same pass warns
with `ambiguous-alternative` when declaration order alone decided a `one-of`
element's alternative (see [Diagnostics, "Alternative
ambiguity"](diagnostics.md#alternative-ambiguity)).

An out-of-range value that falls back to `wrap-value` (the "no candidate
fits" case above) is by default silent, same as it always was — opt into an
`assembly-error` instead per mode (`:strict t`) or globally
(`*strict-operand-range*`), or per hole on a `one-of` element (`:strict t`
on one alternative alone); see [Diagnostics, "Strict operand
range"](diagnostics.md#strict-operand-range). `%choose-variant`'s own
`choices` — the hole-aligned matched-alternative list `try-match-operand-
mode` already computed for the chosen candidate (step 1 above) — rides
along with the chosen descriptor and its holes' AST list all the way to
`%encode`, which is what lets the strict check tell a hole's own matched
alternative apart from its mode's whole-statement setting.

#### Forcing a mode with a mnemonic suffix

A statement whose mnemonic carries a forced addressing-mode suffix (`lda.w`,
`lda.z`; see [Addressing modes, "Forcing a mode with a mnemonic
suffix"](modes.md#forcing-a-mode-with-a-mnemonic-suffix)) narrows the
mnemonic's variants before any filter runs:

1. The suffix resolves to its `mode-descriptor` (`find-mode-by-suffix`) —
   `assembly-error` if no mode declares that suffix.
2. Only the variants of this mnemonic using that mode stay — `assembly-error`
   if none does (this also covers a no-operand variant, whose `mode` is
   `nil`).
3. The syntax, floor, and value filters above then run over what is left,
   so a word-encoded mode still picks between its inline and extra-word
   combos by value. A byte-encoded mode has one variant left, chosen
   unconditionally: an out-of-range value silently wraps at encode time via
   `encode-instruction`'s `wrap-value`. A forced `relative` mode still
   range-checks at encode time and signals `assembly-error` on overflow.
   An operand that does not match the forced mode's syntax is an
   `assembly-error` naming the mode.

#### Forcing one hole with a prefix

A word-encoded hole can be forced with a prefix (`seta #w:5`; see
[Addressing modes, "Forcing one hole with a prefix"](modes.md#forcing-one-hole-with-a-prefix)).
After the syntax match, only combos whose word-field choice for that hole
declares the named `:suffix` (see [Instructions, "Variant
suffixes"](instructions.md#variant-suffixes)) stay. If none does,
`assembly-error` lists the accepted prefixes. A forced inline variant whose
value does not fit signals `assembly-error` once layout has converged.

Both kinds of forcing depend only on the statement's own syntax, never on
the symbol table, so they keep [Convergence](#convergence) below monotone.

```lisp
lda $10       ; constant, fits zero-page -> zero-page (first declared fit)
lda $1000     ; constant, doesn't fit zero-page -> absolute
lda target    ; label -> starts at zero-page (narrowest); widens to absolute
              ; only if target's resolved address doesn't fit one byte
```

### Convergence

`%layout` compares instruction widths, directive effects, and symbol values
between passes; once a pass reproduces the previous one exactly, one further pass
runs with a flag that turns on two checks that only make sense once
relaxation has settled (an `.org` that briefly looked like it moved the
address counter backward mid-relaxation is not actually an error unless it
is still true once widths have stopped changing — see
[Directives](directives.md)), and its result — checked against the same
layout as an assertion — is what `%encode` sees. Width floors only increase.
With cyclic directive dependencies rejected, forward values propagate through
a bounded number of passes between widenings. The assembler calculates that
bound after macro expansion and reports a convergence error if it is exceeded.

## Multi-operand instructions

An instruction whose mode declares more than one `expr` hole
([Addressing modes](modes.md)) wires up one operand encoding field per hole
([Instructions](instructions.md)), evaluated and encoded in hole order.
Layout sizes the statement by the sum of those fields' widths
(`instruction-descriptor-total-operand-width`); encode evaluates *every*
hole's AST against the completed symbol table — including a label bound in
a later hole, not just the first — and hands the whole list of values to
`encode-instruction`:

```lisp
mov $20, target   ; DST ($20) and SRC (target's resolved address) both
target: nop       ; encode, in the machine's own endian order (#66), one
                  ; after the other
```

## Word-encoded instructions (#20)

A machine declaring an `instruction-word` clause ([Machine
model](machine-model.md)) encodes one instruction as a single fixed-width
word rather than an opcode cell plus operand cells — see [Instructions,
"Word-encoded instructions"](instructions.md#word-encoded-instructions-20)
for how `definstruction` declares it. Sizing and relaxation still go through
the same `%choose-variant` pipeline described above, generalized via
`instruction-descriptor-size` instead of assuming a single-cell opcode:

- A variant-bearing operand field expands `definstruction` into several
  `instruction-descriptor`s sharing one mnemonic, mode, and opcode
  value — one all-inline, one (or more) needing an extra word — ordered
  all-inline first, then narrowest-total-extra-word-cells first, exactly
  the "declare narrower modes before wider ones" convention above,
  generalized from addressing-mode width to extra-word cells (#135; an
  `extra-word` field's own trailing word may be narrower or wider than the
  instruction word, via its `:cells` declaration — see
  [Instructions](instructions.md#word-encoded-instructions-20)).
- The value filter's word-encoded branch (above) checks each field's value
  against its own declared inline range rather than a cell width; an
  `extra-word` field fits when the value fits *its own* declared width
  (`:cells`, signed when the field is signed) — no longer unconditionally,
  now that width can be narrower than a full instruction word.
- `instruction-descriptor-size` — a word-encoded machine's own instruction
  word cell width (#53, the target memory's `:cell-width`) plus its chosen
  variant's total extra-word cells (#135) — replaces `1 + total-operand-width`
  everywhere layout and relative-offset arithmetic used to assume a
  single-cell opcode.
- A `:relative` addressing mode, whole-mode or per-hole alike, works the
  same as on a byte-encoded machine (#62, see
  [Instructions, "PC-relative operands"](instructions.md#pc-relative-operands-62))
  — "PC-relative offsets" below covers both encodings; the only difference
  is what the offset is range-checked against (a `word-field-choice`'s own
  declared field, not a cell width).

`encode-instruction` packs the opcode and every inline field's (biased)
value into one instruction word by bit shift, then appends each
`extra-word` field's own value as a separate word in the layout's own
endian order (#66), at that field's own declared cell width (#135) — see
[Instructions](instructions.md#operand-pipeline). See
[`examples/word.lisp`](../examples/word.lisp) for a complete program
assembled and run end to end.

## PC-relative offsets

A `relative`-mode operand ([Addressing modes](modes.md#pc-relative-modes))
folds to an absolute target address like `absolute` does, but that's not
what gets encoded. Once encode evaluates the target against the completed
symbol table, `%encode` computes the signed offset from the address of the
*next* instruction:

```lisp
offset = target-address - (branch-address + instruction-size)
```

`branch-address + instruction-size` is deliberately the same arithmetic
`step-machine` ([Emulator](emulator.md#step-machine)) uses to advance `pc`
past the branch *before* running its semantics — so an instruction's own
`(set! pc (+ pc operand))` adds the offset to exactly the base it was
computed from, at any `:origin`. `instruction-size` here is always the
*whole* encoded instruction's width — constant per descriptor regardless of
which hole is relative, so it's never adjusted per hole (see below).
`encode-instruction` itself needs no special case: `wrap-value` already
renders a negative offset as its two's-complement cell (e.g. `-3` as `#xFD`
on an 8-bit-cell machine).

Each relative hole is range-checked against its own width, independently
of its siblings. Overflow signals `assembly-error`. Ordinary holes keep
their evaluated values and follow normal strict-range rules.

On a word-encoded descriptor, each relative hole uses its selected
`word-field-choice` range or its extra-word width
(`%word-relative-offset-fits-p`, `assembler.lisp`). The check is just as
unconditional as the byte-encoded one above — `%check-strict-operand-range!`
is already a no-op for a word-encoded descriptor regardless of `:strict`,
so `%relative-offset`'s own check is the *only* thing standing between an
out-of-range branch and a silently wrapped one. `%choose-variant`'s own
value filter (above) and its `#104` overflow diagnostic both need the same
target → offset conversion *before* deciding whether any candidate fits at
all, i.e. without `%relative-offset`'s own range check — `%relative-adjusted-values`
factors that unchecked arithmetic out for both call sites.

## `eval-expr`

`instruction.lisp`'s `eval-expr-constant` folds an expression AST with no
label support at all (any `expr-label` signals `unresolved-label`). The
assembler needs the same folding logic once labels *are* known, so
`eval-expr-constant` is defined in terms of a more general function:

```lisp
(eval-expr AST &key symbols pc)  ; symbols: string -> value hash table
                                  ; pc: this statement's address, or NIL
(eval-expr-constant AST &key pc) = (eval-expr AST :symbols nil :pc pc)
```

`eval-expr` looks an `expr-label`'s name up in `symbols` and signals
`unresolved-label` only on a miss (or when `symbols` is `nil`, matching the
old no-labels-ever behavior). An `expr-location` node (the `"*"`
location-counter symbol, below) resolves from `pc` instead, independently of
`symbols` — `eval-expr-constant` still folds no labels, but a caller can
still supply `:pc` to fold a location-counter reference.

### Register aliases (#72)

A `symbols` miss falls back to `*register-aliases*`, a special bound by
`assemble-statements` to the target machine's alias table (`register NAME
:names (...)`, see [Machine model](machine-model.md)) — so `set a, 5`
resolves `a` to its bank index exactly as `set 0, 5` folds the literal.
Symbols are tried first, but the fallback never actually shadows anything:
`%bind-symbol!` (below) rejects a label or `.equ` name that collides with an
alias outright, so the two tables never disagree on a name they both hold.

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
address, which layout already has for every statement (unlike a label,
whose final address may not be known until layout converges), so both
layout and encode pass one through as `eval-expr`'s `:pc`:

- **An instruction operand** resolves against the statement's own address —
  the same address `%choose-variant` uses to pick a mode during layout, and
  the same one `%encode` passes to `eval-expr` afterward. A `relative`-mode
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
is qualified to `scope ++ NUL ++ name` (`%qualify-local`/`%qualify-locals!` in
`assembler.lisp`) before it ever reaches the symbol table or an operand
AST — so `symbols` itself stays the same flat string → address table, and
`eval-expr` needs no scope argument of its own. **A statement's own label is
bound (and, if global, becomes the new scope) before its own operands are
qualified** — so in `loop: bne .x`, `.x` is scoped to `loop`, the label on
that same line, not whatever preceded it.

A local label with no enclosing global label — at its definition or at a
reference — signals `assembly-error`. A global named `loop.next` and a local
`.next` under `loop` can coexist. An `.equ` or `.set` name uses the same key
rule when local.

Alongside `symbols`, `%bind-symbol!` records each entry's unqualified name,
enclosing scope, and kind (label vs. `.equ`, below) in `symbol-info` —
captured when a name is bound. `symbol-info-qualified-name` remains readable,
so both of those names display as `loop.next`; their scope distinguishes them. See
[Listing and source map](listing.md#symbol-table) for the scope-aware lookup
and grouped listing built on `symbol-info`.

## `.equ` / symbol assignment

```lisp
.equ size, 16     ; "size" -> 16, no address occupied
count = 4         ; sugar for ".equ count, 4" -- see Directives
```

An `.equ` (see [Directives, "`.equ`"](directives.md#equ)) binds a name to a
computed value in `symbols` without occupying any address — distinct from a
label, which always binds to the current address. Its value must fold
*during the layout pass that reaches it*, against that pass's `symbols` table
as built so far: an `.equ` can reference any label or assignment bound above it,
never one below (a forward assignment reference signals `assembly-error`). Rebinding
an already-bound name — a label redefined as an `.equ`, an `.equ` redefined
as a label, or a second `.equ` of the same name — is the same duplicate-
symbol `assembly-error` a repeated label signals.

`.org` and `.res` may use labels and address-dependent assignments when
their address dependencies are acyclic:

```lisp
.equ bufsize, 16
.res bufsize

start: nop
.equ size, * - start
.res size           ; reserves one cell
```

An ordinary instruction operand or `.byte`/`.word` value folds during
encoding, after layout has fixed its address.

An `.equ`'s `symbol-info` entry is tagged kind `:equ`, distinct
from a label's `:label` — the discriminator a plain `symbols` lookup can't
give you (its value is just an integer either way), and what fixes the
disassembler's own label/`.equ` ambiguity (see
[Disassembler](disassembler.md)).

## `.set`

`.set name, value` binds a new assignment or replaces an earlier `.equ` or
`.set` assignment. It does not replace a label. Its value folds against the
symbols already bound when layout reaches it. Instruction and data operands
capture the current value at their own source position; a reference before
the first assignment is `assembly-error`. Address-dependent `.set` values
may feed later `.org` and `.res` directives when layout remains acyclic.

`assembly-symbols` retains the final value. Its `symbol-info` entry has kind
`:set` and the last assignment's source location. See [Directives](directives.md#set)
for an example.

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

- `assembly-error` (a subtype of `lasm-syntax-error`) — a duplicate symbol (a
  label or `.equ` name bound twice), a label or assignment
  name colliding (case-insensitively) with a register alias (see
  "Register aliases" above), a local label or assignment name with no enclosing
  global label (see "Local-label scoping" above), an operand whose syntax
  matches none of the mnemonic's declared
  addressing-mode variants (naming the accepted modes and the operand given —
  see [Diagnostics](diagnostics.md)), a `relative`-mode offset that doesn't
  fit its operand's width (see "PC-relative offsets" above), a strict-mode
  operand out of range (see "Choosing a mode" above and
  [Diagnostics](diagnostics.md#strict-operand-range)), or a malformed
  directive use (wrong operand count, a cyclic or undefined `.org`/`.res` operand,
  an assignment value
  referencing a symbol not yet defined, or a backward-moving `.org` — see
  [Directives](directives.md)).
- `include-error` — a malformed or unresolvable `.include` (see
  [Includes](includes.md)).
- `macro-error` — a malformed `.macro`/`.endm` block or invocation (see
  [Macros](macros.md)).
- `unknown-instruction` — an unregistered mnemonic (from
  `find-instruction-variants`, see [Instructions](instructions.md)).
- `unresolved-label` — an operand references a label never bound anywhere in
  the program (from `eval-expr`). It includes the label token's position.
- `unresolved-location` — a `"*"` location-counter reference folded with no
  `:pc` given (see "Location counter" above); does not occur during ordinary
  assembly, only from a standalone `eval-expr`/`eval-expr-constant` call.
- `lex-error` / `parse-failure` — from the front end (`assemble` only).
- `ambiguous-mode` — a warning (program execution continues after it), not
  an error; see "Choosing a mode" above and
  [Diagnostics](diagnostics.md#mode-selection-ambiguity).
- `ambiguous-alternative` — a subtype of `ambiguous-mode` for a tied `one-of`
  alternative; see [Diagnostics](diagnostics.md#alternative-ambiguity).

Every condition above that subtypes `lasm-syntax-error` renders with a
source excerpt and caret via `diagnostic-text` once source text is
available — see [Diagnostics](diagnostics.md).

## Scope

This produces bytes and a symbol table from a statement list, including
directives (`.org`, `.byte`/`.word`, `.res`, `.equ` — see
[Directives](directives.md)) and macro expansion (`.macro`/`.endm` — see
[Macros](macros.md)). Listings are covered in [Listing and source
map](listing.md) and standalone output files in [Binary output](binary-output.md).
