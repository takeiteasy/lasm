# Addressing modes

`defmode` declares an addressing mode: a literal/token pattern with one or
more `expr` holes, matched against a statement's operand tokens by
`match-operand-mode`/`try-match-operand-mode`. `definstruction` (see
[Instructions](instructions.md)) wires one or more modes to encoding and
semantics; this document covers the pattern grammar and matching on their
own.

```lisp
(defmode immediate   "#" expr        :width 1)
(defmode zero-page   expr            :width 1)
(defmode absolute    expr)
(defmode indexed-x   expr "," "X")
(defmode indirect-y  "(" expr ")" "," "Y")
(defmode relative    expr            :width 1 :relative t)
```

`immediate`, `absolute`, and `relative` above are exactly the modes LASM
ships built in (in `mode.lisp`) — there is no special-cased "M1 mode" table
any more; every mode, built-in or user-declared, goes through the same
`defmode`.

## `defmode`

```lisp
(defmode NAME pattern-element... [:width n])
```

`NAME` is a symbol, registered globally (like a lexer — see below).
`pattern-element` is either a string literal (matched against a token's
verbatim text, case-insensitively — so `"X"` matches `x` too) or the symbol
`expr` (parses one expression with the shared Pratt parser, `parse.lisp`).
At least one `expr` is required. `:width`, if given, is this mode's default
operand byte width — see [`(encoding ...)`](instructions.md) and the
width-resolution note below. `:relative t`, if given, marks this
mode's operand as a PC-relative offset rather than an absolute value — see
[PC-relative modes](#pc-relative-modes) below.

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
narrower one that would also have fit.

### Width

A mode's own `:width`, when given, is what `(operand :mode)` and a
multi-mode variant with no `(operand :width n)` of its own resolve to.
`immediate` and `zero-page` above are always one byte; `absolute` gives no
`:width`, so instructions using it fall back to the machine's address width
(see [Instructions, "`(encoding ...)`"](instructions.md)) — sized once per
machine rather than hard-coded into the mode.

A mode with more than one `expr` hole wires up one operand encoding field
per hole — a two-register `mov` is the standard example (see [Instructions,
"Repeated `(operand ...)` subclauses"](instructions.md)). The one
restriction: a `:relative` mode (below) may not have more than one hole,
since its offset applies to the operand as a whole and there is currently no
way to mark just one hole of a multi-hole mode as the relative one.

## PC-relative modes

`relative` matches the *same* bare-`expr` syntax as `absolute` — the two are
disambiguated only by `mode-descriptor-relativep`, not by pattern. What
differs is what the parsed value means and when it's computed:

- An `absolute` operand's value **is** the address encoded, evaluated once
  the symbol table is complete ([Assembler](assembler.md)).
- A `relative` operand's value is a *target address*, but what gets encoded
  is the signed offset from the address of the instruction **after** the
  branch: `offset = target - (branch-address + 1 + operand-width)`. The
  assembler computes this once both the branch and its target have an
  address ([Assembler](assembler.md#pc-relative-offsets)), and signals
  `assembly-error` if the offset doesn't fit the operand's width rather than
  silently wrapping to a branch at the wrong address.
- The emulator sign-extends the fetched operand back to a signed integer
  before running an instruction's `semantics` ([Emulator](emulator.md)), so
  a relative-mode instruction body writes a plain `(set! pc (+ pc
  operand))` rather than tracking its own operand width.

Because a `relative` candidate's value isn't the quantity that gets
range-checked against its width until encode time, `%choose-variant`
(the assembler's mode selector) treats it the same as an unresolved label —
always taking the widest syntax-matching variant. This only matters once a
mnemonic declares `relative` alongside another mode on the same syntax; see
the tracker for the follow-up on giving that case its own value-based
narrowing.

## Matching

- `(match-operand-mode tokens mode)` — `tokens` is a token run (e.g. a
  `statement`'s `operand-tokens`; see [Statement grammar & expression
  parser](parser.md)), `mode` a `mode-descriptor` or a symbol naming one.
  Consumes `mode`'s literal tokens in order and parses each `expr` hole.
  Returns `(values first-ast all-asts)` — `first-ast` alone is what every
  current single-hole mode needs. Signals `parse-failure` if `tokens` don't
  match `mode`, or leave a trailing token unconsumed.
- `(try-match-operand-mode tokens mode)` — the non-signalling form: returns
  `(values asts t)` on a match or `(values nil nil)` on a mismatch. This is
  what the assembler's mode-candidate filter uses to try several of a
  mnemonic's modes against one operand without a `handler-case` per
  candidate.

```lisp
(match-operand-mode (statement-operand-tokens some-statement) 'immediate)
;; => #10 parses to an EXPR-NUMBER AST, (values ast (list ast))
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
  unrelated grammar extension.

## Deviation from the design draft

[`LASM-plan.md`](../LASM-plan.md) §3.4 writes each mode's pattern with a
trailing `-> (tag $1)` arrow (e.g. `(defmode immediate "#" expr -> (imm
$1))`), suggesting a mode tags its parsed value for the assembler to
interpret. Nothing in LASM's pipeline reads such a tag — `operand` is always
bound to the raw decoded integer regardless of mode, and semantics
dereferences explicitly (see [Instructions](instructions.md#deviation-from-the-design-draft)).
`defmode`'s pattern accordingly ends at the last pattern element plus an
optional `:width n`, with no arrow. The draft is left unedited as a rough
plan; this document reflects what's actually implemented.
