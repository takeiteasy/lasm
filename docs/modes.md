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
```

`immediate` and `absolute` above are exactly the two modes LASM ships built
in (in `mode.lisp`) — there is no special-cased "M1 mode" table any more;
every mode, built-in or user-declared, goes through the same `defmode`.

## `defmode`

```lisp
(defmode NAME pattern-element... [:width n])
```

`NAME` is a symbol, registered globally (like a lexer — see below).
`pattern-element` is either a string literal (matched against a token's
verbatim text, case-insensitively — so `"X"` matches `x` too) or the symbol
`expr` (parses one expression with the shared Pratt parser, `parse.lisp`).
At least one `expr` is required. `:width`, if given, is this mode's default
operand byte width — see [`(encoding ...)`](instructions.md#encoding-opcode-n-operand)
and the width-resolution note below.

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
(see [Instructions](instructions.md#encoding-opcode-n-operand)) — sized once
per machine rather than hard-coded into the mode.

A mode with more than one `expr` hole (the pattern grammar allows it) can be
declared, but no instruction can currently *use* one — `definstruction` only
wires up a single operand encoding field, and signals an error naming this
if a mode with more than one hole is used. Multi-operand instructions are a
separate, larger feature.

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

## Why `statement-operand-tokens`, not `operand-tokens`

A `statement`'s `operands` (plural) are already split on top-level commas
(see [Statement grammar & expression parser](parser.md)) — needed for a
future multi-operand instruction, but wrong for mode matching: a pattern
like `indexed-x`'s `expr "," "X"` has its own literal comma, and matching it
against one comma-delimited fragment at a time would make it unmatchable.
Mode matching instead uses `statement-operand-tokens`, the whole run of
tokens after the mnemonic, uncommitted to any comma split.

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
