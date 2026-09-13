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
(defmode NAME pattern-element... [:width n] [:signed t] [:relative t] [:suffix "s"])
```

`NAME` is a symbol, registered globally (like a lexer — see below).
`pattern-element` is either a string literal (matched against a token's
verbatim text, case-insensitively — so `"X"` matches `x` too) or the symbol
`expr` (parses one expression with the shared Pratt parser, `parse.lisp`).
At least one `expr` is required. `:width`, if given, is this mode's default
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
mnemonic suffix](#forcing-a-mode-with-a-mnemonic-suffix) below.

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
per hole — a two-register `mov` is the standard example (see [Instructions,
"Repeated `(operand ...)` subclauses"](instructions.md)). A `:signed` mode
may have any number of holes — each is sign-extended independently (see
[Signed operands](#signed-operands) below). The one restriction is specific
to `:relative`: a `:relative` mode may not have more than one hole, since
its *offset* applies to the operand as a whole and there is currently no way
to mark just one hole of a multi-hole mode as the relative one (see
[PC-relative modes](#pc-relative-modes) below).

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

`:relative` (below) **implies** `:signed` — a branch offset can go either
direction — but the two attributes are otherwise independent: a signed,
non-relative mode (e.g. a signed 8-bit immediate) declares `:signed t` on
its own, with no offset computation attached.

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
[`stack-ref`](machine-model.md) against a stack named in the semantics body:

```lisp
(definstruction hybridfoo lda
  (modes stack-relative)          ; source: lda 1,S
  (encoding (opcode #xA3) (operand :mode))
  (semantics (set! a (stack-ref machine 's operand))))
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
  unrelated grammar; see [Directives](directives.md).

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
