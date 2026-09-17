# Directives

`defdirective` declares an assembler directive: a named-parameter list plus
exactly one action form from a small fixed vocabulary. LASM ships seven
built-in directives (in `directive.lisp`): `.org`, `.byte`, `.word`, `.cell`,
`.dat`, `.res`, `.equ`. The assembler (see [Assembler](assembler.md))
dispatches a statement to a directive by mnemonic, the same way it dispatches
to an instruction's addressing-mode variants — a directive statement is
otherwise an ordinary `statement` (see [Statement grammar & expression
parser](parser.md)), just one whose mnemonic happens to start with `.` under
the default lexer's `ident-chars`.

```lisp
(defdirective ".org"  (address)      (set-origin! address))
(defdirective ".byte" (&rest values) (emit 1 values))
(defdirective ".word" (&rest values) (emit 2 values))
(defdirective ".cell" (&rest values) (emit 1 values))
(defdirective ".dat"  (&rest values) (emit 1 values))
(defdirective ".res"  (count)        (reserve count))
(defdirective ".equ"  (name value)   (assign name value))
```

## `defdirective`

```lisp
(defdirective name params action-form)
```

`name` is a string (e.g. `".org"`), matched case-insensitively. `params` is
`(value-name)` — a directive taking exactly one operand — `(name-name
value-name)` — one taking exactly two (`.equ`'s own shape) — or `(&rest
values-name)` — a variadic directive taking any number of operands,
including zero. `action-form` must be exactly one of:

- `(set-origin! value-name)` — move the assembler's address counter (and, if
  no earlier statement has occupied an address yet, the assembly's own
  `origin` — see "`.org`" below) to `value-name`. Zero layout size.
- `(reserve value-name)` — advance the address counter by `value-name`
  cells (the machine's own addressable unit, #53 — bytes on every
  byte-addressed machine, the only kind before this), zero-filled.
- `(emit width values-name)` — lay down `(length values-name)` `width`-cell
  fields, one per value, in the machine's own endian order (#66 — the same
  `%encode-value-cells` an instruction operand uses, so code and data can't
  disagree). Layout size is `width * (length values-name)`.
- `(assign name-name value-name)` — bind `name-name` (an identifier operand,
  not an expression) to `value-name` in the symbol table, without occupying
  any address (`.equ` below).

`action-form` must reference the directive's own parameter name — this
(along with restricting the body to one recognized action, not arbitrary
Lisp) is what lets the assembler compute a directive statement's layout size
*without evaluating anything*, the same way it already knows an
instruction's size from its chosen `instruction-descriptor` alone. Anything
else — a second form, an unrecognized action head, an action referencing a
different symbol — is an error at `defdirective`-registration time.

Unlike `defmode` (see [Addressing modes](modes.md)), registration is a plain
top-level `setf`, not wrapped in an `eval-when`: `defmode` needs compile-time
registration because `definstruction` resolves mode names at macroexpansion
time, but nothing resolves a directive name at macroexpansion — the
assembler looks a directive up by name at ordinary runtime, the same as
`deflexer`'s `*lexers*` table (see [Lexer](lexer.md)).

`(find-directive-descriptor name)` looks a directive up by name, returning
`nil` (not signalling) on a miss — the assembler uses this to tell a
directive statement from an instruction one, so a miss here is an ordinary
outcome, not a caller error.

## `.org`

```lisp
.org $8000
```

Moves the address counter to its (single, constant) operand. Before any
statement has occupied an address, `.org` also moves the assembly's own
`origin` (see [Assembler, "`:origin`"](assembler.md#origin)) — so a leading
`.org` places the whole program without needing a matching `:origin` key
passed to `assemble`. After that point it only pads *forward*: moving to an
address at or past the current one zero-fills the gap; moving backward
signals `assembly-error` rather than guessing whether the intent was to
overwrite or truncate.

`.org`'s operand must fold to a **label-free constant** — a directive whose
*address effect* moves the counter has to fold before layout can even
compute the addresses layout itself relies on, so `.org some_label` signals
`assembly-error` naming the label, rather than the bare `unresolved-label` a
plain `eval-expr-constant` miss would give. (This is unrelated to layout
running more than one pass to relax addressing-mode choices — `.org`'s own
operand stays label-free on every pass.) This is the one directive-design
rule every other built-in directive is shaped around: a directive whose
*size or address effect* depends on an argument must fold that argument
during layout; one whose size instead comes from argument *count*
(`.byte`/`.word` below) can defer its values to encode, same as an ordinary
instruction operand.

A label on a `.org` line binds to the address `.org` moves *to*:

```lisp
here: .org $8000   ; here == $8000, not the address before the move
```

## `.byte` / `.word`

```lisp
.byte 1, 2, 3        ; three one-cell fields: 01 02 03
.word $1234          ; one two-cell field -- little-endian by default: 34 12
.byte target         ; a label operand -- resolved at encode time, like an
target: nop          ; ordinary instruction operand
```

Variadic; zero or more comma-separated operands (`statement-operands`, see
[Statement grammar & expression parser](parser.md) — each one a bare
expression, not an addressing-mode pattern). `.byte` lays down one cell per
value, `.word` one two-cell field per value, in the machine's own endian
order (`:endian`, #66, [Machine model](machine-model.md#defmachine)) — both
via `%encode-value-cells`, the same cell-splitting `encode-instruction` uses
for an ordinary operand ([Instructions, "Encoding"](instructions.md)), so
directive data and instruction operands can't drift apart in how they lay
cells down. A value out of its field's
range wraps via the existing `wrap-value`, exactly like an instruction
operand (diagnosing that instead of wrapping is a separate, existing
follow-up ticket, not specific to directives).

**On a word-addressed machine** (`:cell-width` other than 8, #53) `.byte`
and `.word` still mean *one* and *two of the machine's own cells* — the
names are inherited from every byte-addressed example so far and are a
misnomer there (`.byte 1, 2` on a 16-bit-cell machine lays down two 16-bit
cells, not two 8-bit bytes). `.cell`/`.dat` (#65, below) are the
cell-sized spellings meant for this case.

Because layout size here is just argument *count*, `.byte`/`.word` values
are evaluated against the completed symbol table at encode time — a label
operand works with no special handling.

## `.cell` / `.dat`

```lisp
.cell 1, 2, 3        ; three one-cell fields, same as .byte
.dat  $1234           ; one one-cell field, DCPU-16 spelling
```

Plain aliases for `.byte` — identical `:emit` action and width-1 descriptor,
so they run through the exact same encode path (`%encode-value-cells`) and
share every rule above, including how a value out of range wraps and how a
label operand resolves at encode time. They exist so a word-addressed
machine's source can write "one of this machine's own cells" without
reaching for the byte-addressed-flavored `.byte` — `.dat` matches the
spelling DCPU-16 assemblers use for the same thing; `.cell` is the
machine-agnostic name. Both are registered globally, so they are visible (as
plain `.byte` synonyms) on byte-addressed machines too, and — like every
other directive — occupy the mnemonic namespace, so no machine may define an
instruction named `.cell` or `.dat`.

```lisp
(defmachine wordfoo
  (register pc :width 16)
  (memory ram :width 16 :addr-width 12 :cell-width 16))

result: .dat 0   ; one 16-bit cell, not two 8-bit bytes
```

## `.res`

```lisp
.res 4   ; four zero-filled cells
```

Advances the address counter by its (single, constant — same label-free
folding rule as `.org`) operand, zero-filled — cells, not necessarily
8-bit bytes (#53; `.org`'s own operand is an address and was always
cell-indexed, so `.org` itself needs no such caveat). A negative count
signals `assembly-error`. Since the emulator has no way to skip over a run
of cells sitting in the middle of the code path (no jump/skip instruction is
part of the core semantics vocabulary), a `.res` run belongs in a data area
the program's control flow doesn't traverse — see
[`examples/directives.lisp`](../examples/directives.lisp).

## `.equ`

```lisp
.equ size, 16       ; "size" -> 16, no address occupied
count = 4           ; sugar for ".equ count, 4"
```

Binds its first operand — a bare identifier, not an expression — to its
second operand's value in the symbol table, occupying no address (#35).
`name = value` is accepted as sugar for `.equ name, value`: the lexer's `=`
punctuator (`lexer.lisp`) has no meaning to the expression parser, so it only
ever appears here; the parser (`parser.lisp`, `%parse-line`) rewrites the
sugar to an ordinary `.equ` statement before the assembler ever sees it,
so there is exactly one code path for both spellings.

An `.equ`'s value folds during the layout pass that reaches it, against that
pass's symbol table *as built so far* — so it can reference any label or
`.equ` bound above it (`* - start` included, once `start:` precedes it), but
never one below; a forward reference is `assembly-error`, the same
"must-fold-now" rule `.org`/`.res`'s own operand already follows. Rebinding
an already-bound name — a label redefined as an `.equ`, an `.equ` redefined
as a label, or a repeated `.equ` — signals the same duplicate-symbol
`assembly-error` a repeated label does; the symbol table is one flat
name → value map regardless of which bound a given name.

```lisp
.equ a, 1
.equ b, a + 1     ; chains off an earlier .equ
start: nop
nop
.equ size, * - start   ; size == 2
```

A label on an `.equ`/`=` line binds as usual, to the statement's own address
— unrelated to the name the `.equ` itself binds:

```lisp
here: count = 4   ; here == this line's address; count == 4
```

A local name (the lexer's `local-label-prefix`, e.g. `.n`) is scoped to its
nearest preceding global label exactly like a local label — see
[Assembler, "Local-label scoping"](assembler.md#local-label-scoping-16).

Because `.org`/`.res` must fold their own operand in pass 1, before any
address is final, they may reference an `.equ` only when it's **pure** — its
value contains no label and no `"*"`, so it can't change across relaxation
passes:

```lisp
.equ bufsize, 16
.res bufsize        ; fine

start: nop
.equ size, * - start
.res size           ; assembly-error -- size depends on start's address
```

An ordinary instruction operand or `.byte`/`.word` value has no such
restriction, since those fold at encode time against the completed table
like any label reference. See [Assembler, "`.equ` / symbol
assignment"](assembler.md#equ--symbol-assignment) for why (#41 tracks
lifting it).

## Scope: `.macro` is not a directive

`.macro`/`.endm` is deliberately **not** built on `defdirective`. A directive
is a per-statement action with a size the assembler can compute without
evaluating anything; a macro instead captures a *range* of statements and
substitutes parameter tokens into them at each invocation site — there is no
action form in `defdirective`'s vocabulary that could express "collect
everything up to the matching `.endm`". It lives in its own statement-
expansion pass instead, `expand-macros` (`macro.lisp`), which
`assemble-statements` ([Assembler](assembler.md)) runs before layout ever
sees the statement list — see [Macros](macros.md). `.equ`, by contrast, fits
`defdirective` just fine even though it binds a *name* rather than sizing
anything: its layout size is statically zero (like `.org`'s), and its
address effect — none at all — is exactly as computable without evaluation
as every other directive's, which is the only thing `defdirective`'s
restricted vocabulary actually requires.

## Conditions

- `assembly-error` — wrong operand count for a directive's declared arity, a
  non-constant `.org`/`.res` operand (a label reference, or a non-pure
  `.equ` reference, which must fold before layout can compute addresses at
  all — see "`.equ`" above), a backward-moving `.org`, a negative `.res`
  count, an `.equ`'s first operand not a bare identifier, an `.equ` value
  referencing a symbol not yet bound (forward reference), or a duplicate
  symbol (a label or `.equ` name bound twice, in any combination).
- `unresolved-label` — a `.byte`/`.word`/`.cell`/`.dat` operand referencing a
  label never bound anywhere in the program (from `eval-expr` at encode time,
  same as an instruction operand).

## Follow-ups

- `.ascii`/`.asciz` — needs a string node in the expression parser
  (`parse-expression`, [Statement grammar & expression parser](parser.md)),
  which has none today.
- `.set` / redefinable assignment — a rebinding counterpart to `.equ`, which
  signals `assembly-error` on any rebind (see "`.equ`" above).
- Lifting `.org`/`.res`'s pure-`.equ`-only restriction (#41).
- The disassembler's undecodable-data lines always render as `.byte $XX`
  (`%data-line-text`, `disassembler.lisp`), even on a word-addressed machine
  — rendering `.cell`/`.dat` there needs the machine's cell width threaded
  through disassembly, which none of `disassemble-cells`/`-assembly`/`-memory`
  currently plumb in.

See the tracker for these.
