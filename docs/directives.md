# Directives

`defdirective` declares an assembler directive: a named-parameter list plus
exactly one action form from a small fixed vocabulary. LASM ships four
built-in directives (in `directive.lisp`): `.org`, `.byte`, `.word`, `.res`.
The assembler (see [Assembler](assembler.md)) dispatches a statement to a
directive by mnemonic, the same way it dispatches to an instruction's
addressing-mode variants — a directive statement is otherwise an ordinary
`statement` (see [Statement grammar & expression parser](parser.md)), just
one whose mnemonic happens to start with `.` under the default lexer's
`ident-chars`.

```lisp
(defdirective ".org"  (address)      (set-origin! address))
(defdirective ".byte" (&rest values) (emit 1 values))
(defdirective ".word" (&rest values) (emit 2 values))
(defdirective ".res"  (count)        (reserve count))
```

## `defdirective`

```lisp
(defdirective name params action-form)
```

`name` is a string (e.g. `".org"`), matched case-insensitively. `params` is
either `(value-name)` — a directive taking exactly one operand — or `(&rest
values-name)` — a variadic directive taking any number of operands,
including zero. `action-form` must be exactly one of:

- `(set-origin! value-name)` — move the assembler's address counter (and, if
  no earlier statement has occupied an address yet, the assembly's own
  `origin` — see "`.org`" below) to `value-name`. Zero layout size.
- `(reserve value-name)` — advance the address counter by `value-name`
  bytes, zero-filled.
- `(emit width values-name)` — lay down `(length values-name)` little-endian
  `width`-byte fields, one per value. Layout size is `width * (length
  values-name)`.

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

`.org`'s operand must fold to a **label-free constant** — pass 1 (layout)
has no symbol table yet, so `.org some_label` signals `assembly-error`
naming the label, rather than the bare `unresolved-label` a plain
`eval-expr-constant` miss would give. This is the one directive-design rule
every other built-in directive is shaped around: a directive whose *size or
address effect* depends on an argument must fold that argument in pass 1;
one whose size instead comes from argument *count* (`.byte`/`.word` below)
can defer its values to pass 2, same as an ordinary instruction operand.

A label on a `.org` line binds to the address `.org` moves *to*:

```lisp
here: .org $8000   ; here == $8000, not the address before the move
```

## `.byte` / `.word`

```lisp
.byte 1, 2, 3        ; three one-byte fields: 01 02 03
.word $1234          ; one two-byte field, little-endian: 34 12
.byte target         ; a label operand -- resolved in pass 2, like an
target: nop          ; ordinary instruction operand
```

Variadic; zero or more comma-separated operands (`statement-operands`, see
[Statement grammar & expression parser](parser.md) — each one a bare
expression, not an addressing-mode pattern). `.byte` lays down one byte per
value, `.word` one little-endian two-byte field per value — both via
`%encode-value-bytes`, the same little-endian byte-splitting
`encode-instruction` uses for an ordinary operand ([Instructions,
"Encoding"](instructions.md)), so directive data and instruction operands
can't drift apart in how they lay bytes down. A value out of its field's
range wraps via the existing `wrap-value`, exactly like an instruction
operand (diagnosing that instead of wrapping is a separate, existing
follow-up ticket, not specific to directives).

Because layout size here is just argument *count*, `.byte`/`.word` values
are evaluated against the completed symbol table in pass 2 — a label operand
works with no special handling.

## `.res`

```lisp
.res 4   ; four zero-filled bytes
```

Advances the address counter by its (single, constant — same pass-1 folding
rule as `.org`) operand, zero-filled. A negative count signals
`assembly-error`. Since the emulator has no way to skip over a run of bytes
sitting in the middle of the code path (no jump/skip instruction is part of
the core semantics vocabulary), a `.res` run belongs in a data area the
program's control flow doesn't traverse — see
[`examples/directives.lisp`](../examples/directives.lisp).

## Scope: `.macro` is not a directive

`.macro`/`.endm` is deliberately **not** built on `defdirective`. A directive
is a per-statement action with a size the assembler can compute without
evaluating anything; a macro instead captures a *range* of statements and
substitutes parameter tokens into them at each invocation site — there is no
action form in `defdirective`'s vocabulary that could express "collect
everything up to the matching `.endm`". It's tracked as its own follow-up
ticket: a statement-expansion pass living inside `assemble-statements`
([Assembler](assembler.md)) so both of its entry points (`assemble` and a
caller already holding a parsed `statement` list) get it.

## Conditions

- `assembly-error` — wrong operand count for a directive's declared arity, a
  non-constant `.org`/`.res` operand (a label reference, since pass 1 has no
  symbol table), a backward-moving `.org`, or a negative `.res` count.
- `unresolved-label` — a `.byte`/`.word` operand referencing a label never
  bound anywhere in the program (from `eval-expr` in pass 2, same as an
  instruction operand).

## Follow-ups

- `.macro`/`.endm` (above).
- `.ascii`/`.asciz` — needs a string node in the expression parser
  (`parse-expression`, [Statement grammar & expression parser](parser.md)),
  which has none today.
- `.equ` / symbol assignment — an expression (now that `*`, the location
  counter, is available too — see [Assembler](assembler.md#location-counter))
  bound to a name outside the address-counter sequence.

See the tracker for these.
