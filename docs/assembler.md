# Assembler

`assemble` turns a program's source text (or an already-parsed statement
list) into encoded bytes, resolving labels along the way.

```lisp
(assemble "        ldx #10
.loop:  dex
        bne .loop
        sta $1000
        hlt" :lexer 'sixtyfoo-syntax :machine 'sixtyfoo)
```

See [`examples/counter.lisp`](../examples/counter.lisp) for a runnable
version, and [Emulator](emulator.md) for running the result.

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

## Two passes, not M2's two-pass

M1 caps every instruction at one addressing mode and a fixed operand width
(`instruction.lisp`), so an instruction's size never depends on a label's
value. That makes a **layout pass** free:

1. **Layout.** Walk the statements with an address counter starting at
   `:origin`. Each `statement-label` binds to the current address; each
   `statement-mnemonic` looks up its `instruction-descriptor` and advances
   the counter by `1 + operand-width`. This is what resolves *forward*
   references (`jmp end` before `end:` appears) — a caller doesn't have to
   write labels before their uses.
2. **Encode.** Walk again, now with the complete symbol table: match each
   operand against its instruction's mode (`match-operand-mode`), evaluate
   it (`eval-expr`, below) against the symbol table, and encode
   (`encode-instruction`).

This is **not** the two-pass assembly M2 defers (`LASM-plan.md` §2). M2's
two-pass exists because multi-mode resolution (e.g. choosing zero-page vs.
absolute) needs a label's *value* before the instruction's *size* is known
— a genuine chicken-and-egg problem M1 never has, since it has only one
mode per instruction.

## `eval-expr`

`instruction.lisp`'s `eval-expr-constant` folds an expression AST with no
label support at all (any `expr-label` signals `unresolved-label`). The
assembler needs the same folding logic once labels *are* known, so
`eval-expr-constant` is now defined in terms of a more general function:

```lisp
(eval-expr AST &key symbols)     ; symbols: string -> address hash table
(eval-expr-constant AST) = (eval-expr AST :symbols nil)
```

`eval-expr` looks an `expr-label`'s name up in `symbols` and signals
`unresolved-label` only on a miss (or when `symbols` is `nil`, matching the
old no-labels-ever behavior). Existing callers of `eval-expr-constant`
(tests, `examples/counter.lisp`'s hand-encoding path) are unaffected.

## `:origin`

`(assemble source :origin #x200)` starts layout at `#x200` instead of `0` —
every label and byte address is computed against that base. The resulting
`assembly`'s `origin` slot carries this forward so `load-program` (see
[Emulator](emulator.md)) places the bytes at the same address the labels
were computed against; passing mismatched origins to `assemble` and
`load-program` is how labels silently end up pointing at the wrong place, so
`load-program` defaults to the assembly's own origin rather than requiring
it be repeated.

## Local labels are flat in M1

A local label (lexer convention: a name starting with a non-alphanumeric
prefix, e.g. `.loop`) is **not** scoped to an enclosing global label here —
it shares one flat symbol table with every other name. Two different
routines both using `.loop` as their loop-back label will collide as a
duplicate-label error. Binding a local label to its nearest preceding global
label is M2 (#16).

## Conditions

- `assembly-error` (a subtype of `lasm-syntax-error`) — a duplicate label,
  or an operand count that doesn't match the instruction's declared
  addressing mode (a mode with zero operands, no mode with an operand, or
  more than one operand — M1 has no multi-operand instructions).
- `unknown-instruction` — an unregistered mnemonic (from `find-instruction`,
  see [Instructions](instructions.md)).
- `unresolved-label` — an operand references a label never bound anywhere in
  the program (from `eval-expr`).
- `lex-error` / `parse-failure` — from the front end (`assemble` only).

## Scope

This produces bytes and a symbol table from a statement list. It does not
cover directives (`.org`, `.byte`/`.word` — M2), macros, or a listing /
source-map output tying addresses back to source lines (a follow-up
ticket).
