# Directives

Directives change layout, emit data, or bind symbols during assembly.
Conditional assembly, macros, includes, and assertions have separate
references.

```asm
.org $8000
.equ size, 4
.byte 1, 2, 3
.res size
```

## `defdirective`

```lisp
(defdirective name params action-form)
```

`name` is matched without case. Parameters take one value, a name and
value, or `(&rest values)` for a variable count. Each directive declares
one of these actions:

| Action | Effect |
| --- | --- |
| `(set-origin! address)` | Move the address counter. |
| `(select-bank! n)` | Select a bank for later banked output. |
| `(reserve count)` | Advance by zero-filled cells. |
| `(emit width values [:endian order] [:terminator cell])` | Write values, each `width` cells wide; `:terminator` follows each string operand.[^emit] |
| `(assign name value)` | Bind a constant without using an address. |
| `(reassign name value)` | Create or update an assignment. |

An action names its directive parameters. `find-directive-descriptor`
returns a descriptor or `nil`. See [Assembler](assembler.md#layout-and-encode).

## `.org`

A leading `.org` sets the assembly origin. Later `.org` directives move
forward and zero-fill gaps; moving backward signals `assembly-error`.
A label on the same line binds to the new address:

```asm
here: .org $8000
```

The operand can use earlier assignments or labels when its address
dependencies are acyclic. See [Assembler](assembler.md#convergence).

## `.byte` / `.word` / `.long`

| Directive | Width of each value | Example |
| --- | --- | --- |
| `.byte` | One memory cell | `.byte 1, 2, 3` |
| `.word` | Two memory cells | `.word $1234` |
| `.long` | Four memory cells | `.long $12345678` |

All accept zero or more expressions, or [string operands](#ascii--asciz).
Values are resolved during encoding, so forward labels work. Fields follow the machine's byte order unless a
custom `emit` action sets `:endian`; out-of-range values wrap.
On a machine with 16-bit cells, `.byte` means one 16-bit cell and `.word`
means two. See [Machine model](machine-model.md#cell-width-and-the-assembler).

## `.cell` / `.dat`

These are one-cell aliases for `.byte`, useful when cells are wider than
8 bits. They use the same range and byte-order rules.

## `.ascii` / `.asciz`

A string operand emits one element per character. `.asciz` adds a `0`
after each string; numbers get no terminator:

```asm
.ascii "hi", 13, 10   ; 68 69 0D 0A
.asciz "a", "b"       ; 61 00 62 00
```

Every `emit` directive takes strings, so `.byte "A", 0` works too.
Each character is one element: one cell for `.ascii`, `.asciz`, `.byte`,
and `.cell`, so a 16-bit-cell machine holds code points up to `$FFFF`.
A character too wide for its element signals `assembly-error`. A string is
valid only as a top-level data operand; anywhere else (`.org "a"`,
`lda #"a"`, `"a" + 1`) it signals `assembly-error`. Escapes are listed in
[Lexer](lexer.md#string-escapes).

## `.res`

`.res count` reserves `count` zero-filled cells. The count must be
nonnegative and resolvable during layout.

## `.bank`

`.bank n` selects a bank for output in a banked region. See
[Banked output](banked-output.md).

## `.equ`

`.equ name, value` binds an identifier without occupying an address.
`name = value` is equivalent source syntax. The value must resolve from
names already defined when layout reaches it; rebinding a name signals
`assembly-error`.

```asm
.equ a, 1
.equ b, a + 1
start: nop
.equ size, * - start
```

A local assignment follows the same scope rule as a local label. An
address-dependent `.equ` can feed a later `.org` or `.res` when the layout
is acyclic. See [Assembler](assembler.md#equ--symbol-assignment).

## `.set`

`.set` creates or updates an assignment, but cannot replace a label or
register alias. Each instruction or data operand captures the value in
effect at its own source position:

```asm
.set count, 2
.byte count
.set count, count + 1
.byte count
```

The final value appears in `assembly-symbols`. A use before the first
assignment signals `assembly-error`.

## Conditions

A malformed `defdirective` signals `directive-definition-error`.
`assembly-error` covers invalid arity, symbols, values, addresses, or
layout dependencies. `unresolved-label` covers a data expression naming
a label absent from the completed program. See [Diagnostics](diagnostics.md).

## Limitations

- Undecodable data renders as `.byte` even on word-addressed machines;
  disassembly does not select `.cell` or `.dat` for those lines, and does
  not render printable runs as `.ascii`
  ([#263](https://todo.sr.ht/~takeiteasy/lasm/263)).

See the [issue tracker](https://todo.sr.ht/~takeiteasy/lasm) for planned
work on these limits.

[^emit]: `:terminator` takes a non-negative integer. `:endian` and
    `:terminator` may each appear once, in either order.
