# Assembler

`assemble` parses source, resolves symbols, chooses instruction variants, and
returns encoded memory cells.

```lisp
(assemble "start: ldx #10
        bne start" :lexer 'sixtyfoo-syntax :machine 'sixtyfoo)
```

For runnable programs, see
[`counter.lisp`](../examples/counter.lisp),
[`modes.lisp`](../examples/modes.lisp), and
[`directives.lisp`](../examples/directives.lisp).

## `assemble` / `assemble-statements`

```lisp
(assemble SOURCE &key machine (lexer 'default) (origin 0) memory file)
(assemble-statements STATEMENTS &key machine (lexer 'default) (origin 0) memory source)
(assemble-file PATH &key machine (lexer 'default) (origin 0) memory)
```

| Entry point | Input | Use |
| --- | --- | --- |
| `assemble` | Source text | Parse and assemble; `:file` names the source in diagnostics. |
| `assemble-statements` | Parsed statements | Assemble a statement list; `:source` retains its source text. |
| `assemble-file` | `.asm` or `.s` path | Read and assemble a file; an unreadable path signals `file-error`. |

All three return an `assembly`:

| Slot | Contents |
| --- | --- |
| `cells` | Encoded cells; gaps from `.org` and `.res` contain zeroes. |
| `cell-width` | Bits per cell, taken from the target memory. |
| `origin` | Address of the first cell, including a leading `.org`. |
| `banks` | Bank images; see [Banked output](banked-output.md). |
| `symbols` | Final name-to-value table for labels and assignments. |
| `symbol-info` | Symbol kind and scope; see [Listing](listing.md#symbol-table). |
| `listing`, `source` | Address mapping and source text; see [Listing](listing.md). |

### `assembly`'s cell width

Addresses count memory **cells**, not bytes. With `:cell-width 16`, address
`2` means two 16-bit cells from the origin. The assembler uses the target
memory's cell width and byte order for instructions and directive data.[^memory]

Specify `:memory` when the machine has memory elements with different cell
widths or byte orders. `load-program` checks the assembly width against its
target memory; see [Emulator](emulator.md#load-program).

## Preprocessing

Before layout, the assembler expands [includes](includes.md),
[macros](macros.md), and [conditional assembly](conditionals.md). Only the
resulting statements reach layout.

## Layout and encode

1. **Layout:** Assign addresses to labels, select instruction variants, and
   apply directives. Repeat until sizes and symbol values settle.
2. **Encode:** Evaluate operands against the settled symbols and emit cells.

A forward label works because later layout passes have the earlier pass's
symbol table. Assignment values from `.set` are captured where they appear
in source.[^layout]

### Choosing a mode

When several variants match an operand, the assembler chooses in this order:

| Step | Rule |
| --- | --- |
| Syntax | Keep matching patterns and `one-of` alternatives. |
| Specificity | Prefer more literal tokens, then register-qualified holes. |
| Size floor | Never choose a smaller variant after a statement widens. |
| Value | Choose the first variant whose operands fit. |
| Unknown value | Start with the narrowest eligible variant. |
| No fitting value | Use the widest eligible variant and wrap, unless the selected `choice` requires an error. |

Declare a narrow mode before a wider mode with the same syntax. Equal-width,
equally specific modes produce `ambiguous-mode`; tied `one-of` alternatives
produce `ambiguous-alternative`.[^selection]

```lisp
lda $10       ; zero-page
lda $1000     ; absolute
lda target    ; starts narrow; widens if target does not fit
```

An out-of-range ordinary operand wraps by default. Use `:strict t` on a
mode or `one-of` alternative, or bind `*strict-operand-range*`, to make it
an error. Relative offsets always signal an error on overflow; see
[Diagnostics](diagnostics.md#strict-operand-range).

#### Forcing a mode with a mnemonic suffix

A suffix such as `lda.w` keeps only that mode's variants before selection.
An unknown suffix, a mode the instruction does not use, or mismatched
operand syntax signals `assembly-error`. A forced ordinary mode still wraps
an out-of-range value by default; a forced relative mode checks its range.
See [Addressing modes](modes.md#forcing-a-mode-with-a-mnemonic-suffix).

#### Forcing one hole with a prefix

A prefix such as `seta #w:5` selects one word-field choice for that operand.
An unknown prefix or an out-of-range forced inline value signals
`assembly-error`. See [Addressing modes](modes.md#forcing-one-hole-with-a-prefix).

### Convergence

Each layout pass may widen an instruction, but never shrink it. The assembler
also compares directive effects and symbol values, then runs final checks
once they settle. Cyclic address dependencies and a layout that exceeds its
iteration bound signal `assembly-error`.[^convergence]

## Multi-operand instructions

A mode can contain several expression holes. Each hole maps to an encoding
field, and the assembler evaluates and writes them in hole order:

```lisp
mov $20, target
target: nop
```

See [Addressing modes](operand-modes.md) and
[Instructions](instructions.md).

## Word-encoded instructions

On a machine with `instruction-word`, the assembler packs opcode and inline
fields into an instruction word, then appends any extra words. Variants with
inline values are tried before variants needing extra cells. Each field uses
its declared range or width; relative fields use the same next-instruction
base as other encodings.[^word]

See [Word-encoded instructions](word-instructions.md) and
[`word.lisp`](../examples/word.lisp).

## PC-relative offsets

A relative operand names a target address. The encoded value is the signed
offset from the **next** instruction:

```text
offset = target address - (branch address + instruction size)
```

Each relative hole is checked against its own field width. Overflow signals
`assembly-error`; other holes follow their usual range rules. See
[Addressing modes](modes.md#pc-relative-modes).

## `eval-expr`

```lisp
(eval-expr AST &key symbols pc)
(eval-expr-constant AST &key pc)
```

`eval-expr` resolves labels through `symbols` and the location counter
through `:pc`. A missing label signals `unresolved-label`.
`eval-expr-constant` accepts no labels, but can use `:pc` for the location
counter.

### Register aliases

If a name is absent from `symbols`, `eval-expr` tries the machine's register
aliases. A label or assignment cannot reuse an alias name. See
[Machine model](machine-model.md#defmachine).

## Location counter

`*` refers to the address of the current item:

```lisp
bne *        ; branch to this instruction
.org *+16    ; move forward 16 cells
.word *      ; emit this entry's address
```

| Context | Address used |
| --- | --- |
| Instruction operand | Instruction start. |
| `.byte` or `.word` element | That element's address. |
| `.org` or `.res` | Address before the directive changes it. |

Without `:pc`, evaluating `*` signals `unresolved-location`.

## Local-label scoping

A local label belongs to the closest preceding global label. The same local
name can appear in multiple routines:

```lisp
a:
.loop: bne .loop   ; a.loop
b:
.loop: bne .loop   ; b.loop
```

A local name without an enclosing global label signals `assembly-error`.
Use `assembly-symbol` with `:scope` to look up a local name; see
[Listing](listing.md#symbol-table).[^locals]

## `.equ` / symbol assignment

`.equ` binds a name to a value without occupying a cell. Its value must use
symbols already defined when layout reaches it. Reusing a label or `.equ`
name signals `assembly-error`.

```lisp
.equ size, 16
.res size
count = 4       ; equivalent to .equ count, 4
```

Address-dependent assignments can feed a later `.org` or `.res` when their
dependencies are acyclic. See [Directives](directives.md#equ).

## `.set`

`.set name, value` creates or replaces an assignment, but cannot replace a
label. Each instruction or data operand captures the value in effect at its
source position. `assembly-symbols` holds the final value. See
[Directives](directives.md#set).

## `:origin`

`(assemble source :origin #x200)` starts at address `#x200`.
`load-program` uses the assembly's origin by default. A leading `.org` can
move that origin before any cells are emitted. See [Directives](directives.md#org).

## Conditions

| Condition | When it occurs |
| --- | --- |
| `assembly-error` | Duplicate or invalid symbols, mismatched operands, relative overflow, strict range failure, invalid directive use, or layout failure. |
| `include-error`, `macro-error`, `conditional-error` | Invalid preprocessing input. |
| `unknown-instruction` | Unregistered mnemonic. |
| `unresolved-label` | Operand names a label that is never bound. |
| `unresolved-location` | `*` is evaluated without `:pc` outside assembly. |
| `lex-error`, `parse-failure` | Invalid source text passed to `assemble`. |
| `ambiguous-mode`, `ambiguous-alternative` | Warnings for tied selections. |

Syntax errors include source excerpts when source text is available. See
[Diagnostics](diagnostics.md).

[^memory]: The target defaults to the machine's sole memory element, or to
  the shared cell width and byte order of its memory elements. The assembly
  retains `cell-width`; the machine descriptor supplies byte order at decode
  time. Local symbol keys use an internal separator; `assembly-symbol`
  handles scoped lookups.
[^layout]: Directives take precedence over instruction names. Instructions
  occupy their selected encoded size; `.org` and `.res` change the address
  counter. Operands parse once per assembly; every relaxation pass and
  encode reuse the parsed operands, and encode reuses the selected variants
  without repeating mode matching.
[^selection]: Syntax matching runs before value checks. A more specific
  pattern does not fall back to a less specific one when its value overflows.
  Ordinary fields accept unsigned or two's-complement signed values;
  `:signed` fields accept only signed values. Relative fields check the
  computed offset. Word fields use their declared ranges or extra-word
  widths. A `choice`-selected field reports an out-of-range value because
  wrapping could decode as another addressing form.
[^convergence]: Forward references start at their narrowest matching
  variant. Later passes use the previous symbol table and widen as needed.
  Final checks include backward-moving `.org` directives. The iteration
  bound is calculated after preprocessing.
[^word]: The encoded size includes the instruction word's memory cells and
  any extra-word cells. Extra words use each field's declared width and the
  machine's byte order. See [Instructions](instructions.md#operand-pipeline).
[^locals]: Local symbols use qualified keys internally. A label on an
  instruction's own line establishes scope before its operands are resolved.
  `.equ` and `.set` names follow the same scoping rule.
