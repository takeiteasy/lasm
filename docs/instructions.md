# Instructions

`definstruction` connects an [addressing mode](modes.md), an encoding, and
[semantics](semantics.md) for a machine instruction.

```lisp
(definstruction sixtyfoo ldx
  (modes immediate)
  (encoding (opcode #xA2) (operand :mode))
  (semantics (set! x operand)))
```

See [`counter.lisp`](../examples/counter.lisp) for a runnable instruction
set and [`modes.lisp`](../examples/modes.lisp) for multiple modes.

## `definstruction`

```lisp
(definstruction MACHINE NAME
  [(modes MODE)]
  (encoding (opcode n) [(operand [NAME] :mode | :width n)]*)
  (semantics form...)
  [(cycles n)])
```

`MACHINE` and `MODE` must already be declared. An instruction with no
`(modes ...)` accepts no operand. Each top-level clause appears at most
once. Invalid clauses and missing required fields signal
`instruction-definition-error` during macroexpansion.[^registration]

| Clause | Purpose |
| --- | --- |
| `(modes MODE)` | One addressing mode; omit for a no-operand instruction. |
| `(encoding ...)` | Opcode and one operand field per expression hole. |
| `(semantics ...)` | Effect when the emulator executes the instruction. |
| `(cycles n)` | Nonnegative cycle cost; defaults to `1`. |

### `(modes MODE)` — sugar for one mode

A single mode uses the shared `(encoding ...)` and `(semantics ...)` clauses.
The built-in modes include `immediate` and `absolute`; see
[Addressing modes](modes.md).

### `(modes (MODE ...) ...)` — several modes, one instruction

Give each mode its own opcode. A mode can override the shared semantics and
cycle cost:

```lisp
(definstruction sixtyfoo lda
  (modes
    (immediate (opcode #xA9) (semantics (set! a operand)))
    (zero-page (opcode #xA5))
    (absolute  (opcode #xAD)))
  (semantics (set! a (mref machine 'ram operand))))
```

`immediate` uses the value directly; the other modes use it as a memory
address. A mode without its own semantics uses the shared body. A mode
without its own `(cycles n)` uses the shared cost or `1`. The shared
`(encoding ...)` clause is unavailable in this form; each mode supplies an
opcode. The [assembler](assembler.md#choosing-a-mode) selects the mode.

### Encoding

For a cell-encoded instruction, the opcode occupies one memory cell.
Each mode expression hole gets one `(operand ...)` subclause, in hole order:

| Form | Field width or effect |
| --- | --- |
| `(operand :mode)` | Mode width, or the target memory's address width rounded to cells. |
| `(operand :width n)` | Explicit width in cells. |
| `(operand NAME :mode)` | Mode width and a `NAME` binding in semantics. |
| `(operand NAME :width n)` | Explicit width and a `NAME` binding. |
| `:register ELEM` | Render this register index using `ELEM`'s declared aliases. |

The mode or instruction must give a width when memory selection makes the
address-width default ambiguous. `:register` requires a banked register
with `:names` and checks the operand index even when strict range checking
is off. It cannot mark a signed or relative hole. See
[Disassembler](disassembler.md#register-index-operand-rendering).

Fields follow the machine's byte order. Values outside an ordinary field's
range wrap unless strict checking is enabled; see
[Diagnostics](diagnostics.md#strict-operand-range).

### `(opcode n :sub s)` — sub-opcode cell

On a cell-encoded machine, `:sub` inserts a discriminator cell after the
opcode: `[opcode][sub][operand cells...]`. Descriptors sharing an opcode
must each have a distinct sub-opcode. A word-encoded machine uses word
fields for discrimination and rejects `:sub`.

```lisp
(definstruction sixtyfoo lda
  (modes
    (immediate (opcode #x10 :sub 0) (operand :mode)
      (semantics (set! a operand)))
    (absolute (opcode #x10 :sub 1) (operand :mode)
      (semantics (set! a (mref machine 'ram operand))))))
```

See [`subopcode.lisp`](../examples/subopcode.lisp).

### `(variant (choice m) (sub s))` — hole-selected sub-opcode

A `one-of` hole can select the sub-opcode from the alternative its syntax
matches:

```lisp
(defmode direct expr)
(defmode indirect "[" expr "]")
(defmode any (one-of direct indirect))

(definstruction sixtyfoo lda
  (modes any)
  (encoding (opcode #x10)
    (operand src :width 1
      (variant (choice direct) (sub 0))
      (variant (choice indirect) (sub 1))))
  (semantics
    (choice-case src
      (direct (set! a src))
      (indirect (set! a (mref machine 'ram src))))))
```

`lda 5` and `lda [5]` share an opcode but use different sub-opcodes. Every
alternative must have one unique `sub` value. Only one hole can select the
cell this way; use a [sub-opcode table](#sub-opcode-table)
when several holes participate. See [`subchoice.lisp`](../examples/subchoice.lisp).

### Sub-opcode table

A table selects one sub-opcode for a combination of `one-of` holes:

```lisp
(sub-opcode
  (variant (choice direct direct)     (sub 0))
  (variant (choice direct indirect)   (sub 1))
  (variant (choice indirect direct)   (sub 2))
  (variant (choice indirect indirect) (sub 3)))
```

Each `choice` lists participating holes in pattern order. Cover every
combination exactly once, with distinct sub-opcodes that fit the cell.
A table cannot share its encoding with an explicit `:sub` or a per-hole
sub-opcode selector. See [`subtable.lisp`](../examples/subtable.lisp).

#### Table holes

`(holes 0 2)` makes holes 0 and 2 participate; indices are zero based.
Unlisted `one-of` holes must have alternatives that agree on width,
signedness, and relative behavior. Their alternative is not recorded for
decode, so disassembly renders the first alternative.[^subcodes]

### `operand-signedness`

A cell-encoded descriptor records signedness and encoded width for each
hole. When selected `one-of` alternatives differ, the descriptor for each
sub-opcode records that alternative's values. See
[Addressing modes](operand-modes.md#per-hole-attributes).

### `operand-registers`

`operand-registers` records the `:register` element for each hole, or
`nil` where none is declared. The disassembler uses it to render register
aliases.

### `relative-holes`

`relative-holes` marks operands encoded as signed offsets from the next
instruction. Several holes can be relative. See
[Assembler](assembler.md#pc-relative-offsets).

### Repeated `(operand ...)` subclauses — multi-operand instructions

A mode with several expression holes needs one field per hole. Named fields
are available in semantics:

```lisp
(defmode reg-reg expr "," expr)

(definstruction sixtyfoo mov
  (modes reg-reg)
  (encoding (opcode #x40)
    (operand dst :width 1)
    (operand src :width 1))
  (semantics (setf (mref machine 'regs dst)
                   (mref machine 'regs src))))
```

`mov 2, 3` encodes as `#x40 #x02 #x03`. A multi-mode variant with one
hole may omit its operand subclause and use the default width; several
holes require explicit fields.

### `(semantics form...)`

Semantics can use `machine`, `operand` (the first decoded value, or `nil`),
and every named operand. Scalar registers and flags are bound as in
`with-machine`; operators such as `set!`, `push`, `pop`, and `trap` are
available. See [Semantics vocabulary](semantics.md).

### `(cycles n)`

`n` is a nonnegative integer; `0` is allowed. A mode-specific cost overrides
the shared cost. The emulator adds the cost to `machine-cycles` on each
execution; see [Emulator](emulator.md#cycle-costs-and-clock-speed).

## PC is a plain register

Declare `pc` as an ordinary register. The emulator advances it past the
current instruction before running semantics, so a relative branch adds its
offset to `pc`:

```lisp
(definstruction sixtyfoo bne
  (modes relative)
  (encoding (opcode #xD0) (operand :mode))
  (semantics (when (zerop z) (set! pc (+ pc operand)))))
```

An absolute jump assigns its target to `pc`. See
[Emulator](emulator.md#step-machine).

## `with-machine-bindings`

`(with-machine-bindings (machine-var machine-name) body...)` binds the same
storage and operators as `with-machine` around an existing machine instance.
Instruction semantics use it for the emulator's machine; `with-machine`
creates a new instance. See [Semantics vocabulary](semantics.md).

## Registration: a mnemonic is a list of variants

`find-instruction-variants` returns a mnemonic's descriptors.
`find-instruction` returns one descriptor and accepts `:mode`; without it,
the first declared variant is returned. Redefining a mnemonic replaces its
variant list and opcode registrations.

### Opcode to descriptor decode

`find-instruction-by-opcode` returns one descriptor;
`find-instruction-descriptors-by-opcode` returns all descriptors for an
opcode. On a cell-encoded machine, shared opcodes require distinct
sub-opcodes. On a word-encoded machine, descriptors sharing an opcode must
accept distinguishable bit patterns. Otherwise registration signals
`opcode-conflict`. A declared [`(fallback)`](word-instructions.md#fallback-instructions)
can accept the words left by more specific descriptors.[^decode]

## Operand pipeline

| Operation | Result |
| --- | --- |
| `eval-expr-constant` | Fold an expression without label lookup. |
| `eval-expr` | Fold with a symbol table; see [Assembler](assembler.md#eval-expr). |
| `encode-instruction` | Encode one descriptor and a list of operand values into cells. |
| `instruction-descriptor-size` | Count the cells for a descriptor. |
| `execute-instruction` | Run semantics on a machine with decoded values. |

`encode-instruction` accepts `:memory` when the machine has memory elements
with different cell widths or byte orders.[^pipeline]

## Word-encoded instructions

Word-encoded machines pack opcode and operand fields into instruction words.
See [Word-encoded instructions](word-instructions.md) for field variants,
layouts, fallbacks, and varying alternatives.

## Note on operand binding

`operand` holds the decoded integer. Semantics explicitly reads memory when
the integer is an address, for example `(mref machine 'ram operand)`. A
multi-mode instruction can therefore use one semantics body for address
modes and another for an immediate value.

## Scope

The [assembler](assembler.md) selects variants and resolves labels. The
[emulator](emulator.md) fetches instructions and advances `pc`. Banked
registers bind as `(NAME idx)` in semantics; see
[Semantics vocabulary](semantics.md).

[^registration]: `definstruction` registers descriptors during compilation
  and loading. A word-encoded instruction compiles its semantics on first
  execution, then shares that function among its variants.
[^subcodes]: The `holes` indices are interpreted in pattern order even if
  listed out of order. Selected alternatives supply descriptor-specific
  signedness, width, and relative flags. Unselected alternatives have no
  decode-time identity.
[^decode]: Word descriptors are checked against one another over the bits
  their fields share, including pinned values and selected layouts. Decode
  uses a per-machine table for words up to 16 bits and scans candidates for
  wider words. It fetches trailing values for each decode, including after
  self-modifying code.
[^pipeline]: On a cell-encoded machine, encoding emits an opcode cell and
  each operand's cells in machine byte order. On a word-encoded machine, it
  emits the packed instruction word and any extra cells. The size operation
  counts both parts.
