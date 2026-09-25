# Disassembler

The disassembler decodes cells using the machine's instruction definitions.
It can render source for reassembly or a listing for inspection.

```lisp
(let* ((a (assemble source :machine 'sixtyfoo))
       (lines (disassemble-assembly a :machine 'sixtyfoo)))
  (print-disassembly lines))
```

See [`disasm.lisp`](../examples/disasm.lisp).

## `decode-instruction-at`

```lisp
(decode-instruction-at read-cell address machine-name &key memory)
;; => (values descriptor values size choices selections)
;; or (values :decode-failure nil nil)
```

This operation reads and decodes without changing PC or executing
semantics. `size` is the number of cells actually consumed, including any
extra words. A read error, including a truncated trailing value, propagates
to the caller.

| Reader | Source |
| --- | --- |
| `machine-cell-reader` | Live memory through `mref`. |
| `machine-peek-reader` | Live memory through `mpeek`, without device read effects. |
| `vector-cell-reader` | A cell sequence with `:origin` and optional `:end`. |

The emulator uses the same decoder; see [Emulator](emulator.md#step-machine).

## `disassemble-cells` / `disassemble-assembly` / `disassemble-memory`

```lisp
(disassemble-cells cells &key machine (origin 0) end symbols symbol-info
                              (lexer 'default) (labels t) (suffixes t)
                              memory data-regions)
(disassemble-assembly assembly &key machine (lexer 'default) (labels t)
                                    (suffixes t) memory (data-regions :auto)
                                    bank region)
(disassemble-memory machine &key memory start count symbols symbol-info
                                 (lexer 'default) (labels t) (suffixes t)
                                 assembly (data-regions :auto))
```

| Entry point | Input | Range |
| --- | --- | --- |
| `disassemble-cells` | Cell sequence | `origin` to `end`, exclusive. |
| `disassemble-assembly` | Assembly and its symbols | Assembly cells; detects declared data automatically. |
| `disassemble-memory` | Live machine, optional assembly | Required `start` and `count`; with `:assembly`, detects declared data automatically. |

All return `disassembly-line` values in address order. Assembly cell width
must match the selected machine memory. `:bank n` selects a bank image;
`:bank :all` includes the main image and every bank. See
[Banked output](banked-output.md#disassembly).

### `disassembly-line`

| Field | Meaning |
| --- | --- |
| `address`, `size`, `cells`, `cell-width` | Source address and raw encoded cells. |
| `descriptor`, `values` | Matched instruction and decoded operands, or `nil` for data. |
| `label`, `text` | Rendered label and source text. |
| `region`, `bank` | Bank location, or `nil` for the main image. |

### Decode failure, mid-stream

An undecodable or truncated instruction becomes a one-cell `.byte` data
line. Decoding continues at the next cell. This preserves the cells for
reassembly without guessing the width of invalid data.

### Data regions

`:data-regions` contains `(start . end)` ranges with exclusive ends.
Cells inside them render as data, even if their bits match an
instruction. Overlapping and adjacent ranges merge. An instruction cannot
cross a data boundary.

| Machine | Region renders as |
| --- | --- |
| Byte-encoded | `.byte` per cell |
| Word-encoded, two-cell instruction word | `.word` per cell pair, else `.byte` |
| Word-encoded, other word width | `.byte` per cell[^wide] |

Pairs start at the region's first cell and read in the memory's endian
order, so the text reassembles to the same cells. The region is first
clipped to the disassembled range. An odd length after merging and clipping
stays `.byte`; `.byte 1,2,3` followed by `.word 4` is one five-cell region.

```text
.word $1234
.word $BEEF
```

`disassemble-assembly` uses [assembly data regions](listing.md#assembly-data-regions)
by default. `disassemble-memory` does the same when given `:assembly`, and
also takes its labels from it. Inside a banked region, the mapped bank's
regions and labels apply when the assembly has an image for it. The main
image's apply there only if `load-program` wrote it into that bank. Pass `nil` to decode everything or a range list to
override. Without `:assembly`, no regions apply.

### Rendering

Numbers use the selected lexer's hex prefix, or decimal when none is
defined. Literal tokens come from the mode pattern, so `immediate` renders
`#$10` and `indexed-x` renders `$10,X`. Negative values render in decimal.

When the encoding records a `one-of` selection, the disassembler renders
that alternative's syntax. A word `choice` field or a cell sub-opcode can
supply this record. Named zero-hole selections use their named slot.
Aliases render with their canonical spelling. See
[Per-operand modes](operand-modes.md#encoding-a-selection).

### Register-index operand rendering

A field declared with `:register ELEM` renders its decoded index using
`ELEM`'s alias. Alias takes precedence over a label at the same numeric
value; an index without an alias renders as a number. See
[Instructions](instructions.md#operand-registers).

### Relative operand rendering

A relative field contains an offset from the next instruction. Source
renders the absolute target: `address + size + offset`.

With `symbol-info`, `:labels t` substitutes actual label names and excludes
`.equ` values. With only a plain symbol table, substitution is restricted
to decoded line starts. `:suffixes t` keeps forced mode suffixes and hole
prefixes when they are needed to reproduce an encoding.[^rendering]

## `disassembly-text` / `print-disassembly`

```lisp
(disassembly-text lines &key stream origin (indent "        "))
(print-disassembly lines &key (stream *standard-output*) origin)
```

`disassembly-text` returns reassemblable source or writes it to `stream`.
It emits `.org` for a nonzero origin and `.bank`/`.org` for bank images.
`print-disassembly` writes an address/cells/text listing for people.
A local and global label with the same visible spelling are rendered
without an ambiguous local substitution.

## Limitations

- Without an encoded selector, a `one-of` hole renders its first
  alternative. The original syntax cannot be recovered.
- An extra-word encoding without a forcing suffix may reassemble in a
  shorter inline form. Forward references can also settle at a different
  width on a new layout pass.
- Comments, macros, original number formatting, and undeclared code/data
  boundaries do not survive encoding. Declare data regions when needed.
- Source name recovery requires the corresponding symbol information;
  encoded cells alone do not carry `.equ` or label names.

[^rendering]: A relative value is adjusted before label substitution.
  Several labels at one address resolve alphabetically. When a local and
  global label have the same visible spelling, the local reference renders
  as a number to keep the text unambiguous. An alias for an encoded choice
  decodes using the canonical alternative.

[^wide]: A word wider than two cells has no single directive to render as
    ([ticket 269](https://todo.sr.ht/~takeiteasy/lasm/269)).
