# Disassembler

Recovers source text from already-encoded cells, driven off the same
`definstruction` specs used for assembly (see [Instructions](instructions.md))
and the same pure decode step [the emulator](emulator.md) uses to fetch
instructions — so decoded output can never disagree with what would actually
execute.

```lisp
(let* ((a (assemble source :machine 'sixtyfoo))
       (lines (disassemble-assembly a :machine 'sixtyfoo)))
  (print-disassembly lines))
```

See [`examples/disasm.lisp`](../examples/disasm.lisp) for a runnable version,
on both a byte-encoded and a word-encoded (DCPU-16-shaped) machine.

## `decode-instruction-at`

```lisp
(decode-instruction-at read-cell address machine-name &key memory)
;; => (values descriptor values size) | (values :decode-failure nil nil)
```

The pure fetch/decode step shared by [the emulator's `step-machine`](emulator.md#step-machine)
and this file's `disassemble-*` functions — it reads cells, matches an
opcode, and decodes operand values, but never writes a PC register and never
executes anything. `read-cell` is a closure of one argument (an address)
returning that cell's unsigned integer value; `machine-cell-reader` and
`vector-cell-reader` build one over the two sources callers actually have:

```lisp
(machine-cell-reader machine memory)          ; reads via mref
(vector-cell-reader cells &key (origin 0) end) ; reads a bare cell sequence
```

`vector-cell-reader` signals `address-out-of-range` outside `[origin, end)`
— the same condition `mref` signals off the end of live memory — so a caller
handling decode failure at the edges of a buffer can use one `handler-case`
for both cell sources.

`size`, the third return value, is the instruction's width in cells,
accumulated during decode rather than taken from
`instruction-descriptor-size`: on a word-encoded machine, the opcode table's
entry for a given opcode is whichever sibling combo
`register-instruction-variants!` registered last — always the combo with the
*most* extra words (see [Instructions, "Word-encoded
instructions"](instructions.md#word-encoded-instructions-20)) — so trusting
its own `instruction-descriptor-size` would overstate the size of a narrower
encoding genuinely present in the stream.

A condition raised by `read-cell` itself (e.g. `address-out-of-range` past
the end of a buffer) propagates out of `decode-instruction-at` rather than
being caught — whether a truncated trailing instruction is a stop condition
or a decodable-as-data byte is the caller's own policy.

## `disassemble-cells` / `disassemble-assembly` / `disassemble-memory`

```lisp
(disassemble-cells cells &key machine (origin 0) end symbols symbol-info
                              (lexer 'default) (labels t) (suffixes t) memory)
(disassemble-assembly assembly &key machine (lexer 'default) (labels t) (suffixes t) memory)
(disassemble-memory machine &key memory start count symbols symbol-info
                                 (lexer 'default) (labels t) (suffixes t))
```

All three return a list of `disassembly-line`, ascending by address.
`disassemble-cells` is the actual driver: it walks a bare sequence of cells
(e.g. an `assembly`'s own `assembly-cells`) from `origin` through `end`
(exclusive, default the whole sequence). `disassemble-assembly` is the
natural way to round-trip `assemble`'s own output — it pulls `cells`,
`origin`, `symbols`, and `symbol-info` (#37) off an `assembly` directly, and
signals if the assembly's own cell width doesn't match `machine`'s declared
one (mirroring [`load-program`'s own check](emulator.md#load-program)).
`disassemble-memory` reads a live `machine`'s memory instead; unlike
`disassemble-cells`' `end`, `start` and `count` are both **required** —
there is no sane default for "the whole address space" of a live machine.
`symbol-info`, when given (e.g. from the `assembly` that produced this
memory's contents), gets the same fix described under "Rendering" below.

`machine` is a machine name (a symbol), same convention as `assemble`'s own
`:machine`; `disassemble-memory`'s first argument, `machine`, is instead a
`machine` runtime instance, same convention as `step-machine`.

### `disassembly-line`

| Field | Meaning |
|---|---|
| `address` | This line's starting address. |
| `size` | Cells consumed, always ≥ 1. |
| `cells` | The raw cells consumed, in address order. |
| `descriptor` | The matched `instruction-descriptor`, or `nil` for an undecodable data line. |
| `values` | Decoded operand values, in the mode's hole order. |
| `label` | A symbol name bound to this line's own address (only when `:symbols`/`:symbol-info` names it and `:labels` is true), or `nil`. |
| `text` | The rendered source line, not including its label. |

### Decode failure, mid-stream

Rather than stopping at the first undecodable cell — which would make the
disassembler useless on any program with a trailing `.byte` data table, the
common case rather than the corner case — a cell that doesn't decode (an
unregistered opcode, an unmatched word-encoded field, or a genuinely
truncated trailing instruction) becomes a one-cell data line
(`.byte $XX`), and the walk advances by exactly one cell and continues. This
is deliberately uniform across both failure shapes and both encoding
schemes: a word machine's unmatched-field failure could in principle be
consolidated into one `width-cells`-wide `.word` line (the whole word was
already read successfully to reach that failure), but a genuinely truncated
buffer cannot safely assume `width-cells` more cells are readable, and one
fallback path is less to get wrong than two. The resulting cells are still
exactly re-assemblable, just as several `.byte` lines rather than one
`.word` line.

### Rendering

Numbers render in hex, using the chosen `lexer`'s own `:hex` number-format
prefix (`"$"` for the `default` lexer, see [Lexer](lexer.md)) — falling back
to decimal when the lexer declares no hex format. A negative value renders
in plain decimal instead (unary minus before a hex literal isn't a verified
lexable form). There is no `:radix` argument: the lexer already *is* the
answer to "what syntax does this dialect read," so deriving from it keeps
output re-assemblable by construction.

An operand's text is built by walking its mode's `pattern`
([Addressing modes](modes.md)) in declaration order — each literal token
emitted verbatim, each `expr` hole consuming the next decoded value — with
no separator inserted between elements, since a mode's own literals already
carry any punctuation: `immediate` renders `#$10`, `indexed-x` renders
`$10,X`, `indirect-y` renders `($10),Y`. Values are paired to holes strictly
by position, never by `instruction-descriptor-operand-names` — an unnamed
field's entry there is `nil`.

A `relative` mode's decoded value is a signed offset from the address of the
*next* instruction (the same convention `assemble`'s own
[PC-relative offset](assembler.md#pc-relative-offsets) computation uses); it
renders as the absolute target address, `address + size + value` — what a
`relative` mode's own bare-`expr` operand syntax expects to see on
re-assembly, not merely a convenience.

`:labels t` (the default) substitutes a symbol name for an operand value.
With `symbol-info` (#37, e.g. via `disassemble-assembly`, which passes it
automatically) this substitutes any real label, whether or not its address
happens to start a decoded line, and never an `.equ`'s folded value — the
`:label`/`:equ` tag on each `symbol-info` entry (see [Listing and source
map](listing.md#symbol-table)) settles the question directly, at the
source. Without `symbol-info` (a bare `symbols` table, or none), the
discriminator doesn't exist, and this falls back to the pre-#37 mitigation:
substitute a value only when it is the address of another line's *start* in
the same disassembly, since `assembly-symbols` alone cannot distinguish a
label's address from an `.equ`'s folded value. That fallback avoids the
worst case (an `.equ` colliding with an unrelated instruction address) but
still cannot render a real label whose address isn't itself a decoded
line's start — `symbol-info` fixes both. Several names bound to one address
(unusual, but not prevented by the assembler) break ties alphabetically for
a deterministic choice.

`:suffixes t` (the default) renders a gas-style forced mode suffix
([Addressing modes, "Forcing a mode with a mnemonic suffix"](modes.md#forcing-a-mode-with-a-mnemonic-suffix)), e.g.
`lda.w $10`, whenever the mnemonic has more than one registered variant and
the lexer declares a `mode-suffix-separator` — needed for fidelity, since
`lda.w $10` and `lda $10` can assemble to a different number of cells.
`:suffixes nil` renders the bare mnemonic instead.

## `disassembly-text` / `print-disassembly`

```lisp
(disassembly-text lines &key stream origin (indent "        "))
(print-disassembly lines &key (stream *standard-output*) origin)
```

`disassembly-text` renders `lines` as re-assemblable source: an leading
`.org <origin>` when `origin` is given and non-zero, each line's own label
on its own line immediately before it, and each line's rendered text
indented. Returns a string when `stream` is `nil` (the default); otherwise
writes to `stream`. `print-disassembly` instead renders an
address/cells/text listing for humans — *not* re-assemblable source.

## Round-trip fidelity — the honest scope

`assemble` → `disassemble-*` → `assemble` reproduces identical cells when the
input came from `assemble` on the same machine, with `:labels nil` and
`:suffixes t`. It does **not** generally hold for:

- **A word-encoded machine's bytes that spent an extra word on a value that
  would fit inline.** The assembler's own mode selection always picks the
  narrowest encoding a value fits (see [Assembler, "Choosing a
  mode"](assembler.md#choosing-a-mode)), and there is no forced-variant syntax for a
  word machine's inline-vs-extra-word choice — unlike a byte machine's mode
  `:suffix` (`lda.w`), nothing in the source syntax can say "use the wider
  encoding." Such bytes round-trip *semantically* (the decoded values are
  correct) but not byte-identically.
- **A program with forward references.** The assembler's mode-relaxation
  floor is monotone across its layout passes, so an original assembly can
  settle on a wider encoding than a from-scratch pass over the final values
  would choose. Re-assembling disassembled text re-runs layout from
  scratch and can legitimately produce fewer cells — this is why the
  round-trip example and tests pass `:labels nil`.
- **Comments, macros, `.equ` names, label names absent from a given
  `:symbols` table, code/data boundaries** (a data region decodes as
  instructions until one fails to decode), **and original number
  radix/formatting.** None of these survive encoding at all, so none of them
  can be recovered by decoding.

## Scope

This covers decoding already-encoded cells back into re-assemblable text and
a human-readable listing. It does not cover:

- Distinguishing genuine code from embedded data ahead of time — a `.byte`
  table decodes as instructions until one fails to decode, which is the
  data-line fallback's whole purpose, but nothing marks a region as data in
  advance.
- Reconstructing an `.equ`'s *name* independent of whether its value
  happens to collide with an instruction address — see "Round-trip
  fidelity" above.
- A forced-variant syntax for word-encoded machines, closing the one
  remaining fidelity gap noted above — tracked as a follow-up.
