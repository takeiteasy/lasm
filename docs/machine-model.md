# Machine model

LASM machine state is a set of named **storage elements**, not a fixed
notion of "registers." Each element has its own width and access discipline,
which is what lets the same `defmachine` form describe register machines,
stack machines, and hybrids.

## `defmachine`

```lisp
(defmachine sixtyfoo
  (register a :width 8)
  (register x :width 8)
  (stack s :width 8 :depth 256)
  (memory ram :width 8 :addr-width 16)
  (flags z n c v))
```

`defmachine` parses its clauses into a `machine-descriptor` and registers it
under `NAME`. Registration happens inside an `eval-when` so the descriptor
is available **at macroexpansion time**, not only after the file is loaded —
this matters once instruction definitions need to resolve storage names and
widths against a machine defined earlier in the same file.

### Clauses

- `(register NAME :width n [:count n] [:names (A B C ...)])` — a
  fixed-width storage cell. `:count n` (n > 1) declares a *banked* register
  (e.g. CHIP8's 16 `V` registers, [`examples/chip8.lisp`](../examples/chip8.lisp)).
  A scalar register (`:count 1`, the default) is read/written by `sref`/
  `(setf sref)` and, inside `with-machine`/instruction semantics, bound
  directly by name (e.g. `a`). A banked register is read/written by
  `regref`/`(setf regref)`, which take a run-time bank index and mask/wrap
  the value to the element's own `:width` exactly like `sref` does; an
  out-of-range index signals `register-index-out-of-range`. `sref` is
  scalar-only and signals `unknown-storage` on a banked element. Inside
  `with-machine`/instruction semantics, a banked register is bound as a
  local macro taking an index, e.g. `(v idx)` reads bank `idx` of `v` and
  `(set! (v idx) n)` writes it — see [Semantics vocabulary](semantics.md).

  `:names` gives each bank cell a symbolic alias, one per cell in index
  order — CHIP8's `V0`–`VF` or DCPU-16's `A B C X Y Z I J`
  ([`examples/dcpu16.lisp`](../examples/dcpu16.lisp)). `:count` defaults to
  `(length names)` when `:names` is given alone; giving both requires them
  to agree. An alias folds like an ordinary symbol in assembly source (so
  `set a, 5` assembles identically to `set 0, 5`), resolved after the
  symbol table on any name it doesn't already bind — a label or `.equ`
  colliding with an alias (case-insensitively) is an assembly error rather
  than a silent shadow. Every alias shares one machine-wide namespace with
  every other storage element name and every other register's aliases.
- `(stack NAME :width n :depth n)` — a fixed-depth LIFO stack of `:width`-bit
  values. Grows upward: the stack pointer starts at 0 and always equals the
  number of live entries, incrementing on push and decrementing on pop.
  Overflow/underflow signal `stack-overflow`/`stack-underflow`; see
  "Conditions" below for how those interact (or currently don't) with `run`.
  Besides the top (via `stack-push`/`stack-pop`), any live entry is readable
  and writable by `stack-ref`/`(setf stack-ref)` — a top-relative, unsigned
  index (`PICK`/`OVER`-style; offset 0 is the top, the same entry
  `stack-pop` would return). This is also what the `stack-relative`
  addressing mode (`n,S`, see [Addressing modes](modes.md)) resolves
  against; the stack pointer itself stays internal bookkeeping either
  way — `stack-depth` is the only way to read it, and there is no `sp`
  storage element. A machine with only a stack (plus PC and memory) is a
  valid, fully expressible machine — see
  [`examples/stack.lisp`](../examples/stack.lisp), the M3 milestone's
  validation that this abstraction isn't secretly register-shaped;
  [`examples/hybrid.lisp`](../examples/hybrid.lisp) is M3's second
  validation case, a stack shared between ordinary data and an implicit
  call stack (`jsr`/`rts` pushing/popping `pc`), reaching an argument
  underneath its own return address via `stack-ref`.
- `(memory NAME :width n :addr-width n [:cell-width n])` — addressable
  storage. `:addr-width` is the number of address bits (so the element has
  `2^addr-width` cells); `:cell-width` is the bit width of each cell and
  defaults to `:width` (byte-addressed). Set `:cell-width` different from 8
  for word-addressed memory (DCPU-16-style). Out-of-range addresses signal
  `address-out-of-range`.
  Memory is currently allocated eagerly as one array of `2^addr-width`
  cells. This is deliberately kept behind the constructor in `storage.lisp`
  so a later region-mapped/sparse backend (ROM/RAM/MMIO, banking) can
  replace it without changing `mref`/`(setf mref)` call sites.
- `(flags NAME...)` — one or more single-bit flags.
- `(instruction-word :width n (field NAME width)...)` — a fixed-width
  instruction word split into named bit fields, MSB-first as declared, one
  of them named `opcode`. Optional; a machine with no such clause keeps the
  default opcode-byte-plus-operand-bytes encoding every earlier milestone
  uses. See [Instructions, "Word-encoded instructions"](instructions.md#word-encoded-instructions-20)
  for how `definstruction` fills a field, and
  [`examples/word.lisp`](../examples/word.lisp) for a complete machine.

  One or more `(layout NAME (field NAME width)...)` forms nested inside the
  same clause (#64) declare *alternate* field splits for a subset of the
  machine's opcodes — e.g. a CHIP8-shaped machine whose opcode nibble alone
  decides whether the rest of the word splits 4/12, 4/4/8, or 4/4/4/4:

  ```lisp
  (instruction-word :width 16
    (field opcode 4) (field x 4) (field y 4) (field n 4)   ; default: 4/4/4/4
    (layout xnn (field opcode 4) (field x 4) (field nn 8)) ; 4/4/8
    (layout nnn (field opcode 4) (field nnn 12)))          ; 4/12
  ```

  Every layout — the default and each alternate — shares the clause's own
  `:width` and declares an `opcode` field identical in width and shift to
  the default's; only the fields below `opcode` vary per layout. A
  `definstruction` names which layout it encodes against with its own
  `(layout NAME)` encoding subclause (default when omitted) — see
  [Instructions, "Per-instruction layouts"](instructions.md#per-instruction-layouts-64).
  A field can also be pinned to a constant with no operand hole at all via
  `(field-value FIELD-NAME n)` — see [Instructions, "Constant discriminator
  fields"](instructions.md#field-value-field-name-n--constant-discriminator-fields-136),
  and [`examples/chip8word.lisp`](../examples/chip8word.lisp) for a complete
  machine using both.
- `(clock-speed n)` — the machine's nominal rate in Hz (#75). Optional; a
  machine with no such clause can still accumulate `machine-cycles` and use
  `run-for-cycles`, just not `run-for-duration` or `machine-elapsed-seconds`
  (which convert a cycle count to wall-time-equivalent seconds, and so need
  a rate to convert against). See
  [Emulator](emulator.md#cycle-cost-model-clock-speed-and-cycle-accurate-execution-75).

Widths and depths must be positive integers; duplicate element names and
unknown clause heads are compile-time errors. `instruction-word`'s field
widths must sum exactly to its own `:width`, which must itself be a whole
number of the machine's own memory cells (#53 — see "Cell width and the
assembler" below; a whole number of 8-bit bytes on every byte-addressed
machine, the only kind before this) — the word is still emitted as
little-endian cells of that width, see "Cell- vs. word-encoded
instructions" below. Every `(layout NAME ...)` alternate (#64) is held to
the same field-width-sums-to-`:width` rule independently, plus the
cross-layout checks above: layout names unique, and an `opcode` field
identical in width and shift to the default's. Those two checks are what let
two co-tenant descriptors at one opcode name *different* layouts (#140,
see [Instructions, "Per-instruction
layouts"](instructions.md#per-instruction-layouts-64)) — every layout
shares one word size and one `opcode` position, so decode can fetch the
opcode off the default layout alone and compare two candidates' fields
bit-for-bit without first knowing which layout matched.

### Note on naming

`flags` names the `defmachine` declaration clause; the semantics operator
that *sets* flags is named `set-flags!` instead, to avoid colliding with
it — see [Semantics vocabulary](semantics.md).

## Runtime state

`(make-machine 'NAME)` instantiates a fresh runtime `machine` for a
registered descriptor. `(reset machine)` zeroes every storage element.

## Width and signedness

All storage is **unsigned**, masked to its declared width on every write
(`wrap-value`) — writing 300 to an 8-bit register stores 44, writing -1
stores 255. Where a value should be read as two's-complement, use
`(signed-value value width)` explicitly; there is no separate signed storage
mode. This is a single rule, chosen once, rather than a per-element
signed/unsigned mode.

## Accessors

| Element kind | Read | Write |
|---|---|---|
| register (scalar) / flag | `(sref machine name)` / `(flag machine name)` | `(setf (sref machine name) v)` / `(setf (flag machine name) v)` |
| register (banked, `:count > 1`) | `(regref machine name index)` | `(setf (regref machine name index) v)` |
| stack | `(stack-pop machine name)`, `(stack-depth machine name)`, `(stack-ref machine name offset)` | `(stack-push machine name v)`, `(setf (stack-ref machine name offset) v)` |
| memory | `(mref machine name address)` | `(setf (mref machine name address) v)` |

`regref` also works on a scalar (`:count 1`) register, treating it as a
one-element bank (`index` 0); `sref` is the reverse restriction, and signals
`unknown-storage` on a banked element rather than aliasing every index to
one cell.

`flag` treats any non-`nil` value as 1 and `nil` as 0 on write, and reads
back as `0`/`1`.

## Conditions

All signalled conditions inherit `lasm-error`: `unknown-storage`,
`address-out-of-range`, `stack-overflow`, `stack-underflow`,
`stack-index-out-of-range` (an out-of-range `offset` to `stack-ref`/
`(setf stack-ref)`), `register-index-out-of-range` (an out-of-range
`index` to `regref`/`(setf regref)`). `lasm-trap` is signalled by the `trap` semantics
operator (see [Semantics vocabulary](semantics.md)) and is not a storage
error.

None of these storage conditions are currently a `run` stop reason (see
[Emulator, "Stop reasons"](emulator.md#stop-reasons)) — an instruction that
overflows or underflows a stack, or addresses memory out of range, escapes
`run` as a raw Lisp condition rather than returning `:trap`,
`:decode-failure`, or `:max-steps`.

## Cell- vs. word-encoded instructions

Every machine before `instruction-word` (M1–M3) encodes one instruction as an
opcode cell followed by fixed-width operand cells chosen by addressing
mode — `instruction-descriptor-total-operand-width` is the cell count that
encoding occupies. A machine declaring `instruction-word` instead encodes
one instruction as a single fixed-width word whose bits are split into
named fields (a DCPU-16-shaped machine, #20) — see
[Instructions](instructions.md#word-encoded-instructions-20) for how
`definstruction` fills those fields, including operand values that pack
inline for a small range or escape to their own following word depending on
the *value* being encoded, not just its addressing-mode syntax.

`instruction-descriptor-size` is the one accessor that covers both schemes —
the total encoded cell count for one use of an instruction, cell-encoded or
word-encoded alike. The instruction word itself (and any extra word an
escaped operand needs) is emitted as little-endian cells at the target
machine's own cell width, so `assembly-cells` is `(vector (unsigned-byte 8))`
on every byte-addressed machine and `(vector (unsigned-byte n))` on one
declaring `:cell-width n` (#53) — see "Cell width and the assembler" below.

## Cell width and the assembler

`:cell-width` isn't only a storage-layer property (`mref`/`(setf mref)`
masking and allocation, above) — the assembler resolves it too, since a
program's labels and location counter are addresses in the *same* units
`mref` indexes by. `assemble`/`assemble-statements`
([Assembler](assembler.md#assemblys-cell-width)) and `load-program`
([Emulator](emulator.md#load-program)) all resolve a machine's code cell
width the same way: the sole memory element's `:cell-width`, or (when a
machine declares several) their shared width if every one agrees. A machine
declaring more than one memory element with *different* cell widths makes
this ambiguous, and each of those entry points takes an explicit `:memory`
argument for exactly that case — the same shape as `%default-address-width`
(instruction.lisp) already uses to pick a sole memory element's address
width when an addressing mode doesn't declare one.

Word-addressed memory and bitfield/variant instruction-word encoding (see
["Word-encoded instructions"](instructions.md#word-encoded-instructions-20))
are independent axes and compose freely — an `instruction-word`'s `:width`
just has to be a whole multiple of the target cell width.
[`examples/dcpu16.lisp`](../examples/dcpu16.lisp) combines both, DCPU-16
shaped.
