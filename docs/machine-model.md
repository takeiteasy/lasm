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

  The read direction — decoding a bank index back to its alias — is a
  `definstruction` encoding subclause, not a machine-model one: see
  [Instructions, `:register`](instructions.md#encoding-opcode-n-operand)
  and [Disassembler, "Register-index operand
  rendering"](disassembler.md#register-index-operand-rendering).
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
- `(stack-pointer REGISTER [:memory NAME] [:grows :down/:up])` — binds an
  existing scalar register as an address pointer into a `:memory` element
  (#166), for machines whose stack is a plain register indexed by push/pop
  convention rather than a `(stack ...)` element — DCPU-16 and ANIMA-16, for
  instance. `:memory` defaults to the machine's sole declared memory element
  (an error if it declares none or more than one). `:grows` (default `:down`)
  picks the convention: `:down` has `REGISTER` point *at* the top item — push
  pre-decrements then stores, pop loads then post-increments; `:up` has it
  point one *past* the top item — push stores then post-increments, pop
  pre-decrements then loads. `push`/`pop` (see [Semantics
  vocabulary](semantics.md)) and an `(interrupts ...)` clause's `:stack`
  (below) both accept a stack-pointer register wherever they accept a
  `(stack ...)` element's name. There is no overflow/underflow condition — a
  wrapping register is the machine's own business, same as the hardware it
  models — and the indexed address is masked to `:memory`'s `:addr-width`,
  so `REGISTER` may be wider than the address space.
- `(memory NAME :width n :addr-width n [:cell-width n] [:endian :little/:big]
  [(region NAME start end [:kind :ram/:rom/:device] [:on-write :ignore/:error]
  [:read fn] [:write fn])...])` —
  addressable storage. `:addr-width` is the number of address bits (so the
  element has `2^addr-width` cells); `:cell-width` is the bit width of each
  cell and defaults to `:width` (byte-addressed). Set `:cell-width` different
  from 8 for word-addressed memory (DCPU-16-style). `:endian` (default
  `:little`) is which cell of a multi-cell value is the low-order one — see
  "Cell width and the assembler" below. Out-of-range addresses signal
  `address-out-of-range`. Memory is allocated eagerly as one array of
  `2^addr-width` cells; a `region` declares a sub-range with different access
  *behavior* over that same array — see "Memory regions" below.
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
  `(extra-word-order FIELD...)` inside the clause (#191) sets the order in
  which trailing words follow the instruction word: the named fields' words
  first, in the order listed, then any other operand's in hole order.
  Fieldless `:trailing-word` operands follow their nearest preceding
  field-bearing operand, keeping their local order.
  Without it trailing words follow operand hole order. It applies to every
  instruction on the machine, resolved through each instruction's own layout:

  ```lisp
  (instruction-word :width 16
    (field av 6) (field bv 5) (field opcode 5)
    (extra-word-order av bv))   ; `a`'s word first, though `b` is written first
  ```

  A field can also be pinned to a constant with no operand hole at all via
  `(field-value FIELD-NAME n)` — see [Instructions, "Constant discriminator
  fields"](instructions.md#field-value-field-name-n--constant-discriminator-fields-136),
  and [`examples/chip8word.lisp`](../examples/chip8word.lisp) for a complete
  machine using both.
- `(device NAME [:id n] [:version n] [:manufacturer n] [:init fn] [:tick fn]
  [:receive fn] [:detach fn])` — a bus-addressed peripheral (#108),
  independent of memory regions above — see [Devices](devices.md).
- `(clock-speed n)` — the machine's nominal rate in Hz (#75). Optional; a
  machine with no such clause can still accumulate `machine-cycles` and use
  `run-for-cycles`, just not `run-for-duration` or `machine-elapsed-seconds`
  (which convert a cycle count to wall-time-equivalent seconds, and so need
  a rate to convert against). See
  [Emulator](emulator.md#cycle-cost-model-clock-speed-and-cycle-accurate-execution-75).
- `(interrupts :vector reg :message reg :save (name...) [:stack name]
  [:queue n] [:on-overflow policy] [:mask-when fn] [:mask-flag name]
  [:cycles n] [:drop-on-zero-vector t/nil] [:mask-on-deliver t/nil])` — the interrupt-delivery model
  (#109): a vector register, what's saved/restored around delivery, a
  pending-signal queue, and optional masking — see
  [Interrupts](interrupts.md). `:stack` accepts either a `(stack ...)`
  element or a `(stack-pointer ...)`-bound register (#166, above).

Widths and depths must be positive integers; duplicate element names and
unknown clause heads are compile-time errors. `instruction-word`'s field
widths must sum exactly to its own `:width`, which must itself be a whole
number of the machine's own memory cells (#53 — see "Cell width and the
assembler" below; a whole number of 8-bit bytes on every byte-addressed
machine, the only kind before this) — the word is still emitted as cells of
that width in the machine's own endian order (#66), see "Cell- vs.
word-encoded instructions" below. Every `(layout NAME ...)` alternate (#64)
is held to
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

## Memory regions

A memory element's `region` forms declare sub-ranges of its address space
with distinct access behavior — `mref`/`(setf mref)` route through whichever
region an address falls in; an address in no declared region keeps the
element's plain, uniform behavior. `start`/`end` are both inclusive; regions
never overlap and every name (region, register alias, storage element, or
[device](devices.md)) shares one machine-wide namespace, checked at
`defmachine` time.

```lisp
(defmachine gb
  (memory ram :width 8 :addr-width 16
    (region bios #x0000 #x00FF :kind :rom)
    (region vram #x8000 #x9FFF)                 ; :ram, the default
    (region io   #xFF00 #xFF0F :kind :device
                 :read io-read :write io-write)))
```

`:kind` is one of:

- `:ram` (the default) — ordinary storage, identical to an address outside
  any region. Only useful to name a sub-range, e.g. for later banking.
- `:rom` — reads hit backing storage; writes are dropped (`:on-write
  :ignore`, the default) or signal `memory-write-protected` (`:on-write
  :error`). A ROM image is *burned in*, not stored by the CPU — `load-program`
  and the debugger write through `%poke`, an internal accessor that bypasses
  region write policy entirely, so loading a program at a ROM region's
  origin still works.
- `:device` — reads and writes are forwarded to `:read`/`:write` instead of
  touching backing storage at all. A region with no `:read` reads as 0; one
  with no `:write` discards the store. Both are function designators —
  write the bare function name, not `#'name`: `defmachine` quotes its whole
  clause body, so a `#'`-form there would freeze to the literal list
  `(function name)` rather than an actual function; a bare symbol survives
  quoting unevaluated and `funcall` resolves it at call time. `:read` is
  called as `(funcall read machine address)`, `:write` as `(funcall write
  machine address value)` — both the *absolute* address, not a
  region-relative offset.

`mpeek` reads backing storage directly, bypassing a `:device` region's
`:read` (returning 0 there, since a device region has no backing cell of its
own) — for inspection paths (the debugger's hex dump, disassembly) that must
not trigger a device's read side effects merely by displaying memory.
`mref`/instruction fetch are real accesses and always consult regions.

Regions are an access-behavior overlay, not a separate storage backend:
`reset` still zeroes the whole underlying array regardless of region, so a
burned-in ROM image does not survive a `reset` and must be reloaded.
Bank-switched regions (a region whose backing changes at runtime) are not
yet supported.

## Runtime state

`(make-machine 'NAME)` instantiates a fresh runtime `machine` for a
registered descriptor. `(reset machine)` zeroes every storage element and
restores the [device bus](devices.md) to its declared shape; any installed
interrupt hook is left alone — see "Interrupt seam" in that doc.

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
| memory | `(mref machine name address)`, `(mpeek machine name address)` | `(setf (mref machine name address) v)` |
| stack-pointer (#166) | `(sp-pop machine reg memory grows)` | `(sp-push machine reg memory grows v)` |

`regref` also works on a scalar (`:count 1`) register, treating it as a
one-element bank (`index` 0); `sref` is the reverse restriction, and signals
`unknown-storage` on a banked element rather than aliasing every index to
one cell.

`sp-push`/`sp-pop` take `memory`/`grows` explicitly rather than resolving
them from `reg` alone, since a `(stack-pointer ...)` clause's own resolved
values are what `push`/`pop` and interrupt delivery already have in hand at
the call site — see [Semantics vocabulary, `push`/`pop`](semantics.md).

`mpeek` is `mref`'s inspection-only sibling — see "Memory regions" above for
what it bypasses and why.

`flag` writes `0` for `nil` or integer `0`, and `1` for `t` or any nonzero
integer. Other values follow Lisp truthiness. It reads back as `0` or `1`.

## Conditions

All signalled conditions inherit `lasm-error`: `unknown-storage`,
`address-out-of-range`, `memory-write-protected` (a store into a `:rom`
region declaring `:on-write :error` — see "Memory regions" above),
`stack-overflow`, `stack-underflow`,
`stack-index-out-of-range` (an out-of-range `offset` to `stack-ref`/
`(setf stack-ref)`), `register-index-out-of-range` (an out-of-range
`index` to `regref`/`(setf regref)`), `no-such-device` (see
[Devices](devices.md)), `interrupt-queue-full` (a `signal-interrupt` past
an `(interrupts ...)` clause's `:queue` depth with the default
`:on-overflow :error` — see [Interrupts](interrupts.md#overflow)).
`lasm-trap` is signalled by the `trap` semantics operator (see [Semantics
vocabulary](semantics.md)) and, on an `:on-overflow :trap` machine, by
`signal-interrupt` past its `:queue` depth as well; neither is a storage
error. See [Conditions](conditions.md) for each condition's readers.

None of these storage conditions are currently a `run` stop reason (see
[Emulator, "Stop reasons"](emulator.md#stop-reasons)) — an instruction that
overflows or underflows a stack, addresses memory out of range, or
overflows an interrupt queue with `:on-overflow :error`, escapes `run` as a
raw Lisp condition rather than returning `:trap`, `:decode-failure`, or
`:max-steps`.

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
escaped operand needs) is emitted as cells at the target machine's own cell
width, in the machine's own endian order (#66), so `assembly-cells` is
`(vector (unsigned-byte 8))` on every byte-addressed machine and
`(vector (unsigned-byte n))` on one declaring `:cell-width n` (#53) — see
"Cell width and the assembler" below.

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

`:endian` (#66) resolves the same way, alongside `:cell-width` — a memory
element's declared `:little` (the default) or `:big`, or the machine's
shared endianness across several elements that agree, ambiguous the same
way and resolved by the same explicit `:memory` argument. It governs cell
order within one multi-cell value — an instruction operand, an
`instruction-word`'s own encoded word and any extra word following it, and
a `.byte`/`.word` directive's data — never which cell a *field* occupies or
which order fields or words themselves fall in. Every encoded quantity in
the codebase ultimately goes through `%encode-value-cells`
(instruction.lisp) and its inverse `%fetch-cells` (decoder.lisp), so
instructions and data always agree on endianness for a given machine.

Word-addressed memory and bitfield/variant instruction-word encoding (see
["Word-encoded instructions"](instructions.md#word-encoded-instructions-20))
are independent axes and compose freely — an `instruction-word`'s `:width`
just has to be a whole multiple of the target cell width.
[`examples/dcpu16.lisp`](../examples/dcpu16.lisp) combines both, DCPU-16
shaped.
