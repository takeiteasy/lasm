# Machine model

`defmachine` declares storage and execution settings. A machine can combine
registers, stacks, memory, devices, and instruction-word layouts.

```lisp
(defmachine sixtyfoo
  (register a :width 8)
  (register x :width 8)
  (stack s :width 8 :depth 256)
  (memory ram :width 8 :addr-width 16)
  (flags z n c v))
```

## `defmachine`

The machine descriptor is available to later `definstruction` forms during
compilation. Names of elements, regions, devices, and register aliases share
one namespace.

### Clauses

| Clause | Defines | More detail |
| --- | --- | --- |
| `(register NAME :width n [:count n] [:names (...)])` | Scalar or banked registers and optional aliases. | [Registers](#registers) |
| `(stack NAME :width n :depth n)` | Fixed-depth LIFO storage. | [Stacks](#stacks) |
| `(stack-pointer REGISTER [:memory NAME] [:grows :down/:up])` | A register used as a memory stack pointer. | [Stacks](#stacks) |
| `(memory NAME :width n :addr-width n [:cell-width n] [:endian ORDER] ...)` | Addressable cells and optional regions. | [Memory regions](#memory-regions) |
| `(flags NAME...)` | Single-bit flags. | [Accessors](#accessors) |
| `(instruction-word :width n (field NAME width)...)` | Named instruction bit fields and optional layouts. | [Instruction words](#cell--vs-word-encoded-instructions) |
| `(device NAME ...)` | Bus-addressed peripheral. | [Devices](devices.md) |
| `(clock-speed n)` | Nominal cycles per second. | [Emulator](emulator.md#cycle-costs-and-clock-speed) |
| `(interrupts ...)` | Delivery, queue, save state, and masking. | [Interrupts](interrupts.md) |
| `(undefined-opcode POLICY)` | Fault, NOP, or trap on an unknown opcode. | [Machine families](machine-families.md#undefined-opcodes) |
| `(properties :key value ...)` | Literal machine properties. | [Machine families](machine-families.md#properties) |

Widths and depths are positive integers. Duplicate names and unknown
clauses fail during compilation. A child machine can inherit clauses and
instructions; see [Machine families](machine-families.md).

### Registers

A scalar register uses `sref`; a banked register (`:count > 1`) uses
`regref` with an index. Writes wrap to the register's width. Inside
semantics, a banked register binds as `(NAME index)`:

```lisp
(set! (v 3) #xff)
```

`:names` assigns one alias per bank cell, such as `A B C X Y Z I J`.
Aliases can be used in assembly source and rendered by the disassembler
when an operand declares `:register`; see [Instructions](instructions.md#encoding).
A label or assignment cannot reuse an alias name.

### Stacks

A fixed `(stack ...)` holds `:depth` values. `stack-ref` addresses live
entries from the top (`0` is the top), while `stack-pointer` reads or sets
the number of live entries. Moving the pointer does not clear stored cells.
Overflow and underflow signal storage conditions.

A `(stack-pointer REGISTER ...)` instead uses a scalar register to index
memory. `:down` (default) points at the top and pre-decrements on push;
`:up` points past the top and post-increments on push. The indexed address
wraps to the memory address width. `push`/`pop` and interrupt delivery accept
either stack form. See [Semantics vocabulary](semantics.md).

## Memory regions

A memory element has `2^addr-width` addressable cells. `:cell-width` sets
bits per cell and defaults to `:width`; `:endian` defaults to `:little`.
Regions cover inclusive address ranges and cannot overlap:

```lisp
(memory ram :width 8 :addr-width 16
  (region bios #x0000 #x00ff :kind :rom)
  (region romx #x4000 #x7fff :kind :rom :banks 8)
  (region io #xff00 #xff0f :kind :device
    :read io-read :write io-write))
```

| Kind | Reads | Writes |
| --- | --- | --- |
| `:ram` | Backing storage. | Backing storage. |
| `:rom` | Backing storage. | Ignored by default, or `memory-write-protected` with `:on-write :error`. |
| `:device` | `:read` hook, or `0`. | `:write` hook, or discarded. |
| `:device` with `:device NAME` | Bound [device's](devices.md#memory-mapped-devices) `:read`. | Its `:write`. |

Hooks are bare function names. Device hooks receive the **absolute**
address: `(read machine address)` and `(write machine address value)`.
`mref` and instruction fetch honor region behavior; `mpeek` reads backing
storage for inspection without device side effects. `load-program` can fill
ROM. `reset` keeps ROM images and clears everything else.[^regions]

### Bank switching

`:banks n` gives a RAM or ROM region separate banks, with bank 0 mapped on
creation and reset. It is unavailable for device regions.

```lisp
(current-bank machine 'romx)              ; => 0
(setf (current-bank machine 'romx) 3)
(bank-peek machine 'romx 5 #x4000)        ; read an unmapped bank
```

`bank-peek` addresses any bank without changing the mapping; its setter
bypasses ROM protection. Normal memory access uses the mapped bank.
`reset` maps bank 0 again, keeps ROM banks and clears RAM banks. See
[Banked output](banked-output.md) for `.bank`
and [Emulator](emulator.md#load-program) for loading a bank.

## Runtime state

`(make-machine 'NAME)` creates a machine. `(reset machine)` clears non-ROM storage,
cycles, pending interrupts, and idle state, drops runtime region bindings, and
rebuilds the declared device bus. Host-installed access and interrupt hooks remain attached.

## Width and signedness

Storage is unsigned. Every write wraps to its declared width: `300` in an
8-bit register becomes `44`; `-1` becomes `255`. Use
`(signed-value value width)` to read two's-complement meaning.

## Accessors

| Element | Read | Write |
| --- | --- | --- |
| Scalar register / flag | `sref`, `flag` | `(setf sref)`, `(setf flag)` |
| Banked register | `regref` | `(setf regref)` |
| Fixed stack | `stack-pop`, `stack-depth`, `stack-pointer`, `stack-ref` | `stack-push`, `(setf stack-pointer)`, `(setf stack-ref)` |
| Memory | `mref`, `mpeek` | `(setf mref)` |
| Register stack pointer | `sp-pop` | `sp-push` |

`regref` accepts a scalar register at index 0; `sref` rejects a banked one.
`flag` stores `0` or `1`. Semantics can omit the fixed-stack name when the
machine has only one fixed stack. See [Semantics vocabulary](semantics.md).

### Access hook

`machine-access-hook` takes `(machine name index access value)` for storage
reads and writes. `access` is `:read` or `:write`; the index is a bank index,
address, stack slot, `:pointer`, or `nil`. A fixed stack's push, pop and
`(setf stack-pointer)` also report `:pointer` as a write of the new depth,
before the depth changes. Inspection peeks, program loading,
instruction fetch, and PC advancement do not call it. The
[debugger](debugger.md#watchpoints) uses this hook.

## Conditions

| Condition | Typical cause |
| --- | --- |
| `unknown-storage` | Missing element or scalar access to a banked register. |
| `address-out-of-range`, `memory-write-protected` | Invalid address or protected ROM write. |
| `stack-overflow`, `stack-underflow`, `stack-index-out-of-range`, `stack-pointer-out-of-range` | Invalid fixed-stack operation. |
| `register-index-out-of-range` | Invalid bank index. |
| `no-such-device`, `interrupt-queue-full` | Device or interrupt error. |

A malformed `defmachine` signals `machine-definition-error`.

These inherit `lasm-error`; see [Conditions](conditions.md) for readers.
During `run`, storage faults return `:fault` and the condition. Direct
stepping signals them. `interrupt-queue-full` still signals with the default
overflow policy; see [Interrupts](interrupts.md#overflow).

## Cell- vs. word-encoded instructions

A cell-encoded instruction has an opcode cell followed by operand cells.
With `instruction-word`, opcode and inline operands share a fixed-width
word, followed by any extra cells. Its fields are declared MSB first, must
sum to the word width, and include `opcode`. Named `(layout NAME ...)` forms
can provide other field splits with the same word width and opcode position.
See [Word-encoded instructions](word-instructions.md).

`instruction-descriptor-size` counts the complete encoding in cells.
`(extra-word-order FIELD...)` changes the order of extra values; otherwise
they follow operand order. See
[Word-encoded layouts](word-instructions.md#per-instruction-layouts).

## Cell width and the assembler

Labels, the location counter, and instruction sizes count the target
memory's **cells**, regardless of their bit width. `assemble` and
`load-program` use the sole memory element's cell width and byte order, or
shared values when several elements agree. Pass `:memory` when they differ.
See [Assembler](assembler.md#assemblys-cell-width).

`:endian` controls the cells within each multi-cell value:

| Order | Example `$0A0B0C0D` in 8-bit cells |
| --- | --- |
| `:little` | `0D 0C 0B 0A` |
| `:big` | `0A 0B 0C 0D` |
| `(:big :little 2)` | `0B 0A 0D 0C` |
| `(:little :big 2)` | `0C 0D 0A 0B` |

The grouped form orders groups with its first keyword and cells within a
group with its second. Word-addressed memory and word-encoded instructions
can be combined; see [`dcpu16.lisp`](../examples/dcpu16.lisp).

[^regions]: Regions change access behavior over one backing array.
  `:device` regions do not store values. `mpeek` reads zero there. A mapper
  can be a device write hook that changes `current-bank`.
