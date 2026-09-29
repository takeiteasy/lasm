# Machine families

A machine can extend another. The child inherits the parent's storage,
clauses and instructions and states only what differs — the shape of a CPU
family: one architecture, several models that add, remove or re-time
instructions and differ in memory size, clock rate and identity.
`defmachine` defines an [ISA and a CPU](isa.md) together; write `defisa` and
`defcpu` when models share one ISA.

```lisp
(defmachine anima16
  (register reg :width 16 :names (a b c x y z i j))
  (register pc :width 16)
  (register sp :width 16)
  (register ia :width 16)
  (memory ram :width 16 :addr-width 16 :cell-width 16)
  (stack-pointer sp :memory ram :grows :down)
  (interrupts :vector ia :message (reg 0) :save (pc (reg 0)) :stack sp :queue 512)
  (clock-speed 100000)
  (identity :model "praxis-100" :id #xC9000001 :version #x0107 :manufacturer #xBAAAAAAD))

(defmachine (mote (:extends anima16))
  (without-instructions bra bsr swp neg)
  (undefined-opcode :trap)
  (interrupts :queue 16)
  (memory ram :addr-width 12)
  (clock-speed 1000000)
  (identity :model "mote-40" :id #xC9000002))
```

`(defmachine (NAME (:extends PARENT)) clause...)` — `PARENT` must already be
defined as an ISA and a CPU. Chains can be any depth.

## Clause merging

A child clause is merged over the parent's clause of the same kind:

| Clause | Merge |
|---|---|
| `register`, `stack`, `memory`, `device` | matched by name; the child's keywords override the parent's, the rest are kept |
| `devices` | each entry matched by name and merged in place; new entries are appended after the inherited devices |
| `memory` `(region ...)` forms | if the child gives any, they replace the parent's regions |
| `flags` | added to the parent's |
| `interrupts`, `identity`, `properties`, `idle` | merged key by key |
| `clock-speed`, `reset-pc`, `undefined-opcode` | replaced |

A clause naming something the parent does not have adds it. Element order and
device bus indices of the parent are kept.

## Backends

A [backend](backends.md#inheritance) can extend another for the same ISA or an
ISA that extends it, so a family shares one compiler-target description.

## Modes

A child sees its parent's [ISA-local modes](modes.md#isa-local-modes)
and can shadow them with `(defmode (NAME (:isa CHILD)) ...)`.

## What a CPU cannot change

Instructions are compiled against the ISA's layout, so a CPU, or a child ISA,
is rejected when it:

- declares `instruction-word` or `stack-pointer`
- changes a register's `:count` or `:names`
- changes a memory's `:cell-width`, `:endian` or operand cell count
  (`ceiling(addr-width / cell-width)`)
- adds or removes a memory or stack element
- removes a register or flag that an `interrupts`, `stack-pointer`, `privilege`
  or `reset-pc` clause names
- changes an `interrupts` clause's `:save` list or stack

Register and memory widths, `:addr-width` (within the operand cell count),
`clock-speed`, `reset-pc`, interrupt queue depth and the other interrupt keys may change.

## Child-only clauses

- `(without-instructions MNEMONIC...)` — removes every mode of each mnemonic.
  The assembler does not recognize it, and the removal is inherited by
  descendants.
- `(instruction-cycles (MNEMONIC n)...)` — overrides the cycle cost of every
  inherited variant of the mnemonic.
- `(without-storage NAME...)` — removes registers and flags. Descendants
  inherit the removal.[^removal]
- `(without-devices NAME...)` — removes declared devices. Later devices move
  down one bus index, and `device-count` shrinks.

```lisp
(defmachine (mote (:extends anima16))
  (without-storage ex)
  (without-devices keyboard)
  (reset-pc #x100))
```

A name the parent lacks gives a style-warning. Removing and redeclaring a name in
the same clause list is an error, as is removing a device a region's `:device`
names.

## Undefined opcodes

`(undefined-opcode POLICY)` sets what a step does when the bytes at PC decode
to no instruction:

| Policy | Behaviour |
|---|---|
| `:fault` (default) | `run` stops with `:decode-failure` |
| `:nop` | step over it; `step-machine` returns `:nop` |
| `:trap` | signal `lasm-trap` with tag `:undefined-opcode` and data `(:pc address :opcode value)`; PC is unchanged, so `run` returns `:trap` |

A removed instruction is skipped whole, without evaluating its operands, and
costs one cycle per cell skipped. An opcode no instruction ever used is one
cell (or one instruction word) and costs one cycle. On a word-encoded machine
a removed instruction is not decoded by a `(fallback)` instruction it shadows
in the parent.

## Identity

`(identity :model "praxis-100" :id n :version n :manufacturer n)` is what a CPU
reports about itself. Every key is optional: `:model` defaults to the CPU's
name and the numbers to `0`. A child merges key by key.

```lisp
(cpu-info 'mote)     ; => #xC9000002, #x0107, #xBAAAAAAD
(cpu-model 'mote)    ; => "mote-40"
(cpu-isa 'mote)      ; => MOTE
```

Each takes a machine, a descriptor or a CPU name. `cpu-info` returns the id,
version and manufacturer as three values, like `device-info`. The ISA name is
the architecture.

An ISA can require every key with `(identity :required t)`; a CPU that omits
one is a definition error naming it. `:required` belongs to `defisa` and the
other keys to `defcpu`; a `defmachine` takes both and splits them.

## Properties

`(properties :key value ...)` attaches free-form data to a machine, readable
from semantics or host code:

```lisp
(machine-property machine :rom-size)           ; a machine, descriptor or name
(machine-property 'mote :rom-size 0)           ; optional default
```

## Later definitions

A `definstruction` on an ISA reaches every CPU of it and every child ISA at
once, except a child ISA that defines the mnemonic itself. A CPU that removed
the mnemonic keeps it removed, and a CPU's cycle override applies to the new
definition. An opcode conflict with a descendant's own instruction is an error
and changes nothing.

An ISA holds only its own instructions. A child ISA and a CPU read the chain
from the root ISA through a view, so no instruction is copied and an
inherited descriptor is the parent's own.[^view]

## Limitations

- A change to a parent's `defmachine` clauses reaches existing children only
  when their `defmachine` forms are evaluated again. Redefining a parent so a
  child no longer fits it gives no warning; the child faults when it runs.
  [#450](https://todo.sr.ht/~takeiteasy/lasm/450)
- Removal is per mnemonic, not per addressing mode.
- Memory and stack elements cannot be removed in a child.
  [#361](https://todo.sr.ht/~takeiteasy/lasm/361)
- A child that removes a register an inherited instruction uses is not
  rejected. [#360](https://todo.sr.ht/~takeiteasy/lasm/360)

[^removal]: An inherited instruction that still uses a removed register fails
    with `unknown-storage` when it runs. Remove it with `without-instructions`.
[^view]: `instruction-descriptor-machine` of an inherited instruction names the
    ISA that defines it. A child that keeps the parent's instruction word and
    storage layout reads it the same way.
