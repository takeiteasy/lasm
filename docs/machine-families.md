# Machine families

A machine can extend another. The child inherits the parent's storage,
clauses and instructions and states only what differs — the shape of a CPU
family: one architecture, several models that add, remove or re-time
instructions and differ in memory size, clock rate and identity.

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
  (properties :architecture "anima16"))

(defmachine (mote (:extends anima16))
  (without-instructions bra bsr swp neg)
  (undefined-opcode :trap)
  (interrupts :queue 16)
  (memory ram :addr-width 12)
  (clock-speed 1000000)
  (properties :model "mote-40" :device-id #xC9000002))
```

`(defmachine (NAME (:extends PARENT)) clause...)` — `PARENT` must already be
defined. Chains can be any depth. Runnable example:
[`examples/family.lisp`](../examples/family.lisp).

## Clause merging

A child clause is merged over the parent's clause of the same kind:

| Clause | Merge |
|---|---|
| `register`, `stack`, `memory`, `device` | matched by name; the child's keywords override the parent's, the rest are kept |
| `memory` `(region ...)` forms | if the child gives any, they replace the parent's regions |
| `flags` | added to the parent's |
| `interrupts`, `properties` | merged key by key |
| `clock-speed`, `undefined-opcode` | replaced |

A clause naming something the parent does not have adds it. Element order and
device bus indices of the parent are kept.

## What a child cannot change

Inherited instructions are compiled against the parent's layout, so a child
is rejected when it:

- declares `instruction-word` or `stack-pointer`
- changes a register's `:count` or `:names`
- changes a memory's `:cell-width`, `:endian` or operand cell count
  (`ceiling(addr-width / cell-width)`)
- adds or removes a memory or stack element
- changes an `interrupts` clause's `:save` list or stack

Register and memory widths, `:addr-width` (within the operand cell count),
`clock-speed`, interrupt queue depth and the other interrupt keys may change.

## Child-only clauses

- `(without-instructions MNEMONIC...)` — removes every mode of each mnemonic.
  The assembler no longer knows it, and the removal is inherited by
  descendants.
- `(instruction-cycles (MNEMONIC n)...)` — overrides the cycle cost of every
  inherited variant of the mnemonic.

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
a removed instruction is not decoded by a `(fallback)` instruction it used to
shadow.

## Properties

`(properties :key value ...)` attaches literal data to a machine, readable
from semantics or host code:

```lisp
(machine-property machine :device-id)          ; a machine, descriptor or name
(machine-property 'mote :model "unknown")      ; optional default
```

## Later definitions

A `definstruction` on a parent is copied to every descendant, except one that
defines the mnemonic itself or removed it. A descendant's cycle override
applies to the new definition. An opcode conflict with a descendant's own
instruction is an error and changes nothing.

Each machine holds its own copies of the inherited instructions; nothing is
looked up through the parent at run time.

## Limits

- A change to a parent's `defmachine` clauses reaches existing children only
  when their `defmachine` forms are evaluated again.
- A mnemonic dropped from a parent stays on its children until they are
  re-evaluated.
- Removal is per mnemonic, not per addressing mode.
- Storage elements and devices cannot be removed in a child.
