# Emulator

A tree-walking fetch/decode/execute loop running encoded bytes (see
[Assembler](assembler.md)) against a live `machine` (see [Machine
model](machine-model.md)), dispatching on opcode to each instruction's
semantics (see [Instructions](instructions.md)).

```lisp
(let ((m (make-machine 'sixtyfoo))
      (a (assemble source :lexer 'sixtyfoo-syntax :machine 'sixtyfoo)))
  (load-program m a)
  (run m))
```

See [`examples/counter.lisp`](../examples/counter.lisp) for a runnable
version.

## PC and program memory: convention, not declaration

M1 adds no new `defmachine` storage kind for "the program counter" or "the
program's memory." Instead, `load-program`, `step-machine`, and `run` all
default to:

- **PC** — the register named `pc`.
- **Program memory** — the machine's sole `:memory` storage element.

This is the same "PC is a plain register" convention already used
throughout [Instructions](instructions.md) and its examples (`(register pc
:width 16)`, `(set! pc operand)` in a branch's semantics). A machine that
names its PC register something else, or declares more than one memory
element, passes `:pc` / `:memory` explicitly to override — the same escape
hatch `%default-absolute-width` already uses for `absolute` mode's default
operand width when more than one memory element is declared.

## `load-program`

```lisp
(load-program MACHINE bytes &key memory origin)
```

`bytes` is an `assembly` (see [Assembler](assembler.md)) or any sequence of
`(unsigned-byte 8)`. Writes each byte into `memory` starting at `origin`,
then sets the PC register to `origin`.

`origin` defaults to the assembly's own `origin` slot when `bytes` is an
`assembly` — so `(assemble source :origin #x200)` and `load-program`
*cannot* silently disagree about where the program's labels point — and to
`0` otherwise.

## `step-machine`

```lisp
(step-machine MACHINE &key pc memory)
```

Fetches the opcode byte at `pc`, decodes it (`find-instruction-by-opcode`),
reads its declared `operand-width` bytes little-endian, **advances `pc` past
the whole instruction, then executes its semantics** — in that order.
This ordering is what lets a branch instruction's own `(set! pc operand)`
override the increment rather than being clobbered by it running afterward.

Returns the executed `instruction-descriptor`, or the keyword
`:decode-failure` (without advancing `pc` or executing anything) if the byte
at `pc` isn't a registered opcode on this machine.

## `run`

```lisp
(run MACHINE &key pc memory (max-steps 10000))
;; => (values reason steps [condition])
```

Calls `step-machine` in a loop until one of three stop conditions:

| `reason` | Meaning |
|---|---|
| `:trap` | An instruction's semantics called `trap` (see [Semantics vocabulary](semantics.md)), signalling `lasm-trap`. `run` catches it; the condition itself is the third return value. This *is* M1's halt mechanism — no dedicated halt primitive exists, or is needed: `(definstruction m hlt (encoding (opcode #x00)) (semantics (trap :halt)))` is enough. A generalized interrupt/exception model replacing `trap` outright is M6. |
| `:decode-failure` | `step-machine` hit a byte that isn't a registered opcode — typically a program with no `hlt` running off the end into zeroed (unassigned) memory, which decodes as opcode `0`. |
| `:max-steps` | `max-steps` instructions executed without stopping otherwise — a runaway-program guard, not a cycle timer (`(cycles n)` on `definstruction` is parsed but not used yet — a separate follow-up). |

`steps` counts instructions that actually executed. A step that traps still
counts (its semantics ran to completion before signalling); a step that
fails to decode does not (nothing executed that iteration).

## Note on flags in your own semantics (#22)

`(setf flag)` treats its value as a Lisp boolean, not an integer 0/1 — `0`
is non-`nil`, so `(setf (flag m 'z) 0)` sets the flag, not clears it. Write
new instruction semantics to pass an actual boolean (e.g. `(zero? x)`,
`(bit-set? a 7)`), not a raw comparison result that happens to be an
integer.

## Scope

This covers fetch/decode/execute over already-encoded bytes and a single
flat halt/decode-failure/step-budget stop model. It does not cover:

- Multiple addressing modes or multi-mode dispatch — M2.
- Cycle-accurate timing using `(cycles n)` — undecided, tracked separately.
- Interrupts, privilege levels, or a generalized trap/interrupt model
  beyond the single `trap` primitive — M6.
- A disassembler recovering source from encoded bytes — M7 (#21).
