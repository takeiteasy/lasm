# Emulator

The emulator loads encoded cells into a machine and executes instructions.

```lisp
(let ((m (make-machine 'sixtyfoo))
      (a (assemble source :lexer 'sixtyfoo-syntax :machine 'sixtyfoo)))
  (load-program m a)
  (run m))
```

See [`counter.lisp`](../examples/counter.lisp) for a runnable program.

## PC and program memory

The emulator uses register `pc` and the machine's sole memory element by
default. Pass `:pc` or `:memory` to select different elements. The PC is an
ordinary register; see [Instructions](instructions.md#pc-is-a-plain-register).

## `load-program`

```lisp
(load-program MACHINE cells &key memory origin bank)
```

`cells` is an `assembly` or a sequence of cells. Loading writes the image
directly, including into ROM, and sets PC to its origin. For an `assembly`,
the origin comes from `assembly-origin` and its cell width must match the
target memory. A plain sequence starts at `0` unless `:origin` is supplied.

An assembly loaded without `:bank` is kept as a `loaded-program` (its
`loaded-program-assembly`, `-memory` and `-origin`), newest first in
`machine-programs`. `machine-program` is the newest one's assembly, and
`machine-program-at` returns the newest holding an address.[^images]

| Load | Retained programs |
|---|---|
| An assembly | Added; older ones in the same memory it covers wholly are dropped. |
| Raw cells | Those they overlap are dropped. |
| `reset` | Only programs wholly in `:rom` regions (including bank images) stay. |

Snapshots do not save them.

An assembly's [bank images](banked-output.md) load without changing the
current mapping. `:bank n` loads a selected bank without changing the
mapping or PC; it requires the image to fit the banked region. Main-image
output in a window whose mapped bank the assembly also images signals an
error; see [Loading](banked-output.md#loading).

## `step-machine`

```lisp
(step-machine MACHINE &key pc memory)
```

A step decodes at PC, advances PC past the complete instruction, and runs
its semantics. This order lets a branch replace PC. Signed fields reach
semantics as signed integers; relative operands are offsets from the next
instruction. Decode uses the same logic as
[`decode-instruction-at`](disassembler.md#decode-instruction-at).

| Result | Cost | Effect |
| --- | ---: | --- |
| Instruction descriptor | Declared plus extra cycles | Executes semantics. |
| `:decode-failure` | `0` | Leaves PC unchanged. |
| `:nop` | Skipped cells | Skips an undefined opcode under `:nop` policy. |
| `:idle` | `(idle :cycles n)`, default `1` | Ticks devices without fetching or advancing PC. |
| `:privilege-violation` | Cycles spent | Returns PC to the violating instruction after [queuing an interrupt](privilege.md#violations-as-interrupts). |

The selected instruction's cost is added to `machine-cycles`. A storage
fault signals during a direct step; `run` catches it. See
[Stop reasons](#stop-reasons).

### Word-encoded machines

The decoder reads an instruction word, selects a matching descriptor, and
reads any extra cells indicated by its fields. PC advances by the cells
actually consumed. Words up to 16 bits use a dispatch table shared by
machines of that type; wider words scan candidates. The table fills each
entry on its first lookup. See [Word-encoded instructions](word-instructions.md) and
[Memory audit](memory-audit.md).[^decode]

### Device ticking

Each completed instruction ticks live devices with its declared cycle cost
before semantics run. `elapse` ticks them inline. A trapping instruction
still ticks for its declared cost and any `elapse` before the trap; a decode
failure does not tick. See [Devices](devices.md#ticking).

### Interrupt delivery

A pending unmasked interrupt is delivered before fetch. The same step then
executes the handler's first instruction. Delivery has no cost of its own.
See [Interrupts](interrupts.md#delivery).

### Idle steps

An idle machine first checks for an interrupt. If it stays idle, a step
ticks devices for the idle cost and returns `:idle` without changing PC.
See [Interrupts](interrupts.md#waking-an-idle-machine).

An `(idle :cycles n)` clause sets that cost; `n` is a positive integer and
defaults to `1`. A child machine's clause overrides its parent's.

```lisp
(defmachine sleepy
  (register pc :width 8)
  (memory ram :width 8 :addr-width 8)
  (idle :cycles 4))
```

## `run`

```lisp
(run MACHINE &key pc memory (max-steps 10000))
;; => (values reason steps [condition])
```

`run` repeats `step-machine` until it reaches a stop reason. `steps` counts
completed steps and attempted steps that trap or fault; a decode failure
does not count. An idle step counts. Direct stepping still signals faults.

### Stop reasons

| Reason | Meaning |
| --- | --- |
| `:trap` | Semantics called `trap`; the condition is returned. |
| `:fault` | A storage access failed; the condition is returned. |
| `:decode-failure` | No instruction matches the fetched cells. |
| `:idle` | No queued interrupt or live device can wake the idle machine. |
| `:max-steps` | The step budget is reached. |
| `:max-cycles` | The cycle budget is reached in `run-for-cycles`. |
| `:duration` | The duration budget is reached in `run-for-duration`. |

### Error locations

A trap or storage fault raised while an instruction runs records that
instruction's address in `runtime-location-pc`. With a retained program
holding that address it also records `runtime-location-listing-line` and
`runtime-location-source-text`, and the report ends with the location:

```text
Stack underflow on DS (machine STACK-TEST-MACHINE) at $0000 (line 1: add)
```

With a label before the instruction, the report names it as `name+offset`
(`runtime-location-label`), also for data and macro-expanded code:

```text
Trap: :HALT NIL at $0002 <start.next> (line 2: .next: hlt)
```

The label is the nearest earlier `:label`, a local before its global on a
tie, in the bank mapped at `pc`. An offset of zero is omitted.

`pc` is the instruction's start, not the already-advanced program counter.
Without a retained program the report ends `at $0000`. A program loaded at
another `:origin` names lines by that load offset, so relocated code reports
the line it was assembled from.[^relocation]

A host can signal an interrupt or call `wake-machine`, then run again.
Undefined-opcode policy can skip or trap instead of reporting decode failure;
see [Machine families](machine-families.md#undefined-opcodes).

## Cycle costs and clock speed

Each instruction costs `(cycles n)` or `1` by default. A declared
`(clock-speed n)` converts cycles into simulated seconds. It is required
for `run-for-duration` and `machine-elapsed-seconds`, but not for cycle
budgets.

```lisp
(machine-elapsed-seconds MACHINE)
(run-for-cycles MACHINE cycles &key pc memory (max-steps 10000))
(run-for-duration MACHINE seconds &key pc memory (max-steps 10000) throttle)
```

Both budgets are checked **after** each step, so one instruction can cross
the limit. `:throttle t` paces `run-for-duration` against real time;
without it, the duration is a simulated-time budget. See
[`cycles.lisp`](../examples/cycles.lisp).[^timing]

### Per-mode cycle cost

A multi-mode instruction can override the shared `(cycles n)` for an
individual mode; see [Instructions](instructions.md#cycles-n).

### Dynamic cycle costs

Call `(elapse n)` inside semantics for a runtime-dependent cost, such as a
taken branch:

```lisp
(semantics (when (zero? z) (set! pc operand) (elapse 1)))
```

The cost contributes to cycle budgets and elapsed time, and devices receive
it at the call, so a trapping step still ticks them. The [listing](listing.md#rendering-listing-text--print-listing)
marks such instructions with `+` in its cycles column.

## Limitations

- A unified trap/interrupt model is outside this execution model.
  [Privilege violations](privilege.md#violations-as-interrupts) can queue an
  interrupt, but traps stay separate.
- Snapshots carry no retained programs, and the debugger's `save` embeds only
  the newest assembly; see [ticket 369](https://todo.sr.ht/~takeiteasy/lasm/369).

[^decode]: Decode returns the selected `one-of` choices to semantics so
  `choice-case` can dispatch on the form actually encoded. Trailing cells
  are fetched for each decode, including self-modifying programs. Define
  instructions and warm the decoder before a latency-sensitive loop.
[^timing]: `:throttle` uses `trivial-high-precision-timer` and sleeps when
  simulated time leads real time. `machine-cycles` accumulates instruction
  and extra costs; reset clears it.
[^relocation]: The offset is the load origin minus `assembly-origin`. The
  lookup matches only the memory the program was loaded into, and the report
  is only right when the code is position-independent or was assembled for
  its load address. A bank-only load leaves the retained programs alone.
[^images]: Where two programs overlap, the newest wins for that address; the
  older one still answers outside the overlap. A bank image counts only while
  its bank is mapped.
