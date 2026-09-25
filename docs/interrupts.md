# Interrupts

An `(interrupts ...)` clause declares queueing, masking, and delivery for
a machine. Devices and software instructions can signal the same queue.

```lisp
(defmachine intfoo
  (register pc :width 16)
  (register ia :width 16)
  (register a :width 16)
  (stack sp :width 16 :depth 64)
  (memory ram :width 8 :addr-width 16)
  (interrupts :vector ia :message a :save (pc)))
```

## `defmachine`'s `interrupts` clause

```lisp
(interrupts :vector reg :message reg :save (name...)
            [:nmi-vector place] [:stack name] [:queue n] [:on-overflow policy]
            [:mask-when fn] [:mask-flag name]
            [:mask-level place] [:mask-level-when fn] [:mask-level-on-deliver t/nil]
            [:cycles n]
            [:drop-on-zero-vector t/nil] [:mask-on-deliver t/nil]
            [:nesting :allow/:priority] [:max-depth n]
            [:deliver-level level])
```

| Key | Effect |
| --- | --- |
| `:vector` | Register holding the handler address. |
| `:nmi-vector` | Register holding the handler address for [non-maskable signals](#non-maskable-signals); defaults to `:vector`. |
| `:message` | Register receiving signal data. |
| `:save` | Registers and flags pushed before delivery. |
| `:stack` | Fixed stack or register-backed stack pointer; defaults when unique. |
| `:queue` | Pending-signal capacity, default `256`. |
| `:on-overflow` | Error, trap, drop, or drop oldest. |
| `:mask-when`, `:mask-flag` | Delivery gate; use at most one. |
| `:mask-level`, `:mask-level-when` | Priority threshold: a register, or a function of the machine; use at most one. See [Level masking](#level-masking). |
| `:mask-level-on-deliver` | Set the `:mask-level` register to the delivered signal's priority. |
| `:cycles` | Delivery cost, default `0`. |
| `:drop-on-zero-vector` | Drop signals while their vector is zero; default `t`. |
| `:mask-on-deliver` | Set the mask flag before handler execution. |
| `:nesting` | `:allow` (default) or `:priority`; see [Nesting](#nesting). |
| `:max-depth` | Cap on running handlers; default unlimited. |
| `:deliver-level` | [Privilege level](privilege.md#interrupt-delivery) the handler runs at. |

Registers can be scalar names or indexed bank cells such as `(reg 0)`.
`:save` order determines push order; `interrupt-return` reverses it.

## Raising an interrupt

| Function | Use |
| --- | --- |
| `device-signal machine device [data]` | Signal through the machine's device hook. |
| `signal-interrupt machine data [:device d] [:priority n] [:non-maskable t/nil]` | Signal directly from software semantics or a host. |

| Function | Use |
| --- | --- |
| `machine-interrupt-pending-count machine` | Number of pending signals. |
| `map-pending-interrupts fn machine` | Call `fn` with `device data priority non-maskable` for each pending signal, in delivery order. |

Pending signals deliver by priority, then arrival order within a priority.

A machine with an interrupt clause installs the queue hook when created.
A software instruction can call `signal-interrupt` inside semantics.

## Overflow

| Policy | When the queue is full |
| --- | --- |
| `:error` (default) | Signal `interrupt-queue-full`. |
| `:trap` | Signal `lasm-trap` tagged `:interrupt-queue-overflow`. |
| `:drop` | Ignore the incoming signal. |
| `:drop-oldest` | Evict the oldest signal of the lowest [priority](#priority), [maskable](#non-maskable-signals) signals first; the incoming signal is dropped instead when it ranks below every candidate. A non-maskable incoming signal always displaces a maskable one. |

## Priority

Each signal has an integer priority; higher delivers first, and signals of
equal priority deliver in arrival order. A device signal takes its
device's `:priority`; a software signal takes the `priority` argument.
Both default to `0`.

```lisp
(device disk :priority 3)
(signal-interrupt machine data :priority 5)   ; software signal, priority 5
```

## Nesting

Machines track running handlers only when `(interrupts ...)` declares
`:nesting :priority` or `:max-depth`. Delivery starts a handler and
`interrupt-return` ends the innermost one; `machine-interrupt-depth`
reports how many run.

| Key | A pending signal waits while |
| --- | --- |
| `:nesting :allow` | Never held by depth (default). |
| `:nesting :priority` | A handler runs and the signal does not strictly outrank it. |
| `:max-depth n` | `n` handlers already run; `1` forbids nesting. |

```lisp
(interrupts :vector ia :message a :save (pc) :nesting :priority :max-depth 4)
```

## Zero vector

With `:drop-on-zero-vector t`, a signal is discarded while the vector
register is zero. Set it to `nil` when a handler legitimately starts at
address zero. Dropped signals never reach the queue.

## Masking

Masking delays delivery; it does not stop enqueueing. A masked queue still
follows its capacity and overflow policy. `:mask-on-deliver` sets the named
mask flag before the handler runs.

A maskable signal delivers only when `:mask-flag` or `:mask-when` allows it
and it clears the [level mask](#level-masking). Delivery skips a masked
signal and takes the highest-priority one that is not masked.

### Level masking

`:mask-level` names a register (or `(NAME INDEX)` cell); `:mask-level-when`
names a function returning an integer. A signal delivers only when its
priority is **strictly greater** than that level, so a level of `0` still
holds back priority-`0` signals.

```lisp
(interrupts :vector ia :message a :save (pc ipl) :mask-level ipl
            :mask-level-on-deliver t)
```

`:mask-level-on-deliver` writes the delivered priority to the register after
the `:save` places are read, so a handler masks its own level and lower.
Listing the register in `:save` makes `interrupt-return` restore it. It
requires `:mask-level`.

### Non-maskable signals

A non-maskable signal ignores `:mask-flag`, `:mask-when` and the level mask.
It still obeys [nesting](#nesting) and the queue's overflow policy.

| Source | Marked by |
| --- | --- |
| Device | `(device NAME :non-maskable t)` |
| Software or host | `(signal-interrupt machine data :non-maskable t)`; the keyword overrides a device's default. |
| Privilege violation | `:on-violation (:interrupt DATA :non-maskable t)`; see [Violations as interrupts](privilege.md#violations-as-interrupts). |

With `:nmi-vector`, a non-maskable signal jumps through that register and
`:drop-on-zero-vector` checks it; other signals use `:vector`.

```lisp
(interrupts :vector irq :nmi-vector nmi :message a :save (pc))
```

## Delivery

A pending unmasked signal is delivered before `step-machine` fetches:

1. Take the highest-priority pending signal that is not [masked](#masking),
   unless [nesting](#nesting) holds it.
2. Read the `:save` places, then switch to `:deliver-level` if declared.
3. Push the values read, in declared order.
4. Write data to `:message` and handler address to `pc`.
5. Add `:cycles` and tick devices when the cost is nonzero.
6. Fetch and execute the handler's first instruction in the same step.

A violation while pushing reports the interrupted instruction's location.

A signal raised by a device during a step is available on the **next**
step, after that step's delivery check. See [Emulator](emulator.md#interrupt-delivery).

## Register-indexed stacks

`:stack` can name a register declared with `(stack-pointer ...)` instead of
a fixed `(stack ...)`. Delivery and return use the register-backed memory
stack with the same save order. There is no fixed-stack overflow or
underflow condition. Each saved place must fit one memory cell. See
[Machine model](machine-model.md#stacks).

## `interrupt-return`

`(interrupt-return)` restores saved places in reverse order, the
[privilege level](privilege.md#interrupt-delivery) last. It requires
an interrupt clause and signals during macroexpansion without one. See
[Semantics vocabulary](semantics.md#operators).

## Waking an idle machine

Interrupt **delivery** clears the idle flag. Enqueueing alone does not,
so a masked machine remains idle until unmasked. A signal raised by an idle
step's device tick wakes it on the next step. A host can call
`wake-machine` directly. See [Emulator](emulator.md#idle-steps).

## `reset`

`reset` clears the queue, handler depth, and idle flag. It leaves the installed interrupt
hook in place as host wiring.

## Limitations

- An idle machine with a zero vector and the default drop policy discards
  every signal, so it cannot wake through interrupts. A diagnostic is
  tracked in [ticket 165](https://todo.sr.ht/~takeiteasy/lasm/165).
- A register-backed stack saves each place in one cell; splitting wider
  places across cells is unavailable.
- Handler depth unwinds only through `interrupt-return`; a handler that
  leaves another way keeps its depth raised until `reset`.
- Vectors are registers; memory-resident vectors are unavailable; see
  [ticket 313](https://todo.sr.ht/~takeiteasy/lasm/313).
- Queue operations cost O(distinct pending priorities); see
  [ticket 312](https://todo.sr.ht/~takeiteasy/lasm/312).
- The debugger does not display pending priorities or handler depth; see
  [ticket 306](https://todo.sr.ht/~takeiteasy/lasm/306).
- A unified trap/interrupt model is outside this subsystem.
