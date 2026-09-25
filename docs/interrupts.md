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
            [:stack name] [:queue n] [:on-overflow policy]
            [:mask-when fn] [:mask-flag name] [:cycles n]
            [:drop-on-zero-vector t/nil] [:mask-on-deliver t/nil])
```

| Key | Effect |
| --- | --- |
| `:vector` | Register holding the handler address. |
| `:message` | Register receiving signal data. |
| `:save` | Registers and flags pushed before delivery. |
| `:stack` | Fixed stack or register-backed stack pointer; defaults when unique. |
| `:queue` | Pending-signal capacity, default `256`. |
| `:on-overflow` | Error, trap, drop, or drop oldest. |
| `:mask-when`, `:mask-flag` | Delivery gate; use at most one. |
| `:cycles` | Delivery cost, default `0`. |
| `:drop-on-zero-vector` | Drop signals while vector is zero; default `t`. |
| `:mask-on-deliver` | Set the mask flag before handler execution. |

Registers can be scalar names or indexed bank cells such as `(reg 0)`.
`:save` order determines push order; `interrupt-return` reverses it.

## Raising an interrupt

| Function | Use |
| --- | --- |
| `device-signal machine device [data]` | Signal through the machine's device hook. |
| `signal-interrupt machine data [device]` | Signal directly from software semantics or a host. |

A machine with an interrupt clause installs the queue hook when created.
A software instruction can call `signal-interrupt` inside semantics.

## Overflow

| Policy | When the queue is full |
| --- | --- |
| `:error` (default) | Signal `interrupt-queue-full`. |
| `:trap` | Signal `lasm-trap` tagged `:interrupt-queue-overflow`. |
| `:drop` | Ignore the incoming signal. |
| `:drop-oldest` | Replace the oldest pending signal. |

## Zero vector

With `:drop-on-zero-vector t`, a signal is discarded while the vector
register is zero. Set it to `nil` when a handler legitimately starts at
address zero. Dropped signals never reach the queue.

## Masking

Masking delays delivery; it does not stop enqueueing. A masked queue still
follows its capacity and overflow policy. `:mask-on-deliver` sets the named
mask flag before the handler runs.

## Delivery

A pending unmasked signal is delivered before `step-machine` fetches:

1. Remove the queue's head.
2. Push `:save` places in declared order.
3. Write data to `:message` and handler address to `pc`.
4. Add `:cycles` and tick devices when the cost is nonzero.
5. Fetch and execute the handler's first instruction in the same step.

A signal raised by a device during a step is available on the **next**
step, after that step's delivery check. See [Emulator](emulator.md#interrupt-delivery).

## Register-indexed stacks

`:stack` can name a register declared with `(stack-pointer ...)` instead of
a fixed `(stack ...)`. Delivery and return use the register-backed memory
stack with the same save order. There is no fixed-stack overflow or
underflow condition. Each saved place must fit one memory cell. See
[Machine model](machine-model.md#stacks).

## `interrupt-return`

`(interrupt-return)` restores saved places in reverse order. It requires
an interrupt clause and signals during macroexpansion without one. See
[Semantics vocabulary](semantics.md#operators).

## Waking an idle machine

Interrupt **delivery** clears the idle flag. Enqueueing alone does not,
so a masked machine remains idle until unmasked. A signal raised by an idle
step's device tick wakes it on the next step. A host can call
`wake-machine` directly. See [Emulator](emulator.md#idle-steps).

## `reset`

`reset` clears the queue and idle flag. It leaves the installed interrupt
hook in place as host wiring.

## Limitations

- An idle machine with a zero vector and the default drop policy discards
  every signal, so it cannot wake through interrupts. A diagnostic is
  tracked in [ticket 165](https://todo.sr.ht/~takeiteasy/lasm/165).
- A register-backed stack saves each place in one cell; splitting wider
  places across cells is unavailable.
- Nested-interrupt priority and a unified trap/interrupt model are outside
  this subsystem.
- Delivery does not change the [privilege level](privilege.md#limitations).
