# Interrupts

An `(interrupts ...)` clause declares a machine's interrupt-delivery model
— a vector register, what gets saved and restored around delivery, a
pending-signal queue with a depth and overflow policy, and optional
masking — so a machine can express `INT`/`RFI`-style instructions without
hand-rolling delivery in Lisp semantics.

```lisp
(defmachine intfoo
  (register pc :width 16)
  (register ia :width 16)
  (register a :width 16)
  (stack sp :width 16 :depth 64)
  (flags iaq)
  (memory ram :width 8 :addr-width 16)
  (device clock :init clock-init :tick clock-tick)
  (interrupts :vector ia :message a :save (pc) :mask-flag iaq))
```

## `defmachine`'s `interrupts` clause

```lisp
(interrupts :vector reg :message reg :save (name...)
            [:stack name] [:queue n] [:on-overflow policy]
            [:mask-when fn] [:mask-flag name] [:cycles n]
            [:drop-on-zero-vector t/nil])
```

At most one per machine. `:vector`/`:message`/`:save`/`:stack`/`:mask-flag`
name existing storage elements — validated once every other clause is
known, so they may appear in any order relative to the elements they name.

| Key | Meaning |
|---|---|
| `:vector` | Register holding the handler address. Written into `pc` on delivery. |
| `:message` | Register a delivered signal's data is written into. |
| `:save` | Registers/flags pushed, in order, before `:message`/`:vector` are written. `interrupt-return` (below) pops them in reverse. |
| `:stack` | Which declared `stack` element `:save` pushes onto/pops from. Defaults to the machine's sole one — an error if it declares none or more than one, same as `push`/`pop` with no stack name (see [Semantics vocabulary](semantics.md)). |
| `:queue` | Max pending, undelivered signals. Default 256. |
| `:on-overflow` | Policy when `signal-interrupt` would exceed `:queue` — see "Overflow" below. Default `:error`. |
| `:mask-when` | Function designator `(machine) -> generalized boolean`. At most one of `:mask-when`/`:mask-flag`. |
| `:mask-flag` | A flag name, read the same way. |
| `:cycles` | Delivery's own extra `machine-cycles` cost. Default 0. |
| `:drop-on-zero-vector` | See "Zero vector" below. Default `t`. |

## Raising an interrupt

Two entry points feed the same queue:

- `device-signal machine device &optional data` (see [Devices](devices.md))
  — a device raises its own signal through `machine-interrupt-hook`, which
  `make-machine` auto-installs to the real queue on any machine declaring
  `(interrupts ...)`.
- `signal-interrupt machine data &optional device` — the device-optional
  entry point an `INT`-style instruction's semantics call directly, the
  same convention as the device bus API (`device-info`, `device-send`):
  `machine` passed explicitly, not bound inside `with-machine-bindings`.

```lisp
(definstruction intfoo int
  (encoding (opcode #x01))
  (semantics (signal-interrupt machine a)))
```

## Overflow

Checked when a signal would push the pending queue past its declared
`:queue` depth:

| `:on-overflow` | Behavior |
|---|---|
| `:error` (default) | Signals `interrupt-queue-full`. |
| `:trap` | Signals `lasm-trap` with tag `:interrupt-queue-overflow` — DCPU-16's "catch fire". |
| `:drop` | Discards the incoming signal; the queue is unchanged. |
| `:drop-oldest` | Evicts the queue's head, then enqueues the incoming signal. |

## Zero vector

DCPU-16/ANIMA-16 treat a zero interrupt-vector register as "interrupts are
off". With `:drop-on-zero-vector t` (the default), a signal arriving while
`:vector`'s register currently reads 0 is dropped outright, before it ever
reaches the queue — so the queue can't silently fill (and hit
`:on-overflow`) while a machine is in this state. Set it `nil` on a
machine whose handler legitimately lives at address 0.

Masking (below) can't express this on its own: a masked machine still
queues normally, so a masked-and-zero-vector machine without this knob
could fill its queue and hit `:on-overflow` despite never intending to
receive anything.

## Masking

`:mask-when`/`:mask-flag` gate whether the queue's head is *delivered* on
a given step — not whether a signal may be *enqueued*. A masked machine
still queues incoming signals, subject to `:queue`/`:on-overflow`; it
simply doesn't pop and deliver until unmasked. A machine declaring
neither is never masked.

## Delivery

A pending, unmasked signal is delivered at the very top of `step-machine`
(see [Emulator, "Interrupt delivery"](emulator.md)), before that step's
own fetch — so the same step both delivers the interrupt and executes the
handler's first instruction:

1. Pop the queue's head.
2. Push every `:save` place, in declared order, onto `:stack`.
3. Write the signal's data into `:message`.
4. Set `:vector`'s value into `pc`.
5. Add `:cycles` to `machine-cycles`, and — only when `:cycles` is
   non-zero — tick every device with that cost.
6. Continue into the step's ordinary fetch/decode/execute, now reading
   from the handler.

This happens identically under `run`/`run-for-cycles`/`run-for-duration`
and the debugger's single-instruction `debug-step` — every path into
execution goes through `step-machine`.

A signal raised *during* a step — a device's own `:tick` calling
`device-signal` mid-step (see [Devices, "Ticking"](devices.md#ticking)) —
is queued too late for that same step's own delivery check, which already
ran before the fetch. It delivers on the *next* `step-machine` call
instead, one step later than a signal already queued beforehand.

## `interrupt-return`

```lisp
(definstruction intfoo rfi
  (encoding (opcode #x02))
  (semantics (interrupt-return)))
```

Bound inside `with-machine-bindings` alongside `trap` (see [Semantics
vocabulary](semantics.md)) — pops every `:save` place, in *reverse*
declared order, restoring exactly what delivery pushed. Signals an error
at macroexpansion time on a machine declaring no `(interrupts ...)`
clause.

## Waking an idle machine (#110)

The `idle` semantics primitive ([Semantics vocabulary](semantics.md)) marks
a machine idle — `step-machine` then skips fetch/decode/execute (see
[Emulator, "Idle steps"](emulator.md#idle-steps-110)) until something wakes
it back up.

**Delivery is what wakes it.** `deliver-pending-interrupt` clears the idle
flag as part of delivering, exactly as it pushes state and sets `pc` — so
an idling machine wakes and starts executing its handler in the very same
step. A signal merely reaching the queue does *not* wake it: masking
(above) still applies, so a masked machine keeps idling while signals pile
up, and only wakes once unmasked and delivery actually runs.

**One-step lag, same as any tick-raised signal.** A device's own `:tick`
call to `device-signal` while the machine is idle is queued too late for
that idle step's own delivery check, which already ran before it — same
rule as an ordinary running step (see "Delivery" above). It delivers, and
wakes the machine, on the *next* `step-machine` call.

**The zero-vector footgun.** With the default `:drop-on-zero-vector t`, a
machine that idles while `:vector`'s register currently reads 0 has every
incoming signal dropped at enqueue, before it ever reaches the queue — such
a machine can never wake on its own. (A diagnostic for this is tracked as a
follow-up, #165.)

**No `(interrupts ...)` clause is not an error.** Unlike `interrupt-return`,
`idle` macroexpands fine on any machine — nothing on such a machine can
wake it, but a host can call `wake-machine` (emulator.lisp) directly, and
`run` (see [Emulator](emulator.md#run)) reports `:idle` rather than
spinning to `:max-steps`.

## `reset`

Clears the pending queue unconditionally — it's machine state. #110's idle
flag is the same — `reset` clears it too. The auto-installed hook is host
wiring, same as before #109: `reset` leaves whatever is currently installed
on `machine-interrupt-hook` alone, whether that's the auto-installed
default or something a host replaced it with.

## Scope

Nested-interrupt priority/depth ordering and privilege levels (gating who
may mask or who a handler runs as) are not covered here — see the tracker
for those as separate tickets. `trap` (the M1 halt primitive, see
[Semantics vocabulary](semantics.md)) is untouched by this subsystem; a
unified trap/interrupt/exception model remains future work. CPU idle/sleep
resumed by an interrupt *is* now covered — see "Waking an idle machine"
above.
