# Devices

A `(device ...)` clause declares a peripheral addressed by instruction and
bus index (DCPU-16's `HWN`/`HWQ`/`HWI`, ANIMA-16's device model), independent
of [memory regions](machine-model.md#memory-regions) — a machine can declare
a device without declaring any MMIO region, or the reverse, or both.

```lisp
(defmachine devfoo
  (register pc :width 16)
  (register a :width 16)
  (memory ram :width 8 :addr-width 16)
  (device clock :id #x0001 :version 1 :manufacturer #x1000
          :init clock-init :tick clock-tick))
```

## `defmachine`'s `device` clause

```lisp
(device NAME [:id n] [:version n] [:manufacturer n]
             [:init fn] [:tick fn] [:receive fn] [:detach fn])
```

`:id`/`:version`/`:manufacturer` (each a non-negative integer, defaulting to
0) are the identity triple an `HWQ`-style instruction reads back — an
instruction's own semantics decide which registers each value lands in (see
"Semantics vocabulary" below).

`:init`/`:tick`/`:receive`/`:detach` are all optional hooks, each a function
*designator* — write the bare function name, not `#'name`, for the same
reason a `:device` [region](machine-model.md#memory-regions)'s `:read`/
`:write` are: `defmachine` quotes its whole clause body, so a `#'`-form there
would freeze to the literal list `(function name)` instead of an actual
function.

| Hook | Called as | When |
|---|---|---|
| `:init` | `(fn machine device)` | Once, when the device is instantiated — at `make-machine`, at every `reset`, and (for a runtime-attached device) at `attach-device`. Its return value becomes the device's own `device-state`. |
| `:tick` | `(fn machine device cycles)` | Once per `step-machine`, with that step's own cycle cost — see "Ticking" below. |
| `:receive` | `(fn machine device)` | An `HWI`-style message send (`device-send`). A device with no `:receive` ignores the send. |
| `:detach` | `(fn machine device)` | Just before `detach-device` clears the device's bus slot. |

A device declares none of these and is still enumerable — a `:device` region
with no `:read`/`:write` is the closest existing precedent.

Every device name shares the one machine-wide namespace every other storage
element name, register alias, and region name does — a device colliding with
any of them is a `defmachine`-time error.

## The bus

Every declared device gets a fixed index, in declaration order, on a runtime
`machine`'s bus. `attach-device` appends a device at runtime — either a
second instance of an already-declared device (by name), or a wholly fresh
one with its identity/hooks given inline:

```lisp
(attach-device machine 'host-sensor :id #x0003 :version 1)
```

A fresh name is checked against the machine's whole namespace exactly as a
declared device's is — it may not collide with a register, alias, region, or
another device (declared or already attached).

`detach-device` removes a device but **leaves a hole**: every other device's
index is unaffected, and `device-count` does not shrink. A program that
cached a device's index — the normal `HWN`-once, `HWQ`-by-index-later
pattern — never has that index silently start addressing a different
device after some other device detaches. A vacant or out-of-range index
signals `no-such-device` (`device-at`, `detach-device`, `device-info`,
`device-send`).

| Function | Behavior |
|---|---|
| `attach-device machine name &key id version manufacturer init tick receive detach` | Appends a device, returns its index. |
| `detach-device machine index` | Runs `:detach`, then clears the slot to a hole. |
| `device-at machine index` | The `device` at `index`. Signals `no-such-device` on a hole or out-of-range index. |
| `device-count machine` | Bus size, holes included — the high-water index bound (`HWN`). |
| `find-device machine name` | The first live device named `name`, or `nil`. |
| `device-info machine index` | `(values id version manufacturer)` (`HWQ`). |
| `device-send machine index` | Calls the device's `:receive` (`HWI`). |
| `tick-devices machine cycles` | Calls every live device's `:tick` with `cycles`. |

`reset` restores the bus to its **declared** shape: any runtime-attached
device is dropped, every hole is refilled, and every declared device's
`:init` runs again.

## Ticking

`step-machine` calls `tick-devices` with the executed instruction's own
cycle cost, once per step — including a step whose semantics signal a trap
(device time stays in lockstep with `machine-cycles`, which is incremented
the same way and for the same reason), but not on a decode failure, where
nothing executed and no time elapsed. This runs identically under `run`,
`run-for-cycles`, `run-for-duration`, and the debugger's single-instruction
`debug-step` — every path into execution goes through `step-machine`.

A device ticks once per whole instruction, with that instruction's whole
cost — a device needing intra-instruction resolution can't express it; a
follow-up ticket tracks finer granularity.

## Semantics vocabulary

The bus API is deliberately **not** bound inside `with-machine`/instruction
semantics the way scalar registers are — the same treatment `:memory`
elements get. An `HWN`/`HWQ`/`HWI`-style instruction calls the functions
above directly, `machine` passed explicitly:

```lisp
(definstruction devfoo hwq
  (encoding (opcode #x01))
  (semantics (multiple-value-bind (id version manufacturer) (device-info machine a)
               (set! a id) (set! b version) (set! c manufacturer))))
```

## Interrupt seam

`device-signal machine device &optional data` calls `machine-interrupt-hook`
— a function `(hook machine device data)` installed on a `machine` instance
— when one is installed, and drops the signal otherwise. `reset` leaves the
hook alone; it's host wiring (who the bus signals), not machine state.

## Scope

Delivery, queueing, masking, and overflow policy for interrupts are out of
scope here — `device-signal`/`machine-interrupt-hook` are the seam a real
interrupt subsystem installs itself into, not that subsystem itself.

Serializing device state for a machine snapshot is likewise out of scope —
`machine-devices` and each device's own `device-state` are what a snapshot
feature walks; no on-disk format is defined here.

Binding a `:device` memory region to a declared device — so one device
object is both bus-addressed and memory-mapped — is not supported; a region
and a device remain two independent mechanisms.
