# Devices

A `(device ...)` clause adds a bus-addressed peripheral, independent of
[memory regions](machine-model.md#memory-regions).

```lisp
(defmachine devfoo
  (register pc :width 16)
  (memory ram :width 8 :addr-width 16)
  (device clock :id #x0001 :version 1 :manufacturer #x1000
    :init clock-init :tick clock-tick))
```

## `defmachine`'s `device` clause

```lisp
(device NAME [:id n] [:version n] [:manufacturer n]
             [:init fn] [:tick fn] [:receive fn] [:detach fn]
             [:save fn] [:load fn])
```

Identity fields default to zero. Hooks are bare function names:

| Hook | Called as | When |
| --- | --- | --- |
| `:init` | `(fn machine device)` | Instantiation or reset; return value becomes device state. |
| `:tick` | `(fn machine device cycles)` | Instruction or extra cycles pass. |
| `:receive` | `(fn machine device)` | `device-send`. |
| `:detach` | `(fn machine device)` | Before detachment. |
| `:save` | `(fn machine device)` | Snapshot capture. |
| `:load` | `(fn machine device data)` | Snapshot restore. |

See [Snapshots](snapshots.md) for device state. A device with no hooks is
still enumerable.

## The bus

Declared devices receive fixed indices in declaration order.
`attach-device` appends another instance or a new host device.
`detach-device` leaves a hole so other indices stay stable.

```lisp
(attach-device machine 'host-sensor :id #x0003 :version 1)
```

| Function | Result |
| --- | --- |
| `device-count` | Bus size, including holes. |
| `device-at` | Device at an index; holes signal `no-such-device`. |
| `find-device` | First live device with a name, or `nil`. |
| `device-info` | ID, version, and manufacturer values. |
| `device-send` | Call the device's `:receive` hook. |
| `tick-devices` | Tick every live device. |

`reset` restores the declared bus and reruns each declared device's
`:init`; runtime attachments disappear.

## Ticking

`step-machine` ticks devices for the instruction's declared cycles,
including a trapping instruction. `extra-cycles` causes a second tick
after semantics returns; a trap skips that second tick. Decode failure
does not tick. See [Emulator](emulator.md#device-ticking).

## Semantics vocabulary

Instruction semantics call bus functions with `machine` explicitly:

```lisp
(multiple-value-bind (id version manufacturer) (device-info machine a)
  (set! a id)
  (set! b version)
  (set! c manufacturer))
```

## Interrupt seam

`device-signal machine device [data]` calls the installed interrupt hook,
or drops the signal when none is installed. Machines with an `(interrupts
...)` clause install the queue hook automatically. See
[Interrupts](interrupts.md).

## Limitations

- A device ticks once for a whole instruction cost; intra-instruction
  timing is unavailable.
- A bus device and a `:device` memory region are independent. One object
  cannot serve as both through a built-in binding.
