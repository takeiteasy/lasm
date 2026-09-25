# Devices

A `(device ...)` clause adds a bus-addressed peripheral. A `:device`
[memory region](machine-model.md#memory-regions) can bind to it, so one
object is both enumerated and memory-mapped.

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
             [:save fn] [:load fn] [:read fn] [:write fn])
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
| `:read` | `(fn machine device address)` | `mref` in a [bound region](#memory-mapped-devices). |
| `:write` | `(fn machine device address value)` | `(setf mref)` in a bound region. |

See [Snapshots](snapshots.md) for device state. A device with no hooks is
still enumerable.

## The bus

Declared devices receive fixed indices in declaration order.
`attach-device` appends another instance or a new host device; it takes the
same keywords as the clause, including `:read`/`:write`.
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

## Memory-mapped devices

A `:device` region with `:device NAME` routes `mref` through that declared
device's `:read` and `:write` hooks. The address is absolute, and `:write`
receives the cell-width-wrapped value.

```lisp
(defmachine devfoo
  (register pc :width 16)
  (memory ram :width 8 :addr-width 16
    (region io #xff00 #xff0f :kind :device :device latch))
  (device latch :id 9 :init latch-init :read latch-read :write latch-write))
```

| Case | Result |
| --- | --- |
| No `:read` / `:write` hook | Reads `0`; writes discarded. |
| Device detached | Reads `0`; writes discarded. |
| After `reset` | Binding restored to the freshly initialised device. |
| `mpeek`, `%poke` | Skip the hooks. |
| Debugger `step-back` | Needs the device's `:save`/`:load`; see [Debugger](debugger.md). |

`:device` cannot be combined with the region's own `:read`/`:write`, and
applies only to `:device` regions. The name must be a declared device.

### Binding at runtime

`bind-region` routes a `:device` region that has no `:read`/`:write` of its
own through any live device by name, declared or attached. It overrides a
declared `:device` binding; `unbind-region` restores it. Give
`attach-device` the `:read`/`:write` hooks inline.

```lisp
(attach-device m 'sensor :read 'sensor-read :write 'sensor-write)
(bind-region m 'io 'sensor)
(unbind-region m 'io)
```

The binding follows the device's bus index, so detaching the device leaves
the region open bus. `reset` drops runtime bindings with runtime devices;
[snapshots](snapshots.md) save them.

## Ticking

`step-machine` ticks devices for the instruction's declared cycles before
semantics run, including a trapping instruction. Devices stay in lockstep
with `machine-cycles`.

| Source | Tick |
| --- | --- |
| Declared `(cycles n)` | Before the semantics body. |
| `(elapse n)` | At the call, mid-body. |
| Interrupt delivery, idle | Once, whole. |

Decode failure does not tick. See [Emulator](emulator.md#device-ticking).

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

- Interrupt delivery and idle steps tick devices once for their whole cost;
  they have no body to subdivide.
