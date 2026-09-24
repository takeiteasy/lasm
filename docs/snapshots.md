# Snapshots

`machine-snapshot` captures a machine's runtime state as plain readable data;
`restore-snapshot` puts it back. The same data can be written to and read
from a file. Every machine gets this from its `defmachine` declaration — no
per-machine code, except an optional hook for stateful devices.

| Function | Behavior |
|---|---|
| `machine-snapshot machine` | The snapshot, a list tree. |
| `restore-snapshot machine snapshot` | Replaces `machine`'s state and returns it. |
| `write-snapshot snapshot path` | Writes the snapshot as an s-expression, replacing any existing file. Returns `path`. |
| `read-snapshot path` | The snapshot stored at `path`. |

```lisp
(let ((m (make-machine 'sixtyfoo)))
  (write-snapshot (machine-snapshot m) "save.snap")
  (restore-snapshot m (read-snapshot "save.snap")))
```

## What is saved

The same state `reset` clears:

- every register (banked ones cell by cell), flag, stack (whole backing
  vector and stack pointer) and memory element
- `machine-cycles`, `machine-extra-cycles` and the idle flag
- pending interrupts
- banked regions: the mapped bank and every bank's contents
- the device bus, holes and bus order included

`machine-interrupt-hook` and `machine-access-hook` are host wiring and are neither saved nor changed by a
restore.

Memory is run-length encoded as `(count . value)` runs, so a mostly-empty
address space stays small.

## Versioning and validation

A snapshot carries a version (`+snapshot-version+`, currently 2), the machine
name, the machine's storage layout and its bank layout. `restore-snapshot` checks all three
and the payload itself before it changes anything, so a rejected snapshot
leaves the machine untouched.

| Condition | Signalled when |
|---|---|
| `snapshot-version-mismatch` | The snapshot's version is not `+snapshot-version+`. |
| `snapshot-machine-mismatch` | The snapshot is for another machine, or the storage or bank layout differs. |
| `snapshot-malformed` | The payload is structurally invalid, a value does not fit its cell, or a file is not a readable snapshot. |
| `snapshot-device-unknown` | A saved device is neither declared on the machine nor already attached. |

All four are `snapshot-error`s; `snapshot-error-detail` gives the message.

`read-snapshot` treats the file as untrusted: reader evaluation is off, and
anything unreadable signals `snapshot-malformed`.

## Devices

A device's `device-state` is whatever its `:init` hook returned, so a device
opts into snapshots with two more [`device`](devices.md) hooks:

| Hook | Called as | When |
|---|---|---|
| `:save` | `(fn machine device)` | At `machine-snapshot`. Returns the device's state as readable data. |
| `:load` | `(fn machine device data)` | At `restore-snapshot`, on a freshly `:init`'d device, with what `:save` returned. |

A device without both hooks is re-`:init`'d on restore and carries no saved
state.

Restore rebuilds the bus at its saved shape. Holes stay holes and every
device keeps its index. A device attached at runtime with `attach-device`
must already be on the target machine's bus to be restored.

Interrupt signal data (the `data` given to `signal-interrupt` or
`device-signal`) is stored as-is and must be readable by `read` to survive a
file round trip.
