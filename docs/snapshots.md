# Snapshots

`machine-snapshot` captures a machine's runtime state as plain readable data;
`restore-snapshot` puts it back. The same data can be written to and read
from a file. Each `defmachine` provides these operations. Stateful devices can add hooks.

| Function | Behavior |
|---|---|
| `machine-snapshot machine &key assembly` | The snapshot, a list tree. `assembly` [embeds the program](#embedded-programs). |
| `restore-snapshot machine snapshot` | Replaces `machine`'s state and returns it. |
| `write-snapshot snapshot path &key format` | Writes the snapshot, replacing any existing file. `format` is `:sexp` (default) or `:binary`. Returns `path`. |
| `read-snapshot path` | The snapshot stored at `path`, in either format. |
| `snapshot-assembly snapshot &key machine` | The assembly rebuilt from the [embedded program](#embedded-programs), or `nil` when there is none. |

```lisp
(let ((m (make-machine 'sixtyfoo)))
  (write-snapshot (machine-snapshot m) "save.snap")
  (restore-snapshot m (read-snapshot "save.snap")))
```

The [command line](cli.md#snapshots) saves and restores snapshots with
`run` and `debug` `--save-snapshot` and `--load-snapshot`, and can resume
from one without the source file; the
[debugger](debugger.md#command-dispatcher-and-repl) has `save` and `load`
commands.

## What is saved

Snapshots hold:

- every register (banked ones cell by cell), flag, stack (whole backing
  vector and stack pointer) and memory element
- `machine-cycles`, `machine-extra-cycles` and the idle flag
- pending interrupts
- banked regions: the mapped bank, every bank's contents, and the bank
  `load-program` wrote the main image into
- the device bus, holes and bus order included
- runtime [`bind-region`](devices.md#binding-at-runtime) bindings

`machine-interrupt-hook` and `machine-access-hook` are host wiring and are neither saved nor changed by a
restore.

Memory is run-length encoded as `(count . value)` runs, so a mostly-empty
address space stays small.

## File formats

| Format | `write-snapshot` | Use |
|---|---|---|
| `:sexp` | default | Readable and diffable. |
| `:binary` | `:format :binary` | Smaller and faster to read for large or dense memory.[^binary] |

Both encode the same data and carry the same snapshot version.
`read-snapshot` tells them apart by the file's first bytes, so `restore-snapshot`
and `load` take either. Device `:save` data is written in either format as
readable data.

## Embedded programs

A snapshot made with `:assembly` carries a `:program` entry: the source text
of the file the assembly came from and of every file it `.include`s, plus the
origin, memory element and lexer it was assembled with.
`snapshot-assembly` reassembles that text, so a program can resume from the
snapshot alone. An `.include` there reads only the embedded files, never the
disk.

```lisp
(write-snapshot (machine-snapshot m :assembly (assemble-file "prog.asm" :machine 'sixtyfoo))
                "prog.snap")
(snapshot-assembly (read-snapshot "prog.snap"))   ; prog.asm need not exist
```

Only an assembly from `assemble-file` has a file to embed; one from
`assemble` on a string does not. `restore-snapshot` ignores `:program`, and a
snapshot without one still restores.

## Versioning and validation

A snapshot carries a version (`+snapshot-version+`), machine name, storage
layout and bank layout. `restore-snapshot` checks all three
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
anything unreadable signals `snapshot-malformed`. A binary file that is
truncated, mis-tagged, nested too deeply or names an unknown package is
malformed too, and a binary format this lasm does not read signals
`snapshot-version-mismatch`. A binding to a missing
device or an unbindable region is malformed too.
`snapshot-assembly` signals `snapshot-malformed` for a damaged `:program` and
`snapshot-machine-mismatch` for another machine's.

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

[^binary]: A binary file is the 8 bytes `89 4C 53 4E 50 0D 0A 1A`, a format
    byte (`1`), then one node. Each node starts with a tag byte:

    | Tag | Node |
    |---|---|
    | `00`, `01` | `nil`, `t` |
    | `80`–`FF` | the integer 0–127 |
    | `02`, `03` | a non-negative or negative integer, as a base-128 varint |
    | `04` | a keyword: its name |
    | `05` | any other symbol: package name, then symbol name |
    | `06` | a string: byte length, then UTF-8 |
    | `07`, `08` | a proper or dotted list: count, then that many nodes (and the tail for `08`) |
    | `09` | any other atom: its printed form as a string, read back without evaluation |

    Names and strings are length-prefixed UTF-8. The reader rejects lists nested deeper than 1000.
