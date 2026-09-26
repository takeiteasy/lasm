# Snapshots

`machine-snapshot` captures a machine's runtime state as plain readable data;
`restore-snapshot` puts it back. The same data can be written to and read
from a file. Each `defmachine` provides these operations. Stateful devices can add hooks.

| Function | Behavior |
|---|---|
| `machine-snapshot machine &key assembly` | The snapshot, a list tree. `assembly` [embeds the program](#embedded-programs). |
| `restore-snapshot machine snapshot` | Replaces `machine`'s state and returns it. |
| `write-snapshot snapshot path &key format` | Writes the snapshot, replacing any existing file. `format` is `:sexp` (default) or `:binary`. Returns `path`. Signals [`snapshot-unwritable`](#snapshot-data) before touching `path` for data it cannot store. |
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
- `machine-cycles` and the idle flag
- pending interrupts with their priorities, and the running-handler depth
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
and `load` take either.

## Snapshot data

Files hold only plain data. `write-snapshot` checks all of it, including
device `:save` state and interrupt signal data, before opening the file.

| Allowed | Notes |
|---|---|
| `nil`, `t`, integers, ratios, finite floats | Infinities and NaN are rejected, as is a number that needs more than 66000 bits.[^number-size] |
| characters, strings | |
| symbols | Read back only when the symbol already exists in the reading image.[^symbols] |
| conses, simple vectors | Shared structure is written as copies. |

Anything else, or a list or vector that contains itself, signals
`snapshot-unwritable` and leaves an existing file at `path` as it was.
`(let ((l (list 1))) (setf (cdr l) l) (write-snapshot l "x.snap"))` is
rejected.

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

## Reading

The text reader is shared with [item programs](items.md#lasm-files): a restricted
[Eclector](https://github.com/s-expressionists/Eclector) reader, `read-restricted-form`.
An [items program](items.md) assembled from a file is embedded as the source
text it renders as.

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
| `snapshot-unwritable` | `write-snapshot` is given circular data or a value outside [the allowed data](#snapshot-data). |
| `snapshot-device-unknown` | A saved device is neither declared on the machine nor already attached. |

All four are `snapshot-error`s; `snapshot-error-detail` gives the message.

`read-snapshot` treats the file as untrusted. It never evaluates, never
interns a symbol, and accepts no `#` syntax beyond `#\`, `#(` and `#:`, so `#.`,
`#S`, `#P` and `#n=` are rejected. A symbol that does not exist,
lists nested deeper than 1000, or a float too large to represent (`1d999999999`)
signals `snapshot-malformed`, as does anything unreadable. A float too small to
represent (`1d-999999999`) reads as a signed zero.[^float-bound] A numeric token
longer than 20000 characters is malformed too. A binary file that is
truncated, mis-tagged, nested too deeply or names an unknown package or symbol
is malformed too, and a binary format this lasm does not read signals
`snapshot-version-mismatch`. A binding to a missing
device or an unbindable region is malformed too.
`snapshot-assembly` signals `snapshot-malformed` for a damaged `:program` and
`snapshot-machine-mismatch` for another machine's.

## Devices

A device's `device-state` is whatever its `:init` hook returned, so a device
opts into snapshots with two more [`device`](devices.md) hooks:

| Hook | Called as | When |
|---|---|---|
| `:save` | `(fn machine device)` | At `machine-snapshot`. Returns the device's state as [snapshot data](#snapshot-data). |
| `:load` | `(fn machine device data)` | At `restore-snapshot`, on a freshly `:init`'d device, with what `:save` returned. |

A device without both hooks is re-`:init`'d on restore and carries no saved
state.

Restore rebuilds the bus at its saved shape. Holes stay holes and every
device keeps its index. A device attached at runtime with `attach-device`
must already be on the target machine's bus to be restored.

Interrupt signal data (the `data` given to `signal-interrupt` or
`device-signal`) is stored as-is and must be [snapshot data](#snapshot-data) to
be written to a file.

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
    | `09` | any other atom, such as a vector: its printed form as a string, read with the text reader |

    Names and strings are length-prefixed UTF-8. The reader rejects lists nested deeper than 1000.

[^symbols]: The text reader is [Eclector](https://github.com/s-expressionists/Eclector)
    with symbol lookup by `find-symbol` only. A snapshot naming a symbol from a
    system that is not loaded is malformed. Load the machine's system before
    reading its snapshot.

[^float-bound]: Only whole tokens shaped like a float with an exponent marker are checked; `A1E12345` is a symbol. An exponent beyond 324 plus the token length is out of range.

[^number-size]: The text reader accepts numeric tokens up to 20000 characters, so `write-snapshot` rejects an integer, or a ratio's numerator and denominator together, above 66000 bits (about 19900 digits).
