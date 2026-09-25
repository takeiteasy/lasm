# Command line

`lasm` assembles, runs, debugs, disassembles and lists programs from the shell. A
machine is defined in a `.lasm` file; the program is ordinary assembly source
(`.asm` or `.s`).

```sh
lasm assemble counter.asm -m sixtyfoo.lasm -o counter.bin
lasm run counter.asm -m sixtyfoo.lasm
```

See [`examples/cli/`](../examples/cli/sixtyfoo.lasm) for a runnable pair.

## Machine files

A `.lasm` file holds the DSL forms — `deflexer`, `defmachine`, `defmode`,
`definstruction`, `defdirective` — read in the `lasm` package, so no
`in-package` line is needed. Definitions live only for one command.

The file's sole machine and sole lexer are used. A file defining several
machines needs `--machine-name`; several lexers need `--lexer`. With no lexer
defined, the default lexer is used.

## Commands

| Command | Does | Options |
| --- | --- | --- |
| `assemble FILE` | writes the assembled program | `-o OUT`, `--format bin\|hex`, `--bank N`, `--region NAME`, `--packing pad\|bits` |
| `run FILE` | assembles, then runs to a stop | `--max-steps N`, `--cycles N`, `--load-snapshot PATH`, `--save-snapshot PATH` |
| `debug FILE` | assembles, then opens the [debugger](debugger.md) | `--break WHERE`, `--commands FILE`, `--history N`, `--load-snapshot PATH`, `--save-snapshot PATH` |
| `disassemble FILE` | disassembles a binary file | `--annotate`, `--data-region START:END`, `--packing pad\|bits`, `--cells N` |
| `listing FILE` | prints the assembly listing | `--symbols`, `--cycle-costs` |

Every command takes `-m FILE` (required), `--machine-name`, `--lexer`,
`--memory` (the memory element to target), `--quiet` (drop assembly
warnings), and `--origin N` (decimal, `$hex`
or `0xhex`; `assemble`, `run`, `listing` and `disassemble`). `-h` prints the
usage.

With [banked output](banked-output.md), `assemble` writes the physical layout;
`--bank N` writes bank `N` alone (`--region` names the region when several
have output), starting at the region's address in HEX. Disassemble a bank's
binary with `--origin` set to the region's start. `listing` shows every bank
with `BB:AAAA` addresses; `run` loads every bank.

`assemble` defaults `OUT` to `FILE` with a `.bin` or `.hex` extension; see
[Binary output](binary-output.md) for the formats. `run` prints the stop reason,
step count and final `pc`. A storage fault or undefined-opcode trap also
prints its condition, ending with the source line; a decode failure prints
the source line at `pc`.
`disassemble` prints re-assemblable source, or with
`--annotate` an address/cells/text listing; it reads cells with the machine's
cell width and endianness. `--data-region` (repeatable; `$hex`, `0xhex` or
decimal bounds, `END` exclusive) renders that address range as `.byte` lines
instead of decoding it; see [Disassembler](disassembler.md#data-regions).

## Debugging

`debug` assembles `FILE`, loads it, and reads debugger commands from standard
input until `quit` or end of input, printing responses to standard output.
It exits 0.

```sh
lasm debug counter.asm -m sixtyfoo.lasm --break .loop --history 100
```

```
Breakpoint 1 at $0002
(lasm-dbg) continue
...
(lasm-dbg) quit
```

| Option | Effect |
| --- | --- |
| `--break WHERE` | Sets a breakpoint before the first prompt. Repeatable; takes what `break` takes, such as `.loop in count` or `main if x == 1`. |
| `--commands FILE` | Runs the commands in `FILE` first, echoing each after the prompt. A `quit` in the file ends the session. |
| `--history N` | Keeps `N` steps for `back` and `reverse-continue`. |

Commands from a pipe work the same way: `echo "continue" | lasm debug ...`.
The [command list](debugger.md#command-dispatcher-and-repl) includes `save
PATH` and `load PATH` for snapshots.

## Snapshots

`--save-snapshot PATH` writes the machine's state to `PATH` when `run` stops
or `debug` ends. `--load-snapshot PATH` loads the program, then restores that
state before running, so `--max-steps` and `--cycles` count from the snapshot.
Both work on `run` and `debug`.

```sh
lasm run counter.asm -m sixtyfoo.lasm --max-steps 5 --save-snapshot s.snap
lasm run counter.asm -m sixtyfoo.lasm --load-snapshot s.snap
```

`FILE` is still assembled, which supplies source lines for faults. A
snapshot that is unreadable or belongs to another machine exits 1; see
[Snapshots](snapshots.md#versioning-and-validation).

## Exit status

| Status | Meaning |
| --- | --- |
| 0 | success |
| 1 | load, assembly or file error, or a `run` ending in a decode failure, storage fault or undefined-opcode trap |
| 2 | usage error |

Diagnostics go to standard error. An error names its source position, for
example `prog.asm:3: No instruction "frobnicate" registered on machine M`,
with the include file or macro body when the statement came from one.
Assembly warnings such as an
[ambiguous mode](diagnostics.md#mode-selection-ambiguity) print as
`prog.asm:3: warning: ...` and do not change the exit status; `--quiet`
drops them. Warnings raised while loading the machine file are never
printed.

## Entry point

```lisp
(run-cli ARGS &key (in *standard-input*) (out *standard-output*) (err *error-output*))
```

Runs the command in `ARGS` (a list of strings, without the program name) and
returns the exit status. `debug` reads its commands from `in`. `lasm.ros` is a thin wrapper around it.

## Building

The script needs [Roswell](https://github.com/roswell/roswell) and, like the
library, `trivial-high-precision-timer` — see
[Getting started](getting-started.md#install).

```sh
ros lasm.ros run examples/cli/counter.asm -m examples/cli/sixtyfoo.lasm
ros build lasm.ros    # standalone ./lasm
```
