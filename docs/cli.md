# Command line

`lasm` assembles, runs, disassembles and lists programs from the shell. A
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
| `assemble FILE` | writes the assembled program | `-o OUT`, `--format bin\|hex`, `--bank N`, `--region NAME` |
| `run FILE` | assembles, then runs to a stop | `--max-steps N`, `--cycles N` |
| `disassemble FILE` | disassembles a binary file | `--annotate`, `--data-region START:END` |
| `listing FILE` | prints the assembly listing | `--symbols` |

Every command takes `-m FILE` (required), `--machine-name`, `--lexer`,
`--memory` (the memory element to target), and `--origin N` (decimal, `$hex`
or `0xhex`; `assemble`, `run`, `listing` and `disassemble`). `-h` prints the
usage.

With [banked output](banked-output.md), `assemble` writes the physical layout;
`--bank N` writes bank `N` alone (`--region` names the region when several
have output), starting at the region's address in HEX. Disassemble a bank's
binary with `--origin` set to the region's start. `listing` shows every bank
with `BB:AAAA` addresses; `run` loads every bank.

`assemble` defaults `OUT` to `FILE` with a `.bin` or `.hex` extension; see
[Binary output](binary-output.md) for the formats. `run` prints the stop reason,
step count and final `pc`; a storage fault also prints its condition.
`disassemble` prints re-assemblable source, or with
`--annotate` an address/cells/text listing; it reads cells with the machine's
cell width and endianness. `--data-region` (repeatable; `$hex`, `0xhex` or
decimal bounds, `END` exclusive) renders that address range as `.byte` lines
instead of decoding it; see [Disassembler](disassembler.md#data-regions).

## Exit status

| Status | Meaning |
| --- | --- |
| 0 | success |
| 1 | load, assembly or file error, or a `run` ending in a decode failure, storage fault or undefined-opcode trap |
| 2 | usage error |

Diagnostics go to standard error.

## Entry point

```lisp
(run-cli ARGS &key (out *standard-output*) (err *error-output*))
```

Runs the command in `ARGS` (a list of strings, without the program name) and
returns the exit status. `lasm.ros` is a thin wrapper around it.

## Building

The script needs [Roswell](https://github.com/roswell/roswell) and, like the
library, `trivial-high-precision-timer` — see
[Getting started](getting-started.md#roswell).

```sh
ros lasm.ros run examples/cli/counter.asm -m examples/cli/sixtyfoo.lasm
ros build lasm.ros    # standalone ./lasm
```
