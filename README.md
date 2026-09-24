# LASM — Lisp Assembly

A Common Lisp library and DSL for building **fantasy assemblers and CPU
emulators**: given a declarative spec of a machine's storage, instruction
encoding, and semantics, LASM generates a lexer/parser for its assembly
syntax, an assembler, and an emulator. It targets custom ("fantasy")
architectures, not real silicon.

## Documentation

- [Machine model](docs/machine-model.md) — storage elements, `defmachine`
- [Machine families](docs/machine-families.md) — extending a machine, removing instructions, undefined opcodes
- [Devices](docs/devices.md) — the device bus, attach/detach, ticking
- [Interrupts](docs/interrupts.md) — `(interrupts ...)`, delivery, masking, overflow
- [Semantics vocabulary](docs/semantics.md) — `with-machine`, `set!`, `push`/`pop`
- [Lexer](docs/lexer.md) — `deflexer`, `tokenize`
- [Statement grammar & expression parser](docs/parser.md) — `parse`, `parse-expression`
- [Addressing modes](docs/modes.md) — `defmode`, pattern matching
- [Instructions](docs/instructions.md) — `definstruction`, encoding, semantics
- [Directives](docs/directives.md) — `defdirective`, data, layout, `.equ` and `.set`
- [Macros](docs/macros.md) — `.macro`/`.endm`, parameter substitution, expansion
- [Includes](docs/includes.md) — `.include`, path resolution, nesting, cycle guard
- [Assembler](docs/assembler.md) — `assemble`, label resolution, mode selection, encoded bytes
- [Binary output](docs/binary-output.md) — `write-binary`, `write-intel-hex`
- [Command line](docs/cli.md) — `lasm` assemble/run/disassemble/listing
- [Snapshots](docs/snapshots.md) — `machine-snapshot`/`restore-snapshot`, versioned snapshot files
- [Emulator](docs/emulator.md) — `load-program`, `step-machine`, `run`
- [Disassembler](docs/disassembler.md) — decode cells back to source, round-trip fidelity
- [Listing and source map](docs/listing.md) — retained address/statement mapping, `print-listing`, scope-aware symbol table
- [Getting started](docs/getting-started.md) — install, run, test

## Quickstart

```lisp
(asdf:load-system :lasm)

(in-package #:lasm)

(defmachine sixtyfoo
  (register a :width 8)
  (stack s :width 8 :depth 256)
  (memory ram :width 8 :addr-width 16)
  (flags z n c v))

(with-machine (m sixtyfoo)
  (set! a 42)
  (push a s))
```

See [`examples/sixtyfoo.lisp`](examples/sixtyfoo.lisp) for a runnable version.

## License

```text
LASM

Copyright (C) 2026 George Watson

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program. If not, see <https://www.gnu.org/licenses/>.
```
