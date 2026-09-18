# LASM — Lisp Assembly

A Common Lisp library and DSL for building **fantasy assemblers and CPU
emulators**: given a declarative spec of a machine's storage, instruction
encoding, and semantics, LASM generates a lexer/parser for its assembly
syntax, an assembler, and an emulator. It targets custom ("fantasy")
architectures, not real silicon.

## Documentation

- [Machine model](docs/machine-model.md) — storage elements, `defmachine`
- [Devices](docs/devices.md) — the device bus, attach/detach, ticking
- [Interrupts](docs/interrupts.md) — `(interrupts ...)`, delivery, masking, overflow
- [Semantics vocabulary](docs/semantics.md) — `with-machine`, `set!`, `push`/`pop`
- [Lexer](docs/lexer.md) — `deflexer`, `tokenize`
- [Statement grammar & expression parser](docs/parser.md) — `parse`, `parse-expression`
- [Addressing modes](docs/modes.md) — `defmode`, pattern matching
- [Instructions](docs/instructions.md) — `definstruction`, encoding, semantics
- [Directives](docs/directives.md) — `defdirective`, `.org`, `.byte`/`.word`, `.res`
- [Macros](docs/macros.md) — `.macro`/`.endm`, parameter substitution, expansion
- [Assembler](docs/assembler.md) — `assemble`, label resolution, mode selection, encoded bytes
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

GPLv3. LASM is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option)
any later version. See [`LICENSE`](LICENSE) for the full text.
