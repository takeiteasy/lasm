# LASM — Lisp Assembly

A Common Lisp library and DSL for building **fantasy assemblers and CPU
emulators**: given a declarative spec of a machine's storage, instruction
encoding, and semantics, LASM generates a lexer/parser for its assembly
syntax, an assembler, and an emulator. It targets custom ("fantasy")
architectures, not real silicon.

## Documentation

Start with [Getting started](docs/getting-started.md), then browse the
[documentation index](docs/README.md) for machine definitions, assembly,
emulation and tools.

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

## Examples

Full emulators packaged as ASDF systems are listed in [Examples](docs/examples.md).

## License

Copyright (C) 2026 George Watson. LASM is licensed under
[GPL-3.0-or-later](LICENSE).
