# LASM documentation

- [Getting started](getting-started.md) — install, load, run the example and tests
- [Machine model](machine-model.md) — storage elements and `defmachine`
- [Conditions](conditions.md) — every condition type and its readers
- [Devices](devices.md) — the device bus, `(device ...)`, attach/detach, ticking
- [Interrupts](interrupts.md) — `(interrupts ...)`, delivery, masking, overflow policy
- [Semantics vocabulary](semantics.md) — `with-machine` and the DSL operators
- [Lexer](lexer.md) — `deflexer` and `tokenize`
- [Statement grammar & expression parser](parser.md) — `parse` and `parse-expression`
- [Addressing modes](modes.md) — `defmode` and pattern matching
- [Instructions](instructions.md) — `definstruction`, encoding, semantics
- [Directives](directives.md) — `defdirective`, `.org`, `.byte`/`.word`, `.res`
- [Macros](macros.md) — `.macro`/`.endm`, parameter substitution, expansion
- [Assembler](assembler.md) — `assemble`, label resolution, mode selection, encoded bytes
- [Emulator](emulator.md) — `load-program`, `step-machine`, `run`
- [Disassembler](disassembler.md) — decode cells back to source, round-trip fidelity
- [Listing and source map](listing.md) — retained address/statement mapping, `print-listing`
- [Diagnostics](diagnostics.md) — source-excerpt rendering, mode-mismatch/ambiguity reporting, strict operand range
- [Debugger](debugger.md) — breakpoints, step/continue, register/memory inspection, a reference REPL

This directory covers what exists today. Planned work lives in the
[issue tracker](https://todo.sr.ht/~takeiteasy/lasm); this directory stays
limited to reference documentation for what's implemented.
