# LASM documentation

- [Getting started](getting-started.md) — install, load, run the example and tests
- [Memory audit](memory-audit.md) — measured build, runtime and per-CPU costs
- [Machine model](machine-model.md) — storage elements and `defmachine`
- [Machine families](machine-families.md) — `(:extends ...)`, inherited instructions, removal, undefined-opcode policy
- [Conditions](conditions.md) — every condition type and its readers
- [Devices](devices.md) — the device bus, `(device ...)`, attach/detach, ticking
- [Interrupts](interrupts.md) — `(interrupts ...)`, delivery, masking, overflow policy
- [Semantics vocabulary](semantics.md) — `with-machine` and the DSL operators
- [Lexer](lexer.md) — `deflexer` and `tokenize`
- [Statement grammar & expression parser](parser.md) — `parse` and `parse-expression`
- [Addressing modes](modes.md) — `defmode` and pattern matching
- [Instructions](instructions.md) — `definstruction`, encoding, semantics
- [Directives](directives.md) — `defdirective`, data, layout, `.equ` and `.set`
- [Macros](macros.md) — `.macro`/`.endm`, parameter substitution, expansion
- [Conditional assembly](conditionals.md) — `.if`/`.ifdef`/`.elseif`/`.else`/`.endif`, constant conditions
- [Assertions](assertions.md) — `.assert` and `.error`
- [Includes](includes.md) — `.include`, path resolution, nesting, cycle guard
- [Assembler](assembler.md) — `assemble`, label resolution, mode selection, encoded bytes
- [Snapshots](snapshots.md) — `machine-snapshot`/`restore-snapshot`, versioned snapshot files
- [Emulator](emulator.md) — `load-program`, `step-machine`, `run`
- [Disassembler](disassembler.md) — decode cells back to source, round-trip fidelity
- [Listing and source map](listing.md) — retained address/statement mapping, `print-listing`
- [Binary output](binary-output.md) — raw binary and Intel HEX files from an `assembly`
- [Banked output](banked-output.md) — `.bank`, `bank(label)`, bank images, banked listings and loading
- [Command line](cli.md) — `lasm assemble`/`run`/`disassemble`/`listing`
- [Diagnostics](diagnostics.md) — source-excerpt rendering, mode-mismatch/ambiguity reporting, strict operand range
- [Debugger](debugger.md) — breakpoints, step/continue, register/memory inspection, a reference REPL

This directory covers what exists today. Planned work lives in the
[issue tracker](https://todo.sr.ht/~takeiteasy/lasm); this directory stays
limited to reference documentation for what's implemented.
