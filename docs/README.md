# LASM documentation

- [Getting started](getting-started.md) — install, load, run the example and tests
- [Machine model](machine-model.md) — storage elements and `defmachine`
- [Semantics vocabulary](semantics.md) — `with-machine` and the DSL operators
- [Lexer](lexer.md) — `deflexer` and `tokenize`
- [Statement grammar & expression parser](parser.md) — `parse` and `parse-expression`
- [Addressing modes](modes.md) — `defmode` and pattern matching
- [Instructions](instructions.md) — `definstruction`, encoding, semantics
- [Directives](directives.md) — `defdirective`, `.org`, `.byte`/`.word`, `.res`
- [Macros](macros.md) — `.macro`/`.endm`, parameter substitution, expansion
- [Assembler](assembler.md) — `assemble`, label resolution, mode selection, encoded bytes
- [Emulator](emulator.md) — `load-program`, `step-machine`, `run`

This directory covers what exists today. For the design rationale and the
milestone roadmap, see [`LASM-plan.md`](../LASM-plan.md) at the repo root —
that document is a working draft, not reference documentation, and may
change independently of what's described here.
