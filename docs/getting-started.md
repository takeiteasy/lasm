# Getting started

## Install

LASM is not yet on Quicklisp. Clone it into a location ASDF or Quicklisp can
find, e.g.:

```sh
git clone https://git.sr.ht/~takeiteasy/lasm ~/quicklisp/local-projects/lasm
```

Then, from a Lisp REPL (SBCL):

```lisp
(ql:quickload :lasm)
;; or, without Quicklisp, given lasm.asd is on asdf:*central-registry*:
(asdf:load-system :lasm)
```

## Run the example

```sh
sbcl --script examples/sixtyfoo.lisp
```

This defines a small register machine (`sixtyfoo`, from
[`LASM-plan.md`](../LASM-plan.md) §3.1) and exercises registers, the stack,
memory, and flags through `with-machine`, printing the resulting state. No
instruction set or assembler is involved yet — see
[Semantics vocabulary](semantics.md) for why.

```sh
sbcl --script examples/counter.lisp
```

This defines a syntax with `deflexer`, then tokenizes and parses a
hand-written counter-loop program, printing the resulting tokens and
statement AST. It then defines a small instruction set with `definstruction`
(one addressing mode per instruction), assembles the program (resolving its
`bne .loop` label reference) to bytes, loads them into a fresh machine, and
runs the emulator loop to completion — see [Lexer](lexer.md), [Statement
grammar & expression parser](parser.md), [Instructions](instructions.md),
[Assembler](assembler.md), and [Emulator](emulator.md).

```sh
sbcl --script examples/modes.lisp
```

The same pipeline, but `lda`/`adc` each declare several addressing modes
(`immediate`/`zero-page`/`absolute`/`indexed-x`) via `defmode`, and the
assembler picks which one each operand actually uses — see [Addressing
modes](modes.md) and [Assembler](assembler.md#choosing-a-mode).

```sh
sbcl --script examples/mov.lisp
```

A two-hole addressing mode (`expr "," expr`) wired to a two-field `mov`
instruction — one operand encoding field per hole, named `dst`/`val` so
`(semantics ...)` reads them directly — see [Instructions, "Repeated
`(operand ...)` subclauses"](instructions.md).

```sh
sbcl --script examples/directives.lisp
```

A leading `.org` places the program at a fixed address, `.byte` lays down a
small data table read back through `lda`/`adc`'s `absolute` mode, and `.res`
reserves a zero-filled scratch run — see [Directives](directives.md).

```sh
sbcl --script examples/pc-and-scopes.lisp
```

Two routines each define their own `.loop:` local label without colliding
(scoped to `count_down`/`count_up`), and a `.word *` emits its own address —
see [Assembler, "Location counter"](assembler.md#location-counter) and
[Assembler, "Local-label scoping"](assembler.md#local-label-scoping-16).

## Run the tests

```sh
sbcl --non-interactive \
     --eval '(asdf:load-system :lasm/test)' \
     --eval '(fiveam:run! (quote lasm:lasm))'
```

or, from a REPL:

```lisp
(asdf:load-system :lasm/test)
(fiveam:run! 'lasm:lasm)
```

Note: `(asdf:test-system :lasm/test)` only loads the system — `lasm.asd`
does not define a `test-op` method, so it does not actually invoke
`fiveam:run!`. Use one of the forms above.

## Next

- [Machine model](machine-model.md) for the full `defmachine` clause reference.
- [Semantics vocabulary](semantics.md) for what you can write inside `with-machine`.
- [Lexer](lexer.md) for the full `deflexer` clause reference.
- [Statement grammar & expression parser](parser.md) for `parse` and `parse-expression`.
- [Addressing modes](modes.md) for `defmode` and pattern matching.
- [Instructions](instructions.md) for `definstruction`, encoding, and semantics.
- [Directives](directives.md) for `defdirective`, `.org`, `.byte`/`.word`, `.res`.
- [Assembler](assembler.md) for `assemble`, label resolution, and mode selection.
- [Emulator](emulator.md) for `load-program`, `step-machine`, and `run`.
