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
statement AST. It then defines a small instruction set with `definstruction`,
matches each parsed statement's operand against its addressing mode, encodes
it to bytes, and executes a short instruction sequence against a live
machine — see [Lexer](lexer.md), [Statement grammar & expression
parser](parser.md), and [Instructions](instructions.md). There is still no
full assembler pass or emulator loop: the branch instruction's label operand
is printed as unresolved rather than encoded, since label resolution belongs
to the assembler pass.

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
- [Instructions](instructions.md) for `definstruction`, addressing modes, and encoding.
