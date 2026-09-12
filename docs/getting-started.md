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

## Run the tests

```sh
sbcl --non-interactive \
     --eval '(asdf:test-system :lasm/test)'
```

or, from a REPL:

```lisp
(asdf:load-system :lasm/test)
(fiveam:run! 'lasm:lasm)
```

## Next

- [Machine model](machine-model.md) for the full `defmachine` clause reference.
- [Semantics vocabulary](semantics.md) for what you can write inside `with-machine`.
