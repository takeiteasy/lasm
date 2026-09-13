# Semantics vocabulary

`with-machine` is the single entry point for writing machine semantics
against a `defmachine`-declared machine:

```lisp
(with-machine (m sixtyfoo)
  (set! a 42)
  (push a s)
  (set-flags! (z (zero? a)) (n (bit-set? a 7)))
  (setf (mref m 'ram #x1000) 1))
```

`(with-machine (machine-var machine-name) &body body)` instantiates a fresh
runtime machine (`(make-machine 'machine-name)`, bound to `machine-var`) and
evaluates `body` with:

- every **scalar** register and flag of the machine bound as a symbol, e.g.
  `a` reads/sets the `a` register directly (via `symbol-macrolet`, so
  `(set! a (+ a 1))` and plain `a` both work). Banked registers (`:count >
  1`) are not bound this way — see [Machine model](machine-model.md).
- the operators below, available as local macros for the extent of `body`.
- stacks and memory accessed by name through the operators/accessors
  directly (`push`/`pop` and `mref`), since they take an explicit operand.

## Operators

- `(set! place value)` — `(setf place value)`. Works on any bound register/
  flag symbol, or any other `setf`-able place (e.g. `(mref m 'ram addr)`).
- `(push value stack-name)` — push `value` onto the named stack.
- `(pop stack-name)` — pop and return the top of the named stack.
- `(set-flags! (flag-name form)...)` — set each named flag to the result of
  evaluating `form`, e.g. `(set-flags! (c (> r 255)) (z (zero? a)))`.
- `(trap tag &optional data)` — signal a `lasm-trap` condition carrying
  `tag`/`data`. This is a placeholder for the full interrupt/exception model
  planned for M6 (`deftrap`/`definterrupt`, vectors, priority/masking); for
  now it's just a condition signal with no vectoring.
- `(zero? value)`, `(bit-set? value bit)` — small predicates used in flag
  expressions.

## `push`/`pop` and Common Lisp

`push` and `pop` here are LASM's stack-semantics operators, not
`cl:push`/`cl:pop`. `lasm`'s package definition **shadows** `#:push` and
`#:pop` (`(:shadow #:push #:pop)`) rather than re-exporting the `cl:`
symbols — SBCL's package locks forbid `macrolet` from locally rebinding a
`cl:`-package symbol, even lexically, so `with-machine` needs its own
distinct `push`/`pop` symbols to rebind via `macrolet` for the extent of
`body`. Code in a package that `:use`s `lasm` therefore sees *these*
`push`/`pop` everywhere, not `cl:push`/`cl:pop` — outside `with-machine`
they're plain unbound-as-functions symbols (no ordinary list-`push`/`pop`
meaning). LASM's own source (`storage.lisp`, `machine.lisp`, ...) avoids
the ambiguity by calling `cl:push`/`cl:pop` explicitly wherever it wants
list operations.

## Deviation from the design draft

The design draft ([`LASM-plan.md`](../LASM-plan.md) §3.2–3.3) shows `push`/
`pop` taking no stack argument (`(push (+ (pop) (pop)))`), implying a single
implicit stack, and a `flags` operator that collides with the `defmachine`
clause of the same name. LASM's `push`/`pop` take an explicit stack name
(since a machine can declare more than one stack), and the flag-setting
operator is named `set-flags!` instead of `flags`. The draft is left
unedited as a rough plan; this document reflects what's actually
implemented.

## `with-machine-bindings`

`with-machine` both creates a fresh machine instance and binds the
vocabulary against it. `with-machine-bindings` is the binding half split
out on its own, for callers that already have a machine instance to bind
against rather than wanting a fresh one:

```lisp
(with-machine-bindings (m existing-machine-instance)
  (set! a 42))
```

`(with-machine-bindings (machine-var machine-name) &body body)` binds the
same symbol-macros and operators as `with-machine`, but expects
`machine-var` to already be bound by the caller. `with-machine` is defined
in terms of it:

```lisp
(defmacro with-machine ((var name) &body body)
  `(let ((,var (make-machine ',name)))
     (with-machine-bindings (,var ,name) ,@body)))
```

`definstruction`'s `(semantics ...)` clause (see
[Instructions](instructions.md)) expands its body through
`with-machine-bindings` rather than a separate evaluator, so instruction
semantics and these standalone examples share one vocabulary.
