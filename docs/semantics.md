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
  `(set! a (+ a 1))` and plain `a` both work).
- every **banked** register (`:count > 1`) bound as a local macro taking a
  run-time index instead, e.g. `(v idx)` reads bank `idx` of `v` (expanding
  to `regref`) and `(set! (v idx) n)` writes it (through `set!`'s plain
  `setf` expansion, so no separate write form is needed) — see
  [Machine model](machine-model.md).
- the operators below, available as local macros for the extent of `body`.
- memory accessed by name through the `mref` accessor, since it takes an
  explicit address operand. Stacks are accessed through `push`/`pop` by
  name too, though the name may be omitted on a single-stack machine — see
  below.

## Operators

- `(set! place value)` — `(setf place value)`. Works on any bound register/
  flag symbol, or any other `setf`-able place (e.g. `(mref m 'ram addr)`).
- `(push value &optional stack-name)` — push `value` onto the named stack.
- `(pop &optional stack-name)` — pop and return the top of the named stack.

When `stack-name` is omitted, it resolves to the machine's sole `stack`
element, mirroring `emulator.lisp`'s `%resolve-memory` convention for the
sole `memory` element. A machine declaring no stack, or more than one,
signals an error when the `push`/`pop` form is macroexpanded (not merely
when it runs) if the name is left out — name one explicitly in that case.
- `(set-flags! (flag-name form)...)` — set each named flag to the result of
  evaluating `form`, e.g. `(set-flags! (c (> r 255)) (z (zero? a)))`.
- `(trap tag &optional data)` — signal a `lasm-trap` condition carrying
  `tag`/`data`. This is a placeholder for the full interrupt/exception model
  planned for M6 (`deftrap`/`definterrupt`, vectors, priority/masking); for
  now it's just a condition signal with no vectoring.
- `(zero? value)`, `(bit-set? value bit)` — small predicates used in flag
  expressions.

## `choice-case`

Dispatches on a matched addressing-mode alternative. `choice-case` is
available inside a `definstruction` `(semantics ...)` body
alongside the operators above — but, unlike them, it is not part of
`with-machine-bindings` itself, since it needs information
`with-machine-bindings`'s standalone callers never have: which `one-of`
alternative (see [Addressing modes, "Per-operand
modes"](modes.md#per-operand-modes)) an operand hole actually matched.

```lisp
(choice-case name
  (mode-or-modes form...)
  ...
  [(otherwise form...)])
```

`name` is an operand field name from the instruction's `(operand ...)`
subclauses, or `operand` for the first field (mirroring the `operand`
binding every semantics body already gets). Each clause's key is one
mode-name symbol or a list of them, exactly like `cl:case`; every key must
be one of `name`'s hole's own `one-of` alternatives — checked at
`definstruction` time — unless that hole isn't governed by a `one-of` at
all, in which case there is nothing to check a key against and the check is
skipped. At runtime, `choice-case` dispatches on which alternative the hole
was actually decoded (or assembled) as; with no `otherwise` clause, a hole
matching none of the given keys — including one with no recorded
alternative at all — signals `no-matching-choice` instead of silently
falling through.

This is the piece a word-encoded field's `(choice mode)` variant selector
(see [Instructions, "CHOICE-selected word
fields"](instructions.md#choice-selected-word-fields-104)) leaves open on
its own: `(choice mode)` steers a field's own *encoding*, but every sibling
descriptor its combinations expand into still shares one semantics body.
`choice-case` reads back which alternative was really written, so `[reg]`
can genuinely dereference while a bare `reg` reads the register's own value:

```lisp
(definstruction anima16foo ld
  (modes ld-mode)
  (encoding
    (opcode 1)
    (operand dst :field b)
    (operand src :field a
      (variant (choice a-reg) inline :range (0 7) :bias #x00)
      (variant (choice a-ind) inline :range (0 7) :bias #x08)
      (variant (choice a-mem) (extra-word :escape #x1e))))
  (semantics
    (set! (reg dst)
      (choice-case src
        (a-reg (reg src))
        (a-ind (mref machine 'ram (reg src)))
        (a-mem (mref machine 'ram src))))))
```

See [`examples/anima16.lisp`](../examples/anima16.lisp) for this run end to
end: the three forms of `ld` now produce three genuinely different results
for the same written value, not just three different encodings.

A **cell-encoded** machine's decoded instruction carries a record of which
`one-of` alternative was assembled only for a hole governed by a sub-opcode
selector — its own hole-selected `(variant (choice m) (sub s))` (see
[Instructions, "`(variant (choice m)
(sub s))`"](instructions.md#variant-choice-m-sub-s--hole-selected-sub-opcode)),
or membership in a `(sub-opcode ...)` table's participating holes (see
["multi-hole sub-opcode
selection"](instructions.md#sub-opcode--multi-hole-sub-opcode-selection))
— and [Addressing modes, "What `one-of` does and does not
do"](modes.md#what-one-of-does-and-does-not-do) — `choice-case` dispatches
on it exactly as it does on a word-encoded machine's `(choice mode)` field,
independently at each hole a table governs.
A hole with no such selector still carries no record at all, so `choice-case`
there always sees it as unmatched, signalling `no-matching-choice` unless
given an `otherwise` clause.

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
