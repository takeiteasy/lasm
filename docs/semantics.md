# Semantics vocabulary

`with-machine` is the single entry point for writing machine semantics
against a `defmachine`-declared machine:

```lisp
(with-machine (m sixtyfoo)
  (set! a 42)
  (push a s)
  (set-flags! (z (zero? a)) (n (bit-set? a 7)))
  (setf (mref m #x1000) 1))
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
  [Machine model](machine-model.md). A banked register's own `:names`
  additionally bind one ordinary symbol-macro per alias, its bank index
  baked in — e.g. DCPU-16's `i` reads/writes bank 6 of `reg` directly,
  same as a scalar register's own symbol, alongside the indexed `(reg idx)`
  form for a run-time-computed index.
- the operators below, available as local macros for the extent of `body`.
- memory accessed through `mref` with an address. Its name defaults to the
  sole memory element. Fixed-stack accessors and `push`/`pop` accept bare
  stack names and can omit the name when a default exists.

The [device](devices.md) bus API (`device-count`, `device-info`,
`device-send`, ...) is **not** bound here — an `hwn`/`hwq`/`hwi`-
style instruction calls it directly, `machine` passed explicitly.
`signal-interrupt` (see [Interrupts](interrupts.md)) follows the same
convention for a software `int`-style instruction.

## Operators

- `(set! place value)` — `(setf place value)`. Works on any bound register/
  flag symbol, or any other `setf`-able place (e.g. `(mref m addr)`).
- `(mref machine [memory-name] address)` — read or write a memory cell. In
  `with-machine` and instruction semantics, omit `memory-name` when the
  machine declares exactly one memory element. With zero or multiple memory
  elements, an omitted name signals at macroexpansion time; pass a quoted
  name such as `(mref machine 'ram address)`. Host code always supplies the
  name: `(mref machine 'ram address)`.
- `(push value &optional stack-name)` — push `value` onto the named stack.
- `(pop &optional stack-name)` — pop and return the top of the named stack.
- `(stack-depth &optional stack-name)` — read the fixed stack's live depth.
- `(stack-pointer &optional stack-name)` — read or set the fixed stack's live
  depth, as in `(setf (stack-pointer) 1)`.
- `(stack-ref offset &optional stack-name)` — read or set a fixed-stack cell
  relative to the top, as in `(setf (stack-ref 0) 7)`.

The three fixed-stack accessors default to the sole `(stack ...)` element.
With zero or multiple fixed stacks, give a bare name such as `(stack-ref 0
return-stack)`. They do not operate on register-backed stack pointers. Host
code uses `(stack-depth machine name)`, `(stack-pointer machine name)`, and
`(stack-ref machine name offset)`.

For `push`/`pop`, `stack-name`, given or defaulted, may name either a
`(stack ...)` element or a register bound by a `(stack-pointer ...)` clause
(see [Machine model, `stack-pointer`](machine-model.md)) — `push`/`pop` expand to
`stack-push`/`stack-pop` or `sp-push`/`sp-pop` accordingly, transparently to
the caller. This works whether or not the machine declares an
`(interrupts ...)` clause.

When `stack-name` is omitted, it resolves to the machine's sole `stack`
element, mirroring `emulator.lisp`'s `%resolve-memory` convention for the
sole `memory` element; only when the machine declares no `stack` element does
the sole stack-pointer become the default instead. A machine declaring more
than one candidate of whichever kind applies (or none at all) signals an
error when the `push`/`pop` form is macroexpanded (not merely when it runs)
if the name is left out — name one explicitly in that case.
- `(set-flags! (flag-name form)...)` — set each named flag to the result of
  evaluating `form`, e.g. `(set-flags! (c (> r 255)) (z (zero? a)))`.
  Integer `0` clears a flag; nonzero integers set it.
- `(trap tag &optional data)` — signal a `lasm-trap` condition carrying
  `tag`/`data`. Unchanged by #109's interrupt delivery (below) — the two
  remain separate mechanisms; a model unifying them is future M6 work.
- `(idle)` — mark the machine idle (see [Emulator, "Idle
  steps"](emulator.md#idle-steps-110) and [Interrupts, "Waking an idle
  machine"](interrupts.md#waking-an-idle-machine-110)). Unlike `trap`, this
  is not a control transfer — it sets a flag and the rest of the semantics
  body runs to completion. Works on any machine, including one declaring no
  `(interrupts ...)` clause; such a machine can only be woken by a host
  calling `wake-machine` directly.
- `(extra-cycles n)` — add `n` cycles to the running instruction's cost, on
  top of its declared `(cycles n)`; see [Emulator, "Dynamic cycle
  costs"](emulator.md#dynamic-cycle-costs-90). Like `idle`, it does not
  unwind. Multiple calls accumulate.
- `(interrupt-return)` — on a machine declaring an `(interrupts ...)`
  clause (see [Interrupts](interrupts.md)), pops every `:save` place in
  reverse declared order, restoring exactly what delivery pushed. Signals
  an error at macroexpansion time on a machine declaring no such clause.
- `(zero? value)`, `(bit-set? value bit)` — small predicates used in flag
  expressions.
- `(page-crossed? from to &optional (page-size 256))` — true when `from` and
  `to` lie in different pages; the test behind a page-crossing penalty.

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
binding every semantics body already gets). It may also be a named `one-of`
slot, including a slot whose selected alternative contributes no operand
hole. Each clause's key is one
mode-name symbol or a list of them, exactly like `cl:case`; every key must
be one of `name`'s hole or named slot's own `one-of` alternatives — checked
at `definstruction` time — unless that hole isn't governed by a `one-of` at
all, in which case there is nothing to check a key against and the check is
skipped. At runtime, `choice-case` dispatches on which alternative the hole
was actually decoded (or assembled) as; with no `otherwise` clause, a hole
matching none of the given keys — including one with no recorded
alternative at all — signals `no-matching-choice` instead of silently
falling through. An `:alias` alternative is never decoded, so a clause keyed
on it never runs; the canonical alternative's clause covers both spellings.

`name` may also be an extra operand a *sibling* alternative's own hole count
contributes but this one's own mode use doesn't (see [Addressing modes,
"Varying hole counts across
alternatives"](modes.md#varying-hole-counts-across-alternatives),
[Instructions, "`for-choice`"](instructions.md#for-choice--extra-holes-for-a-varying-alternative))
— reading it outside a `choice-case` body works too, bound to `nil` (rather
than left unbound) in a descriptor that has no such hole of its own; a
`choice-case` clause reading it is reachable only from the sibling
descriptor that actually has it, since that is the only descriptor whose
own governing hole ever matches that clause's key.

Varying tuples share one semantics function. Each concrete descriptor carries
an explicit positional map into that function's operand layout, including
unnamed holes and absent extra holes; execution never uses operand-name lookup
to reorder values.

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
