# Instructions

`definstruction` declares one instruction: an optional addressing mode, an
encoding (opcode + operand width), and a semantics body expanded through the
same vocabulary as [`with-machine`](semantics.md), rather than a separate
evaluator. That single mode is wired to operand parsing and byte encoding.

```lisp
(defmachine sixtyfoo
  (register x :width 8)
  (register pc :width 16)
  (memory ram :width 8 :addr-width 16)
  (flags z))

(definstruction sixtyfoo ldx
  (modes immediate)
  (encoding (opcode #xA2) (operand :mode))
  (semantics (set! x operand) (set-flags! (z (zero? x)))))

(definstruction sixtyfoo sta
  (modes absolute)
  (encoding (opcode #x8D) (operand :mode))
  (semantics (setf (mref machine 'ram operand) x)))

(definstruction sixtyfoo dex
  (encoding (opcode #xCA))
  (semantics (set! x (wrap-value (1- x) 8)) (set-flags! (z (zero? x)))))
```

See [`examples/counter.lisp`](../examples/counter.lisp) for a runnable
version that assembles and runs a small counter-loop program end to end.

## `definstruction`

```lisp
(definstruction MACHINE NAME
  (modes MODE)                                  ; optional; 0 or 1 in M1
  (encoding (opcode n) [(operand :mode) | (operand :width n)])
  (semantics form...)
  (cycles n))                                   ; optional
```

`MACHINE` is a machine name already registered with `defmachine`, resolved
at macroexpansion time (like `defmachine` itself resolves its own storage
clauses) so `definstruction` can validate against it as soon as the form is
compiled, not only after the file loads. Registration happens inside an
`eval-when` for the same reason.

### `(modes MODE)`

Zero or one addressing mode. M1 keeps mode resolution trivial: at most one
mode per instruction, and only `immediate` or `absolute` are recognized —
declaring more than one, or anything else, is an error naming M2 (`defmode`)
as where multi-mode resolution belongs. Omitting the clause declares a
no-operand instruction (e.g. `dex` above).

| mode | operand syntax | encoded width |
|---|---|---|
| `immediate` | `#` then an expression, e.g. `#10` | 1 byte |
| `absolute` | an expression, e.g. `$1000` | see below |

### `(encoding (opcode n) [(operand ...)])`

`(opcode n)` is required and gives the instruction's one-byte opcode.
`(operand ...)` is required exactly when `(modes ...)` declares a mode, and
absent exactly when it doesn't — mismatching the two is an error.

- `(operand :mode)` — take the operand width from the mode: `immediate` is
  always 1 byte; `absolute` defaults to the machine's sole `memory`
  element's `:addr-width` rounded up to whole bytes (so a 16-bit-addressed
  memory gives a 2-byte absolute operand). If the machine declares more
  than one memory element, this is ambiguous and signals an error asking
  for an explicit width instead.
- `(operand :width n)` — override with an explicit byte count.

Encoded bytes are little-endian and each byte is masked with the existing
`wrap-value` (see [Machine model](machine-model.md)), so an over-wide
operand value wraps rather than erroring.

### `(semantics form...)`

Expanded via `with-machine-bindings` (see below) with two extra bindings in
scope for the duration of `form...`:

- `machine` — the runtime `machine` instance, for explicit memory/stack
  access, e.g. `(mref machine 'ram operand)`. There is no `with-machine`
  form here to let the instruction author pick this name, so it's fixed.
- `operand` — the already-evaluated operand integer (see "Scope" below), or
  `nil` for a no-operand instruction.

Every scalar register and flag of `MACHINE` is bound as in `with-machine`
(`x`, `z`, etc. above), and `set!`/`push`/`pop`/`set-flags!`/`trap` are
available the same way.

### `(cycles n)`

Parsed and stored on the instruction descriptor but **not used** — there is
no timing model yet. Accepted (rather than rejected as an unknown clause)
because [`LASM-plan.md`](../LASM-plan.md) §3.2's mockup includes it and
users are expected to copy that shape. A follow-up ticket tracks giving it
meaning.

## PC is a plain register

There is no special program-counter storage element or `branch-if`
operator in M1. A machine that wants one declares `(register pc :width
16)` like any other register, and semantics writes it with plain `set!`:

```lisp
(definstruction sixtyfoo bne
  (modes absolute)
  (encoding (opcode #xD0) (operand :mode))
  (semantics (when (zerop z) (set! pc operand))))
```

Advancing `pc` past the current instruction on every step (rather than only
on a taken branch) is the [emulator loop](emulator.md)'s job, not
`definstruction`'s.

## `with-machine-bindings`

`with-machine` (see [Semantics vocabulary](semantics.md)) both creates a
fresh machine instance and binds the vocabulary against it — but
instruction semantics run against a machine instance the emulator already
owns, not a fresh one. `with-machine-bindings` is the binding half of
`with-machine` split out for exactly this: `(with-machine-bindings
(machine-var machine-name) &body body)` binds the same symbol-macros and
operators as `with-machine`, but expects `machine-var` to already be bound
by the caller rather than instantiating one. `with-machine` is defined in
terms of it:

```lisp
(defmacro with-machine ((var name) &body body)
  `(let ((,var (make-machine ',name)))
     (with-machine-bindings (,var ,name) ,@body)))
```

`definstruction`'s `(semantics ...)` clause expands into
`(with-machine-bindings (machine MACHINE) ,@forms)`, wrapped in a `(lambda
(machine operand) ...)`.

## Operand pipeline

Two functions do the work between a parsed `operand` (see [Statement
grammar & expression parser](parser.md)) and encoded bytes or execution:

- `(match-operand-mode op mode)` — matches `op`'s token run against
  `mode`'s literal prefix (if any) and parses the rest as one expression.
  Signals `parse-failure` if the tokens don't match `mode`, or leave a
  trailing token unconsumed.
- `(eval-expr-constant ast)` — folds a constant expression AST (numbers,
  unary/binary operators) to an integer. Signals `unresolved-label` on an
  `expr-label` — this is a *constant* folder with no label support.
  `(eval-expr ast :symbols table)` is the general form the
  [Assembler](assembler.md) calls with its completed label table;
  `eval-expr-constant` is just `eval-expr` with `symbols` omitted.

Given an evaluated integer, `(encode-instruction descriptor value)` returns
a list of `(unsigned-byte 8)` bytes (opcode then operand, little-endian),
and `(execute-instruction descriptor machine value)` runs the instruction's
semantics against a live `machine`.

`(find-instruction machine-name mnemonic)` and `(find-instruction-by-opcode
machine-name opcode)` look up a registered `instruction-descriptor` by
mnemonic or by opcode (the decode direction the [emulator loop](emulator.md)
uses); both signal `unknown-instruction` rather than an unrelated error if
nothing is registered under that key.

## Scope

This covers one instruction and one already-evaluated operand. It does not
cover:

- A statement-list → byte-vector driver, or a symbol table for label
  resolution — see [Assembler](assembler.md).
- A fetch/execute loop advancing `pc` over encoded bytes — see
  [Emulator](emulator.md).
- Multiple addressing modes per instruction, or declaring new modes with
  `defmode` — M2.
- Indexed access for banked (`:count > 1`) registers in semantics — the
  `:count > 1` skip in `with-machine-bindings` (see
  [Semantics vocabulary](semantics.md)) applies here too.

## Deviation from the design draft

[`LASM-plan.md`](../LASM-plan.md) §3.2 shows semantics dereferencing
`operand` directly (`(+ A operand C)`), implying the assembler picks a
target memory element and loads it before calling into semantics. LASM
instead always binds `operand` to the raw decoded integer regardless of
mode, and semantics dereferences explicitly (`(mref machine 'ram
operand)`). This means `definstruction` never has to guess which memory
element an `absolute` operand addresses — a guess that stops being safe
once a machine declares more than one memory region (M5). The draft is left
unedited as a rough plan; this document reflects what's actually
implemented.
