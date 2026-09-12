# Instructions

`definstruction` declares one instruction: one or more addressing modes
(declared with [`defmode`](modes.md)), an encoding (opcode + operand width)
per mode, and a semantics body expanded through the same vocabulary as
[`with-machine`](semantics.md), rather than a separate evaluator.

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
version of the single-mode shape above assembled and run end to end, and
[`examples/modes.lisp`](../examples/modes.lisp) for a multi-mode 6502-shaped
program (below).

## `definstruction`

`(modes ...)` accepts two shapes. The single bare symbol above is sugar for
the common case (zero or one mode, sharing the rest of the instruction's
`(encoding ...)`); an instruction accepting more than one mode declares each
mode's own opcode (and, when it differs from the shared default, its own
semantics) in a list instead:

```lisp
(definstruction MACHINE NAME
  (modes MODE)                                    ; sugar: 0 or 1 mode
  (encoding (opcode n) [(operand :mode) | (operand :width n)])
  (semantics form...)
  (cycles n))                                     ; optional

(definstruction MACHINE NAME
  (modes (MODE (opcode n)
               [(operand :width n)]
               [(semantics form...)])
         ...)                                     ; 2+ modes, one per line
  (semantics form...)                             ; shared default; required
  (cycles n))                                     ; unless every mode above
                                                   ; supplies its own
```

`MACHINE` is a machine name already registered with `defmachine`, and each
`MODE` a name already registered with `defmode` (see [Addressing
modes](modes.md)), both resolved at macroexpansion time — like `defmachine`
itself resolves its own storage clauses — so `definstruction` can validate
against them as soon as the form is compiled, not only after the file loads.
Registration happens inside an `eval-when` for the same reason.

### `(modes MODE)` — sugar for one mode

Zero or one addressing mode, with `(encoding ...)` (below) giving its opcode
and operand width. Omitting the clause declares a no-operand instruction
(e.g. `dex` above). More than one bare symbol here is an error pointing at
the multi-mode form instead.

`immediate` and `absolute` are the two modes `defmode` ships built in (see
[Addressing modes](modes.md) for their syntax and the rest of the built-in
set, and for declaring your own).

### `(modes (MODE ...) ...)` — several modes, one instruction

Each mode gets its own `(opcode n)` (required), and optionally its own
`(operand :width n)` (default: the mode's own `:width`, if `defmode` gave it
one, else the machine's address width — see [Addressing
modes](modes.md#width)) and its own `(semantics ...)` override. A mode
without its own `(semantics ...)` uses the instruction's shared top-level
`(semantics ...)` as its default; a mode with neither is an error. A
top-level `(encoding ...)` clause is not allowed here, since each mode
supplies its own opcode.

The per-mode override exists because a mode's syntax doesn't determine its
semantics: an `immediate` operand is a literal value, ready to use directly,
while `zero-page`/`absolute`/`indexed-x` etc. are all *addresses* whose value
has to be dereferenced first (see "Deviation from the design draft" below).
6502-shaped `LDA` is the standard example:

```lisp
(definstruction sixtyfoo lda
  (modes
    (immediate (opcode #xA9) (semantics (set! a operand)))
    (zero-page (opcode #xA5))
    (absolute  (opcode #xAD)))
  ;; ZERO-PAGE and ABSOLUTE both fall through to this default -- they
  ;; address RAM the same way, differing only in operand width, which the
  ;; assembler picks per use (see Assembler, "Choosing a mode").
  (semantics (set! a (mref machine 'ram operand))))
```

Which of a mnemonic's modes a given operand actually uses is decided at
assembly time, not here — see [Assembler](assembler.md#choosing-a-mode).

### `(encoding (opcode n) [(operand ...)])`

Only valid with the single-mode sugar form. `(opcode n)` is required and
gives the instruction's one-byte opcode. `(operand ...)` is required exactly
when `(modes ...)` declares a mode, and absent exactly when it doesn't —
mismatching the two is an error.

- `(operand :mode)` — take the operand width from the mode's own `:width`
  (see [Addressing modes](modes.md#width)), falling back to the machine's
  address width when the mode declares none: the machine's sole `memory`
  element's `:addr-width` rounded up to whole bytes (so a 16-bit-addressed
  memory gives a 2-byte operand by default). If the machine declares more
  than one memory element, this fallback is ambiguous and signals an error
  asking for an explicit width instead.
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

There is no special program-counter storage element or `branch-if` operator.
A machine that wants one declares `(register pc :width 16)` like any other
register, and semantics writes it with plain `set!`. A real branch is
[`relative`](modes.md), not `absolute` — its operand is a signed offset from
the *next* instruction's address, not an absolute target, so semantics adds
it to `pc` rather than assigning it directly:

```lisp
(definstruction sixtyfoo bne
  (modes relative)
  (encoding (opcode #xD0) (operand :mode))
  (semantics (when (zerop z) (set! pc (+ pc operand)))))
```

By the time this runs, `pc` already points past `bne` and its operand — see
[`step-machine`](emulator.md#step-machine) — so `(+ pc operand)` lands
exactly where the assembler computed the offset from. An instruction that
really does want an absolute jump target (e.g. a 6502-style `jmp`) still
uses `absolute` and plain `(set! pc operand)`, as before.

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
(machine operand) ...)` — one such lambda per mode variant, each becoming
its own `instruction-descriptor`.

## Registration: a mnemonic is a list of variants

A mnemonic registers as a list of `instruction-descriptor`s — one per
addressing mode it accepts (a no-operand or single-mode instruction's list
has exactly one). `(find-instruction-variants machine-name mnemonic)`
returns the list; `(find-instruction machine-name mnemonic &key mode)`
returns one variant, defaulting to the first declared when `mode` is
omitted. `(find-instruction-by-opcode machine-name opcode)` is unaffected by
any of this: each variant carries its own distinct opcode, so opcode → 
descriptor decode stays one-to-one regardless of how many modes a mnemonic
declares.

Redefining a mnemonic (a repeated `definstruction`) replaces its whole
variant list; any old opcode not reused by the new list is dropped from the
opcode table, so a redefinition that drops a mode never leaves
`find-instruction-by-opcode` resolving a stale opcode to a descriptor that no
longer exists.

## Operand pipeline

Addressing-mode pattern matching itself — `match-operand-mode`,
`try-match-operand-mode` — now lives in [Addressing modes](modes.md), since
it operates purely on a mode's pattern and a token run, with no instruction
involved. What's left here is folding an already-matched operand to a value
and turning it into bytes or an executed effect:

- `(eval-expr-constant ast)` — folds a constant expression AST (numbers,
  unary/binary operators) to an integer. Signals `unresolved-label` on an
  `expr-label` — this is a *constant* folder with no label support.
  `(eval-expr ast :symbols table)` is the general form the
  [Assembler](assembler.md) calls with its completed label table;
  `eval-expr-constant` is just `eval-expr` with `symbols` omitted.
- `(encode-instruction descriptor value)` returns a list of
  `(unsigned-byte 8)` bytes (opcode then operand, little-endian) for one use
  of instruction `descriptor` with operand value `value`.
- `(execute-instruction descriptor machine value)` runs `descriptor`'s
  semantics against a live `machine`.

## Scope

This covers one instruction variant and one already-evaluated operand. It
does not cover:

- A statement-list → byte-vector driver, a symbol table for label
  resolution, or *choosing* which variant an operand's syntax and value
  select — see [Assembler](assembler.md).
- A fetch/execute loop advancing `pc` over encoded bytes — see
  [Emulator](emulator.md).
- More than one operand per instruction (a mode's pattern may declare
  several `expr` holes, but `definstruction` only wires up one operand
  encoding field per instruction) — a separate feature.
- Indexed access for banked (`:count > 1`) registers in semantics — the
  `:count > 1` skip in `with-machine-bindings` (see
  [Semantics vocabulary](semantics.md)) applies here too.

## Deviation from the design draft

[`LASM-plan.md`](../LASM-plan.md) §3.2 and §3.4 show semantics
dereferencing `operand` directly (`(+ A operand C)`) and a mode's pattern
producing a tagged form (`-> (imm $1)`), implying the assembler picks a
target memory element and loads it before calling into semantics. LASM
instead always binds `operand` to the raw decoded integer regardless of
mode, and semantics dereferences explicitly (`(mref machine 'ram
operand)`); `defmode`'s pattern accordingly has no `-> tag` arrow to produce
one (see [Addressing modes](modes.md)). This means `definstruction` never
has to guess which memory element an address-shaped operand addresses — a
guess that stops being safe once a machine declares more than one memory
region (M5) — and it's why a multi-mode instruction like `LDA` above needs a
per-mode semantics override for `immediate` (a value) while
`zero-page`/`absolute` share one default (an address). The draft is left
unedited as a rough plan; this document reflects what's actually
implemented.
