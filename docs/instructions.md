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
version of the single-mode shape above assembled and run end to end,
[`examples/modes.lisp`](../examples/modes.lisp) for a multi-mode 6502-shaped
program (below), and [`examples/mov.lisp`](../examples/mov.lisp) for a
multi-operand instruction ("Repeated `(operand ...)` subclauses" below).

## `definstruction`

`(modes ...)` accepts two shapes. The single bare symbol above is sugar for
the common case (zero or one mode, sharing the rest of the instruction's
`(encoding ...)`); an instruction accepting more than one mode declares each
mode's own opcode (and, when it differs from the shared default, its own
semantics) in a list instead:

```lisp
(definstruction MACHINE NAME
  (modes MODE)                                    ; sugar: 0 or 1 mode
  (encoding (opcode n)
            [(operand [NAME] :mode)
             | (operand [NAME] :width n)]*)       ; one per MODE's hole
  (semantics form...)
  (cycles n))                                     ; optional

(definstruction MACHINE NAME
  (modes (MODE (opcode n)
               [(operand [NAME] :width n)]*       ; one per MODE's hole
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
modes](modes.md#width)), its own `(semantics ...)` override, and its own
`(cycles n)` override ([below](#cycles-n)). A mode without its own
`(semantics ...)` uses the instruction's shared top-level `(semantics ...)`
as its default (a mode with neither is an error); a mode without its own
`(cycles ...)` likewise uses the shared top-level `(cycles ...)`, or `1` if
there is none either. A top-level `(encoding ...)` clause is not allowed
here, since each mode supplies its own opcode.

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

### `(encoding (opcode n) [(operand ...)]*)`

Only valid with the single-mode sugar form. `(opcode n)` is required and
gives the instruction's one-cell opcode (a byte on every byte-addressed
machine, the only kind before #53). An `(operand ...)` subclause is
required exactly once per `EXPR` hole in `(modes MODE)`'s mode (zero when
`(modes ...)` is omitted) — mismatching the count in either direction is an
error.

- `(operand :mode)` — take this field's width from the mode's own `:width`
  (see [Addressing modes](modes.md#width)), falling back to the machine's
  address width when the mode declares none: the machine's sole `memory`
  element's `:addr-width` rounded up to whole *cells* of that element's own
  `:cell-width` (#53) — so a 16-bit-addressed byte-cell memory gives a
  2-byte operand by default, but a 16-bit-addressed 16-bit-cell (word-
  addressed) memory gives a 1-cell operand, not 2. If the machine declares
  more than one memory element, this fallback is ambiguous and signals an
  error asking for an explicit width instead.
- `(operand :width n)` — override with an explicit cell count.
- `(operand NAME :mode)` / `(operand NAME :width n)` — as above, and also
  bind `NAME` to this field's value in `(semantics ...)` (see "Named operand
  fields" below).

A single-hole mode needs exactly one `(operand ...)` subclause here; a mode
with more holes (see "Repeated `(operand ...)` subclauses" below) needs one
per hole, in hole order.

Encoded cells are little-endian and each field's cells are masked with the
existing `wrap-value` (see [Machine model](machine-model.md)) at the
machine's own cell width, so an over-wide value wraps rather than erroring.

### Repeated `(operand ...)` subclauses — multi-operand instructions

A mode's pattern (`defmode`, [Addressing modes](modes.md)) may declare more
than one `EXPR` hole — a two-register `mov`'s `expr "," expr`, say. Each
hole gets its own operand encoding field: one `(operand ...)` subclause per
hole, in the same order the holes appear in the pattern, each independently
`:mode`- or `:width`-sized:

```lisp
(defmode reg-reg expr "," expr)

(definstruction sixtyfoo mov
  (modes reg-reg)
  (encoding (opcode #x40)
            (operand dst :width 1)
            (operand src :width 1))
  (semantics (setf (mref machine 'regs dst) (mref machine 'regs src))))
```

Assembling `mov 2, 3` matches `reg-reg`'s two holes against `2` and `3`,
encodes as `#x40 #x02 #x03` (each field little-endian at its own width, in
hole order), and `(semantics ...)` sees `dst` bound to `2` and `src` to `3`.
A multi-mode variant (the `(modes (MODE ...) ...)` form) works the same
way, with one exception: a **single**-hole mode there may still omit
`(operand ...)` entirely and take its default width, exactly as before —
only a mode with more than one hole *requires* explicit subclauses, since
there's no single width left to default to.

Declaring the wrong number of `(operand ...)` subclauses for a mode's hole
count — too few or too many — is an error at `definstruction`'s
macroexpansion time, not a runtime surprise.

A `:relative` mode ([Addressing modes](modes.md#pc-relative-modes)) may not
have more than one hole: its offset applies to the operand as a whole, and
there is currently no way to mark just one hole of a multi-hole mode as the
relative one (see [Addressing modes](modes.md#width) and the tracker for
this follow-up).

### `(semantics form...)`

Expanded via `with-machine-bindings` (see below) with two extra bindings in
scope for the duration of `form...`:

- `machine` — the runtime `machine` instance, for explicit memory/stack
  access, e.g. `(mref machine 'ram operand)`. There is no `with-machine`
  form here to let the instruction author pick this name, so it's fixed.
- `operand` — the first (or only) operand field's already-evaluated value,
  or `nil` for a no-operand instruction.
- any `NAME` given to an `(operand NAME ...)` subclause ("Repeated
  `(operand ...)` subclauses" above) — bound to that field's own value, so a
  multi-operand instruction can write `dst`/`src` directly instead of
  picking values apart itself.

Every scalar register and flag of `MACHINE` is bound as in `with-machine`
(`x`, `z`, etc. above), and `set!`/`push`/`pop`/`set-flags!`/`trap` are
available the same way.

### `(cycles n)`

This instruction's cycle cost (#75) — the amount the emulator's step loop
adds to `machine-cycles` each time it executes (see
[Emulator](emulator.md#cycle-cost-model-clock-speed-and-cycle-accurate-execution-75)).
Optional; an instruction with no `(cycles n)` costs `1`. On a multi-mode
instruction, a variant's own `(cycles n)` overrides this shared default for
that mode alone — e.g. a 6502-shaped `LDA`'s `zero-page` mode costing less
than its `absolute` sibling. `n` must be a non-negative integer; `0` is
allowed (an instruction that consumes no time at all, e.g. a metadata-only
no-op).

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
- `(encode-instruction descriptor values)` returns a list of
  `(unsigned-byte cell-width)` cells (`cell-width` being `descriptor`'s
  machine's own code cell width, #53 — 8 on every byte-addressed machine)
  for one use of instruction `descriptor` with operand field values `values`
  (a list, one per operand encoding field, or `nil` for a no-operand
  instruction): on an ordinary cell-encoded machine, the opcode followed by
  each field's cells little-endian in turn; on a word-encoded one (#20), the
  instruction word (opcode and every field packed in by bit shift) followed
  by each `:extra-word` field's own value, also little-endian.
- `(instruction-descriptor-size descriptor)` — total encoded cells for one
  use of `descriptor`, covering both encoding schemes: `1 +` operand cell
  widths on a cell-encoded machine, or the instruction-word's own cell
  width times `1 +` its extra-word count on a word-encoded one. This is what
  the assembler's layout/relaxation and the emulator's fetch loop use
  instead of assuming a one-byte opcode.
- `(execute-instruction descriptor machine values)` runs `descriptor`'s
  semantics against a live `machine`, the same list `values` bound as
  `operand` (and any named fields) in the semantics body.

## Word-encoded instructions (#20)

Everything above assumes a cell-encoded machine (opcode cell + fixed-width
operand cells). A machine declaring an `instruction-word` clause (see
[Machine model](machine-model.md)) instead encodes one instruction as a
single fixed-width word split into named bit fields, and `(operand ...)`
reads differently:

```lisp
(operand [NAME] :field FIELD-NAME
  [(variant (range LO HI) inline [:bias N])
   (variant :else (extra-word :escape N))]*)
```

`NAME` binds as before; `FIELD-NAME` names one of the machine's declared
`instruction-word` fields instead of giving a byte width. With no
`(variant ...)` forms at all, the field just holds the value directly
(biased by 0) over its own full unsigned range — the word-encoded
equivalent of `(operand :mode)`'s implicit default. With one or more:

- `(variant (range LO HI) inline [:bias N])` — a value in `LO..HI` (before
  biasing) packs straight into the field as `value + N` (default bias 0).
  `:bias` is what lets a small *negative* value (DCPU-16's own `-1..30`) pack
  into a field with no sign bit of its own — `-1` biased by `+1` is `0`,
  decoded back by subtracting the same bias.
- `(variant :else (extra-word :escape N))` — the fallback: instead of
  packing the value, the field holds the literal `N` and the real value
  follows in its own word, immediately after the instruction word.

```lisp
(defmachine wordfoo
  (register pc :width 16)
  (register a :width 16)
  (memory ram :width 8 :addr-width 16)
  (instruction-word :width 16
    (field opcode 4)
    (field dst 2)
    (field src 10)))

(defmode wimm "#" expr)

(definstruction wordfoo seta
  (modes wimm)
  (encoding
    (opcode 1)
    (operand value :field src
      (variant (range -1 30) inline :bias 1)
      (variant :else (extra-word :escape #x3ff))))
  (semantics (set! a operand)))
```

Declaring this expands `seta` into **two** `instruction-descriptor`s sharing
one mnemonic, mode, and opcode value — one all-inline, one needing an extra
word — the same way a multi-mode `(modes (MODE ...) ...)` clause expands
into one descriptor per mode. Choosing between them per statement is
`%choose-variant`'s job (see [Assembler](assembler.md#choosing-a-mode)):
exactly the same syntax → floor → value filter pipeline that picks between
two addressing-mode widths, generalized to pick between extra-word counts
instead, all-inline tried before any variant needing an extra word.

Both a variant's biased inline range and any `:else` escape value must fit
`FIELD-NAME`'s declared bit width, and an escape value may never fall inside
an inline variant's biased range — that ambiguity would leave a decoder
unable to tell a genuine inline value from the escape marker apart reading
the same raw bits. Both are checked at `definstruction`'s macroexpansion
time, not left as an encode- or decode-time surprise. The `opcode` value
itself is checked the same way, against the `opcode` field's own width.

A `:relative` addressing mode is not supported on a word-encoded machine —
its offset arithmetic (`%relative-offset`, [Assembler](assembler.md))
assumes a cell-counted operand width. A word-encoded field also has no `:signed`
mode of its own to sign-extend on decode the way a cell-encoded operand
does — a negative inline value's sign is carried entirely by its variant's
`:bias`, decoded back by subtracting the same bias, not by two's-complement
reinterpretation.

Unlike the cell-encoded multi-mode form, a single-hole mode may **not** omit
`(operand ...)` here even though the mode itself has only one hole — there
is no "default field" a word-encoded operand could fall back to the way a
cell-encoded one falls back to the machine's address width, so an omitted
subclause against a mode with holes is an error rather than silently
dropping that hole's value.

See [`examples/word.lisp`](../examples/word.lisp) for a complete machine
assembled and run end to end, both packing a value inline and escaping one
to its own extra word.

## Scope

This covers one instruction variant and its already-evaluated operand
field(s). It does not cover:

- A statement-list → cell-vector driver, a symbol table for label
  resolution, or *choosing* which variant an operand's syntax and value
  select — see [Assembler](assembler.md).
- A fetch/execute loop advancing `pc` over encoded cells — see
  [Emulator](emulator.md).
- Marking just one hole of a multi-hole mode as PC-relative — a `:relative`
  mode may only have one hole (see "Repeated `(operand ...)` subclauses"
  above and [Addressing modes](modes.md#pc-relative-modes)) — a separate,
  follow-up feature.

Banked (`:count > 1`) registers work the same way in word-encoded semantics
as everywhere else — bound as `(NAME idx)` via `regref` — see [Semantics
vocabulary](semantics.md). [`examples/dcpu16.lisp`](../examples/dcpu16.lisp)
combines this mechanism with word-addressed memory (see [Machine model,
"Cell width and the assembler"](machine-model.md#cell-width-and-the-assembler))
and a banked register for DCPU-16's eight named registers, end to end.

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
