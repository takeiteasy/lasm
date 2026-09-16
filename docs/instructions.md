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
has to be dereferenced first (see "Note on operand binding" below).
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

### `(opcode n :sub s)` — sub-opcode cell

A byte-encoded machine's only per-instruction bits are the opcode cell
itself, so two modes of one mnemonic (or two different mnemonics) normally
can't share an opcode there — see "Opcode to descriptor decode" below. `:sub`
gives decode a second, purely discriminating value to key off: `(opcode n
:sub s)` reserves the cell right after the opcode for `s`, so the instruction
encodes as `[n][s][operand cells...]` instead of the usual `[n][operand
cells...]`. Valid wherever a plain `(opcode n)` is — the multi-mode
`(modes (MODE (opcode n :sub s) ...) ...)` form, `(encoding (opcode n :sub
s) ...)`, and the single-mode `(modes MODE)` sugar's own `(encoding ...)`.

`s` must fit the machine's code cell width, the same requirement `(opcode n)`
itself is held to on a word-encoded machine's `opcode` field. `:sub` is a
byte-machine-only mechanism — a `definstruction`-time error on a machine
declaring an `instruction-word` clause, since a word-encoded machine already
has its own operand-field discrimination (below) and no use for a second,
separate cell.

```lisp
(definstruction sixtyfoo lda
  (modes
    (immediate (opcode #x10 :sub 0) (operand :mode) (semantics (set! a operand)))
    (absolute  (opcode #x10 :sub 1) (operand :mode)
      (semantics (set! a (mref machine 'ram operand))))))
```

`lda #5` and `lda $2000` now share opcode `#x10` on a byte-encoded machine,
told apart purely by the cell after it — `:sub 0` for the immediate form,
`:sub 1` for the absolute one. See
[`examples/subopcode.lisp`](../examples/subopcode.lisp) for this run end to
end, including a second mnemonic sharing an opcode the same way; this is the
byte-machine counterpart of `examples/sharedopcode.lisp`'s word-machine
story.

`:sub` above is selected per `(modes ...)` clause, not per operand hole — for
that, see hole-selected sub-opcodes next.

### `(variant (choice m) (sub s))` — hole-selected sub-opcode

The byte-machine analogue of `(choice mode)` (below): rather than fixing
`:sub` once for a whole mode, an `(operand ...)` subclause whose hole came
from a `(one-of ...)` pattern element ([Addressing modes, "Per-operand
modes"](modes.md#per-operand-modes)) can pick the sub-opcode cell's value by
*which alternative the operand's own syntax actually matched*:

```lisp
(defmode sc-direct expr)
(defmode sc-indirect "[" expr "]")
(defmode sc-any (one-of sc-direct sc-indirect))

(definstruction sixtyfoo lda
  (modes sc-any)
  (encoding (opcode #x10)
            (operand src :width 1
              (variant (choice sc-direct) (sub 0))
              (variant (choice sc-indirect) (sub 1))))
  (semantics (choice-case src
               (sc-direct (set! a src))
               (sc-indirect (set! a (mref machine 'ram src))))))
```

`lda 5` encodes `#x10 00 05`; `lda [5]` encodes `#x10 01 05` — one mnemonic,
one mode, one opcode, told apart purely by which `one-of` alternative the
operand matched, rather than needing a separate `(modes ...)` clause per
form the way a plain `(opcode n :sub s)` would. `definstruction` registers
one `instruction-descriptor` per claimed alternative (two here), each with
its own `sub-opcode` and a hole-aligned `sub-choices` record naming that
alternative — the same mechanism that lets several descriptors share one
byte-machine opcode above, but chosen per operand hole instead of per whole
mode. See [`examples/subchoice.lisp`](../examples/subchoice.lisp) for this
run end to end, including `choice-case` dispatch and disassembly.

Rules, all checked at `definstruction` time:

- The carrying hole must be a `(one-of ...)` hole; a selector on a plain
  `expr` hole has nothing to select between.
- **Every** alternative of the carrying hole must be claimed by exactly one
  `(variant (choice m) (sub s))` — unlike a word-encoded field's mixed
  `choice`/value-selected variants (below), there is no value-selected
  fallback for an unclaimed alternative to resolve into, so partial coverage
  is a permanent error here, not a gap.
- Only one operand hole per mode may carry its own selector this way — two
  holes each wanting to pick the (singular) sub-opcode cell need a
  `(sub-opcode ...)` table instead (below).
- `s` must be pairwise distinct across the carrying hole's alternatives and
  fit the machine's code cell width, same requirement as a plain `:sub`.
- An explicit `(opcode n :sub s)` and a hole-selected selector may not both
  be given on one mode — both would be writing the same cell.

Once assembled, decode (`decode-instruction-at`) reads the sub-opcode cell
back and reports the matched descriptor's `sub-choices` as its own `choices`
return value — the same hole-aligned record a word-encoded machine's
`(choice mode)` field already produces. This is what makes `choice-case`
(below) and the disassembler's rendering of the matched alternative both
reachable on a byte-encoded machine for the first time, not only a
word-encoded one.

This selector is also what lets the carrying hole's `one-of` alternatives
disagree on `:signed` (see [Addressing modes, "Per-hole
`:signed`"](modes.md#per-hole-signed)) — each expanded descriptor's own
`operand-signedness` (a hole-aligned list of booleans, precomputed at
`definstruction` time from which alternative it was claimed for) tells
`decode-instruction-at` which holes to sign-extend, per descriptor rather
than per whole mode.

The same selector, again requiring the carrying hole's `(operand ...)`
subclause to be `(operand :mode)` rather than an explicit `(operand :width
n)`, also lets it disagree on `:width` (see [Addressing modes, "Per-hole
`:width`"](modes.md#per-hole-width)) — each expanded descriptor's own
`operand-widths` (below) is stamped from which alternative it was claimed
for, so a `ldw 200`/`ldw #300`-shaped pair of statements can encode and
decode with genuinely different operand sizes while sharing one opcode.

It similarly lets a hole disagree on `:relative` (see [Addressing modes,
"Per-hole `:relative`"](modes.md#per-hole-relative)) — each expanded
descriptor's own `relative-hole-index` (below) is stamped from which
alternative it was claimed for, so a `jmr 10`/`jmr #target`-shaped pair of
statements can encode one operand plainly and the other as a PC-relative
offset while sharing one opcode.

### `(sub-opcode ...)` — multi-hole sub-opcode selection

The above selects the sub-opcode cell by *one* hole's matched alternative. A
mode with two or more `one-of` holes that each want a say in the cell needs a
`(sub-opcode ...)` subclause instead — sibling to `(operand ...)`, valid
wherever it is:

```lisp
(defmode sc-direct expr)
(defmode sc-indirect "[" expr "]")
(defmode sc-two (one-of sc-direct sc-indirect) "," (one-of sc-direct sc-indirect))

(definstruction sixtyfoo mov
  (modes sc-two)
  (encoding (opcode #x10)
            (operand dst :width 1)
            (operand src :width 1)
            (sub-opcode
              (variant (choice sc-direct sc-direct)     (sub 0))
              (variant (choice sc-direct sc-indirect)   (sub 1))
              (variant (choice sc-indirect sc-direct)   (sub 2))
              (variant (choice sc-indirect sc-indirect) (sub 3))))
  (semantics ...))
```

Each `(variant (choice m1 m2 ...) (sub s))` names one alternative per
participating `one-of` hole, in hole order — **every** `one-of` hole of the
mode participates; there is no way to name only a subset (see the tracker
for that follow-up). `mov 20, 5` encodes `#x10 00 14 05`; `mov [20], [5]`
encodes `#x10 03 14 05` — one mnemonic, one mode, one opcode, told apart by
which combination of the two holes' alternatives matched. `definstruction`
registers one `instruction-descriptor` per claimed combination (four here),
each with its own `sub-opcode` and a `sub-choices` record populated at every
participating hole. See
[`examples/subtable.lisp`](../examples/subtable.lisp) for this run end to
end.

Rules, all checked at `definstruction` time — the multi-hole generalization
of the single-hole selector's own rules above:

- The mode must have at least one `one-of` hole; a table on a mode with none
  has nothing to select between.
- Every `(choice ...)`'s arity must match the number of participating holes,
  and each name must belong to its own hole's alternatives.
- **Every combination of the cross product must be claimed exactly once** —
  neither missing nor duplicated; there is no value-selected fallback for an
  unclaimed combination to resolve into, the same permanent-error rule the
  single-hole selector holds every alternative to.
- The cross product's size must fit the machine's code cell width, and `s`
  must be pairwise distinct and fit it too.
- A `(sub-opcode ...)` table and a per-hole `(variant (choice m) (sub s))`
  selector on another hole may not both be given, nor may a table and an
  explicit `(opcode n :sub s)` — all would be writing the same cell.

Decode, `operand-signedness`, and `operand-widths` (below) all work exactly
as the single-hole case describes, just across every hole the table names
rather than one — any number of `one-of` holes may now disagree on
`:signed` or `:width` at once, as long as each is one of the table's
participating holes.

### `operand-signedness`

Every byte-encoded `instruction-descriptor` carries a hole-aligned
`operand-signedness` list, parallel to `operand-widths` — entry *i* is `t`
when hole *i*'s operand is a signed quantity. For a hole not governed by any
`one-of`, this is just the mode's own `signedp`, unchanged from before
per-hole `:signed`. For a `one-of` hole whose alternatives agree on
signedness, it's their shared value. For a `one-of` hole whose alternatives
*disagree*, it's whichever alternative this particular sibling descriptor
was claimed for — the value in `sub-choices` at the same hole. Computed once
per expanded descriptor (`%byte-descriptor-forms`), not re-derived at decode
time, so the emulator's hot decode path never re-resolves a mode name per
instruction.

Every byte-encoded `instruction-descriptor` also carries a hole-aligned
`operand-widths` list — entry *i* is the encoded cell width of hole *i*'s
operand. For a hole not governed by any `one-of`, or whose `(operand ...)`
subclause gives its own width (`(operand :mode)`'s mode default, or an
explicit `(operand :width n)`), this is just that declared width, exactly
as before per-hole `:width`. For a `(operand :mode)` hole whose `one-of`
alternatives disagree on `:width`, it's whichever alternative this
particular sibling descriptor was claimed for, the same `sub-choices`
lookup `operand-signedness` uses — so two sibling descriptors sharing one
opcode can have genuinely different total sizes. Computed once per expanded
descriptor (`%byte-descriptor-forms`), for the same reason
`operand-signedness` is.

### `relative-hole-index`

Every byte-encoded `instruction-descriptor` also carries a single
`relative-hole-index` slot — `nil` when no hole of this descriptor is a
PC-relative offset, else the 0-based index of the one hole that is. Unlike
`operand-signedness`/`operand-widths`, which are hole-aligned *lists* (any
number of holes may independently be signed, or independently disagree on
width), `:relative` is **positional** — at most one hole of a pattern may
ever be the relative one (`%check-relative-mode-holes`/
`%check-byte-one-of-relative`, `instruction.lisp`) — so a single index
suffices. A whole-mode `relative` mode (`mode-descriptor-relativep`) always
has exactly one hole, so it always stamps `relative-hole-index` as `0`; a
`one-of` alternative declaring its own `:relative` (see [Addressing modes,
"Per-hole `:relative`"](modes.md#per-hole-relative)) stamps whichever hole
it belongs to, or `nil` for a sibling descriptor whose matched alternative
at that hole isn't relative. Both cases fold into this one slot, so every
consumer — the assembler's mode selector and `%encode` (see [Assembler,
"PC-relative offsets"](assembler.md#pc-relative-offsets)), and the
disassembler's operand rendering (see
[Disassembler](disassembler.md#relative-operand-rendering)) — reads
`relative-hole-index` uniformly rather than branching on
`mode-descriptor-relativep` separately. Computed once per expanded
descriptor (`%byte-descriptor-forms`, `%byte-relative-hole-index`), for the
same reason `operand-signedness`/`operand-widths` are. Always `nil` on a
word-encoded descriptor — `:relative` stays banned outright there
(`%check-word-relative`/`%check-word-one-of-relative`), since
`%relative-offset`'s arithmetic assumes a cell-counted operand width a
word-encoded operand doesn't have.

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

A `(one-of mode...)` pattern element ([Addressing modes, "Per-operand
modes"](modes.md#per-operand-modes)) counts as one hole, the same as a plain
`expr` — every alternative it names is validated at `defmode` time to have
the same hole count as every other, so `(operand ...)` subclause counting
here doesn't need to know or care whether a given hole came from a bare
`expr` or a `one-of`.

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
omitted.

Redefining a mnemonic (a repeated `definstruction`) replaces its whole
variant list; any old opcode not reused by the new list is dropped from the
opcode table, so a redefinition that drops a mode never leaves decode
resolving a stale opcode to a descriptor that no longer exists.

### Opcode to descriptor decode

`(find-instruction-by-opcode machine-name opcode)` returns one descriptor
registered under `opcode`; `(find-instruction-descriptors-by-opcode
machine-name opcode)` returns every descriptor registered there. On a
byte-encoded machine these agree whenever `opcode`'s bucket has no `:sub`
co-tenancy (below) — the ordinary case, exactly one descriptor, enforced at
`definstruction` time. On a word-encoded machine an opcode can carry several:
sibling combos one `(modes ...)` clause's own operand-field variants expand
into (`%expand-word-combos`, always compatible with each other), and,
independently, several genuinely distinct descriptors — different mnemonics,
or one mnemonic's different modes — that `definstruction` has verified are
*decode-distinguishable*: some operand field's raw bits accept disjoint value
sets between every such pair. Decode (`decode-instruction-at`) tries each
candidate registered at an opcode in turn and returns the first whose fields
the fetched bits actually match; since co-tenants are pairwise disjoint at
some hole, at most one can ever match a given word, so this is never a race
between overlapping candidates.

Declaring two descriptors at one opcode that are *not* decode-distinguishable
is a `definstruction`-time `opcode-conflict` error, not a silent
last-write-wins overwrite: an unrelated mnemonic already claiming the opcode
(as before); the same mnemonic under a second `(modes ...)` clause whose
fields can't be told apart from the first's; or, on a byte-encoded machine,
a second descriptor at all when neither one declares its own `:sub` — a byte
encoding has no per-field discriminator for decode to key off there, so two
modes of one mnemonic (or two different mnemonics) may not share an opcode
regardless of what their operand syntax looks like, *unless* every descriptor
sharing that opcode gives itself a distinct `:sub` value (see `(opcode n
:sub s)` above) — that sub-opcode cell is exactly the per-instruction
discriminator a byte encoding otherwise lacks. Mixing a `:sub`-bearing
descriptor with a `:sub`-less one at the same opcode is still an error
(`opcode-conflict`'s `:sub-opcode-required` reason) — decode couldn't tell
whether the cell after the opcode is a sub-opcode or the first operand —
and so is reusing the same `:sub` value twice (`:duplicate-sub-opcode`).

A hole-selected sub-opcode (above) reaches the same registration check from
a single `definstruction` form rather than two: one `(operand ...)`
subclause claiming a `one-of` hole's alternatives expands into one
descriptor per alternative, all sharing one mnemonic, mode, and opcode.
Registration's pairwise check runs on these exactly as it would on unrelated
co-tenants, and passes because their `:sub` values are pairwise distinct by
construction (`definstruction` itself enforces this before registration ever
sees them) — so a hole-selected sub-opcode's own descriptors never trip
`:duplicate-sub-opcode` or `:sub-opcode-required` in ordinary use.

```lisp
(defmode a-reg expr)
(defmode a-lit "#" expr)

(definstruction anima16foo ld
  (modes
    (a-reg (opcode 1)
      (operand dst :field b)
      (operand src :field a (variant (range 0 7) inline))
      (semantics (set! (reg dst) (reg src))))
    (a-lit (opcode 1)
      (operand dst :field b)
      (operand lit :field a (variant (range 0 30) inline :bias 33))
      (semantics (set! (reg dst) lit)))))
```

`ld 1, 0` and `ld 1, #5` share opcode 1, told apart purely by which raw
values field `a` falls into (0–7 for the register form, 33–63 for the
literal form) — `ld 1, 0` decodes back to the register mode, `ld 1, #5` to
the literal mode, neither `:decode-failure` nor the other's mode. See
[`examples/sharedopcode.lisp`](../examples/sharedopcode.lisp) for this run
end to end, including a second mnemonic sharing an opcode the same way, and
[`examples/subopcode.lisp`](../examples/subopcode.lisp) for the byte-machine
`:sub`-opcode counterpart.

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

### CHOICE-selected word fields (#104)

Both `(variant ...)` forms above pick an encoding purely from the operand's
*value*. A third selector picks by *syntax* instead — which alternative of a
`(one-of mode...)` pattern element (see [Addressing modes, "Per-operand
modes"](modes.md#per-operand-modes)) a hole actually matched:

```lisp
(operand [NAME] :field FIELD-NAME
  [(variant (choice MODE) inline :range (LO HI) [:bias N])
   (variant (choice MODE) (extra-word :escape N))]*)
```

`MODE` must be one of the alternatives named by the `one-of` element that
produced this hole — declaring `(choice mode)` for a hole that isn't a
`one-of` at all, or naming a mode that isn't one of *that* `one-of`'s own
alternatives, is a `definstruction`-time error. A `choice`-selected `inline`
variant requires its own `:range (LO HI)` — unlike `(range LO HI)`, the
selector itself carries no range to double as one. A `choice`-selected
`(extra-word :escape N)` variant writes `N` into the field and spends the
value's own trailing word **unconditionally** once `MODE` is the matched
alternative, regardless of what the value actually is — unlike `:else`,
which only escapes when no inline variant's range fits.

A field's variants may freely mix `choice`-selected and value-selected
(`range`/`:else`) ones (#118) — the value-selected variants are then
selected, at both assemble and decode time, by whichever `one-of`
alternative no `choice`-selected variant on this field already claims.
There must be exactly one such unclaimed alternative: zero means every
alternative already routes to a `choice`-selected variant, so the
value-selected ones could never be selected at all; two or more means
nothing tells decode which of them the value-selected variants belong to —
both are `definstruction`-time errors. Given exactly one, `definstruction`
resolves the value-selected variants' own choice to it silently — no syntax
marks this in the `(variant ...)` form itself, since there is nothing left
to disambiguate once every other alternative is claimed. Every
`choice`-selected range and escape must fit
`FIELD-NAME`'s bit width the same as a value-selected one, and — reachable
now that several variants of one kind can share a field — no two inline
ranges may overlap and no two escapes may collide, checked the same way as
the existing inline-vs-escape ambiguity check, all at `definstruction` time.

```lisp
(defmode a-reg expr)
(defmode a-ind "[" expr "]")
(defmode a-mem "(" expr ")")
(defmode a-lit "#" expr)
(defmode ld-mode expr "," (one-of a-mem a-ind a-lit a-reg))

(definstruction anima16foo ld
  (modes ld-mode)
  (encoding
    (opcode 1)
    (operand dst :field b)
    (operand src :field a
      (variant (choice a-reg) inline :range (0 7) :bias #x00)
      (variant (choice a-ind) inline :range (0 7) :bias #x08)
      (variant (choice a-mem) (extra-word :escape #x1e))
      (variant (range -1 30) inline :bias 33)
      (variant :else (extra-word :escape #x1f))))
  (semantics (set! (reg dst) src)))
```

`a-reg`, `a-ind`, and `a-mem` are `choice`-selected; `a-lit` — the one
`ld-mode` alternative none of them claims — is not, so its two
value-selected variants are resolved to it. `ld 1, 0` and `ld 1, [0]` encode
into *different* field codes for the identical value 0 (disjoint biased
halves of `a`'s 0–15 sub-range); `ld 1, (0)` always spends a trailing word,
however small the address; `ld 1, #5` packs its literal inline in a third
sub-range, purely because `#5`'s syntax matched `a-lit`, exactly the way the
other three rows are chosen by syntax — the only difference is that its own
variant then also filters by *value*, same as an ordinary value-selected
field would. Selection among all five variants at assemble time is
`%choose-variant`'s job (see [Assembler](assembler.md#choosing-a-mode)) — a
candidate whose field is `choice`-selected (which, after resolution, means
every variant on a mixed field) is dropped outright unless its `MODE` is
the alternative the hole actually matched, before the ordinary floor/value
filters ever run. A matched-but-out-of-range value — whether the field code
is nominally `choice`-selected or a resolved value-selected one — has no
wider sibling on this field to relax into, so this is an `assembly-error`
rather than a silent wrap; `a-lit`'s own inline/`:else` pair is exactly the
value filter's ordinary business, so `ld 1, #5` still packs inline and
`ld 1, #1000` still escapes to its own word, purely by value, once `a-lit`'s
variant is the eligible one.

Encoding is only half the picture — every sibling descriptor `ld`'s five
variants expand into still shares one *body*, but that body can read back
which alternative was actually matched, `a-lit` included: `choice-case`
(see
[Semantics vocabulary](semantics.md#choice-case--dispatching-on-a-matched-addressing-mode-alternative))
dispatches on it directly, so `[reg]` really means "dereference" while a
bare `reg` means "use the value directly" (see [Semantics vocabulary,
"`choice-case`"](semantics.md#choice-case) for the full picture, including a
cell-encoded machine's own hole-selected sub-opcode form above — `choice-case`
reads back a real matched alternative there too, not only on a word-encoded
machine):

```lisp
(semantics
  (set! (reg dst)
    (choice-case src
      (a-reg (reg src))
      (a-ind (mref machine 'ram (reg src)))
      (a-mem (mref machine 'ram src))
      (a-lit src))))
```

See [`examples/anima16.lisp`](../examples/anima16.lisp) for this run end to
end, including the decode/disassemble round-trip — the matched
alternative's own syntax renders back for every one of the five forms,
`choice`-selected or resolved value-selected alike, not always the
`one-of`'s first alternative (see [Disassembler](disassembler.md)).

A `:relative` addressing mode is not supported on a word-encoded machine —
its offset arithmetic (`%relative-offset`, [Assembler](assembler.md))
assumes a cell-counted operand width.

#### `CHOICE`-selected fields and `:SIGNED`

A `choice`-selected variant's own `MODE` may declare `:signed t` — see
[Addressing modes, "Per-hole `:signed`"](modes.md#per-hole-signed). Unlike a
plain value-selected variant, whose negative-value sign is carried entirely
by its `:bias` (decoded back by subtracting the same bias, no
two's-complement reinterpretation involved), a `choice`-selected variant's
raw field bits are reinterpreted as two's-complement — over its own field
width for an `inline` variant, or over the fetched extra word's own width
for an `(extra-word ...)` one — before debiasing or handing the value back,
whenever the matched `MODE` says `:signed t`. This is stamped once per
`word-field-choice`, at `definstruction` time, from the `choice`d mode's own
`signedp`; a value-selected variant (no `choice` of its own) is never
signed, even after #118's mixed-field resolution gives it a `choice`
retroactively — there is no `one-of` alternative for it to read `:signed`
off in the first place, so a mixed field combining a value-selected
fallback with a signed `one-of` alternative is rejected at `definstruction`
time rather than silently doing nothing.

```lisp
(defmode a-pos expr)
(defmode a-neg "#" expr :signed t)
(defmode a-mix (one-of a-pos a-neg))

(definstruction anima16foo seta
  (modes a-mix)
  (encoding
    (opcode 3)
    (operand val :field a
      (variant (choice a-pos) inline :range (0 31) :bias 0)
      (variant (choice a-neg) inline :range (-32 -1) :bias 0)))
  (semantics (set! (reg 0) val)))
```

`seta 20` decodes back as the plain unsigned `20`; `seta #-10` decodes back
as the signed `-10` — the same field, two different reinterpretations,
decided purely by which alternative was written (see
[`examples/anima16.lisp`](../examples/anima16.lisp) for this run end to
end, disassembly included).

A `one-of` hole whose alternatives disagree on `:signed` must have *every*
one of its field variants `choice`-selected — `definstruction` signals an
error otherwise, since a value-selected fallback has no decode-time record
of which alternative, and so which signedness, a raw value came from.
Alternatives that agree on `:signed` need no such coverage at all.

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

## Note on operand binding

`operand` is always bound to the raw decoded integer regardless of mode,
and semantics dereferences explicitly (`(mref machine 'ram operand)`);
`defmode`'s pattern accordingly has no `-> tag` arrow to produce one (see
[Addressing modes](modes.md)). This means `definstruction` never has to
guess which memory element an address-shaped operand addresses — a guess
that stops being safe once a machine declares more than one memory region
(M5) — and it's why a multi-mode instruction like `LDA` above needs a
per-mode semantics override for `immediate` (a value) while
`zero-page`/`absolute` share one default (an address).
