# Emulator

A tree-walking fetch/decode/execute loop running encoded cells (see
[Assembler](assembler.md)) against a live `machine` (see [Machine
model](machine-model.md)), dispatching on opcode to each instruction's
semantics (see [Instructions](instructions.md)).

```lisp
(let ((m (make-machine 'sixtyfoo))
      (a (assemble source :lexer 'sixtyfoo-syntax :machine 'sixtyfoo)))
  (load-program m a)
  (run m))
```

See [`examples/counter.lisp`](../examples/counter.lisp) for a runnable
version.

## PC and program memory: convention, not declaration

M1 adds no new `defmachine` storage kind for "the program counter" or "the
program's memory." Instead, `load-program`, `step-machine`, and `run` all
default to:

- **PC** — the register named `pc`.
- **Program memory** — the machine's sole `:memory` storage element.

This is the same "PC is a plain register" convention already used
throughout [Instructions](instructions.md) and its examples (`(register pc
:width 16)`, `(set! pc operand)` in a branch's semantics). A machine that
names its PC register something else, or declares more than one memory
element, passes `:pc` / `:memory` explicitly to override — the same escape
hatch `%default-absolute-width` already uses for `absolute` mode's default
operand width when more than one memory element is declared.

## `load-program`

```lisp
(load-program MACHINE cells &key memory origin)
```

`cells` is an `assembly` (see [Assembler](assembler.md)) or any sequence of
`(unsigned-byte n)`. Writes each cell into `memory` starting at `origin`,
then sets the PC register to `origin`.

`origin` defaults to the assembly's own `origin` slot when `cells` is an
`assembly` — so `(assemble source :origin #x200)` and `load-program`
*cannot* silently disagree about where the program's labels point — and to
`0` otherwise.

When `cells` is an `assembly`, its own `assembly-cell-width` (#53, see
[Assembler](assembler.md#assemblys-cell-width)) must match `memory`'s
declared `:cell-width` — `load-program` signals otherwise, rather than
silently placing every cell one address too far apart, which is what would
happen if a program assembled against a byte-addressed memory element were
loaded into a word-addressed one with no other symptom.

## `step-machine`

```lisp
(step-machine MACHINE &key pc memory)
```

The fetch/decode step itself — both paths described below — lives in
`decode-instruction-at` (see [Disassembler](disassembler.md#decode-instruction-at)),
a pure function taking a cell-reading closure rather than a live `machine`
directly; `step-machine` resolves `pc`/`memory` as described here, calls it,
advances `pc` by the decoded size, and executes. The disassembler
(`disassemble-cells`/`disassemble-assembly`/`disassemble-memory`) calls the
same function, so encoded cells decode identically whether they're about to
be executed or merely read back as text.

Fetches the opcode cell at `pc`, decodes it (`find-instruction-by-opcode`),
reads its declared `operand-widths` fields in the machine's own endian
order (`:little` by default, #66) **one after another**, each cell masked
at the machine's own `:cell-width` (#53 — 8 bits
on every byte-addressed machine) rather than a fixed 8 (each field's own
width, in the order `definstruction` wired them up — see [Instructions,
"Repeated `(operand ...)` subclauses"](instructions.md)),
**advances `pc` past the
whole instruction (opcode plus every field), then executes its semantics**
— in that order. This ordering is what lets a branch instruction's own
`(set! pc operand)` override the increment rather than being clobbered by
it running afterward. It's also the base the assembler computes a
`relative`-mode offset from (see below) — `pc` is already the *next*
instruction's address by the time semantics runs.

If the decoded instruction's mode is `:signed`
([Addressing modes, "Signed operands"](modes.md#signed-operands)) — `relative`
([Addressing modes, "PC-relative modes"](modes.md#pc-relative-modes)) included,
since `:relative` implies `:signed` — every fetched field is reinterpreted as
a signed integer (`signed-value`, each by its own operand width) before being
passed to `execute-instruction`. The assembler encoded a `relative` operand
specifically as a two's-complement offset
([Assembler](assembler.md#pc-relative-offsets)); either way, fetching treats
every operand as unsigned like any other mode, so this is undone here rather
than in every signed instruction's own `semantics`. A `relative` instruction's
body therefore just writes `(set! pc (+ pc operand))`. A `relative` mode
always has exactly one field (`definstruction` rejects one with more,
[Instructions, "Repeated `(operand ...)` subclauses"](instructions.md)), so
this reinterprets the sole fetched value for it, never several — but an
ordinary (non-`relative`) `:signed` mode may have more than one field, and
each is reinterpreted independently. This is per hole, not just per whole
mode: a `one-of` hole with per-hole `:signed` ([Addressing modes, "Per-hole
`:signed`"](modes.md#per-hole-signed)) is reinterpreted only when the
matched descriptor's own `operand-signedness` (instruction.lisp) says that
hole is signed — a sibling descriptor claimed for the mode's unsigned
alternative leaves the same hole untouched.

Returns `(values result cost)`: `result` is the executed
`instruction-descriptor`, or the keyword `:decode-failure` (without
advancing `pc` or executing anything, `cost` `0`) if the cell at `pc` isn't a
registered opcode on this machine. `cost` is the executed instruction's
cycle cost (`(cycles n)`, [Instructions](instructions.md#cycles-n) — 1 when
undeclared), already added to `machine-cycles` — see "Cycle-cost model and
clock speed" below.

### Word-encoded machines (#20)

On a machine declaring an `instruction-word` clause ([Machine
model](machine-model.md)), `step-machine` instead fetches one whole
instruction word (in the layout's own endian order, #66,
`instruction-word-layout-width-cells` cells at the machine's own
`:cell-width`, #53), extracts its `opcode` field,
and tries every `instruction-descriptor` registered under that opcode in
turn (see [Instructions, "Opcode to descriptor
decode"](instructions.md#opcode-to-descriptor-decode)) — one candidate on a
mnemonic with no co-tenant, several when sibling combos of one operand-field
variant share the opcode, or when `definstruction` has confirmed several
genuinely distinct descriptors are decode-distinguishable there — decoding
each operand field against a candidate's own `word-alternatives` (every
variant its `definstruction` declared, not just one combo) and returning the
first candidate whose fields the fetched bits actually match: a fetched
field value equal to some alternative's `:escape` means the real value
follows in its own word (fetched and consumed in turn); a value inside some
alternative's biased inline range means the value *is* the field, debiased.
A raw value matching no candidate's alternatives at all is `:decode-failure`,
the same as an unregistered opcode. `pc` advances by the actual number of
cells consumed — the matched candidate's instruction word plus, for each
`extra-word` field decoded, that matched variant's own declared cell width
(#135, `:cells`) — which need not match any one candidate's own
`extra-cells` total, since decode reconstructs the real encoding from the
fetched bits rather than trusting which combo it happened to try first.

A value-selected field's negative-value handling is entirely its variant's
`:bias` (see [Instructions, "Word-encoded
instructions"](instructions.md#word-encoded-instructions-20)), already
undone by the debiasing above — no sign extension happens there. A
`choice`-selected field's own matched `MODE` may declare `:signed t`
instead (see [Instructions, "`CHOICE`-selected fields and
`:SIGNED`"](instructions.md#choice-selected-fields-and-signed)): its raw
field bits (or, for an `extra-word` variant, the fetched extra word) are
reinterpreted as two's-complement before debiasing, the word-encoded
analogue of the whole-mode `:signed` reinterpretation above.

`decode-instruction-at` returns a fourth value on both paths: `choices`, the
`one-of` alternative each operand hole actually matched (a word-encoded
field's own `word-field-choice`, or, on a cell-encoded machine, the matched
descriptor's own `sub-choices` when it declares a sub-opcode selector at one
or more holes (see [Instructions, "hole-selected
sub-opcode"](instructions.md#variant-choice-m-sub-s--hole-selected-sub-opcode)
and ["multi-hole sub-opcode
selection"](instructions.md#sub-opcode--multi-hole-sub-opcode-selection))
— `nil` throughout when there is no such record). `step-machine` forwards
it straight through to `execute-instruction`, so a `(semantics ...)` body's
`choice-case` (see [Semantics vocabulary,
"`choice-case`"](semantics.md#choice-case)) sees exactly what was actually
decoded, not just the operand values, on either encoding scheme.

## `run`

```lisp
(run MACHINE &key pc memory (max-steps 10000))
;; => (values reason steps [condition])
```

Calls `step-machine` in a loop until one of three stop conditions:

### Stop reasons

| `reason` | Meaning |
|---|---|
| `:trap` | An instruction's semantics called `trap` (see [Semantics vocabulary](semantics.md)), signalling `lasm-trap`. `run` catches it; the condition itself is the third return value. This *is* M1's halt mechanism — no dedicated halt primitive exists, or is needed: `(definstruction m hlt (encoding (opcode #x00)) (semantics (trap :halt)))` is enough. A generalized interrupt/exception model replacing `trap` outright is M6. |
| `:decode-failure` | `step-machine` hit a cell that isn't a registered opcode — typically a program with no `hlt` running off the end into zeroed (unassigned) memory, which decodes as opcode `0`. |
| `:max-steps` | `max-steps` instructions executed without stopping otherwise — a runaway-program guard, not a cycle timer. `run-for-cycles`/`run-for-duration` below add the cycle-based budgets `(cycles n)` was accepted for. |
| `:max-cycles` | `run-for-cycles` only — see below. |
| `:duration` | `run-for-duration` only — see below. |

`steps` counts instructions that actually executed. A step that traps still
counts (its semantics ran to completion before signalling); a step that
fails to decode does not (nothing executed that iteration). The same rule
governs `machine-cycles` (below): a trapping instruction's cost is still
added (its semantics ran to completion before signalling); a decode failure
adds nothing.

**Not currently a stop reason:** a storage condition raised from inside an
instruction's semantics — `stack-overflow`, `stack-underflow`,
`stack-index-out-of-range`, `address-out-of-range` (see [Machine model,
"Conditions"](machine-model.md)) — propagates straight out of `run` as an
ordinary Lisp error, since
`step-machine` only catches `unknown-instruction` and `run` only catches
`lasm-trap`. `tests/emulator.lisp`'s `stack-underflow-escapes-run` and
`stack-overflow-escapes-run` pin this down as the current behaviour;
whether `run` should instead catch `storage-error` and return a fourth stop
reason is tracked as a follow-up.

## Cycle-cost model, clock speed, and cycle-accurate execution (#75)

Every `instruction-descriptor` carries a cycle cost — its own `(cycles n)`
clause ([Instructions](instructions.md#cycles-n)), or `1` when undeclared.
`step-machine` accumulates each executed instruction's cost onto
`machine-cycles`, a running total on the `machine` struct itself (not a
storage element, so it isn't touched by `sref`/`mref` and isn't zeroed by
iterating a machine's declared elements — `reset` zeroes it explicitly).
This accumulation happens unconditionally, regardless of whether the
machine's `defmachine` declares a clock speed:

```lisp
(defmachine sixtyfoo
  ...
  (clock-speed 1000000)) ; optional -- Hz, only needed to convert cycles to seconds
```

`(clock-speed n)` is optional and purely declarative — a machine with no
such clause can still read `machine-cycles` and call `run-for-cycles`; only
`run-for-duration` and `machine-elapsed-seconds` need one, since converting
a cycle count to a wall-time-equivalent duration has no other input.

```lisp
(machine-elapsed-seconds MACHINE)
;; => machine-cycles / declared clock-speed, as seconds. Pure arithmetic —
;; no timer touched. Signals if MACHINE's descriptor declares no clock-speed.

(run-for-cycles MACHINE cycles &key pc memory (max-steps 10000))
;; => (values reason steps [condition])
;; Like `run`, but also stops with :max-cycles once machine-cycles has
;; advanced by at least CYCLES since this call started. No clock-speed
;; needed -- a plain cycle budget.

(run-for-duration MACHINE seconds &key pc memory (max-steps 10000) throttle)
;; => (values reason steps [condition])
;; Like `run`, but also stops with :duration once the wall-time-equivalent
;; of the cycles consumed since this call started reaches SECONDS. Requires
;; a declared clock-speed -- signals otherwise, same message as
;; machine-elapsed-seconds.
```

Both budget checks happen **after** the step executes, not before — a
step's cost isn't known until it has already been decoded and run, so a
budget may be overshot by at most one instruction's own cost. This mirrors
`step-machine`/`run`'s own PC-then-execute ordering rather than adding a
second, inconsistent convention.

`run-for-duration`'s `:throttle` (default `nil`) additionally paces real
wall-clock time to match the simulated schedule, using
[`trivial-high-precision-timer`](https://sr.ht/~takeiteasy/trivial-high-precision-timer/)
(lasm's only dependency, resolved the same way as lasm itself — see [Getting
started](getting-started.md)). After each step it compares real elapsed time
against simulated elapsed time (`machine-cycles`-derived) and `sleep`s off
any surplus once it exceeds roughly a millisecond — recomputed from scratch
every step rather than accumulated, so it self-corrects instead of drifting.
SBCL's `sleep` floors near that same millisecond resolution, so sleeping on
every single step (each perhaps a few hundred nanoseconds of simulated time
on a fast fantasy CPU) would slow execution by orders of magnitude rather
than pace it — hence the threshold. With `:throttle nil` (the default),
`:duration` is purely a cycle budget expressed in simulated seconds; no
timer is touched at all, keeping the default path as cheap as
`run-for-cycles`.

### Per-mode cycle cost

A multi-mode instruction ([Instructions, "Multiple addressing
modes"](instructions.md)) can give each mode its own cost, overriding the
mnemonic's shared default for that mode alone:

```lisp
(definstruction m lda
  (modes
    (immediate (opcode #xA1) (semantics ...) (cycles 2))
    (zero-page (opcode #xA6) (semantics ...) (cycles 4)))
  (semantics ...)
  (cycles 1)) ; default for a mode that declares no cycles of its own
```

This is a *static* per-mode cost, fixed at `definstruction` time. Dynamic
adjustments that depend on runtime state — a page-crossing penalty, a
branch-taken penalty — are a separate, follow-up feature; they need the
step loop to inspect the actual operand/branch outcome, not just which
mode was chosen.

## Note on flags in your own semantics (#22)

`(setf flag)` treats its value as a Lisp boolean, not an integer 0/1 — `0`
is non-`nil`, so `(setf (flag m 'z) 0)` sets the flag, not clears it. Write
new instruction semantics to pass an actual boolean (e.g. `(zero? x)`,
`(bit-set? a 7)`), not a raw comparison result that happens to be an
integer.

## Scope

This covers fetch/decode/execute over already-encoded cells and a single
flat halt/decode-failure/step-budget stop model. Multiple addressing modes
per mnemonic ([Addressing modes](modes.md)) need no change here: a
byte-encoded mode variant still carries its own distinct opcode, and a
word-encoded machine's shared-opcode co-tenants (see [Instructions, "Opcode
to descriptor decode"](instructions.md#opcode-to-descriptor-decode)) are
resolved by `decode-instruction-at` itself, so `step-machine` sees one
correctly-matched descriptor regardless of how many modes or mnemonics an
opcode carries. It does not cover:

- Interrupts, privilege levels, or a generalized trap/interrupt model
  beyond the single `trap` primitive — M6.
- Recovering source text from encoded cells — see [Disassembler](disassembler.md)
  (#21), built on this file's own `decode-instruction-at`.
- Stopping at a chosen point and inspecting live state interactively — see
  [Debugger](debugger.md) (#76), built directly on `step-machine`/`run`.
