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
then sets the PC register to `origin`. The write burns the image in directly
regardless of any [memory region](machine-model.md#memory-regions) at
`origin` — a `:rom` region's write protection doesn't apply, since a ROM
image is burned in rather than stored by the CPU.

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

The fetch/decode logic — both paths described below — is shared with
`decode-instruction-at` (see [Disassembler](disassembler.md#decode-instruction-at)).
`step-machine` resolves the selected memory's cell width and endianness,
decodes through that shared logic, advances `pc` by the decoded size, and
executes. Looped runs resolve those properties once for the run. Direct
`decode-instruction-at` calls resolve them for each call. It returns the
executed descriptor and its cycle cost, `:decode-failure` and `0`, `:idle`, or
`:nop` when an [`(undefined-opcode :nop)`](machine-families.md#undefined-opcodes)
machine steps over an instruction it does not implement.

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

Each signed operand field is decoded as a signed integer before semantics
runs. A relative field is always signed and contains an offset from the
address after the complete instruction. On a byte-encoded machine, the
descriptor's `operand-signedness` list selects the fields to sign-extend;
on a word-encoded machine, each matched field choice carries its own
signedness. Other fields remain unsigned. See [Addressing modes](modes.md#per-hole-relative).

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
`:cell-width`, #53), and selects a descriptor matching the fetched bits
(see [Instructions, "Opcode to descriptor
decode"](instructions.md#opcode-to-descriptor-decode)). Words up to 16 bits
use a shared table with one descriptor reference per possible word, at most
512 KiB of entries on a 64-bit host. The first decode builds the table;
subsequent decodes use direct lookup. Wider words scan their opcode bucket.
Define instructions and warm the decoder before a latency-sensitive loop.

Operand emission order is precomputed when alternative field layouts agree.
Decode results are local to each call, and generated semantics read through
operand mappings without copying operand lists. Independent machines can
decode concurrently; definition changes require callers to stop execution
first. Machine redefinition creates a fresh descriptor and dispatch table.
For measurements and a runnable benchmark, see [Memory audit](memory-audit.md).

Each operand field is decoded against the selected descriptor's
`word-alternatives`: a fetched
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

### Device ticking (#108)

`step-machine` also ticks every live [device](devices.md) on the machine's
bus with the step's declared cycle cost — beside where `machine-cycles` is
incremented, so a trapping instruction's devices still tick instead of that
step silently going missing from device time. Cycles added with
[`extra-cycles`](#dynamic-cycle-costs-90) tick devices a second time, after
the semantics return. Not called on a `:decode-failure`, where nothing
executed and no time elapsed.
This runs the same way regardless of entry point — `run`/`run-for-cycles`/
`run-for-duration` below and the debugger's single-instruction `debug-step`
all go through `step-machine`.

### Interrupt delivery (#109)

`step-machine` also delivers a pending, unmasked interrupt — if the
machine declares an `(interrupts ...)` clause and one is queued — before
this step's own fetch even begins. Delivery pushes state, writes the
signal's data and vector, then this same step continues into its ordinary
fetch/decode/execute, now reading from the handler: one step both
delivers the interrupt and executes the handler's first instruction. Zero
cost on a machine declaring no `(interrupts ...)` clause. See
[Interrupts](interrupts.md#delivery) for the full model. Like device
ticking above, this runs identically under every entry point —
`run`/`run-for-cycles`/`run-for-duration` and the debugger's `debug-step`
all go through `step-machine`.

### Idle steps (#110)

After that delivery attempt, if the machine is still idle — the `idle`
semantics primitive ([Semantics vocabulary](semantics.md)) ran on some
earlier step and nothing has woken it since — `step-machine` ticks devices
and adds one cycle to `machine-cycles`, but does not fetch, decode,
execute, or advance `pc`. Returns `(values :idle 1)` instead of an
`instruction-descriptor`. Checked *after* delivery, not before, so a
signal delivered this same step both wakes the machine and executes the
handler's first instruction — the same one-step coincidence delivery
itself gets against an ordinary fetch. See [Interrupts, "Waking an idle
machine"](interrupts.md#waking-an-idle-machine-110) for how a machine
wakes back up. TODO: the idle cost is fixed at 1 cycle — a declarable
idle cost is a follow-up ticket (#164).

## `run`

```lisp
(run MACHINE &key pc memory (max-steps 10000))
;; => (values reason steps [condition])
```

Calls `step-machine` in a loop until one of several stop conditions:

### Stop reasons

| `reason` | Meaning |
|---|---|
| `:trap` | An instruction's semantics called `trap` (see [Semantics vocabulary](semantics.md)), signalling `lasm-trap`. `run` catches it; the condition itself is the third return value. This *is* M1's halt mechanism — no dedicated halt primitive exists, or is needed: `(definstruction m hlt (encoding (opcode #x00)) (semantics (trap :halt)))` is enough. `trap` and #109's interrupt delivery remain two separate mechanisms; a model unifying them is future M6 work. |
| `:fault` | A storage access signalled `storage-error`, including stack, register-bank, and memory range errors. The condition is the third return value. |
| `:decode-failure` | `step-machine` hit a cell that isn't a registered opcode — typically a program with no `hlt` running off the end into zeroed (unassigned) memory, which decodes as opcode `0`. A machine's [`(undefined-opcode ...)`](machine-families.md#undefined-opcodes) clause can instead skip the instruction (`step-machine` returns `:nop`) or trap. |
| `:idle` | #110: the machine went idle (see [Idle steps](#idle-steps-110) above) and, with `run`'s own no-budget call, nothing left running it could ever wake it back up — its pending interrupt queue is empty and no live device remains on its bus. A host can `signal-interrupt` or `wake-machine` and call `run` again, exactly as it already can after `:trap`. `run-for-cycles`/`run-for-duration` are unaffected by this check — an idle step there just keeps costing cycles until their own budget stops the loop. |
| `:max-steps` | `max-steps` instructions executed without stopping otherwise — a runaway-program guard, not a cycle timer. `run-for-cycles`/`run-for-duration` below add the cycle-based budgets `(cycles n)` was accepted for. |
| `:max-cycles` | `run-for-cycles` only — see below. |
| `:duration` | `run-for-duration` only — see below. |

`steps` counts completed steps and attempts that trap or fault. A fetch or
interrupt-delivery storage fault counts as one attempted step. A step that
fails to decode does not count. An idle step
(#110) counts too, even though it executes no instruction — it still cost
a cycle and ticked devices, the same reasoning that counts a trapping step.
An instruction's declared cycle cost is added before its semantics runs.
A fault during fetch adds no instruction cycles. Faults leave machine state
as it stands when signalled. Direct `step-machine` calls still signal the
condition. `run-for-cycles` and `run-for-duration` return the same `:fault`
shape. Errors outside stepping, including run-loop callbacks, still signal.

```lisp
(multiple-value-bind (reason steps condition) (run machine)
  (when (eq reason :fault)
    (format *error-output* "Fault after ~D steps: ~A~%" steps condition)))
```

`interrupt-queue-full` is not a `storage-error`; its `:on-overflow :error`
path still signals. Its `:on-overflow :trap` path returns `:trap`.

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

This is a *static* per-mode cost, fixed at `definstruction` time.

### Dynamic cycle costs (#90)

A cost that depends on runtime state — a page-crossing or branch-taken
penalty — is added from inside `(semantics ...)` with `(extra-cycles n)`:

```lisp
(definstruction m bne
  (modes relative)
  (encoding (opcode #xD0) (operand :mode))
  (cycles 2)
  (semantics (when (zerop z)
               (when (page-crossed? pc (+ pc operand)) (extra-cycles 1))
               (set! pc (+ pc operand))
               (extra-cycles 1))))
```

`step-machine` adds the accumulated extra to `machine-cycles` — including
when the instruction traps — and returns declared cost plus extra as its cost
value, so `run-for-cycles`, `run-for-duration` and `machine-elapsed-seconds`
all see it. Devices receive the extra in a second tick once the semantics
return (not on a trap); a machine that never calls `extra-cycles` is ticked
once per step, as before. `machine-extra-cycles` holds the running
instruction's extra and is zeroed at the start of each step and by `reset`.

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

- Privilege levels, or a generalized trap/interrupt model unifying `trap`
  with #109's interrupt delivery — future M6 work. Interrupt delivery
  itself is covered — see "Interrupt delivery (#109)" above and
  [Interrupts](interrupts.md). CPU idle/sleep resumed by an interrupt is
  likewise covered — see "Idle steps (#110)" above and [Interrupts, "Waking
  an idle machine"](interrupts.md#waking-an-idle-machine-110).
- Recovering source text from encoded cells — see [Disassembler](disassembler.md)
  (#21), built on this file's own `decode-instruction-at`.
- Stopping at a chosen point and inspecting live state interactively — see
  [Debugger](debugger.md) (#76), built directly on `step-machine`/`run`.
