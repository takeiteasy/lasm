# Emulator

A tree-walking fetch/decode/execute loop running encoded bytes (see
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
(load-program MACHINE bytes &key memory origin)
```

`bytes` is an `assembly` (see [Assembler](assembler.md)) or any sequence of
`(unsigned-byte 8)`. Writes each byte into `memory` starting at `origin`,
then sets the PC register to `origin`.

`origin` defaults to the assembly's own `origin` slot when `bytes` is an
`assembly` — so `(assemble source :origin #x200)` and `load-program`
*cannot* silently disagree about where the program's labels point — and to
`0` otherwise.

## `step-machine`

```lisp
(step-machine MACHINE &key pc memory)
```

Fetches the opcode byte at `pc`, decodes it (`find-instruction-by-opcode`),
reads its declared `operand-widths` fields little-endian **one after
another** (each field's own width, in the order `definstruction` wired them
up — see [Instructions, "Repeated `(operand ...)` subclauses"](instructions.md)),
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
each is reinterpreted independently.

Returns the executed `instruction-descriptor`, or the keyword
`:decode-failure` (without advancing `pc` or executing anything) if the byte
at `pc` isn't a registered opcode on this machine.

### Word-encoded machines (#20)

On a machine declaring an `instruction-word` clause ([Machine
model](machine-model.md)), `step-machine` instead fetches one whole
instruction word (little-endian, `instruction-word-layout-width-bytes`
bytes), extracts its `opcode` field to find the `instruction-descriptor`
(`find-instruction-by-opcode`, same as the byte-encoded path — a
word-encoded family's several sibling descriptors, one per operand-field
variant, all share one opcode value, so *whichever* sibling occupies the
opcode table works equally well here), then decodes each operand field
against that descriptor's `word-alternatives` — every variant its
`definstruction` declared, not just the one combo the opcode table happens
to hold: a fetched field value equal to some alternative's `:escape` means
the real value follows in its own word (fetched and consumed in turn); a
value inside some alternative's biased inline range means the value *is*
the field, debiased. A raw value matching no declared alternative at all is
`:decode-failure`, the same as an unregistered opcode. `pc` advances by the
actual number of words consumed — the instruction word plus one per
`extra-word` field decoded, which need not match the registered sibling's
own `extra-words` count, since decode reconstructs the real encoding from
the fetched bits rather than trusting which combo it happened to look up.

No `:signed`-mode sign extension happens on this path — a word-encoded
field's negative-value handling is entirely its variant's `:bias` (see
[Instructions, "Word-encoded instructions"](instructions.md#word-encoded-instructions-20)),
already undone by the debiasing above.

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
| `:decode-failure` | `step-machine` hit a byte that isn't a registered opcode — typically a program with no `hlt` running off the end into zeroed (unassigned) memory, which decodes as opcode `0`. |
| `:max-steps` | `max-steps` instructions executed without stopping otherwise — a runaway-program guard, not a cycle timer (`(cycles n)` on `definstruction` is parsed but not used yet — a separate follow-up). |

`steps` counts instructions that actually executed. A step that traps still
counts (its semantics ran to completion before signalling); a step that
fails to decode does not (nothing executed that iteration).

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

## Note on flags in your own semantics (#22)

`(setf flag)` treats its value as a Lisp boolean, not an integer 0/1 — `0`
is non-`nil`, so `(setf (flag m 'z) 0)` sets the flag, not clears it. Write
new instruction semantics to pass an actual boolean (e.g. `(zero? x)`,
`(bit-set? a 7)`), not a raw comparison result that happens to be an
integer.

## Scope

This covers fetch/decode/execute over already-encoded bytes and a single
flat halt/decode-failure/step-budget stop model. Multiple addressing modes
per mnemonic ([Addressing modes](modes.md)) need no change here: each mode
variant carries its own distinct opcode, so `find-instruction-by-opcode`'s
decode step stays one-to-one regardless of how many modes a mnemonic
declares. It does not cover:

- Cycle-accurate timing using `(cycles n)` — undecided, tracked separately.
- Interrupts, privilege levels, or a generalized trap/interrupt model
  beyond the single `trap` primitive — M6.
- A disassembler recovering source from encoded bytes — M7 (#21).
