# Machine model

LASM machine state is a set of named **storage elements**, not a fixed
notion of "registers." Each element has its own width and access discipline,
which is what lets the same `defmachine` form describe register machines,
stack machines, and hybrids.

## `defmachine`

```lisp
(defmachine sixtyfoo
  (register a :width 8)
  (register x :width 8)
  (stack s :width 8 :depth 256)
  (memory ram :width 8 :addr-width 16)
  (flags z n c v))
```

`defmachine` parses its clauses into a `machine-descriptor` and registers it
under `NAME`. Registration happens inside an `eval-when` so the descriptor
is available **at macroexpansion time**, not only after the file is loaded —
this matters once instruction definitions need to resolve storage names and
widths against a machine defined earlier in the same file.

### Clauses

- `(register NAME :width n [:count n])` — a fixed-width storage cell.
  `:count n` (n > 1) declares a *banked* register (e.g. CHIP8's 16 `V`
  registers). **Currently `:count > 1` is parsed and stored on the
  descriptor but there is no indexed accessor yet** — only scalar
  (`:count 1`, the default) registers are readable/writable. Indexed access
  is tracked as a follow-up ticket.
- `(stack NAME :width n :depth n)` — a fixed-depth LIFO stack of `:width`-bit
  values. Grows upward: the stack pointer starts at 0 and always equals the
  number of live entries, incrementing on push and decrementing on pop.
  Overflow/underflow signal `stack-overflow`/`stack-underflow`; see
  "Conditions" below for how those interact (or currently don't) with `run`.
  The stack pointer itself is internal bookkeeping, not an addressable
  storage element — `stack-depth` is the only way to read it, and there is
  no `stack-ref`/`(setf stack-ref)` for indexed access into the stack (no
  `PICK`/`OVER`, no stack-relative addressing mode; tracked as a follow-up).
  A machine with only a stack (plus PC and memory) is a valid, fully
  expressible machine — see [`examples/stack.lisp`](../examples/stack.lisp),
  the M3 milestone's validation that this abstraction isn't secretly
  register-shaped.
- `(memory NAME :width n :addr-width n [:cell-width n])` — addressable
  storage. `:addr-width` is the number of address bits (so the element has
  `2^addr-width` cells); `:cell-width` is the bit width of each cell and
  defaults to `:width` (byte-addressed). Set `:cell-width` different from 8
  for word-addressed memory (DCPU-16-style). Out-of-range addresses signal
  `address-out-of-range`.
  Memory is currently allocated eagerly as one array of `2^addr-width`
  cells. This is deliberately kept behind the constructor in `storage.lisp`
  so a later region-mapped/sparse backend (ROM/RAM/MMIO, banking) can
  replace it without changing `mref`/`(setf mref)` call sites.
- `(flags NAME...)` — one or more single-bit flags.

Widths and depths must be positive integers; duplicate element names and
unknown clause heads are compile-time errors.

### Deviation from the design draft

[`LASM-plan.md`](../LASM-plan.md) §3.1 uses `flags` for both the
`defmachine` declaration clause and a semantics operator that *sets* flags
(§3.2: `(flags (C ...) (Z ...))`). LASM resolves that name collision by
naming the semantics operator `set-flags!` instead — see
[Semantics vocabulary](semantics.md). The draft itself is left as a rough
plan and not edited to match.

## Runtime state

`(make-machine 'NAME)` instantiates a fresh runtime `machine` for a
registered descriptor. `(reset machine)` zeroes every storage element.

## Width and signedness

All storage is **unsigned**, masked to its declared width on every write
(`wrap-value`) — writing 300 to an 8-bit register stores 44, writing -1
stores 255. Where a value should be read as two's-complement, use
`(signed-value value width)` explicitly; there is no separate signed storage
mode. This is a single rule, chosen once, rather than a per-element
signed/unsigned mode.

## Accessors

| Element kind | Read | Write |
|---|---|---|
| register / flag | `(sref machine name)` / `(flag machine name)` | `(setf (sref machine name) v)` / `(setf (flag machine name) v)` |
| stack | `(stack-pop machine name)`, `(stack-depth machine name)` | `(stack-push machine name v)` |
| memory | `(mref machine name address)` | `(setf (mref machine name address) v)` |

`flag` treats any non-`nil` value as 1 and `nil` as 0 on write, and reads
back as `0`/`1`.

## Conditions

All signalled conditions inherit `lasm-error`: `unknown-storage`,
`address-out-of-range`, `stack-overflow`, `stack-underflow`. `lasm-trap` is
signalled by the `trap` semantics operator (see
[Semantics vocabulary](semantics.md)) and is not a storage error.

None of these storage conditions are currently a `run` stop reason (see
[Emulator, "Stop reasons"](emulator.md#stop-reasons)) — an instruction that
overflows or underflows a stack, or addresses memory out of range, escapes
`run` as a raw Lisp condition rather than returning `:trap`,
`:decode-failure`, or `:max-steps`.
