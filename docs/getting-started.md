# Getting started

## Install

LASM is not yet on Quicklisp. Clone it into a location ASDF or Quicklisp can
find, e.g.:

```sh
git clone https://git.sr.ht/~takeiteasy/lasm ~/quicklisp/local-projects/lasm
```

LASM depends on
[`trivial-high-precision-timer`](https://git.sr.ht/~takeiteasy/trivial-high-precision-timer)
(used by the emulator's `run-for-duration :throttle t`, [Emulator, "Cycle-cost
model, clock speed, and cycle-accurate
execution"](emulator.md#cycle-cost-model-clock-speed-and-cycle-accurate-execution-75)).
It isn't on Quicklisp either, so it needs the same treatment — clone it
alongside `lasm` under `~/quicklisp/local-projects/`, not fetched from the
Quicklisp dist:

```sh
git clone https://git.sr.ht/~takeiteasy/trivial-high-precision-timer ~/quicklisp/local-projects/trivial-high-precision-timer
```

Then, from a Lisp REPL (SBCL):

```lisp
(ql:quickload :lasm)
;; or, without Quicklisp, given lasm.asd (and cffi's and trivial-high-
;; precision-timer's own .asd files) are all on asdf:*central-registry*:
(asdf:load-system :lasm)
```

Every `examples/*.lisp` script runs standalone via `sbcl --script` (no
`~/.sbclrc`), so each one bootstraps Quicklisp itself — see the top of
[`examples/counter.lisp`](../examples/counter.lisp) — rather than assuming
`cffi`/`trivial-high-precision-timer` are reachable any other way.

## Roswell

The [`lasm` command line](cli.md) is a Roswell script. Roswell resolves
systems from `~/.roswell/local-projects`, not `~/quicklisp/local-projects`, so
link `lasm` and `trivial-high-precision-timer` into it:

```sh
ln -s ~/quicklisp/local-projects/lasm ~/.roswell/local-projects/lasm
ln -s ~/quicklisp/local-projects/trivial-high-precision-timer ~/.roswell/local-projects/
```

Then `ros roswell/lasm.ros --help`, or `ros build roswell/lasm.ros` for a
standalone `roswell/lasm`.

## Run the example

```sh
sbcl --script examples/sixtyfoo.lisp
```

This defines a small register machine (`sixtyfoo`) and exercises registers, the stack,
memory, and flags through `with-machine`, printing the resulting state. No
instruction set or assembler is involved yet — see
[Semantics vocabulary](semantics.md) for why.

```sh
sbcl --script examples/counter.lisp
```

This defines a syntax with `deflexer`, then tokenizes and parses a
hand-written counter-loop program, printing the resulting tokens and
statement AST. It then defines a small instruction set with `definstruction`
(one addressing mode per instruction), assembles the program (resolving its
`bne .loop` label reference) to bytes, loads them into a fresh machine, and
runs the emulator loop to completion — see [Lexer](lexer.md), [Statement
grammar & expression parser](parser.md), [Instructions](instructions.md),
[Assembler](assembler.md), and [Emulator](emulator.md).

```sh
sbcl --script examples/modes.lisp
```

The same pipeline, but `lda`/`adc` each declare several addressing modes
(`immediate`/`zero-page`/`absolute`/`indexed-x`) via `defmode`, and the
assembler picks which one each operand actually uses — see [Addressing
modes](modes.md) and [Assembler](assembler.md#choosing-a-mode).

```sh
sbcl --script examples/mov.lisp
```

A two-hole addressing mode (`expr "," expr`) wired to a two-field `mov`
instruction — one operand encoding field per hole, named `dst`/`val` so
`(semantics ...)` reads them directly — see [Instructions, "Repeated
`(operand ...)` subclauses"](instructions.md).

```sh
sbcl --script examples/hole-attributes.lisp
```

Expression holes with independent signed and relative attributes. The
example encodes two branch targets in one instruction and runs the result.

```sh
sbcl --script examples/directives.lisp
```

A leading `.org` places the program at a fixed address, `.equ` uses a modulo
expression, `.byte` lays down a small data table read back through
`lda`/`adc`'s `absolute` mode, and `.res` reserves a zero-filled scratch run —
see [Directives](directives.md).

```sh
sbcl --script examples/pc-and-scopes.lisp
```

`examples/scoped-counter-alias.lisp` shows a `$` location-counter alias and
two distinct symbols that share the readable spelling `loop.next`:

```sh
sbcl --script examples/scoped-counter-alias.lisp
```

Two routines each define their own `.loop:` local label without colliding
(scoped to `count_down`/`count_up`), and a `.word *` emits its own address —
see [Assembler, "Location counter"](assembler.md#location-counter) and
[Assembler, "Local-label scoping"](assembler.md#local-label-scoping-16).

```sh
sbcl --script examples/macros.lisp
```

A macro uses a default parameter and a local `.equ` in two invocations under
the same global label — see [Macros](macros.md).

```sh
sbcl --script examples/include/include.lisp
```

A program split across `main.asm` and a shared `defs.asm` via `.include`,
loaded with `assemble-file` — see [Includes](includes.md).

```sh
sbcl --script examples/complete.lisp
```

`.equ` constants, a `.macro` expansion, and a forced addressing-mode suffix
(`sta.w`/`lda.z`, overriding the normal zero-page-vs-absolute choice) working
together in one program — see [Directives](directives.md#equ), [Macros](macros.md),
and [Addressing modes, "Forcing a mode with a mnemonic suffix"](modes.md#forcing-a-mode-with-a-mnemonic-suffix).

```sh
sbcl --script examples/stack.lisp
```

A pure stack-based machine (`stackfoo`) with no general-purpose registers at
all — just PC, a data stack, and RAM — running a counted loop that sums
1..5 through `push`/`pop`-based arithmetic. This is the M3 milestone's
validation case that the storage abstraction generalizes beyond
register-shaped machines — see [Machine model](machine-model.md) and
[Semantics vocabulary](semantics.md).

```sh
sbcl --script examples/hybrid.lisp
```

M3's second validation case: a hybrid machine (`hybridfoo`) with an
accumulator, index registers, and one stack doing double duty as both a
data stack and an implicit call stack. `jsr`/`rts` call and return by
`push`/`pop`-ing `pc` onto that stack, and a subroutine reaches its
argument — sitting just underneath its own return address — with the
`stack-relative` addressing mode's `stack-ref` accessor — see [Machine
model](machine-model.md) and [Addressing modes, "Stack-relative
addressing"](modes.md#stack-relative-addressing).

```sh
sbcl --script examples/word.lisp
```

A machine (`wordfoo`) whose whole instruction is one fixed-width 16-bit
word split into bit fields, rather than an opcode byte plus fixed-width
operand bytes — `seta`/`setb`'s operand packs inline into a 10-bit field
for a small value, or escapes to its own following word for a large one,
picked by the same relaxation loop that already picks between addressing-
mode widths — see [Instructions, "Word-encoded
instructions"](instructions.md#word-encoded-instructions-20).

```sh
sbcl --script examples/wordaddr.lisp
```

A machine (`wordaddrfoo`) whose memory declares `:cell-width 16`, so its
assembled output is a vector of 16-bit cells rather than 8-bit bytes and
`.word` means two of the machine's own cells — the assembler/encoder
pipeline typed to a machine's own memory cell width rather than fixed at 8
bits — see [Machine model, "Cell width and the
assembler"](machine-model.md#cell-width-and-the-assembler).

```sh
sbcl --script examples/chip8.lisp
```

M4's first validation case: a CHIP8-shaped machine
(`chip8foo`) with a banked 8-bit `V` register (16 elements) and a scalar
12-bit `I` register sharing one machine — `addi`'s `I += V[x]` moves a
value from an 8-bit source into a 12-bit destination, wrapping at 12 bits
rather than 8. Banked registers are read/written by `regref` and bound in
semantics bodies as `(v idx)` rather than a plain symbol — see [Machine
model](machine-model.md) and [Semantics vocabulary](semantics.md).

```sh
sbcl --script examples/dcpu16.lisp
```

M4's second validation case: a DCPU-16-shaped machine
(`dcpu16foo`) combining word-addressed memory with bitfield/variant
instruction-word encoding for the first time — DCPU-16's real instruction
layout (6-bit `a`, 5-bit `b`, 5-bit `opcode` fields) over `:cell-width 16`
memory, and a banked register standing in for DCPU-16's eight named
registers. `set 1, 1000` exercises the extra-word escape path; every other
operand packs inline — see [Instructions, "Word-encoded
instructions"](instructions.md#word-encoded-instructions-20) and [Machine
model, "Cell width and the assembler"](machine-model.md#cell-width-and-the-assembler).

```sh
sbcl --script examples/chip8word.lisp
```

M4's per-instruction layout (#64) and constant-discriminator-field (#136)
cases together: a CHIP8-shaped machine (`chip8wordfoo`) whose
`instruction-word` clause declares two named alternates alongside its
default — `1NNN`/`2NNN`/`ANNN` split 4/12, `3XNN`/`6XNN`/`7XNN`/`5XY0`/`9XY0`/
`8XY_` split 4/4/8 or stay on the default 4/4/4/4, `DXYN` on the default —
all sharing one 16-bit word and `opcode` field. Nibble-faithful for every
family CHIP8 defines except `0NNN` (a stated, deliberate gap — see the
example's own header): the `8XY_` ALU ops, `5XY0`/`9XY0`, `EX9E`/`EXA1`,
`FX__`, and `00E0`/`00EE` all share an opcode with siblings told apart purely
by a `field-value`-pinned field, not an operand hole. Unlike
`examples/chip8.lisp` above (which proves non-uniform *register* widths on
the ordinary cell-encoded path), this one proves non-uniform
*instruction-word* layouts and constant discriminator fields on the same
machine — see [Instructions, "Per-instruction
layouts"](instructions.md#per-instruction-layouts-64) and ["Constant
discriminator fields"](instructions.md#field-value-field-name-n--constant-discriminator-fields-136).

```sh
sbcl --script examples/cycles.lisp
```

The same counter-loop program as `examples/counter.lisp`, but `sixtyfoo2`
declares a `(clock-speed 1000000)` and each instruction its own `(cycles n)`
— run to completion reports total cycles and wall-time-equivalent
microseconds, `run-for-cycles` stops the same program partway through on a
cycle budget, and `run-for-duration` stops it on a simulated-time budget —
see [Emulator, "Cycle-cost model, clock speed, and cycle-accurate
execution"](emulator.md#cycle-cost-model-clock-speed-and-cycle-accurate-execution-75).

```sh
sbcl --script examples/debugger.lisp
```

The same counter-loop program again, driven through the debugger's
`debug-command` dispatcher instead of a plain `run` — sets a breakpoint on
the loop label, steps, continues, and inspects registers and memory. See
[Debugger](debugger.md).

```sh
sbcl --script examples/regions.lisp
```

A machine whose memory splits into a ROM code region (writes dropped), an
ordinary RAM data region, and a `:device` output port whose write handler
collects bytes instead of touching backing storage — `load-program` still
loads at the ROM region's origin (it burns the image in, bypassing write
protection), a CPU store into ROM is silently dropped, and `mpeek` confirms
the device region never gets a backing cell of its own. See [Machine model,
"Memory regions"](machine-model.md#memory-regions).

```sh
sbcl --script examples/devices.lisp
```

A machine with two declared devices, enumerated and messaged `HWN`/`HWQ`/
`HWI`-style (a countdown clock, an output port) plus one host-attached at
runtime — independent of any memory region entirely. See
[Devices](devices.md).

```sh
sbcl --script examples/interrupts.lisp
```

A machine declaring an `(interrupts ...)` clause — a device's own signal
delivered through the auto-installed hook, a software `int`-style
instruction raising one directly, masking via a flag, and `rfi`
restoring exactly what delivery pushed. See [Interrupts](interrupts.md).

```sh
ros roswell/lasm.ros run examples/cli/counter.asm -m examples/cli/sixtyfoo.lasm
```

The same counter-loop program assembled and run from the shell against a
machine defined in `examples/cli/sixtyfoo.lasm` — see [Command line](cli.md).

## Run the tests

```sh
sbcl --non-interactive \
     --eval '(asdf:load-system :lasm/test)' \
     --eval '(fiveam:run! (quote lasm:lasm))'
```

or, from a REPL:

```lisp
(asdf:load-system :lasm/test)
(fiveam:run! 'lasm:lasm)
```

Note: `(asdf:test-system :lasm/test)` only loads the system — `lasm.asd`
does not define a `test-op` method, so it does not actually invoke
`fiveam:run!`. Use one of the forms above.

## Next

- [Machine model](machine-model.md) for the full `defmachine` clause reference.
- [Semantics vocabulary](semantics.md) for what you can write inside `with-machine`.
- [Lexer](lexer.md) for the full `deflexer` clause reference.
- [Statement grammar & expression parser](parser.md) for `parse` and `parse-expression`.
- [Addressing modes](modes.md) for `defmode` and pattern matching.
- [Instructions](instructions.md) for `definstruction`, encoding, and semantics.
- [Directives](directives.md) for `defdirective`, `.org`, `.byte`/`.word`, `.res`.
- [Assembler](assembler.md) for `assemble`, label resolution, and mode selection.
- [Emulator](emulator.md) for `load-program`, `step-machine`, and `run`.
