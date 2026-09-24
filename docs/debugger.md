# Debugger

A gdb-like interactive debugger (#76, M7), built directly on the emulator's
existing `step-machine`/`run` primitives — not a second execution engine. A
`debug-session` wraps a live `machine` (and, optionally, the `assembly` that
produced its program) with breakpoints, step/continue commands, and
inspection of registers/flags/stacks/memory (plus switching banks) driven off the
machine's own declared storage elements — consistent with LASM's
storage-abstraction pillar.

```lisp
(let* ((assembly (assemble source :lexer 'sixtyfoo-syntax :machine 'sixtyfoo))
       (machine (make-machine 'sixtyfoo)))
  (load-program machine assembly)
  (let ((session (make-debug-session machine :assembly assembly)))
    (debug-break session "count.loop")
    (debug-continue session)
    (debug-state-text session :stream t)))
```

See [`examples/debugger.lisp`](../examples/debugger.lisp) for a runnable
version.

## Two layers

The machine-agnostic API below (`debug-break`, `debug-step`,
`debug-continue`, `debug-state-text`, ...) is the real deliverable. A command
dispatcher, `debug-command`, sits on top of it — string in, text out, the
same `:stream nil` → string convention `listing-text`/`symbols-text`/
`disassembly-text` already use — so every command is unit-testable by
asserting on returned strings, with no `*standard-input*`/`*standard-output*`
mocking needed. `debugger-repl` is a thin read/dispatch/print loop over
`debug-command` — the reference command-line front end.

## Sessions

```lisp
(make-debug-session machine &key assembly pc memory lexer)
```

`machine` is a live `machine` instance (`make-machine`, with a program
already loaded via `load-program`). `assembly`, when given, is what a
program's `.equ`s, labels, and source lines the debugger reads. `pc`/`memory`
override the usual by-convention resolution (`%resolve-pc`/`%resolve-memory`,
same as `step-machine`'s own keywords), and are resolved once at session
creation, not re-resolved on every command. `lexer` (default `'default`)
tokenizes [breakpoint conditions](#conditional-breakpoints).

## Breakpoints

```lisp
(debug-break session where &key scope bank condition) ; => breakpoint
(debug-unbreak session id-or-address &key bank) ; => t or nil
(debug-breakpoints session)              ; => list of breakpoint, by address
```

`where` is either an address (an integer) or a label name (a string),
resolved through `assembly-symbol` (`listing.lisp`, #37) against the
session's attached `assembly` — `scope`, when given, qualifies a local label
the same way `assembly-symbol` itself does (e.g. `scope "count"`, `where
".loop"` looks up that local). Without `scope`, `"count.loop"` names a
global with that spelling, if one exists.
Breaking on a label:

- Signals if the session has no attached `assembly`.
- Signals if the name names an `.equ` rather than a label — an `.equ`'s
  value is not an address (the same label/`.equ` ambiguity `symbol-info`'s
  `kind` already resolves elsewhere in the codebase; breakpoints reuse it
  rather than reintroducing it).

A local label is named by its qualified spelling (`"count.loop"`, as `info sym`
prints it) or by `where ".loop"` with `scope "count"`. A global with the
spelling wins over a local.

Setting a second breakpoint at an address that already has one replaces it —
there is only ever one stop per address and bank.

A label defined under [`.bank`](banked-output.md#bank) carries its bank, and
`:bank n` qualifies an integer address the same way. A bank-qualified
breakpoint only stops while that bank is mapped; an unqualified one stops
whichever bank is mapped. `:bank` must name a bank of the region containing the
address, and must agree with a label's own bank.

## Conditional breakpoints

```lisp
(debug-break session ".loop" :scope "count" :condition "x == 1")
```

A breakpoint with a `condition` only stops while the expression is nonzero,
checked after the step that lands on it. The expression uses the assembler's
operators and number formats, parsed with `parse-expression`, over:

- scalar registers, flags and register aliases, read live;
- labels and `.equ`s of the attached assembly (looked up under `scope`, then
  globally);
- `*`, the current PC;
- `mem(addr)`, the memory cell at `addr`, read without triggering watchpoints
  or [device](machine-model.md#memory-regions) side effects.

A name that is both storage and a label is storage. `bank()`, `defined()`,
`lowcell()` and `highcell()` are rejected, and the assembler rejects `mem()`.
A custom lexer must declare `mem` in its
[`function-operators`](lexer.md#clauses) to use it in a condition. Syntax errors and unknown names
signal from `debug-break`. An error while evaluating stops the run as
`:breakpoint` with the error as the third value.

## Watchpoints

```lisp
(debug-watch session target &key access index scope bank) ; => watchpoint
(debug-unwatch session id)                               ; => t or nil
(debug-watchpoints session)                              ; => list, by id
```

`target` is a scalar register, flag or register alias name, a fixed stack
name, a label, or a memory address. A banked register takes `:index`; a stack
takes `:index` for one bottom-relative slot (0 is the oldest entry) and
otherwise stops on any access. `stack-push` and `(setf stack-ref)` are writes;
`stack-pop` and `stack-ref` are reads. A push reports `old` as `nil`. `access` is `:read`,
`:write` (default) or `:read-write`. `scope` and `bank` qualify a memory
target as for `debug-break`. Breakpoints and watchpoints share one id space.

Execution stops after the instruction that made the access, with reason
`:watchpoint` and a `watch-hit` (`watch-hit-watchpoint`, `-access`, `-old`,
`-new`) as the third value. Instruction fetch, PC advance and the debugger's
own inspection never trigger a watchpoint; a `:read` hit reports the value
read for both `old` and `new`. `debug-step`, `debug-continue` and
`debug-continue-to` all honour watchpoints. The debugger installs the
machine's [access hook](machine-model.md#access-hook) only while an
instruction runs, and only when watchpoints exist.

## Step and continue

```lisp
(debug-step session &optional n)                     ; => (values reason steps)
(debug-continue session &key max-steps)               ; => (values reason steps [condition])
(debug-continue-to session where &key scope bank max-steps) ; => (values reason steps [condition])
```

All three are built directly on the emulator's existing loop:
`debug-continue`/`debug-continue-to` are `%run-loop` (the shared engine
behind `run`/`run-for-cycles`/`run-for-duration`) with a `stop-p` predicate
checking the machine's current PC — against the breakpoint table for
`debug-continue`, against a single target address/label for
`debug-continue-to`. No new execution loop exists anywhere in this file.

`reason` is one of:

| Reason | Meaning |
|---|---|
| `:step` | `debug-step` executed all `n` instructions requested |
| `:breakpoint` | `debug-continue` stopped on a live breakpoint |
| `:watchpoint` | a watched register, flag or address was accessed; the third value is the `watch-hit` |
| `:until` | `debug-continue-to` reached its target |
| `:trap` | an instruction's semantics signalled `lasm-trap` (e.g. `hlt`) |
| `:fault` | `debug-continue` or `debug-continue-to` encountered a `storage-error`; the condition is the third return value |
| `:decode-failure` | the byte(s) at PC don't decode to a registered opcode |
| `:idle` | `debug-continue`/`debug-continue-to` only (#110) — the machine went idle with nothing left to wake it, see [Emulator](emulator.md#idle-steps-110). `debug-step` instead counts an idle step as an ordinary executed step, reporting `:step` |
| `:max-steps` | the runaway-program guard tripped with no other stop |

`debug-step` still signals storage errors. The `continue` and `until`
commands display the condition message when they stop with `:fault`.

**Continuing from a breakpoint runs past it, not immediately again.** `stop-p`
is checked *after* each step executes (`%run-loop`'s own contract, see
[Emulator](emulator.md)), so calling `debug-continue` again while already
stopped on a breakpoint executes at least one more instruction before it can
trigger again — the same behaviour gdb's `continue` has when already stopped
on a breakpoint.

## Inspection (read-only)

```lisp
(debug-state-text session &key stream)          ; registers, flags, stacks
(debug-memory-text session address count &key stream)  ; a memory range
(debug-where-text session &key context stream)  ; pc, disassembly, source line
```

All three follow `listing-text`'s own convention: a string when `:stream` is
`nil` (the default), or written to `:stream` (returning `nil`) otherwise.

`debug-state-text` walks `machine-descriptor-elements` so it needs no
per-architecture code. A `:register` element branches on
`storage-element-count`: a scalar register reads via `sref`, a banked
register (`:count > 1`, e.g. CHIP8's V0–VF or DCPU-16's A/B/C/X/Y/Z/I/J)
reads every bank via `regref` — `sref` itself signals on a banked element, so
a naive walk calling it on every register would crash on the first such
machine. A banked register declaring `:names` renders each cell as
`alias=value` (`reg = [a=1 b=7 c=0 d=4]`); `print` accepts an alias directly
(`print b` → `b = 7`). `:memory` elements are not dumped here (use `debug-memory-text`
instead — there is no sane default range for "the whole address space").

`debug-memory-text` pads each address to a fixed 4 hex digits (matching
`listing-text`/`print-disassembly`'s own address column) and each cell value
to the memory's own cell width in hex digits — sized from the machine's
actual cell width. It reads via `mpeek`, not
`mref` — a hex dump is inspection, not a CPU access, so it must not trigger
a [`:device` region](machine-model.md#memory-regions)'s `:read` side effects
merely by displaying memory.

`debug-where-text` shows the current PC, the next few disassembled
instructions at it (via `disassemble-memory`, passing the attached
assembly's `symbol-info` so labels resolve in the output), and — when an
assembly is attached — the originating source line (via `listing-line-at`).

Inspection is read-only, apart from [switching banks](#banks): there is no
`set register`/poke command.

## Banks

```lisp
(debug-banks-text session &key stream)         ; each banked region's current bank
(debug-set-bank session region bank)           ; map a bank in
(debug-memory-text session address count &key bank stream)
```

`debug-banks-text` lists every [banked region](machine-model.md#bank-switching)
with its address range and current bank out of its bank count.
`debug-set-bank` maps a bank in and signals `bank-out-of-range` for an invalid
one. `debug-memory-text :bank n` dumps bank `n` of the region containing
`address` through `bank-peek`, mapped or not; it signals before printing if
the address is not in a banked region or the range runs past its end.

`where` finds the source line for a PC in a banked region from the mapped
bank's [listing entries](banked-output.md#listings-and-symbols).
[Breakpoints](#breakpoints) and `until` on banked labels wait for their bank.

## Command dispatcher and REPL

```lisp
(debug-command session line &key stream)  ; => (values text quit-p)
(debugger-repl session &key input output prompt)
```

`debug-command` parses one command line and returns its response text (plus
a second value, `t` exactly on `"quit"`, since `quit` has no text of its own
for a caller to distinguish it by). It never signals — a malformed command
or argument returns/writes an error message as ordinary response text — so a
REPL loop, or a caller batching several commands, never needs its own error
handling around it.

Commands:

| Command | Effect |
|---|---|
| `break ADDR\|LABEL` | set a breakpoint |
| `break BANK:ADDR` | set a breakpoint that only stops while that bank is mapped |
| `break .LOCAL in GLOBAL` | set a breakpoint on a local label (`until` and `watch` take it too) |
| `break ... if EXPR` | stop only while `EXPR` is nonzero |
| `watch TARGET [r\|w\|rw]` | watch an address, label, register, `REG[N]`, alias, flag, `STACK` or `STACK[N]` (default `w`) |
| `delete ID\|ADDR` | remove a breakpoint or watchpoint (every bank at an address) |
| `delete BANK:ADDR` | remove the breakpoint at an address in one bank |
| `info break` | list breakpoints and watchpoints |
| `info reg` | dump registers/flags/stacks |
| `info banks` | list banked regions and their current bank |
| `info sym` | list symbols (needs an attached assembly) |
| `step [N]` | execute N instructions (default 1) |
| `continue` | run until a breakpoint, watchpoint, trap, or decode failure |
| `until ADDR\|LABEL` | run until a target is reached (`BANK:ADDR` waits for a bank) |
| `print NAME` | print a register, register alias or flag's value |
| `x/N ADDR` | dump N memory cells starting at ADDR |
| `x/N BANK:ADDR` | dump N cells of a bank of the banked region at ADDR |
| `bank REGION N` | map bank N into a banked region |
| `where` | show pc, current instruction, and source context |
| `help` | list commands |
| `quit` | end the session |

`BANK` is a decimal bank number; `info break` shows banked entries as `BB:AAAA`.

An address argument accepts `0x`/`$`/`0b` prefixes (hex/hex/binary), the same
number formats LASM's own lexer understands, or a plain label name when an
assembly is attached.

`debugger-repl` is a thin read/dispatch/print loop over `debug-command` —
the ticket's reference command-line front end — reading one line at a time
from `:input` (default `*standard-input*`), writing responses to `:output`
(default `*standard-output*`), and exiting on `"quit"` or end of input.

## Scope

This covers breakpoints (optionally conditional), watchpoints,
step/step-N/continue/continue-to-address, and read-only state inspection,
plus the reference REPL. It does not cover:

- Reverse/step-back execution.
- Stepping/continuing by cycle budget rather than instruction count (though
  `run-for-cycles` already exists and pairs naturally with this once wired
  through — see [Emulator](emulator.md)'s cycle-cost model).
- Writable inspection (`set register`/poke).
