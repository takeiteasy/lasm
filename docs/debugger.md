# Debugger

A gdb-like interactive debugger (#76, M7), built directly on the emulator's
existing `step-machine`/`run` primitives — not a second execution engine. A
`debug-session` wraps a live `machine` (and, optionally, the `assembly` that
produced its program) with breakpoints, step/continue commands, and
read-only inspection of registers/flags/stacks/memory driven off the
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
(make-debug-session machine &key assembly pc memory)
```

`machine` is a live `machine` instance (`make-machine`, with a program
already loaded via `load-program`). `assembly`, when given, is what a
program's `.equ`s, labels, and source lines the debugger reads. `pc`/`memory`
override the usual by-convention resolution (`%resolve-pc`/`%resolve-memory`,
same as `step-machine`'s own keywords), and are resolved once at session
creation, not re-resolved on every command.

## Breakpoints

```lisp
(debug-break session where &key scope)   ; => breakpoint
(debug-unbreak session id-or-address)    ; => t or nil
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

Setting a second breakpoint at an address that already has one replaces it —
there is only ever one stop per address.

## Step and continue

```lisp
(debug-step session &optional n)                     ; => (values reason steps)
(debug-continue session &key max-steps)               ; => (values reason steps [condition])
(debug-continue-to session where &key scope max-steps) ; => (values reason steps [condition])
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
| `:until` | `debug-continue-to` reached its target |
| `:trap` | an instruction's semantics signalled `lasm-trap` (e.g. `hlt`) |
| `:decode-failure` | the byte(s) at PC don't decode to a registered opcode |
| `:idle` | `debug-continue`/`debug-continue-to` only (#110) — the machine went idle with nothing left to wake it, see [Emulator](emulator.md#idle-steps-110). `debug-step` instead counts an idle step as an ordinary executed step, reporting `:step` |
| `:max-steps` | the runaway-program guard tripped with no other stop |

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
machine. `:memory` elements are not dumped here (use `debug-memory-text`
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

Inspection is deliberately read-only: no `set register`/poke command. Writing
a register would inherit an existing bug where `(setf flag)` treats any
non-`nil` value, including `0`, as true (see [Emulator](emulator.md)) —
staying read-only sidesteps it rather than baking it in; see this file's
scope section below.

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
| `delete ID\|ADDR` | remove a breakpoint |
| `info break` | list breakpoints |
| `info reg` | dump registers/flags/stacks |
| `info sym` | list symbols (needs an attached assembly) |
| `step [N]` | execute N instructions (default 1) |
| `continue` | run until a breakpoint, trap, or decode failure |
| `until ADDR\|LABEL` | run until a target is reached |
| `print NAME` | print a register/flag's value |
| `x/N ADDR` | dump N memory cells starting at ADDR |
| `where` | show pc, current instruction, and source context |
| `help` | list commands |
| `quit` | end the session |

An address argument accepts `0x`/`$`/`0b` prefixes (hex/hex/binary), the same
number formats LASM's own lexer understands, or a plain label name when an
assembly is attached.

`debugger-repl` is a thin read/dispatch/print loop over `debug-command` —
the ticket's reference command-line front end — reading one line at a time
from `:input` (default `*standard-input*`), writing responses to `:output`
(default `*standard-output*`), and exiting on `"quit"` or end of input.

## Scope

This covers the ticket's core: address/label breakpoints, step/step-N/
continue/continue-to-address, and read-only state inspection, plus the
reference REPL. It does not cover, by design:

- Watchpoints (break on read/write to a register or memory address).
- Reverse/step-back execution.
- Conditional breakpoints (`break ADDR if EXPR`).
- Stepping/continuing by cycle budget rather than instruction count (though
  `run-for-cycles` already exists and pairs naturally with this once wired
  through — see [Emulator](emulator.md)'s cycle-cost model).
- Writable inspection (`set register`/poke).

Each is tracked as a follow-up.
