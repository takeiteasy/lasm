# Debugger

A `debug-session` adds breakpoints, watchpoints, stepping, and inspection
and editing of state to a live machine. Attach an assembly for symbols and source
locations.

```lisp
(let* ((assembly (assemble source :machine 'sixtyfoo))
       (machine (make-machine 'sixtyfoo)))
  (load-program machine assembly)
  (let ((session (make-debug-session machine :assembly assembly)))
    (debug-break session "count.loop")
    (debug-continue session)
    (debug-state-text session :stream t)))
```

See [`debugger.lisp`](../examples/debugger.lisp).

## Sessions

```lisp
(make-debug-session machine &key assembly pc memory lexer history)
```

`machine` is already running or loaded. `assembly` supplies label, `.equ`,
and source information. `:pc` and `:memory` override the machine defaults;
`:lexer` parses breakpoint expressions. `:history` enables
[step back](#step-back); it is off by default.

## Breakpoints

```lisp
(debug-break session where &key scope bank condition)
(debug-unbreak session id-or-address &key bank)
(debug-breakpoints session)
```

`where` is an address or label. Use `:scope` for a local label. An attached
assembly is required for label lookup; `.equ` names are not addresses.
Setting a breakpoint at the same address and bank replaces the existing
one. A bank-qualified breakpoint stops only while that bank is mapped.

## Conditional breakpoints

```lisp
(debug-break session ".loop" :scope "count" :condition "x == 1")
```

The expression is checked when execution reaches the breakpoint. It can
read scalar registers, flags, aliases, attached assembly symbols, `*` for
PC, and `mem(addr)` for side-effect-free memory inspection. An unknown name
or unsupported function signals when the breakpoint is set. A custom lexer
must include `mem` in `function-operators`; see [Lexer](lexer.md#clauses).

## Watchpoints

```lisp
(debug-watch session target &key access index scope bank)
(debug-unwatch session id)
(debug-watchpoints session)
```

Targets include registers, flags, stack entries, labels, and addresses.
A banked register or stack entry uses `:index`. `:access` is `:read`,
`:write` (default), or `:read-write`. Breakpoints and watchpoints share an
ID space.

A watchpoint stops **after** the instruction makes the access, returning
`:watchpoint` and a `watch-hit` with access, old, and new values. Fetch,
PC advancement, and debugger inspection do not trigger one. See the
[access hook](machine-model.md#access-hook).

## Step and continue

```lisp
(debug-step session &optional n)
(debug-step-cycles session cycles &key max-steps)
(debug-continue session &key max-steps cycles)
(debug-continue-to session where &key scope bank max-steps)
```

| Reason | Meaning |
| --- | --- |
| `:step`, `:until` | Requested steps, cycles, or target completed. |
| `:breakpoint`, `:watchpoint` | A stop condition matched. |
| `:trap`, `:fault`, `:decode-failure` | Execution stopped in the emulator. |
| `:idle`, `:max-steps` | Machine cannot wake, or the step guard fired. |
| `:max-cycles` | `debug-continue :cycles` spent its budget. |

`debug-step-cycles` steps until `cycles` have been spent, ignoring
breakpoints like `debug-step`. `debug-continue :cycles` also stops at
breakpoints and keeps running an idle machine to spend the budget. Either can
overshoot by one instruction's cost. Both signal on a machine that declares no
`(cycles n)`; see [Emulator](emulator.md#cycle-costs-and-clock-speed).

Continuing from a breakpoint executes at least one more step before
checking it again. `debug-step` reports an idle step as `:step`. Direct
stepping signals storage faults; continue returns `:fault` and the
condition.

## Step back

```lisp
(make-debug-session machine :history 1000)
(debug-step-back session &optional n)
```

`debug-step-back` undoes `n` steps and returns `:back` and the number undone,
or `:history-start` when fewer were recorded. The session snapshots the
machine (see [Snapshots](snapshots.md)) at the start of every step or
continue command and every 256 steps, keeping at least `:history` steps.
Step back restores the nearest earlier snapshot and replays forward.

- It signals when history is off, or when a device on the bus has no `:save`
  hook.
- Restoring rebuilds device objects, so references a host holds to them go
  stale.
- Replay runs device side effects again and assumes deterministic
  semantics.
- Change machine state between commands, not during them: `debug-set`,
  `debug-set-bank`, or `signal-interrupt` before the next command is captured.

## Inspection

```lisp
(debug-state-text session &key stream)
(debug-memory-text session address count &key stream)
(debug-where-text session &key context stream)
```

These return text, or write to `:stream`. State includes registers, flags,
and stacks; banked register aliases render by name. Memory inspection uses
`mpeek`, avoiding device read effects. `where` shows PC, nearby decoded
instructions, and an attached source line. With an attached assembly,
declared data renders as `.byte` and labels come from the main image and
the mapped bank.

## Writing state

```lisp
(debug-set session target value &key index scope bank)
```

`target` is a register, flag, or alias name (`:index` picks a banked
register cell), a fixed stack name with `:index` as a live bottom-relative
slot, a label, or an address. `value` is an integer, wrapped to the target's
width; `debug-set` returns the stored value.

Memory is poked directly: a `:rom` region is writable, and a `:device`
region signals. A write never notifies the access hook, so watchpoints do not
fire.

## Banks

```lisp
(debug-banks-text session &key stream)
(debug-set-bank session region bank)
(debug-memory-text session address count &key bank stream)
```

Bank inspection names each region's current bank. `debug-set-bank` changes
the mapping; `debug-memory-text :bank n` reads an unmapped bank without
switching it. Banked breakpoints and `until` wait for the requested bank.

## Command dispatcher and REPL

```lisp
(debug-command session line &key stream)
(debugger-repl session &key input output prompt)
```

`debug-command` returns response text and a `quit-p` value. Invalid
commands return an error message as text. `debugger-repl` reads, dispatches,
and prints until `quit` or end of input.

| Command | Effect |
| --- | --- |
| `break ADDR\|LABEL [if EXPR]` | Add a breakpoint, optionally conditional. |
| `watch TARGET [r\|w\|rw]` | Watch a register, stack entry, label, or address. |
| `delete ID\|ADDR`, `info break` | Remove or list stops. |
| `step [N]`, `step N cycles` | Execute instructions, or until a cycle budget is spent. |
| `continue`, `continue N cycles`, `until ADDR\|LABEL` | Run to a stop condition. |
| `back [N]` | Undo steps. |
| `info reg`, `info banks`, `info sym` | Inspect state and symbols. |
| `print EXPR`, `x/N ADDR`, `where` | Inspect a value, memory, or source location. |
| `set TARGET = EXPR` | Store an expression in a register, flag, `REG[N]`, `STACK[N]`, or memory. |
| `bank REGION N` | Map a bank. |
| `help`, `quit` | Show commands or end the session. |

`set` evaluates `EXPR` like a breakpoint condition, so `set x = x + 1` and
`set pc = count.loop` work.

Addresses accept decimal, `$` or `0x` hexadecimal, and `0b` binary. A bank
address uses `BANK:ADDR`; a local label uses `.LOCAL in GLOBAL`.

## Limitations

The debugger has no reverse continue.

