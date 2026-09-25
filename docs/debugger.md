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
and source information; it defaults to the machine's retained
`machine-program`. `:pc` and `:memory` override the machine defaults;
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
PC, `REG[N]` for a banked register cell, `STACK[N]` for a live stack slot
(bottom first), `STACK.depth` for a fixed stack's depth, and `mem(addr)` for
side-effect-free memory inspection. `N` may be any expression, for example
`v[v[1]] == 2` or `ds[0] + ds[1] > 9`. A literal `N` is range-checked when
the breakpoint is set; a computed `N` or a dead stack slot is checked when
the condition runs and stops the breakpoint like any condition error. An unknown name
or unsupported function signals when the breakpoint is set. A custom lexer
must include `mem` in `function-operators`; see [Lexer](lexer.md#clauses).

## Watchpoints

```lisp
(debug-watch session target &key access index scope bank)
(debug-unwatch session id)
(debug-watchpoints session)
```

Targets include registers, flags, stack entries, labels, and addresses.
A banked register or stack entry uses `:index`. `"ds.depth"`, or a stack name
with `:index :depth`, watches a fixed stack's depth: it fires on every push,
pop and depth write, and `:access :read` signals. `:access` is `:read`,
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

## Step back and reverse continue

```lisp
(make-debug-session machine :history 1000)
(debug-step-back session &optional n)
(debug-reverse-continue session)
(debug-reverse-continue-to session where &key scope bank)
```

`debug-step-back` undoes `n` steps and returns `:back` and the number undone,
or `:history-start` when fewer were recorded. The session checkpoints the
machine at the start of every step or continue command and every 256 steps,
keeping at least `:history` steps.[^checkpoints]

`debug-reverse-continue` runs backwards to the latest earlier step where a
breakpoint holds or a watchpoint fired. `debug-reverse-continue-to` runs back
to the latest earlier step at an address or label. Both return the reason and
the steps undone. The step the session is at never counts as a hit.

| Reason | Meaning |
| --- | --- |
| `:breakpoint`, `:watchpoint` | The latest earlier hit. A watchpoint stops after the access, in the state a forward `continue` stops in, and returns its `watch-hit`. |
| `:until` | The latest earlier step at the address. |
| `:history-start` | No earlier hit. The session is at its oldest recorded step. |

- It signals when history is off, or when a device on the bus has no `:save`
  hook.
- Restoring rebuilds device objects, so references a host holds to them go
  stale.
- Replay runs device side effects again and assumes deterministic
  semantics.
- Change machine state between commands, not during them: `debug-set`,
  `debug-write`, `debug-set-bank`, or `signal-interrupt` before the next command is captured.

## Inspection

```lisp
(debug-state-text session &key stream)
(debug-memory-text session address count &key stream)
(debug-where-text session &key context stream)
```

These return text, or write to `:stream`. State includes registers, flags,
and stacks; banked register aliases render by name. `print v[2]` reads one
cell of a banked register, `print ds[1]` a live stack slot (bottom first), and
`print ds.depth` a stack's depth. All three also work inside expressions
(`print v[3] + 1`); an out-of-range index or dead slot is an error message. Memory inspection uses
`mpeek`, avoiding device read effects. `where` shows PC, nearby decoded
instructions, and an attached source line. With an attached assembly,
declared data renders as `.byte` and labels come from the main image and
the mapped bank.

## Writing state

```lisp
(debug-set session target value &key index scope bank)
(debug-write session where value &key scope bank)
```

`target` is a register, flag, or alias name (`:index` picks a banked
register cell), a fixed stack name with `:index` as a live bottom-relative
slot, a label, or an address. `value` is an integer, wrapped to the target's
width; `debug-set` returns the stored value.

Memory is poked directly: a `:rom` region is writable, and a `:device`
region signals. A write never notifies the access hook, so watchpoints do not
fire.

### Stacks

| Call | Effect |
| --- | --- |
| `(debug-set session "ds" 2 :index :depth)` | Set the depth. Cells a grown stack uncovers keep their old values. |
| `(debug-set session "ds" '(1 2 3))` | Replace the entries, bottom first. `'()` clears the stack. |

A depth outside `0..:depth`, or a list that is too long or holds a
non-integer, signals and leaves the stack untouched. Both forms need a
fixed `(stack ...)`.

A `(stack-pointer REG ...)` machine has no fixed stack: `REG` is a plain
register and the stack is memory. Push or pop by hand with
`set REG = N` and a memory `set`.

### CPU-faithful memory writes

`debug-write` stores through the machine's own write path, as a CPU store
would. A `:rom` region drops the value, or signals `memory-write-protected`
with `:on-write :error`, and a `:device` region's `write` hook runs. It
returns the cell now at `where` and whether it holds `value`. A `:bank` other
than the mapped one signals. It targets memory only; registers, flags, and
stacks use `debug-set`. It suppresses the access hook, like `debug-set`.

```
(lasm-dbg) write $10 = 5
$10 = 0 (dropped)
```

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
and prints until `quit` or end of input. `lasm debug` runs it from the
[command line](cli.md#debugging).

| Command | Effect |
| --- | --- |
| `break ADDR\|LABEL [if EXPR]` | Add a breakpoint, optionally conditional. |
| `watch TARGET [r\|w\|rw]` | Watch a register, stack entry (`N` may be an expression, evaluated once when the watch is set), label, or address. `watch STACK.depth` takes `w` or `rw`. |
| `delete ID\|ADDR`, `info break` | Remove or list stops. |
| `step [N]`, `step N cycles` | Execute instructions, or until a cycle budget is spent. |
| `continue`, `continue N cycles`, `until ADDR\|LABEL` | Run to a stop condition. |
| `back [N]` | Undo steps. |
| `reverse-continue`, `rc`, `reverse-until ADDR\|LABEL` | Run back to the previous hit or address. |
| `info reg`, `info banks`, `info sym` | Inspect state and symbols. |
| `print EXPR`, `x/N ADDR`, `where` | Inspect a value, memory, or source location. |
| `print REG[N]`, `print STACK[N]`, `print STACK.depth` | Read a banked register cell, a live stack slot (bottom first), or a stack's depth. Usable inside expressions and conditions. |
| `set TARGET = EXPR` | Store an expression in a register, flag, `REG[N]`, `STACK[N]`, or memory. `N` may be an expression. |
| `set STACK.depth = EXPR`, `set STACK = [EXPR, ...]` | Set a fixed stack's depth, or replace its entries bottom first. |
| `write TARGET = EXPR` | Store to memory through the CPU write path. |
| `bank REGION N` | Map a bank. |
| `save PATH`, `load PATH` | Write the machine's [snapshot](snapshots.md) to a file, or restore it. `load` rebuilds device objects and drops recorded hits; step-back history continues from the restored state. A bad or missing file is an error message. |
| `help`, `quit` | Show commands or end the session. |

`set` evaluates `EXPR` like a breakpoint condition, so `set x = x + 1` and
`set pc = count.loop` work. `set ds = [x, x + 1]` evaluates each item.

Addresses accept decimal, `$` or `0x` hexadecimal, and `0b` binary. A bank
address uses `BANK:ADDR`; a local label uses `.LOCAL in GLOBAL`.

[^checkpoints]: Step back restores the nearest earlier checkpoint and replays
    forward. A checkpoint is a full [snapshot](snapshots.md) (an anchor,
    every 16th) or a delta holding the registers, devices and other small
    state plus the memory and bank cells changed since the previous
    checkpoint. A delta compares only the 64-cell memory pages written since
    the previous checkpoint, so it costs time in proportion to the changes;
    an anchor copies all memory. History is kept back to an anchor, so it can
    exceed `:history`.
    Reverse continue first jumps to the latest breakpoint or watchpoint hit
    recorded while the session ran forward. Otherwise it replays one
    checkpoint segment at a time, newest first, skipping segments whose steps
    never reached a breakpoint address or accessed what a watchpoint
    watches. A session with `:history` records each segment's reads and
    writes of every register, flag and stack, and of each 64-cell memory
    page, through the machine's access hook. Changing breakpoints or watchpoints
    drops the recorded hits, as does `debug-set`, `debug-write` or
    `debug-set-bank`.
