# Privilege levels

A `(privilege ...)` clause names the storage that holds the current level.
Memory regions, instructions, registers, flags and stacks can require a
minimum level, and interrupt delivery can switch level.

```lisp
(defmachine privfoo
  (register pc :width 16)
  (register a :width 8)
  (memory ram :width 8 :addr-width 16
    (region kernel #x0000 #x00ff :privilege supervisor)
    (region user   #x0100 #xffff))
  (flags s)
  (privilege :level s :levels (user supervisor)))

(definstruction privfoo super
  (privilege supervisor)
  (encoding (opcode #x40))
  (semantics (set! a #xff)))
```

## `defmachine`'s `privilege` clause

```lisp
(privilege :level NAME :levels (LEVEL...)
           [:on-violation :fault/:trap/(:interrupt DATA [PRIORITY])])
```

| Key | Effect |
| --- | --- |
| `:level` | A flag or scalar register holding the current level's value. |
| `:levels` | Levels ordered least to most privileged. |
| `:on-violation` | `:fault` (default), `:trap` or `(:interrupt DATA [PRIORITY])`; see [Violations](#violations). |

Each level is `NAME` or `(NAME VALUE)`. `VALUE` is what `:level` holds while
at that level, and defaults to the level's position from `0`. Values are
unique and fit `:level`'s width.

| `:levels` | Level register holds |
| --- | --- |
| `(user supervisor)` | `0` for `user`, `1` for `supervisor`. |
| `((ring3 3) (ring2 2) (ring1 1) (ring0 0))` | `0` for `ring0`, the most privileged. |

A level is allowed when it ranks at least as high as the required one. A
value no level maps to ranks below every level, so every gated access
violates. `(privilege-level machine)` returns the current level's name, or
`nil` for such a value.

## Gating a region

`:privilege LEVEL` on a `(region ...)` gates CPU reads, writes and
instruction fetches. It combines with any kind, including `:rom` and
`:device`.

```lisp
(mref machine 'ram #x10)            ; user level => privilege-violation
(setf (sref machine 's) 1)
(mref machine 'ram #x10)            ; supervisor => reads the cell
```

`mpeek`, `bank-peek`, `load-program`, `reset`, snapshots and the
debugger's `write` reach gated memory at any level.[^bypass]

## Gating registers, flags and stacks

`:privilege LEVEL` on a `register`, `stack` or flag gates instruction
semantics that use the element by name, for reads and writes.

```lisp
(register cr :width 8 :privilege supervisor)
(stack ks :width 8 :depth 4 :privilege supervisor)
(flags s (ie :privilege supervisor) z)
```

| Semantics form | Gated by |
| --- | --- |
| `cr`, an alias, `(bank 1)`, a flag name | The element's level. |
| `(set-flags! (ie 1))` | The flag's level. |
| `push`, `pop`, `stack-ref`, `stack-pointer`, `stack-depth` | The stack's level, or the stack-pointer register's. |

The level register itself can be gated, for example to keep user code from
writing it. Host access, [interrupt delivery](#interrupt-delivery),
`interrupt-return`, the debugger and snapshots reach gated elements at any
level. A machine extending another keeps each element's `:privilege`.[^direct]

## Interrupt delivery

`(interrupts ... :deliver-level LEVEL)` switches to `LEVEL` when a signal is
delivered, so the handler runs at that level.

1. Delivery reads every `:save` place, including the level register.
2. It sets the level register to `LEVEL`.
3. It pushes the values read, so a saved level is the interrupted one.

`interrupt-return` restores the level last, after every other saved place, so
its pops still run at the handler's level. Without `:deliver-level`, delivery
keeps the current level, and a stack or handler in a gated region violates at
user level. A violation during delivery reports the interrupted instruction's
location. See [Interrupts](interrupts.md#delivery).

## Gating an instruction

`(privilege LEVEL)` in `definstruction` applies to every mode of the
mnemonic. The check runs after decode and before the PC advances, so a
violation leaves the PC on the instruction and charges no cycles.[^inherit]

## Violations

| `:on-violation` | Result |
| --- | --- |
| `:fault` | Signals `privilege-violation`; `run` returns `:fault`. |
| `:trap` | Signals `lasm-trap` tagged `:privilege-violation`; `run` returns `:trap`. |
| `(:interrupt DATA [PRIORITY])` | Queues an interrupt and returns to the violating instruction; see [below](#violations-as-interrupts). |

`privilege-violation-kind` and the trap's `:kind` are `:memory`,
`:instruction`, `:register`, `:flag` or `:stack`. The trap's data is
`(:kind KIND :name NAME :address ADDRESS :required LEVEL)`. `NAME` is the
element, or the mnemonic for an instruction. `ADDRESS` is set only for
`:memory`. `lasm run` prints either outcome and exits `1`. See
[Conditions](conditions.md).

### Violations as interrupts

`(:interrupt DATA [PRIORITY])` needs an `(interrupts ...)` clause. `DATA`
is a non-negative integer that fits the `:message` register, and `PRIORITY`
defaults to `0`.

```lisp
(privilege :level s :levels (user supervisor) :on-violation (:interrupt 1))
```

| When | Effect |
| --- | --- |
| The step is running | The step stops, `PC` returns to the violating instruction, and `step-machine` returns `:privilege-violation`. |
| The next step | Delivery follows the usual [rules](interrupts.md#delivery), including masking and nesting. |
| The signal is dropped | The violation faults as `:fault` does. |
| Outside a step | A host access faults as `:fault` does. |

Cycles spent before the violation stay counted. `(privilege-violation-info
machine)` returns `(:pc PC :kind KIND :name NAME :address ADDRESS :required
LEVEL :current LEVEL)` for the last such violation. `reset` clears it and
snapshots do not save it. With `:deliver-level` and a saved level register,
`interrupt-return` resumes the violating instruction at the original level.
See `examples/privilege.lisp`.

## Limitations

- The level is a whole flag or register; a level held in bits of a status
  register needs a dedicated register. See
  [ticket 299](https://todo.sr.ht/~takeiteasy/lasm/299).
- A region, register, flag or stack has one level for reads and writes. See
  [ticket 303](https://todo.sr.ht/~takeiteasy/lasm/303).
- Explicit `sref`, `regref`, `flag` and stack function calls in semantics are
  not gated. See [ticket 307](https://todo.sr.ht/~takeiteasy/lasm/307).
- A violation interrupt does not undo effects an instruction had before it
  violated, and its details are not snapshotted. See
  [ticket 308](https://todo.sr.ht/~takeiteasy/lasm/308).
- A violation interrupt carries a fixed `DATA`. See
  [ticket 309](https://todo.sr.ht/~takeiteasy/lasm/309).
- A masked machine repeats a violation interrupt on the same instruction
  each step until the queue overflows; non-maskable signals are tracked in
  [ticket 305](https://todo.sr.ht/~takeiteasy/lasm/305).
- A unified trap/interrupt model is outside this subsystem.

[^bypass]: The gate runs before the access hook, so a rejected access is
    not reported to it. A `:rom` region with `:on-write :error` reports the
    write first.
[^direct]: Only the names bound in semantics are gated; a host function called
    with an element's name is not.
[^inherit]: A machine extending another keeps its `:level`, `:levels` and
    values; it can change `:on-violation`.
