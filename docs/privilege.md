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
(privilege :level NAME [:shift N] [:width N] :levels (LEVEL...)
           [:on-violation :fault/:trap/(:interrupt DATA [:priority N] [:non-maskable t/nil])])
```

| Key | Effect |
| --- | --- |
| `:level` | A flag or scalar register holding the current level's value. |
| `:shift`, `:width` | The [bit field](#level-in-a-bit-field) of `:level` that holds the level. Default `0` and the rest of the register. |
| `:levels` | Levels ordered least to most privileged. |
| `:on-violation` | `:fault` (default), `:trap` or `(:interrupt DATA ...)`; see [Violations](#violations). |

Each level is `NAME` or `(NAME VALUE)`. `VALUE` is what `:level` holds while
at that level, and defaults to the level's position from `0`. Values are
unique and fit `:level`'s width.

| `:levels` | Level register holds |
| --- | --- |
| `(user supervisor)` | `0` for `user`, `1` for `supervisor`. |
| `((ring3 3) (ring2 2) (ring1 1) (ring0 0))` | `0` for `ring0`, the most privileged. |

### Level in a bit field

`:shift` and `:width` select the bits of a wider register, so a status
register can hold the level next to other flags. Levels' values fit `:width`.

```lisp
(register sr :width 16)
(privilege :level sr :shift 13 :width 1 :levels (user supervisor))   ; 68000 S bit
(privilege :level cs :width 2 :levels ((ring3 3) (ring0 0)))         ; x86 CPL
```

Delivery and `(privilege-level machine)` touch only those bits. A flag is one
bit, so `:shift` and `:width` must select it.

A level is allowed when it ranks at least as high as the required one. A
value no level maps to ranks below every level, so every gated access
violates. `(privilege-level machine)` returns the current level's name, or
`nil` for such a value.

## Gating a region

`:privilege LEVEL` on a `(region ...)` gates CPU reads, writes and
instruction fetches. It combines with any kind, including `:rom` and
`:device`. A [plist](#separate-read-write-and-execute-levels) gates the
accesses apart.

```lisp
(mref machine 'ram #x10)            ; user level => privilege-violation
(setf (sref machine 's) 1)
(mref machine 'ram #x10)            ; supervisor => reads the cell
```

`mpeek`, `bank-peek`, `load-program`, `reset`, snapshots and the
debugger's `write` reach gated memory at any level.[^bypass]

## Separate read, write and execute levels

`:privilege` takes a plist to gate accesses apart. An access the plist omits
is open to every level.

```lisp
(region kernel #x00 #xff :privilege (:read user :write supervisor :execute supervisor))
(region code   #x100 #x1ff :privilege (:read supervisor))   ; user code runs but cannot read it
(register cr :width 8 :privilege (:write supervisor))
(flags s (ie :privilege (:write supervisor)))
```

| Key | Gates | Valid on |
| --- | --- | --- |
| `:read` | `mref`, and semantics reads of the element | Regions, registers, flags, stacks |
| `:write` | `(setf mref)`, and semantics writes of the element | Regions, registers, flags, stacks |
| `:execute` | Instruction fetches | Regions |

An instruction fetch checks only `:execute`, and `mref` never does. `incf` on
a register checks a read and a write. A stack needs `:write` to `push` and
both to `pop`; `stack-ref`, `stack-depth` and `stack-pointer` need `:read`.
`privilege-violation-access` is the access that failed.

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
| `(sref machine 'cr)`, `(regref ...)`, `(flag ...)`, `(stack-push ...)`, `(stack-pop ...)`, `(sp-push ...)`, `(sp-pop ...)` | The named element's level.[^explicit] |
| `(set-flags! (ie 1))` | The flag's write level. |
| `push`, `pop`, `stack-ref`, `stack-pointer`, `stack-depth` | The stack's level, or the stack-pointer register's. |

The level register itself can be gated, for example to keep user code from
writing it. Host access, [interrupt delivery](#interrupt-delivery),
`interrupt-return`, the debugger and snapshots reach gated elements at any
level. A machine extending another keeps each element's `:privilege`.[^direct]

## Gating bits of a register

`:fields` in a register's `:privilege` plist gates writes to some of its bits,
so user code can write a status register but not its level bit.

```lisp
(register sr :width 16
  :privilege (:fields ((#x2000 supervisor)                      ; S bit
                       (#x0700 supervisor :on-write :ignore)))) ; IPL mask
(privilege :level sr :shift 13 :width 1 :levels (user supervisor))
```

Each entry is `(MASK LEVEL [:on-write POLICY])`. A write that leaves the masked
bits unchanged is always allowed.

| `:on-write` | A write below `LEVEL` that changes the bits |
| --- | --- |
| `:violate` (default) | Violates as a `:register` write and leaves the register untouched. |
| `:ignore` | Keeps the old bits and stores the rest. |

`:fields` combines with `:read` and `:write`, and applies to banked registers
and their aliases. It cannot gate a `(stack-pointer ...)` register, and masks
must not overlap. Host access, delivery and `interrupt-return` bypass it as
they bypass other [gates](#gating-registers-flags-and-stacks). See
`examples/privilege.lisp`.

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
| `(:interrupt DATA [:priority N] [:non-maskable t/nil])` | Queues an interrupt and returns to the violating instruction; see [below](#violations-as-interrupts). |

`privilege-violation-kind` and the trap's `:kind` are `:memory`,
`:instruction`, `:register`, `:flag` or `:stack`. The trap's data is
`(:kind KIND :name NAME :address ADDRESS :required LEVEL :access ACCESS)`.
`NAME` is the element, or the mnemonic for an instruction. `ADDRESS` is set
only for `:memory`. `ACCESS` is `:read`, `:write` or `:execute`, and `nil` for
an instruction. `lasm run` prints either outcome and exits `1`. See
[Conditions](conditions.md).

### Violations as interrupts

`(:interrupt DATA ...)` needs an `(interrupts ...)` clause. `DATA` is a
non-negative integer that fits the `:message` register. `:priority` defaults
to `0`. `:non-maskable t` makes the signal
[ignore interrupt masks](interrupts.md#non-maskable-signals), so a masked
machine still delivers it instead of repeating the violation each step.

```lisp
(privilege :level s :levels (user supervisor)
           :on-violation (:interrupt 1 :priority 3 :non-maskable t))
```

| When | Effect |
| --- | --- |
| The step is running | The step stops, `PC` returns to the violating instruction, and `step-machine` returns `:privilege-violation`. |
| The next step | Delivery follows the usual [rules](interrupts.md#delivery), including masking and nesting. |
| The signal is dropped | The violation faults as `:fault` does. |
| Outside a step | A host access faults as `:fault` does. |

Cycles spent before the violation stay counted. `(privilege-violation-info
machine)` returns `(:pc PC :kind KIND :name NAME :address ADDRESS :required
LEVEL :current LEVEL :access ACCESS)` for the last such violation. `reset` clears it and
snapshots do not save it. With `:deliver-level` and a saved level register,
`interrupt-return` resumes the violating instruction at the original level.
See `examples/privilege.lisp`.

## Limitations

- A `:fields` violation does not report which mask failed. See
  [ticket 315](https://todo.sr.ht/~takeiteasy/lasm/315).
- A helper function called from semantics, or `(funcall 'sref ...)`, is not
  gated. See [ticket 310](https://todo.sr.ht/~takeiteasy/lasm/310).
- A violation interrupt does not undo effects an instruction had before it
  violated, and its details are not snapshotted. See
  [ticket 308](https://todo.sr.ht/~takeiteasy/lasm/308).
- A violation interrupt carries a fixed `DATA`. See
  [ticket 309](https://todo.sr.ht/~takeiteasy/lasm/309).
- A maskable violation interrupt on a masked machine repeats on the same
  instruction each step until the queue overflows; use `:non-maskable t`.
- A unified trap/interrupt model is outside this subsystem.

[^bypass]: The gate runs before the access hook, so a rejected access is
    not reported to it. A `:rom` region with `:on-write :error` reports the
    write first.
[^direct]: Only forms written in semantics are gated; a host function called
    with an element's name is not.
[^explicit]: A quoted element name is resolved when the instruction compiles;
    any other name expression looks the element up on every call. A register
    gates as `:register`, a flag as `:flag`, and stack calls as `:stack`.
[^inherit]: A machine extending another keeps its `:level`, `:shift`,
    `:width`, `:levels` and values, and each element's `:privilege` including
    `:fields`; it can change `:on-violation`.
