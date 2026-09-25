# Privilege levels

A `(privilege ...)` clause names the storage that holds the current level.
Memory regions and instructions can require a minimum level.

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
(privilege :level NAME :levels (LEVEL...) [:on-violation :fault/:trap])
```

| Key | Effect |
| --- | --- |
| `:level` | A flag or scalar register holding the current level's value. |
| `:levels` | Levels ordered least to most privileged. |
| `:on-violation` | `:fault` (default) or `:trap`. |

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

## Gating an instruction

`(privilege LEVEL)` in `definstruction` applies to every mode of the
mnemonic. The check runs after decode and before the PC advances, so a
violation leaves the PC on the instruction and charges no cycles.[^inherit]

## Violations

| `:on-violation` | Result |
| --- | --- |
| `:fault` | Signals `privilege-violation`; `run` returns `:fault`. |
| `:trap` | Signals `lasm-trap` tagged `:privilege-violation`; `run` returns `:trap`. |

The trap's data is `(:name NAME :address ADDRESS :required LEVEL)`. `NAME` is
the memory element, or the mnemonic with `:address nil` for an instruction.
`lasm run` prints either outcome and exits `1`. See
[Conditions](conditions.md).

## Limitations

- The level is a whole flag or register; a level held in bits of a status
  register needs a dedicated register. See
  [ticket 299](https://todo.sr.ht/~takeiteasy/lasm/299).
- Registers, flags and stacks cannot be gated. See
  [ticket 300](https://todo.sr.ht/~takeiteasy/lasm/300).
- Interrupt delivery does not change the level. At user level, delivery
  faults when its stack or handler lies in a gated region, and a violation
  during delivery carries no PC. See
  [ticket 301](https://todo.sr.ht/~takeiteasy/lasm/301).
- A violation cannot raise an interrupt. See
  [ticket 302](https://todo.sr.ht/~takeiteasy/lasm/302).
- A region has one level for reads, writes and fetches. See
  [ticket 303](https://todo.sr.ht/~takeiteasy/lasm/303).

[^bypass]: The gate runs before the access hook, so a rejected access is
    not reported to it. A `:rom` region with `:on-write :error` reports the
    write first.
[^inherit]: A machine extending another keeps its `:level`, `:levels` and
    values; it can change `:on-violation`.
