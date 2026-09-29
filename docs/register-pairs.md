# Register pairs

A [backend](backends.md) names pairs of narrow registers, and a
[`.lsp`](language.md) word lives in one. A program written for 16-bit words
compiles for an 8-bit machine.

```lisp
(defbackend pairfoo-lang-abi (:machine pairfoo)
  (registers :pairs ((ab a b) (cd c d) (ef e f) (gh g h))
             :return (ab) :scratch (ab cd) :callee-saved (ef gh)
             :stack-pointer sp :operand reg)
  (frame :grows :down :slot sp-idx :offsets :cells :counts :cells)
  (ops (:const (r v) (ldi (:lo r) (imm (:lo v))) (ldi (:hi r) (imm (:hi v))))
       (:add (d s) (add (:lo d) (:lo s)) (adc (:hi d) (:hi s)))
       ...))
```

```sh
lasm run fact.lsp -m pairfoo.lisp --backend pairfoo-lang-abi   # ab is 120 when it halts
```

See [`tests/fixtures/cli/pairfoo.lisp`](../tests/fixtures/cli/pairfoo.lisp), a
complete 8-bit machine with a carry chain, and
[`examples/chip8/host.lisp`](../examples/chip8/host.lisp), the [CHIP-8 example's](examples.md#chip-8)
8-bit host.

## Declaring pairs

`(registers :pairs ((NAME HIGH LOW)...))` makes `NAME` a pseudo-register for
`HIGH` and `LOW`. Every role list names pairs, never a half, so the compiler holds
a value only in a pair:

| Where | Takes |
| --- | --- |
| `:return` `:arguments` `:scratch` `:caller-saved` `:callee-saved` | Pairs. |
| `call :args` | Pairs. |
| `(:call f ... :keep (...))`, `:save` | Pairs. |
| `(asm (:clobbers ...))` | A pair, or a half, which clobbers its pair.[^clobbers] |

The first `:return` pair is the accumulator. A register that is in no role list
is free for a template to use inside one operation.

A pair is an error when a half is not a register, a register is in two pairs, the
halves differ in width, or its name is a register, alias or pair. A backend with
pairs needs every operation to write only its destination pair.

## Halves in templates

An operation template splits an operand with `(:hi X)` and `(:lo X)`. The
pair name is never an instruction operand.

| `X` is | `(:lo X)` is |
| --- | --- |
| A register operand `(reg ab)` | `(reg b)` |
| A pair name `ab`, as `:peek` and `:poke` take | The register `b` |
| An integer | The integer masked to the half's width; `-1` gives `255` |
| A label or expression | `(& X 255)`, and `(& (>> X 8) 255)` for `:hi` |
| A frame slot `(:local i)`, `(:arg i)` | A cell of that slot, or a register when the argument is one |

Writing a pair whole, `(jzp r target)` for a pair `r`, is an `items-malformed`
error.

## Words and slots

A word is two halves wide, so [`backend-word-cells`](language.md#words-wider-than-a-cell)
is `2 × half width ÷ cell width`, whatever the stack pointer's `:width`. A `defvar`
reserves that many cells, and `aref` strides by it.

A backend with a `(frame :slot KIND)` needs `:offsets :cells` and `:counts :cells`
and a half as wide as a cell, so `(:lo slot)` and `(:hi slot)` are two cells.
The low half is the lower cell on a little-endian memory and the higher one on a
big-endian memory.[^endian]

```lisp
(:get (r slot) (lds (:lo r) (:lo slot)) (lds (:hi r) (:hi slot)))
```

## Calls

A backend's `:push` and `:pop` move two halves in whatever order fits its memory.
A stack argument that is a frame slot goes through a free `:scratch` pair first,
so both halves read the slot at the same depth.[^push] Without one the call is
`items-malformed`.

## Limitations

| Limitation | Ticket |
| --- | --- |
| A word is exactly two registers. | [#423](https://todo.sr.ht/~takeiteasy/lasm/423) |
| A half is a register, not a memory cell, so the 6502 has too few registers. | [#424](https://todo.sr.ht/~takeiteasy/lasm/424) |

[^clobbers]: `(:clobbers a)` and `(:clobbers ab)` both mark `ab`.

[^endian]: A machine whose `:endian` is neither `:little` nor `:big` cannot have
  pairs. Slot cell `k` of a word in a `(frame :slot KIND)` operand is offset `n + k`.

[^push]: A `:push` template that pushes two cells moves the stack pointer between
  them, so a slot read by the second would be one cell off. A user-written
  `(:push (:local i))` on a pair backend has the same problem; push a register.
