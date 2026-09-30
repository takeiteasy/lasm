# Register words

A [backend](backends.md) names words of two or more narrow registers, or of memory
cells, and a [`.lsp`](language.md) word lives in one. A program written for 16- or
32-bit words compiles for an 8-bit machine.

```lisp
(defbackend pairfoo-lang-abi (:isa pairfoo)
  (registers :words ((ab a b) (cd c d) (ef e f) (gh g h))
             :return (ab) :scratch (ab cd) :callee-saved (ef gh)
             :stack-pointer sp :operand reg)
  (frame :grows :down :slot sp-idx :offsets :cells :counts :cells)
  (ops (:const (r v) (ldi (:lo r) (imm (:lo v))) (ldi (:hi r) (imm (:hi v))))
       (:add (d s) (add (:lo d) (:lo s)) (adc (:hi d) (:hi s)))
       ...))
```

```sh
lasm run fact.lsp -m pairfoo.lisp --backend pairfoo-lang-abi     # ab is 120 when it halts
lasm run fact32.lsp -m quadfoo.lisp                              # a to d hold 3628800
```

See [`tests/fixtures/cli/pairfoo.lisp`](../tests/fixtures/cli/pairfoo.lisp), an
8-bit machine with 16-bit words and a carry chain,
[`tests/fixtures/cli/quadfoo.lisp`](../tests/fixtures/cli/quadfoo.lisp), one with
[32-bit words](#four-part-words) in four registers,
[`tests/fixtures/cli/zpfoo.lisp`](../tests/fixtures/cli/zpfoo.lisp), one whose
words live in [zero-page cells](#memory-parts), and
[`examples/chip8/host.lisp`](../examples/chip8/host.lisp), the [CHIP-8 example's](examples.md#chip-8)
8-bit host.

## Declaring words

`(registers :words ((NAME PART PART...)...))` makes `NAME` a pseudo-register for
its parts, most significant first. Every word has the same number of parts, two or
more, all the same width. Every role list names words, never a part, so the
compiler holds a value only in a word:

| Where | Takes |
| --- | --- |
| `:return` `:arguments` `:scratch` `:caller-saved` `:callee-saved` | Words. |
| `call :args` | Words. |
| `(:call f ... :keep (...))`, `:save` | Words. |
| `(asm (:clobbers ...))` | A word, or a part, which clobbers its word.[^clobbers] |

The first `:return` word is the accumulator. A register that is in no role list
is free for a template to use inside one operation.

A word is an error when a part is neither a register nor a [memory address](#memory-parts),
a part is in two words, the parts differ in width, the words differ in part count,
or its name is a register, alias or word. A backend with words needs every operation
to write only its destination word.

## Parts in templates

An operation template splits an operand with `(:part K X)`, where `K` counts from
0 at the least significant part whatever the machine's endianness. `(:lo X)` is
`(:part 0 X)`, and `(:hi X)` is the most significant part. The word name is never
an instruction operand.

| `X` is | `(:part 1 X)` is |
| --- | --- |
| A register operand `(reg wa)` for `(wa a b c d)` | `(reg c)` |
| A word name `wa`, as `:peek` and `:poke` take | The register `c` |
| A memory operand `(zp w0)`, [memory parts](#memory-parts) | `(zp 17)` for `(w0 19 18 17 16)` |
| A word name `w0`, memory parts | The address `17` |
| An integer | The integer's bits 8 to 15, for 8-bit parts; `-1` gives `255` |
| A label or expression | `(& (>> X 8) 255)`; `(& X 255)` for part 0 |
| A frame slot `(:local i)`, `(:arg i)` | A cell of that slot, or a register when the argument is one |

`K` outside the word is a definition error. A word used whole, `(jzp r target)`
for a word `r`, is an `items-malformed` error.

## Four-part words

A 32-bit word on an 8-bit machine has four parts. `(:hi X)` is bits 24 to 31 there,
not the high byte of a 16-bit address: a template that takes an address from a
word writes `(:part 1 f)` and `(:lo f)`, not `(:hi f)` and `(:lo f)`.

```lisp
(registers :words ((wa a b c d) (wb e f g h) (wc i j k l) (wd m n o p))
           :return (wa) :scratch (wa wb) :callee-saved (wc wd) ...)
(call :args :stack :return-address-cells 2)
(ops (:add (d s) (add (:lo d) (:lo s)) (adc (:part 1 d) (:part 1 s))
                 (adc (:part 2 d) (:part 2 s)) (adc (:hi d) (:hi s)))
     (:call ((f reg)) (callp (:part 1 f) (:lo f)))
     ...)
```

A return address narrower than a word needs [`:return-address-cells`](backends.md#calling-convention)
rather than `:return-address-slots`, so the stack arguments lie past its cells. A
frame pointer narrower than a word, a 16-bit one beside 32-bit words, needs
[`(frame :pointer-cells 2)`](conventions.md#frame-pointer) the same way, as in
[`quadfoo-fp.lisp`](../tests/fixtures/cli/quadfoo-fp.lisp).

## Memory parts

A machine with too few registers, such as the 6502 with A, X and Y, keeps a word
in memory cells. An integer part is the cell's address.

```lisp
(registers :words ((w0 #x11 #x10) (w1 #x13 #x12))
           :return (w0) :scratch (w0 w1) :operand zp ...)
(ops (:move (d s) (lda (:lo s)) (sta (:lo d)) (lda (:hi s)) (sta (:hi d)))
     (:peek (d a) (ldw (zp (:hi d)) (zp (:lo d)) (zp (:hi a)) (zp (:lo a)))))
```

| Rule | Meaning |
| --- | --- |
| Address | An integer within the address space of the backend's memory: its stack pointer's, else the sole memory. |
| Width | The memory's cell width. |
| Kind | Every word is registers or every word is addresses; a word does not mix them. |
| Operands | `(:lo d)` of a `(zp w0)` operand is `(zp 16)`; a template that takes a word name writes `(zp (:lo a))`. |
| Registers | A register in no role list, the 6502's A, is free for a template to use. |
| Cells | The parts need not be adjacent. Code and data must not lie over them. |
| `(asm (:clobbers ...))` | Names a word; a memory part has no name. |

A raw instruction in a [`.lasm`](items.md#lasm-files) body takes a part too,
`(lda (:part 3 (zp w0)))`. Using a memory word whole, `(lda (zp w0))`, is an
`items-malformed` error. A call argument that is an integer, a label or an expression is a
value: [call lowering](#calls) loads it with `:const`.

## Words and slots

A word is `N × part width ÷ cell width` cells wide, whatever the stack pointer's
`:width`, so [`backend-word-cells`](language.md#words-wider-than-a-cell) is 2 for
16-bit words and 4 for 32-bit words on 8-bit cells. A `defvar` reserves that many
cells, and `aref` strides by it.

A backend with a `(frame :slot KIND)` needs `:offsets :cells` and `:counts :cells`
and a part as wide as a cell, so `(:part k slot)` is one cell of the slot. Part 0 is the
lowest cell on a little-endian memory and the highest on a big-endian memory.[^endian]

```lisp
(:get (r slot) (lds (:lo r) (:lo slot)) (lds (:hi r) (:hi slot)))
```

## Calls

A backend's `:push` and `:pop` move the parts in whatever order fits its memory.
A stack argument that is a frame slot goes through a free `:scratch` word first,
so every part reads the slot at the same depth.[^push] A stack argument that is a
value is loaded into that word with `:const`. The word is not one a register
argument or the call target reads; without one the call is `items-malformed`.
A value in an argument register is loaded with `:const` after the other moves.

```lisp
(:call f 300 some-label)   ; :const, then the call
```

[^clobbers]: `(:clobbers a)` and `(:clobbers ab)` both mark `ab`. A register that is in no
  word marks itself.

[^endian]: A machine whose `:endian` is neither `:little` nor `:big` cannot have
  words. Cell `c` of a slot in a `(frame :slot KIND)` operand is offset `n + c`, and
  part `k` is cell `k` on a little-endian memory and cell `N - 1 - k` on a big-endian one.

[^push]: A `:push` template that pushes several cells moves the stack pointer between
  them, so a slot read by a later one would be off. A user-written
  `(:push (:local i))` on a words backend has the same problem; push a register.
