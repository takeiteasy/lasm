# Examples

Each example is an ASDF system depending on `:lasm`, with its own test system.

| System | Front end | Shows |
| --- | --- | --- |
| [`:dcpu16`](#dcpu-16) | Lisp DSL | A complete DCPU-16 v1.7 machine: operand modes, word-encoded instructions, interrupts and devices. |

## Loading

ASDF must find the system. Any one of these works, from the repository root:

```lisp
(asdf:load-asd #p"examples/dcpu16/dcpu16.asd")

(cl:push #p"examples/dcpu16/" asdf:*central-registry*)

(ql:register-local-projects)   ; when LASM is in quicklisp/local-projects
```

Then load the system:

```lisp
(asdf:load-system :dcpu16)
```

## Testing

```lisp
(asdf:test-system :dcpu16)
```

`(asdf:test-system :lasm)` runs each example's tests too, in a separate Lisp
process.

## DCPU-16

The spec is the block comment at the top of `examples/dcpu16/dcpu16.lisp`.

```lisp
(asdf:load-system :dcpu16)

(let ((machine (dcpu16:load-dcpu16 "set a, 5
add a, [0x1000]
end: set pc, end")))
  (dcpu16:run-dcpu16 machine)
  (dcpu16:reg-value machine :a))
```

| File | Holds |
| --- | --- |
| `package.lisp` | A package that only `use`s `#:lasm` ([your own package](machine-model.md#your-own-package)). |
| `dcpu16.lisp` | [`defmachine`](machine-model.md), [operand modes](operand-modes.md), [word-encoded instructions](word-instructions.md), [interrupts](interrupts.md). |
| `devices.lisp` | The Generic Clock and Keyboard as [devices](devices.md). |
| `test.lisp` | A FiveAM suite: encodings, EX, conditionals, stack, interrupts, hardware. |

Source uses the spec's syntax. `set` and the other mnemonics are
case-insensitive; a program halts by jumping to itself.

| Operand | Written |
| --- | --- |
| Register | `a` to `j` |
| `[register]`, `[register + next word]` | `[b]`, `[b + 5]` |
| `PUSH`, `POP`, `PEEK`, `PICK n` | `push`, `pop`, `peek`, `pick 3` |
| `SP`, `PC`, `EX` | `sp`, `pc`, `ex` |
| `[next word]`, literal | `[0x1000]`, `5`, `-1` |

`run-dcpu16` steps a machine until an instruction leaves PC unchanged.
`key-down` and `key-up` feed the keyboard.

## Limitations

| Limitation | Ticket |
| --- | --- |
| `[sp]` and `[sp + n]` do not parse; write `peek` and `pick n`. | [#410](https://todo.sr.ht/~takeiteasy/lasm/410) |
| `[5 + b]` assembles as the address `[6]`; write the register first. | [#411](https://todo.sr.ht/~takeiteasy/lasm/411) |
| `0xffff` takes a next word; `-1` packs into the instruction. | [#412](https://todo.sr.ht/~takeiteasy/lasm/412) |
| IF skipping decodes the skipped instruction by hand. | [#413](https://todo.sr.ht/~takeiteasy/lasm/413) |
