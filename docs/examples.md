# Examples

Each example is an ASDF system depending on `:lasm`, with its own test system.

| System | Front end | Shows |
| --- | --- | --- |
| [`:dcpu16`](#dcpu-16) | Lisp DSL | A complete DCPU-16 v1.7 machine: operand modes, word-encoded instructions, interrupts and devices. |
| [`:chip8`](#chip-8) | `.lsp` language | A CHIP-8 interpreter written in the [source language](language.md), compiled for a small host machine. |

## Loading

ASDF must find the system. Any one of these works, from the repository root:

```lisp
(asdf:load-asd #p"examples/dcpu16/dcpu16.asd")   ; likewise chip8/chip8.asd

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
| `[register]`, `[register + next word]` | `[b]`, `[b + 5]` or `[5 + b]` |
| `PUSH`, `POP`, `PEEK`, `PICK n` | `push`, `pop`, `peek` or `[sp]`, `pick 3` or `[sp + 3]` |
| `SP`, `PC`, `EX` | `sp`, `pc`, `ex` |
| `[next word]`, literal | `[0x1000]`, `5`, `-1` or `0xffff` |

`run-dcpu16` steps a machine until an instruction leaves PC unchanged.
`key-down` and `key-up` feed the keyboard.

## CHIP-8

The spec is the block comment at the top of `examples/chip8/chip8.lsp`.

```lisp
(asdf:load-system :chip8)

(let ((machine (chip8:load-chip8 #(#x60 #x05    ; V0 = 5
                                   #x61 #x07    ; V1 = 7
                                   #x80 #x14    ; V0 += V1
                                   #x12 #x06))))  ; jump to itself: halt
  (chip8:run-chip8 machine)
  (chip8:v-reg machine 0))                       ; 12
```

| File | Holds |
| --- | --- |
| `package.lisp` | A package that only `use`s `#:lasm`. |
| `host.lisp` | The machine the emulator runs on and its [backend](backends.md): 8-bit registers and cells, [register pairs](register-pairs.md) for 16-bit words, [static frames](static-frames.md), byte access, compare-and-branch, and the [operations the language needs](language.md#backend-requirements). |
| `chip8.lsp` | The interpreter: [`defarray`](language.md#arrays-strings-and-byte-access) memory and display, one byte a host cell, [macros](language.md#macros), and a table of [function values](language.md#function-values) indexed by opcode. |
| `chip8.lisp` | Compiles `chip8.lsp` with `assemble-source-file`, loads a ROM and reads the state back. |
| `test.lisp` | A FiveAM suite: a small ROM for each group of instructions, and one that asserts registers, memory and display together. |

`run-chip8` stops when the ROM jumps to itself or after `:max-steps` host
instructions, and returns `:halted` or `:running`; any other stop is an error. A ROM waiting on `FX0A`
stays `:running`; press a key with `key-down` and call it again to resume.

| Reader | Returns |
| --- | --- |
| `(v-reg m n)`, `(i-reg m)`, `(chip8-pc m)` | Registers. |
| `(delay-timer m)`, `(sound-timer m)` | Timers. |
| `(memory-byte m address)` | A byte of CHIP-8 memory. |
| `(pixel m x y)`, `(display-rows m)` | The 64x32 display. |

## Limitations

| Limitation | Ticket |
| --- | --- |
| IF skipping decodes the skipped instruction by hand. | [#413](https://todo.sr.ht/~takeiteasy/lasm/413) |
