# Examples

Each example is an ASDF system depending on `:lasm`, with its own test system.

| System | Front end | Shows |
| --- | --- | --- |
| [`:dcpu16`](#dcpu-16) | Lisp DSL | A complete DCPU-16 v1.7 machine: operand modes, word-encoded instructions, interrupts and devices. |
| [`:chip8`](#chip-8) | `.lsp` language | A CHIP-8 interpreter written in the [source language](language.md), compiled for a small host machine. |
| [`"6502"`](#6502) | `.lasm` items, assembly text | A MOS 6502 with every documented opcode and addressing mode, and a program that mixes raw instructions with a backend's operations and calls. |

## Loading

ASDF must find the system. Any one of these works, from the repository root:

```lisp
(asdf:load-asd #p"examples/dcpu16/dcpu16.asd")   ; likewise chip8/chip8.asd, 6502/6502.asd

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
| `host.lisp` | The machine the emulator runs on and its [backend](backends.md): 8-bit registers and cells, [register words](register-words.md) for 16-bit words, [static frames](static-frames.md), byte access, compare-and-branch, and the [operations the language needs](language.md#backend-requirements). |
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

## 6502

The spec is the block comment at the top of `examples/6502/6502.lisp`. The system
is named `"6502"`, which is not a symbol, so its package is `mos6502`.

```lisp
(asdf:load-system "6502")

(let* ((assembly (lasm:assemble-items-file mos6502:*demo*))
       (machine (mos6502:load-6502 assembly)))
  (mos6502:reset-6502 machine)                  ; PC from the vector at $FFFC
  (mos6502:run-6502 machine)                    ; :halted, on JAM
  (mos6502:word-at machine (mos6502:demo-symbol assembly "product")))   ; 2100
```

| File | Holds |
| --- | --- |
| `package.lisp` | A package that `use`s `#:lasm`, and imports the built-in [mode](modes.md) names. |
| `6502.lisp` | The [machine](machine-model.md) with its [interrupts](interrupts.md), [ISA-local modes](modes.md#isa-local-modes) that shadow and extend the built-in ones, a [lexer](lexer.md), the 56 [instructions](instructions.md) with a semantics macro per addressing mode, decimal mode, and a [backend](backends.md) with [zero-page words](register-words.md#memory-parts) and [static frames](static-frames.md). |
| `timer.lisp` | The [device](devices.md) mapped at `$D000` that raises IRQ or NMI. |
| `demo.lasm` | A [`.lasm`](items.md#lasm-files) program: raw instructions beside `(:op ...)`, `(:function ...)` with a [static local](static-frames.md#lasm-functions) and `(:call ...)`, and an IRQ handler fed by the timer. |
| `test.lisp` | A FiveAM suite: the opcode matrix, mode selection, flags, decimal mode, the stack, BRK, IRQ, NMI and RTI, the timer, cycles, the demo, and `.lsp` programs on the backend. |

Assembly text uses `assemble-6502`; `$` is hex and `;` starts a comment.

### Interrupts and the timer

IRQ and NMI are declared with an `interrupts` clause; `signal-interrupt` and the
timer raise them. A signal is delivered before the next instruction: PC then P
(B clear) are pushed, I is set, and PC is read from `$FFFE` (IRQ) or `$FFFA`
(NMI). IRQ waits while I is set; NMI does not. BRK pushes P with B set and
reads `$FFFE` itself, and RTI undoes all three.[^irq]

| Address | Timer register |
| --- | --- |
| `$D000`, `$D001` | Period in CPU cycles, low byte first; `0` stops it. |
| `$D002` | Bit 0 enables it; bit 1 raises NMI instead of IRQ. |
| `$D003` | Interrupts raised, modulo 256; a write clears it. |

```lisp
(signal-interrupt machine 1)                        ; an IRQ
(signal-interrupt machine 1 :non-maskable t)        ; an NMI
```

[^irq]: The clause has no `:message`, so delivery writes no data. Delivery costs 7
  cycles. Unset vectors read as zero and are not dropped, so a signal with no handler
  jumps to `$0000`. Signals the queue cannot hold are dropped.

| Operand | Written |
| --- | --- |
| Immediate | `#$10` |
| Zero page, absolute | `$10`, `$1234`: a value that fits a byte is zero page, and `lda.w`, `lda.z` force one |
| Indexed | `$10,x`, `$10,y`, `$1234,x`, `$1234,y` |
| Indirect | `($1234)` for `jmp` |
| Indexed indirect, indirect indexed | `($10,x)`, `($10),y` |
| Accumulator | `asl a` |
| Branch | a label, as a signed byte from the next instruction |

In a `.lasm` file an operand is `(KIND value)`: `imm`, `zp`, `zpx`, `zpy`, `abs`,
`absx`, `absy`, `indx`, `indy`, `ind` and `acc`. A branch or jump takes a bare label.

`run-6502` stops on `JAM` (`$02`) or after `:max-steps` instructions, and returns
`:halted` or `:running`; any other stop is an error. `BRK` is a real interrupt
through the vector at `$FFFE`.

| Reader | Returns |
| --- | --- |
| `(reg m 'a)` | A register: `a`, `x`, `y`, `s`, `pc`. |
| `(status m)`, `(flag-set-p m 'c)` | The flags, packed as `PHP` pushes them without B, or one flag. |
| `(ram m address)`, `(word-at m address)` | A cell, or a little-endian word. |
| `(demo-symbol assembly "label")` | The address of a label. |
