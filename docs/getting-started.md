# Getting started

After cloning, run the example and test commands from the repository root.

## Install

Clone LASM and its timer dependency into Quicklisp's local projects
directory.[^dependencies]

```sh
git clone https://git.sr.ht/~takeiteasy/lasm ~/quicklisp/local-projects/lasm
git clone https://git.sr.ht/~takeiteasy/trivial-high-precision-timer ~/quicklisp/local-projects/trivial-high-precision-timer
```

Load LASM in SBCL:

```lisp
(ql:quickload :lasm)
```

For the [command line](cli.md), link both projects into Roswell's local
projects directory:

```sh
ln -s ~/quicklisp/local-projects/lasm ~/.roswell/local-projects/lasm
ln -s ~/quicklisp/local-projects/trivial-high-precision-timer ~/.roswell/local-projects/
ros lasm.ros --help
```

## Try an example

```sh
sbcl --script examples/counter.lisp
```

This example parses, assembles, and runs a counter loop. Each example script
runs on its own.[^scripts]

## More examples

Run a Lisp example with `sbcl --script <path>`.

| Path | Shows | Reference |
| --- | --- | --- |
| `examples/sixtyfoo.lisp` | Registers, stack, memory, and flags | [Semantics](semantics.md) |
| `examples/modes.lisp` | Choosing an addressing mode | [Modes](modes.md) |
| `examples/mov.lisp` | An instruction with two operands | [Instructions](instructions.md) |
| `examples/hole-attributes.lisp` | Signed and relative operands | [Modes](modes.md) |
| `examples/directives.lisp` | Data, constants, and reserved space | [Directives](directives.md) |
| `examples/pc-and-scopes.lisp` | Location counter and local labels | [Assembler](assembler.md) |
| `examples/scoped-counter-alias.lisp` | Scoped labels and `$` location alias | [Assembler](assembler.md) |
| `examples/macros.lisp` | Macro parameters and local constants | [Macros](macros.md) |
| `examples/include/include.lisp` | Assembling included files | [Includes](includes.md) |
| `examples/complete.lisp` | Constants, macros, and forced modes | [Assembler](assembler.md) |
| `examples/stack.lisp` | A stack-based machine | [Machine model](machine-model.md) |
| `examples/hybrid.lisp` | Registers and a shared data/call stack | [Machine model](machine-model.md) |
| `examples/word.lisp` | Bit fields and extra instruction words | [Instructions](instructions.md) |
| `examples/wordaddr.lisp` | 16-bit addressable cells | [Machine model](machine-model.md) |
| `examples/chip8.lisp` | Registers with different widths | [Machine model](machine-model.md) |
| `examples/dcpu16.lisp` | Word-addressed memory and instruction fields | [Instructions](instructions.md) |
| `examples/chip8word.lisp` | Instruction layouts and fixed field values | [Instructions](instructions.md) |
| `examples/cycles.lisp` | Cycle and duration budgets | [Emulator](emulator.md) |
| `examples/debugger.lisp` | Breakpoints, stepping, and inspection | [Debugger](debugger.md) |
| `examples/regions.lisp` | ROM, RAM, and device memory regions | [Machine model](machine-model.md) |
| `examples/devices.lisp` | Attached and declared devices | [Devices](devices.md) |
| `examples/interrupts.lisp` | Interrupt delivery and masking | [Interrupts](interrupts.md) |

Run the command line example with:

```sh
ros lasm.ros run examples/cli/counter.asm -m examples/cli/sixtyfoo.lasm
```

## Run the tests

```sh
sbcl --non-interactive --eval '(asdf:test-system :lasm)'
```

The suite runs the Lisp examples and reports failures with a nonzero exit
status.[^tests]

## Next

Start with [Machine model](machine-model.md), [Instructions](instructions.md),
or the [documentation index](README.md).

[^dependencies]: LASM and `trivial-high-precision-timer` are not on Quicklisp.
  Without Quicklisp, make their system files and CFFI's available to ASDF,
  then run `(asdf:load-system :lasm)`.
[^scripts]: Each script loads `examples/boot.lisp` to find its dependencies
  without relying on `~/.sbclrc`.
[^tests]: The suite runs each `examples/**/*.lisp` script in its own SBCL
  process. It caches a bootstrapped core as `examples.core` beside compiled
  LASM files; delete that core after changing Quicklisp dependencies. Set
  `LASM_BENCH=1` to run the external benchmarks with STAR at `../star`.
