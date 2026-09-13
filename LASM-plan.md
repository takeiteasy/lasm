# LASM — Lisp Assembly

A Lisp library and DSL for building **fantasy assemblers and CPU emulators**.

## 1. Description & Goal

LASM is a platform for designing custom ("fantasy") CPU architectures and
the assembly languages that target them — not a tool for emulating real
silicon like x86 or ARM. Given a declarative specification of a machine's
storage, instruction encoding, and semantics, LASM should be able to
generate:

- A **lexer + parser** for the corresponding assembly syntax (mnemonics,
  operand/addressing-mode grammar, expressions, directives, labels).
- An **assembler** that turns that syntax into encoded machine words/bytes.
- An **emulator** that executes the encoded program against a modeled
  machine state.

The core design tension is deliberate: flexible enough to describe very
different kinds of machines (register machines, stack machines, hybrids,
word-addressed memory, non-uniform register widths) without becoming so
open-ended that using it is indistinguishable from just hand-writing an
emulator. The way LASM resolves this is a **constrained semantics
vocabulary** (`set!`, `push`, `pop`, `branch-if`, `flags`, `trap`, …)
evaluated against a **declared storage model**, with an explicit escape
hatch for the rare instruction that doesn't fit the vocabulary.

**Non-goals (initially):** real ISA fidelity, cycle-accurate timing,
binary translation / JIT performance, matching real silicon's undefined
behavior. These are explicitly out of scope for v1, but the architecture
should not preclude stretching toward them later (see "Stretch: Real
Architectures" in the roadmap) — an "unlikely but doable" design ethos
rather than a hard boundary.

### Design pillars

1. **Storage abstraction, not register bias.** Machine state is a set of
   named storage elements (`register`, `stack`, `memory`), each with its
   own width and discipline. Instructions bind to storage classes, not to
   "registers" specifically — this is what makes register machines, stack
   machines, and hybrids (accumulator + index registers + call stack) all
   expressible through the same mechanism.
2. **One spec drives both assembler and emulator.** Instruction
   descriptors (`definstruction`) carry syntax (addressing modes),
   encoding (bitfields), and semantics together, so they can't drift out
   of sync with each other.
3. **Per-element width and addressing granularity.** Bit width is
   declared per storage element, not assumed globally. Memory declares
   its own cell width (byte-addressed vs. word-addressed), which is what
   makes machines like CHIP8 (mixed 8/12-bit registers) and DCPU-16
   (word-addressed memory, variant bitfield encoding) both expressible.
4. **Optional, zero-cost-when-unused subsystems.** Memory regions
   (ROM/RAM/MMIO) with an optional MMU layer, privilege levels, and an
   interrupt/exception model (generalized from a single trap primitive)
   are all opt-in. A minimal fantasy CPU declares none of them and
   behaves like a flat single-mode machine; a more ambitious one turns
   them on individually.

## 2. Roadmap / Milestones

### M0 — Foundations
- Core Lisp macro scaffolding: `defmachine`, `register`, `stack`,
  `memory`, `flags`.
- Basic storage model: named elements, per-element width, read/write
  access.
- Minimal semantics vocabulary: `set!`, `push`, `pop`.
- **Target:** define a trivial register machine (a handful of registers,
  flat memory) with no instructions yet — prove the storage model holds
  together.

### M1 — First working pipeline (register machine)
- `definstruction`: encoding + semantics for a small register-based
  instruction set (arithmetic, load/store, unconditional/conditional
  branch).
- Fixed lexer (parameterized: comment styles, number formats, label
  syntax) + fixed line/statement grammar.
- Shared Pratt/precedence expression parser (labels, arithmetic,
  lo/hi-byte operators).
- Single addressing mode (`immediate` or `absolute` only) to keep mode
  resolution trivial.
- **Target:** assemble and run a tiny hand-written program (e.g. a
  counter loop) end to end: source → tokens → AST → encoded bytes →
  emulated execution → correct final register state.

### M2 — Addressing modes & multi-mode resolution
- `defmode`: declarative addressing-mode grammar (literal/token pattern
  + one `expr` hole).
- Multiple modes per instruction, mode legality driven by the same
  `(modes ...)` list used for encoding.
- Two-pass assembly (label resolution before final mode/encoding
  selection) to handle overlapping syntax (e.g. zero-page vs. absolute).
- Directives: `.org`, `.byte`/`.word`, basic macros.
- **Target:** reproduce something 6502-shaped — enough addressing modes
  and directives to assemble a small non-trivial program.

### M3 — Stack machines & hybrids
- `push`/`pop` semantics fully exercised; stack-relative addressing
  mode.
- A stack-based fantasy CPU built entirely through the same
  `defmachine`/`definstruction` forms as M1/M2, to validate the storage
  abstraction actually generalizes rather than being register-shaped in
  disguise.
- A hybrid machine (e.g. accumulator + index registers + implicit call
  stack) as a second validation case.

### M4 — Non-uniform width & alternative memory models
- Per-element bit widths that differ from each other (e.g. CHIP8-style
  8-bit registers + 12-bit index register).
- Word-addressed memory (`:cell-width`) as an alternative to
  byte-addressed.
- Bitfield/variant encoding (value-dependent instruction length /
  inline small-literal packing, DCPU-16-style).
- **Target:** CHIP8 and a DCPU-16-like machine both expressible without
  changing the core model.

### M5 — Memory regions, MMIO, banking
- Region-map memory model: `region` with `:range`, `:access`
  (`ro`/`rw`/`mmio`), `:on-write` hooks.
- ROM/RAM separation, bank-switching via a hook on a bank-select write.
- MMIO regions dispatching to a pluggable device table (groundwork for
  peripherals).
- Optional MMU layer in front of the region map (off by default).

### M6 — Privilege levels & interrupts/exceptions
- `defprivilege-levels`, storage/region tagging with a minimum level.
- Register banking by mode as a lookup indirection over multiple
  storage elements sharing a logical name.
- Generalized trap/interrupt primitive (`deftrap`/`definterrupt`):
  software traps, illegal-opcode traps, hardware-triggered interrupts
  (timer, scanline, external) unified under one save/vector mechanism.
- Pluggable priority/masking policy (deliberately not a fixed
  real-hardware scheme).

### M7 — Tooling & polish
- Disassembler derived from the same instruction specs (reversible
  encoding where possible).
- Better diagnostics (parse errors, encoding ambiguity, mode
  mismatches).
- Example gallery: a small library of fully worked fantasy CPUs
  (register, stack, hybrid, CHIP8-alike, DCPU-16-alike) as both
  documentation and regression tests.

### Stretch — Real architectures ("unlikely but doable")
- Treated as a separate, larger layer reusing the encoding/semantics
  *language*, not an extension of the fantasy-CPU runtime itself.
- Would require: much larger instruction tables (likely generated from
  machine-readable spec sheets rather than hand-authored), multi-mode
  execution with banked registers, a real MMU, an interrupt controller
  model, ABI/calling-convention conformance, and a move from
  tree-walking interpretation to JIT/binary translation for viable
  performance.
- Explicitly not a v1–v7 concern; noted here so early architecture
  decisions don't accidentally foreclose it.

## 3. Mockups

### 3.1 Machine definition
```lisp
(defmachine SIXTYFOO
  (register A :width 8)
  (register X :width 8)
  (stack S :width 8 :depth 256)
  (memory RAM :width 8 :addr-width 16)
  (flags Z N C V))
```

### 3.2 Instruction definition (register machine)
```lisp
(definstruction ADC
  (modes immediate zero-page absolute)
  (encoding (opcode #x69) (operand :mode))
  (semantics
    (let ((r (+ A operand C)))
      (set! A (mod r 256))
      (flags (C (> r 255)) (Z (zero? A)) (N (bit-set? A 7)))))
  (cycles 2))
```

### 3.3 Instruction definition (stack machine)
```lisp
(definstruction ADD
  (encoding (opcode #x02))
  (semantics
    (push (+ (pop) (pop)))))
```

### 3.4 Addressing modes
```lisp
(defmode immediate   "#" expr             -> (imm $1))
(defmode zero-page   expr                 -> (zp $1))
(defmode absolute    expr                 -> (abs $1))
(defmode indexed-x   expr "," "X"         -> (idx-x $1))
(defmode indirect-y  "(" expr ")" "," "Y" -> (ind-y $1))
```

### 3.5 Lexer configuration
```lisp
(deflexer
  (comment-styles (";" "line") ("//" "line") ("/*" "*/" "block"))
  (number-formats (hex "$" "0x") (bin "%" "0b") (dec :default) (char "'"))
  (label-suffix ":")
  (local-label-prefix ".")
  (string-delim "\"")
  (ident-chars :alnum "_.")
  (line-continuation "\\"))
```

### 3.6 Directives
```lisp
(defdirective ".org" (expr) -> (set-pc! $1))
(defdirective ".byte" (expr ("," expr) *) -> (emit-bytes $@))
(defdirective ".macro" (name params... body) -> (register-macro! ...))
```

### 3.7 Non-uniform width (CHIP8-style)
```lisp
(defmachine CHIP8-LIKE
  (register V :count 16 :width 8)
  (register I :width 12)
  (memory RAM :width 8 :addr-width 12))
```

Implemented: `:count > 1` declares a banked register, read/written by
`regref`/`(setf regref)` at a run-time index and bound in semantics bodies
as `(V idx)` rather than a plain symbol, since indexed access can't be a
symbol-macro. See [docs/machine-model.md](docs/machine-model.md) and
[docs/semantics.md](docs/semantics.md) for the accessor and binding form,
and [`examples/chip8.lisp`](examples/chip8.lisp) for a complete CHIP8-shaped
machine (banked 8-bit V, 12-bit I, ordinary opcode-plus-operand-cells
encoding) end to end -- proving this section's non-uniform-width target
without needing #3.8's instruction-word mechanism, since CHIP8's real
opcode nibble layouts are themselves non-uniform per instruction and need a
further, separate mechanism.

### 3.8 Word-addressed memory + variant encoding (DCPU-16-style)

Implemented as two separate pieces, both now landed: the bitfield/variant
encoding below (value-dependent instruction length), and word-addressed
memory (the assembler/encoder pipeline is typed to a machine's own code
cell width rather than fixed at 8 bits) — a full DCPU-16-shaped example
combining both is a separate, further ticket.

Field widths are declared once, machine-level, in `defmachine`'s
`instruction-word` clause — not per instruction, since decode has to split
the word into fields *before* it knows which instruction it is. `(opcode n)`
keeps its established meaning everywhere else in LASM (the opcode's *value*,
not a field width). An `extra-word` variant's escape value is spelled out
explicitly (`:escape n`), rather than an implicit "highest field value is
reserved" convention, so a machine wanting more than one escape form isn't
blocked and the reserved value is visible in the source:

```lisp
(defmachine wordmachine
  (memory ram :width 8 :addr-width 16)
  (instruction-word :width 16
    (field opcode 4)
    (field a 6)
    (field b 6)))

(definstruction wordmachine set
  (modes some-mode)
  (encoding
    (opcode 4)
    (operand value :field a
      (variant (range -1 30) inline :bias 1)
      (variant :else (extra-word :escape #x3f))))
  (semantics (set! ... operand)))
```

See [docs/instructions.md](docs/instructions.md#word-encoded-instructions-20)
and [`examples/word.lisp`](examples/word.lisp) for the bitfield/variant
mechanism end to end (on a byte-addressed machine), and
[docs/machine-model.md](docs/machine-model.md#cell-width-and-the-assembler)
and [`examples/wordaddr.lisp`](examples/wordaddr.lisp) for word-addressed
memory end to end (with the ordinary opcode-plus-operand-cells encoding,
not this section's bitfield scheme) — a machine combining both, DCPU-16
shaped, is the further ticket noted above.

### 3.9 Memory regions & MMIO
```lisp
(defmemory-map
  (region ROM   :range (#x0000 #x3FFF) :access ro)
  (region RAM   :range (#x4000 #x7FFF) :access rw)
  (region VRAM  :range (#x8000 #x9FFF) :access rw :on-write video-sync)
  (region IO    :range (#xA000 #xA0FF) :access mmio :ports device-table))
```

### 3.10 Privilege levels
```lisp
(defprivilege-levels (user 0) (kernel 1))
(register SP-user   :width 16 :level user)
(register SP-kernel :width 16 :level kernel)
(region IO :access mmio :min-level kernel)
```

### 3.11 Interrupts / exceptions
```lisp
(definterrupt VBLANK
  :vector #x0040
  :trigger (on-scanline 224)
  :save (PC flags)
  :maskable t)

(definterrupt RESET
  :vector #x0000
  :trigger external
  :maskable nil)
```
