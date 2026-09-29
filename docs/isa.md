# ISA and CPU

An **ISA** is the architecture: storage layout, instruction word, instructions
and modes. A **CPU** is one machine that runs it: clock, memory size, devices,
and which instructions it drops. Several CPUs share one ISA.

```lisp
(defisa anima16
  (register reg :width 16 :names (a b c x y z i j))
  (register pc :width 16)
  (memory ram :width 16 :addr-width 16 :cell-width 16))

(definstruction anima16 nop (encoding (opcode 0)) (semantics nil))

(defcpu (praxis-100 (:isa anima16))
  (clock-speed 100000))

(defcpu (mote-40 (:isa anima16))
  (without-instructions swp)
  (register pc :width 12)
  (memory ram :addr-width 12)
  (clock-speed 1000000))
```

`(make-machine 'mote-40)` builds a machine. An ISA alone is not a machine.

## Which form names what

| Names an ISA | Names a CPU |
| --- | --- |
| `definstruction`, `defmode (NAME (:isa A))`, `defbackend (:isa A)` | `make-machine`, `assemble :cpu`, `disassemble-cells :cpu`, `--cpu`, snapshots |
| `find-isa-descriptor` | `find-machine-descriptor`, `find-instruction`, `find-instruction-variants` |

A lookup by CPU returns the ISA's instructions minus that CPU's removals, with
its cycle overrides applied.

## Clauses

| `defisa` | `defcpu` |
| --- | --- |
| `register`, `stack`, `memory` (no regions), `flags` | `register NAME :width n`, `memory NAME :addr-width n` and `(region ...)`, for names the ISA declares |
| `instruction-word`, `stack-pointer` | `device`, `clock-speed`, `reset-pc`, `idle` |
| `interrupts` without `:queue`, `:on-overflow`, `:cycles`, `:max-depth` | `interrupts` with only those keys |
| `privilege` | `undefined-opcode`, `properties` |
| `identity :required t` | `identity :model :id :version :manufacturer` |
| | `without-instructions`, `instruction-cycles`, `without-storage`, `without-devices` |

A CPU declares no storage its ISA lacks, and cannot change what the ISA
compiles into its instructions.[^fixed]

## Extending

```lisp
(defisa (anima16-fp (:extends anima16))
  (register fp :width 16))

(defcpu (anima16-fp-1 (:isa anima16-fp) (:extends praxis-100)))
```

- `(defisa (NAME (:extends ISA)))` adds registers, flags and instructions.
- `(defcpu (NAME (:extends CPU)))` inherits the CPU's clauses, removals and
  cycle overrides, and its ISA. `(:isa ISA)` moves it to an ISA that extends
  that one.

## `defmachine`

`defmachine` defines an ISA and a CPU of the same name. Its clauses split by the
table above.

```lisp
(defmachine sixtyfoo
  (register a :width 8)
  (memory ram :width 8 :addr-width 16 (region rom #xf000 #xffff :kind :rom))
  (clock-speed 1000000))
```

`(defmachine (mote (:extends anima16)))` defines an ISA that extends ISA
`anima16` and a CPU that extends CPU `anima16`. A clause naming storage the
parent declares goes to the CPU, and new storage goes to the ISA.[^bridge]

Use `defisa` and `defcpu` when one architecture has several models.

## Limitations

- An ISA that extends an ISA holds copies of its parent's instructions.
  [#448](https://todo.sr.ht/~takeiteasy/lasm/448)
- `(defmachine (NAME (:extends P)))` needs an ISA named `P`, so it cannot
  extend a CPU defined with `defcpu` alone. Use `defcpu` with `:extends`.

[^fixed]: [Machine families](machine-families.md#what-a-cpu-cannot-change) lists the checks.
[^bridge]: `defmachine` without `:extends` rejects `without-instructions`,
    `instruction-cycles`, `without-storage` and `without-devices`.
