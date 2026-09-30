# Backends

`defbackend` describes an ISA as a compiler target: register roles, a
calling convention, frame layout, the operand kinds a front end may name, and
primitive operations. A backend is separate from `defisa`; one ISA can
have several. [Items](items.md) are assembled against one.

```lisp
(defbackend callfoo-abi (:isa callfoo)
  (registers :return (a) :scratch (a b) :callee-saved (c d)
             :stack-pointer sp :program-counter pc)
  (call :args :stack :order :right-to-left :cleanup :caller :return-address-slots 1)
  (frame :grows :down)
  (operands (reg call-reg) (imm call-imm) (sp-idx call-sp-idx))
  (ops (:add (d s) (add d s))
       (:call (f) (call f))
       (:return () (ret))))
```

The ISA, modes and instructions must already be defined. Items assemble for a
CPU: the backend's `(:cpu NAME)`, else the CPU named like the ISA, else the
`:cpu` passed to `assemble-items`. Registers,
modes and mnemonics are matched by name, so a backend can be written in any
package. A mistake signals `backend-definition-error`.

## Clauses

| Clause | Defines |
| --- | --- |
| `(registers ...)` | [Register roles](#registers). |
| `(call ...)` | [Calling convention](#calling-convention). |
| `(frame ...)` | [Frame layout](#frame). |
| `(operands (KIND MODE)...)` | [Operand kinds](#operand-kinds). |
| `(ops (NAME (PARAM...) FORM...)...)` | [Operations](#operations). |
| `(branches MNEMONIC...)` | The instructions whose operands are [branch targets](#branches). |
| `(stack-writers [ENTRY...] [:except ENTRY...])` | Adds to and removes from the [variants that write the stack pointer](#stack-writers). An entry is `MNEMONIC` or `(MNEMONIC MODE)`. |
| `(without-ops NAME...)` | Drops [inherited](#inheritance) operations. |

Each clause is optional and may appear once.

## Registers

Every name is a register or register alias of the machine.

| Key | Value |
| --- | --- |
| `:return` `:arguments` `:scratch` `:caller-saved` `:callee-saved` | A list of registers. A [call](conventions.md#register-cycles) breaks an argument cycle through a `:scratch` one, and copies a register its target reads into one. |
| `:stack-pointer` `:program-counter` `:frame-pointer` | One register. |
| `:address` | The [pointer register](#pointer-register) that memory access goes through. |
| `:operand` | The [operand kind](#operand-kinds) that writes a register. |
| `:words` | `((NAME PART PART...)...)`: [register words](register-words.md) of two or more registers, most significant first, or of [memory cells](register-words.md#memory-parts) when the parts are addresses. Every role list then names them. |

A register cannot be both `:caller-saved` and `:callee-saved`. When the
machine declares a [`stack-pointer`](machine-model.md#stacks), `:stack-pointer`
names that register.

`(backend-register backend :return)` returns the upcased names.

## Calling convention

| Key | Values | Default |
| --- | --- | --- |
| `:args` | `:stack`, or a list of registers | `:stack` |
| `:order` | `:left-to-right`, `:right-to-left` | `:right-to-left` |
| `:cleanup` | `:caller`, `:callee` | `:caller` |
| `:return-address-slots` | Slots a call pushes | `1` |
| `:return-address-cells` | Cells a call pushes, for a return address narrower than a word, replacing `:return-address-slots`; needs `(frame :offsets :cells)` | none |

The values are stored and readable with `backend-descriptor-call`. Items lower
calls from them; see [Calling conventions](conventions.md).

## Frame

| Key | Values | Default |
| --- | --- | --- |
| `:grows` | `:down`, `:up` | The machine's stack-pointer direction, else `:down` |
| `:alignment` | Positive integer, in slots | `1` |
| `:slot` | The [operand kind](#operand-kinds) that addresses a stack slot by its offset from the stack pointer, or from the frame pointer when there is one | None |
| `:stack-slot` | With a `:pointer`, the operand kind that addresses a slot from the stack pointer, for a function with [`:frame nil`](conventions.md#opting-out) | None |
| `:label-slot` | The operand kind that addresses a [static word](static-frames.md#lasm-functions) by its label | None |
| `:pointer` | The register that is the [frame pointer](conventions.md#frame-pointer) | None |
| `:pointer-cells` | With a `:pointer` and `:offsets :cells`, the cells `:enter` pushes for it, when that is not a whole word | None |
| `:offsets` | `:slots`, `:cells`: the unit of the distance handed to `:slot`/`:stack-slot` | `:slots` |
| `:counts` | `:slots`, `:cells`: the unit of the count handed to `:alloc`, `:free` and `:return-pop` | `:slots` |
| `:static` | `t`: a [`.lsp`](static-frames.md) program keeps locals at fixed addresses, and needs no stack slot operations or `:slot` | `nil` |

[`:cells`](conventions.md#slots-and-cells) multiplies by the backend's word size in cells.

`:grows` must agree with the machine's `(stack-pointer ... :grows ...)` for the
backend's `:stack-pointer`. `:pointer` fills `(registers :frame-pointer)`, and
must agree with it when both are given; it cannot be the stack pointer or a
`:return` or `call :args` register. With a `:pointer`, `:slot` must address
relative to it.

A machine stack pointer that stores in the other order from its direction
(`:down` with `:push :post`, `:up` with `:push :pre`) moves every slot one cell:
lowering adds `1` to each offset. A word of more than one cell then needs
`:offsets :cells`, or the definition fails.[^push]

## Operand kinds

`(KIND MODE)` names an addressing mode by a short kind. An [item](items.md#operands)
writes `(KIND value...)`; the values fill the mode's `expr` holes in order.
A kind cannot be an expression operator such as `+`, or a lexer's function operator: `bank`, `lowcell`, `highcell`, `defined` and `mem` in the default lexer. A kind that a custom lexer spells as a function operator is rejected when items are assembled with it.

```lisp
(operands (imm call-imm) (sp-idx call-sp-idx))
;; (imm 21)      -> # 21
;; (sp-idx 1)    -> [ sp + 1 ]
```

## Operations

`(NAME (PARAM...) FORM...)` expands to instruction forms. Each form is
`(mnemonic operand...)`; operands are `(KIND value...)`, expressions, or a
parameter. An argument replaces every occurrence of its parameter, so it can
be a whole operand or one value inside one.

```lisp
(ops (:load (r v) (ldi r (imm v))))
;; (:op :load (reg a) 5)  ->  (ldi (reg a) (imm 5))
```

`:pushes` and `:pops` after the parameters declare the slots an operation puts
on or takes off the stack: an integer, or a parameter. A function without a
[frame pointer](conventions.md#stack-depth) tracks a declared effect instead of
rejecting the operation. The operations call lowering emits cannot declare one.

```lisp
(ops (:grab (n) :pops n (adds (sp) (imm n)))
     (:push2 (a b) :pushes 2 (pushv a) (pushv b)))
```

`(backend-expand-op backend name args)` returns the forms; a `(:label NAME)`
form is returned as written. A form's mnemonic
must exist on the machine, and its operand kinds must be declared.

### Operand-kind clauses

Several clauses may share a NAME: a parameter may be `(NAME KIND)`, restricting
it to an operand of that declared kind, and lowering tries the clauses in the
order written, expanding the first whose arguments match.[^dispatch] This lets
one operation template differently by what it is given -- a computed call
target, a register, needs a different instruction than a label does.

```lisp
(ops (:call ((f reg)) (callr f))   ; a register target
     (:call (f) (call f)))         ; a label
```

`(:call (reg a) ...)` (`items.md#convention-lowering`) expands the first
clause; `(:call double ...)` the second. A single clause still reports a plain
argument-count mismatch when called with the wrong number of arguments; with
several, none matching is `items-malformed`.

A frame operand, `(:arg i)` or `(:local i)`, dispatches as the operand it addresses: the frame's slot kind on the stack, the register kind in a register.

### Labels

A `(:label NAME)` form in a template defines a label of that operation. Each
expansion gets its own copy, so an operation can branch over its own
instructions. A bare `NAME` in an operand refers to it, and a parameter cannot
share its name.

```lisp
(ops (:cjz (r target) (tst r) (jz skip) (br target) (:label skip)))
;; (:op :cjz (reg a) done) twice ->  tst a / jz .skip__LASM_1 / br done / .skip__LASM_1: ...
```

The generated name is `NAME__LASM_N`, or `NAMELASMN` when the [lexer's](lexer.md) identifiers exclude `_`. It is local (the lexer's local-label
prefix) once a non-local label precedes it, so labels of the surrounding code
keep their scope; before any, it is global. An argument named like a template
label is never captured. Operations that [call lowering](conventions.md#backend-operations)
emits may define labels too.

`:push` `:pop` `:alloc` `:free` `:move` `:exchange` `:call` `:return` `:return-pop`
`:enter` and `:leave` are the operations
[call lowering](conventions.md#backend-operations) emits; each has a fixed
number of parameters.

### Language operations

The [source language](language.md) emits `:const :get :set :peek :poke
:peek-byte :poke-byte :jump :branch-zero :halt`, the arithmetic operations
`:add :sub :mul :div :mod :and :or :xor :shl :shr` and the comparisons
`:eq :ne :lt :gt :le :ge`. Each has a fixed number of parameters, checked at
definition, and a backend defines those its programs use; `:peek-byte`/
`:poke-byte` are needed by `peek-byte`/`poke-byte`, or `:peek-byte-pointer`/
`:poke-byte-pointer` with a [pointer register](#pointer-register). Packed-string
access (#366, #379) uses them too, and without them goes through the
[cell](language.md#arrays-strings-and-byte-access). An optional `:byte-address (d)` turns a cell address in `d` into
the byte address those take; without it, the address is multiplied by the
8-bit characters a cell holds.

Optional `:peek-label (d label)` and `:poke-label (label s)` read and write the
word at a label in one operation. The compiler uses them for a global, a
static frame slot and an `aref`/`aset` of an array or string at a constant
index, in place of `:const` then `:peek`/`:poke`.

```lisp
(ops (:peek-label (d label) (ldwm (:hi d) (:lo d) label))
     (:poke-label (label s) (stwm (:hi s) (:lo s) label)))
;; (+ g 1)         ->  :peek-label a gvg, :add-imm a 1
;; (aref table 3)  ->  :peek-label a (+ table 3)
;; (+ x (aref table 3)) -> :add-label a (+ table 3)
```

Each arithmetic and comparison operation may also have an `-imm`, a `-slot`
and a `-label` variant, such as `:add-imm (d v)`, `:add-slot (d slot)` and
`:add-label (d label)` (the word at a global, a static slot or a constant-index `aref`), that the compiler
uses [when the right operand allows](language.md#backend-requirements).
Each comparison also has an optional `:branch-eq`...`:branch-ge (a b target)`
operation, with the same variants, that a condition jumps on directly.
`:branch-ne-imm` also serves a jump on a nonzero value, as `a 0 target`.

### Pointer register

A machine that reaches memory only through one pointer register, loaded in its
own step, names it with `(registers :address REG)` and defines the operations
that load and use it. The compiler keeps `REG` out of its register pools.

| Operation | Does |
| --- | --- |
| `:point (r)` | `REG` = register `r`. Required. |
| `:point-label (label)` | Optional: `REG` = `label`. Without it, `:const` then `:point`. |
| `:peek-pointer (d)` `:poke-pointer (s)` | `d` = the word at `REG`; the word at `REG` = `s`. Required, and they leave `REG` as it was. |
| `:peek-byte-pointer (d)` `:poke-byte-pointer (s)` | As above, a byte. Optional: for `peek-byte`, `poke-byte`, `aref-byte` and `aset-byte` on a backend with no `:peek-byte`/`:poke-byte`. |

A global, a static slot and a constant-index `aref`/`aset` load and store through it, and a computed address
does when the backend has no `:peek`/`:poke`. `:peek-label`/`:poke-label` win
when both exist. The compiler skips `:point-label` while `REG` still holds the
label, and skips `:point` and the address computation for a repeated computed
address made of locals, arguments, constants, array labels and operators over
them, such as `(aref a i)` twice. It forgets the address at a label, a call, a
`:point` and an `(asm ...)` that may write `REG`, and the computed address when
a variable in it is set. A [static frame](static-frames.md) that spills a temporary through the register points it at the slot, so the address loads again.

```lisp
(registers :return (a) :scratch (a b) :address i :operand reg)
(ops (:point (r) (movi (reg r)))
     (:point-label (label) (ldi (imm label)))
     (:peek-pointer (d) (ldm (reg d)))
     (:poke-pointer (s) (stm (reg s))))
;; (set g (+ g 1))  ->  :point-label gvg, :peek-pointer a, :add-imm a 1, :poke-pointer a
```

A store compiles its value first when the address is such a computed address and the value sets no variable, so `(aset a i (+ (aref a i) 1))` points the register once.

## Branches

`(branches call jz br)` lists the instructions whose operands are branch
targets. In a function without a frame pointer, only their operands are checked
against [label depths](conventions.md#stack-depth). Without the clause every
instruction is checked.

## Stack writers

An instruction variant is a stack writer when its `semantics` set the stack
pointer and not the program counter, so `call`, `ret` and `rti` are
not.[^writers] In a function without a frame pointer, a raw instruction or
undeclared `(:op)` expansion using one is `items-malformed`: the
[stack depth](conventions.md#stack-depth) cannot follow it. Use
`(:push)`/`(:pop)`, an `(:op)` that declares `:pushes`/`:pops`, or a frame
pointer.

The variant the operands select decides, as the assembler selects it. With
`addx` taking `a, b` in one mode and `sp, #n` in another, `addx a, b` is
accepted and `addx sp, #2` is rejected. A write under a `choice-case` clause,
or under a test of a variable holding one, counts for that alternative only.
With `drop` taking a zero-page or absolute operand, `drop 5` is checked as
zero-page and `drop 70000` as absolute.

`(stack-writers ENTRY... :except ENTRY...)` adds to and removes from those
variants. An entry is a mnemonic, covering every mode, or `(MNEMONIC MODE)`:

| Clause | Effect |
| --- | --- |
| `(stack-writers movv)` | Every `movv` variant is a stack writer. |
| `(stack-writers (movv call-rr))` | Only the `call-rr` variant is. |
| `(stack-writers pushv :except (pushv call-reg))` | Every `pushv` variant but `call-reg`. |
| `(stack-writers (pushv call-reg) :except pushv)` | Error: an `:except` entry covers an added one. |

`(backend-stack-writers backend)` returns one `(MNEMONIC MODE...)` per
instruction with a stack-writing variant, sorted; `(MNEMONIC)` stands for a
variant without a mode.

```lisp
(backend-stack-writers 'callfoo-abi)
;; => (("ADDS" "CALL-SPI") ("POPR" "CALL-REG") ("PUSH" "CALL-IMM")
;;     ("PUSHV" "CALL-IMM" "CALL-REG" "CALL-SP-IDX") ("SUBS" "CALL-SPI"))
```

## Inheritance

`(defbackend CHILD (:extends PARENT) clause...)` starts from the parent's
clauses. The child's clauses merge over them and the result is checked against
the child's ISA or CPU.

```lisp
(defbackend callfoo-fp-abi (:extends callfoo-abi :isa callfoo-fp)
  (frame :pointer fp :slot fp-idx :stack-slot sp-idx)
  (operands (fp-idx call-fp-idx))
  (ops (:enter () (pushfp) (movfs))
       (:leave () (movsf) (popfp))))
```

| Clause | Merge |
| --- | --- |
| `registers` `call` `frame` | By key. A key the child gives replaces the parent's value; a list is replaced, not appended. |
| `operands` | By kind. |
| `ops` | By operation name: the child's clauses for a name replace all of the parent's for it, together (#365). |
| `branches` `stack-writers` | The child's clause replaces the parent's. |
| `(without-ops NAME...)` | Removes those parent operations; a name the parent lacks is an error. |

`:isa` defaults to the parent's ISA. When given, it must be that ISA or one that
[extends it](isa.md#extending). `(:cpu NAME)` narrows a backend to one CPU of the
ISA; a child inherits its parent's CPU, and may name that CPU or one extending it.
Every operation, register and mode is checked again against the child's CPU, or
its ISA without one, so an operation using an instruction the CPU removed is an
error until the child overrides or drops it.
Redefining a parent rebuilds its children, and their children, from their own
clauses. If one no longer builds, the redefinition is a `backend-definition-error`
naming it, and every backend stays as it was. A `without-ops` name the new
parent no longer defines is dropped from the child with a `stale-backend`
[warning](conditions.md). A backend cannot extend one that extends it.

## Lookup

| Function | Returns |
| --- | --- |
| `(find-backend name)` | The `backend-descriptor`; `unknown-backend` if none. |
| `(backend-stack-writers name)` | The [stack writers](#stack-writers) as `(MNEMONIC MODE...)`, upcased and sorted. |
| `backend-descriptor-isa` `-cpu` `-registers` `-call` `-frame` `-operands` `-ops` `-branches` `-stack-writers` `-stack-writer-exceptions` | The stored clauses. |

The command line loads backends from its machine file; see [Command line](cli.md).

[^push]: The shift is one cell whatever the slot width, so `:offsets :slots` cannot write it for a multi-cell word. A stack pointer's `:base` is not part of the offset: the `:slot` operand's addressing mode adds it, as it adds the register.

[^dispatch]: An untyped parameter position (`NAME`, no `KIND`) matches any
  argument; a match needs the same number of arguments as the clause has
  parameters, and each typed position's argument to be `(KIND value...)` of
  that kind. `:extends` replaces a name's whole group of clauses with the
  child's, at the parent's first clause of that name.

[^writers]: The walk reads the instruction's own `set!`, `setf`, `push`, `pop` and `interrupt-return` forms, through macros, including those a `macrolet` in `semantics` or around the `definstruction` defines, with the `choice-case` clauses around each. A `macrolet` expander body can use the macros bound around it, but not its sibling macros. A `let` or `let*` variable bound to a `choice-case` carries that choice into the branches of an `if`, `when`, `unless`, `and` or `cond` testing it, a `choice-case` tested directly, or their `not`, `null`, `and` and `or`: the then branch of an `and` and the else branch of an `or` take the conditions of their operands, the other branches none. A clause that returns a literal decides the branch; a clause whose result is computed may return either, so it stays in both: the then branch excludes only the clauses that return nil, the else branch only those that return non-nil. A variable assigned anywhere in its scope, by `setq` or a macro expanding to an assignment, is not followed. Other conditions are not followed, so a write under one is unconditional ([#357](conventions.md#limitations)). When the operand syntax leaves several variants tied on width, such as zero-page and absolute, constant operands select the one the assembler picks; an operand with no value yet, such as a label, is judged by the variant a layout of the items chooses. `assemble-items` uses the assembler's, `items-size` its `:assume`, and `render-items` `:widest`. See the [limitations](conventions.md#limitations).
