# Backends

`defbackend` describes a machine as a compiler target: register roles, a
calling convention, frame layout, the operand kinds a front end may name, and
primitive operations. A backend is separate from `defmachine`; one machine can
have several. [Items](items.md) are assembled against one.

```lisp
(defbackend callfoo-abi (:machine callfoo)
  (registers :return (a) :scratch (a b) :callee-saved (c d)
             :stack-pointer sp :program-counter pc)
  (call :args :stack :order :right-to-left :cleanup :caller :return-address-slots 1)
  (frame :grows :down)
  (operands (reg call-reg) (imm call-imm) (sp-idx call-sp-idx))
  (ops (:add (d s) (add d s))
       (:call (f) (call f))
       (:return () (ret))))
```

The machine, modes and instructions must already be defined. Registers,
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
| `:operand` | The [operand kind](#operand-kinds) that writes a register. |

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
| `:return-address-slots` | Cells a call pushes | `1` |

The values are stored and readable with `backend-descriptor-call`. Items lower
calls from them; see [Calling conventions](conventions.md).

## Frame

| Key | Values | Default |
| --- | --- | --- |
| `:grows` | `:down`, `:up` | The machine's stack-pointer direction, else `:down` |
| `:alignment` | Positive integer, in cells | `1` |
| `:slot` | The [operand kind](#operand-kinds) that addresses a stack slot by its offset from the stack pointer, or from the frame pointer when there is one | None |
| `:stack-slot` | With a `:pointer`, the operand kind that addresses a slot from the stack pointer, for a function with [`:frame nil`](conventions.md#opting-out) | None |
| `:pointer` | The register that is the [frame pointer](conventions.md#frame-pointer) | None |

`:grows` must agree with the machine's `(stack-pointer ... :grows ...)` for the
backend's `:stack-pointer`. `:pointer` fills `(registers :frame-pointer)`, and
must agree with it when both are given; it cannot be the stack pointer or a
`:return` or `call :args` register. With a `:pointer`, `:slot` must address
relative to it.

## Operand kinds

`(KIND MODE)` names an addressing mode by a short kind. An [item](items.md#operands)
writes `(KIND value...)`; the values fill the mode's `expr` holes in order.
A kind cannot be an expression operator such as `+`.

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

`:pushes` and `:pops` after the parameters declare the cells an operation puts
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
the child's machine.

```lisp
(defbackend callfoo-fp-abi (:extends callfoo-abi :machine callfoo-fp)
  (frame :pointer fp :slot fp-idx :stack-slot sp-idx)
  (operands (fp-idx call-fp-idx))
  (ops (:enter () (pushfp) (movfs))
       (:leave () (movsf) (popfp))))
```

| Clause | Merge |
| --- | --- |
| `registers` `call` `frame` | By key. A key the child gives replaces the parent's value; a list is replaced, not appended. |
| `operands` | By kind. |
| `ops` | By operation name. |
| `branches` `stack-writers` | The child's clause replaces the parent's. |
| `(without-ops NAME...)` | Removes those parent operations; a name the parent lacks is an error. |

`:machine` defaults to the parent's machine. When given, it must be that machine
or one that [extends it](machine-families.md). Every operation, register and mode
is checked again on the child's machine, so an operation using an instruction the
child machine removed is an error until the child overrides or drops it.
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
| `backend-descriptor-machine` `-registers` `-call` `-frame` `-operands` `-ops` `-branches` `-stack-writers` `-stack-writer-exceptions` | The stored clauses. |

The command line loads backends from its machine file; see [Command line](cli.md).

[^writers]: The walk reads the instruction's own `set!`, `setf`, `push`, `pop` and `interrupt-return` forms, through macros, including those a `macrolet` in `semantics` or around the `definstruction` defines, with the `choice-case` clauses around each. A `let` or `let*` variable bound to a `choice-case` whose clauses all return literals carries that choice into the branches of an `if`, `when`, `unless`, `and` or `cond` testing it, a `choice-case` tested directly, or their `not`, `null`, `and` and `or`: the then branch of an `and` and the else branch of an `or` take the conditions of their operands, the other branches none. A variable assigned anywhere in its scope, by `setq` or a macro expanding to an assignment, is not followed. Other conditions are not followed, so a write under one is unconditional ([#353](conventions.md#limitations)). When the operand syntax leaves several variants tied on width, such as zero-page and absolute, constant operands select the one the assembler picks; an operand with no value yet, such as a label, is judged by the variant the assembler chose, after assembly, so `render-items` and `items-size` accept it ([#352](conventions.md#limitations)). A macro used inside another local macro's expander body is not expanded ([#354](conventions.md#limitations)). See the [limitations](conventions.md#limitations).
