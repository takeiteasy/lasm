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
| `(without-ops NAME...)` | Removes those parent operations; a name the parent lacks is an error. |

`:machine` defaults to the parent's machine. When given, it must be that machine
or one that [extends it](machine-families.md). Every operation, register and mode
is checked again on the child's machine, so an operation using an instruction the
child machine removed is an error until the child overrides or drops it.
Redefining a parent rebuilds its children, and their children, from their own
clauses. If one no longer builds, the redefinition is a `backend-definition-error`
naming it, and every backend stays as it was. A backend cannot extend one that
extends it.

## Lookup

| Function | Returns |
| --- | --- |
| `(find-backend name)` | The `backend-descriptor`; `unknown-backend` if none. |
| `backend-descriptor-machine` `-registers` `-call` `-frame` `-operands` `-ops` | The stored clauses. |

The command line loads backends from its machine file; see [Command line](cli.md).

## Limitations

| Limitation | Ticket |
| --- | --- |
| A child's `without-ops` naming an operation the new parent drops blocks redefining the parent. | [#339](https://todo.sr.ht/~takeiteasy/lasm/339) |
