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

Each clause is optional and may appear once.

## Registers

Every name is a register or register alias of the machine.

| Key | Value |
| --- | --- |
| `:return` `:arguments` `:scratch` `:caller-saved` `:callee-saved` | A list of registers. |
| `:stack-pointer` `:program-counter` `:frame-pointer` | One register. |

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

The values are stored and readable with `backend-descriptor-call`. Calls are
not lowered.[^lowering]

## Frame

| Key | Values | Default |
| --- | --- | --- |
| `:grows` | `:down`, `:up` | The machine's stack-pointer direction, else `:down` |
| `:alignment` | Positive integer, in cells | `1` |

`:grows` must agree with the machine's `(stack-pointer ... :grows ...)` for the
backend's `:stack-pointer`.

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

`(backend-expand-op backend name args)` returns the forms. A form's mnemonic
must exist on the machine, and its operand kinds must be declared.

## Lookup

| Function | Returns |
| --- | --- |
| `(find-backend name)` | The `backend-descriptor`; `unknown-backend` if none. |
| `backend-descriptor-machine` `-registers` `-call` `-frame` `-operands` `-ops` | The stored clauses. |

The command line loads backends from its machine file; see [Command line](cli.md).

## Limitations

| Limitation | Ticket |
| --- | --- |
| Calls and frames are described, not lowered. | [#320](https://todo.sr.ht/~takeiteasy/lasm/320) |
| Frames are stack-pointer relative only. | [#321](https://todo.sr.ht/~takeiteasy/lasm/321) |
| Register-passed arguments are not lowered. | [#322](https://todo.sr.ht/~takeiteasy/lasm/322) |
| A backend cannot extend another. | [#323](https://todo.sr.ht/~takeiteasy/lasm/323) |
| An operation cannot define labels of its own. | [#325](https://todo.sr.ht/~takeiteasy/lasm/325) |

[^lowering]: A front end emits the argument pushes, call and clean-up itself,
  as [items](items.md).
