# Semantics vocabulary

`with-machine` creates a machine and binds its registers, flags, and
operators for a semantics body:

```lisp
(with-machine (m sixtyfoo)
  (set! a 42)
  (push a s)
  (set-flags! (z (zero? a)))
  (setf (mref m 'ram #x1000) 1))
```

Scalar registers and flags read as symbols. Banked registers read as
`(NAME index)` or by a declared alias. Memory and devices use explicit
access functions; see [Machine model](machine-model.md) and
[Devices](devices.md).

## Operators

| Form | Effect |
| --- | --- |
| `(set! place value)` | Write a register, flag, or other settable place. |
| `(mref machine [memory] address)` | Read or write a memory cell. |
| `(push value [stack])`, `(pop [stack])` | Use a fixed stack or register-backed stack pointer. |
| `(stack-depth [stack])`, `(stack-pointer [stack])`, `(stack-ref offset [stack])` | Inspect a fixed stack. |
| `(set-bank! region n)` | Map a memory bank. |
| `(set-flags! (flag form)...)` | Set named flags. |
| `(trap tag [data])` | Signal `lasm-trap`. |
| `(idle)` | Mark the machine idle after semantics finishes. |
| `(extra-cycles n)` | Add runtime-dependent cost to this instruction. |
| `(interrupt-return)` | Restore the state saved at interrupt delivery. |
| `(zero? value)`, `(bit-set? value bit)` | Predicates for flag expressions. |
| `(page-crossed? from to [page-size])` | Test whether two addresses cross a page boundary. |

`mref` defaults to the sole memory element in semantics; name it when there
are several. Stack operators default to the sole fixed stack, or the sole
register-backed stack pointer if there is no fixed stack. An ambiguous
default signals during macroexpansion. Fixed-stack inspection does not
operate on a register-backed stack pointer.[^stack]

`idle` does not stop the current semantics body. `trap` signals a condition.
`extra-cycles` accumulates and contributes to cycle budgets; see
[Emulator](emulator.md#dynamic-cycle-costs). Device operations and
`signal-interrupt` take the machine explicitly.

## `choice-case`

Inside an instruction's semantics, `choice-case` dispatches on the decoded
alternative of a `one-of` operand or a named choice slot:

```lisp
(choice-case src
  (a-reg (reg src))
  (a-ind (mref machine 'ram (reg src)))
  (otherwise src))
```

The selector is a named operand, `operand` for the first field, or a named
slot. Keys name alternatives. A missing match without `otherwise` signals
`no-matching-choice`. Decode must preserve the selection through a word
`choice` field or cell sub-opcode; otherwise only `otherwise` can handle
it. See [Per-operand modes](operand-modes.md#encoding-a-selection).

Nested varying alternatives can be queried by path: `(choice-case (src ind)
...)` selects inside outer alternative `ind`. An extra operand absent from
a shorter alternative is bound to `nil` there; read it inside the matching
branch. See [Per-operand modes](operand-modes.md#nested-varying-alternatives).

## `push`/`pop` and Common Lisp

Inside semantics, `push` and `pop` operate on machine stacks. The `lasm`
package shadows Common Lisp's list operators; use `cl:push` and `cl:pop`
for lists in code using the `lasm` package.

## `with-machine-bindings`

`with-machine-bindings` provides the same bindings around an **existing**
machine instance:

```lisp
(with-machine-bindings (m sixtyfoo)
  (set! a 42))
```

`definstruction` uses these bindings for its semantics body. See
[Instructions](instructions.md#semantics-form).

[^stack]: `push` and `pop` accept a fixed-stack name or a register declared
  by `(stack-pointer ...)`. `stack-ref` uses a top-relative index into a
  fixed stack. `set-bank!` requires a banked region and checks bank range
  when executed. `interrupt-return` requires an interrupt declaration.
