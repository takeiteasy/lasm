# Calling conventions

A [backend](backends.md)'s `call` and `frame` clauses lower to instructions.
A front end writes functions and calls; the pushes, clean-up, prologue,
epilogue and argument addressing come from the backend.

```lisp
(assemble-items '((:call double (imm 21))
                  (hlt)
                  (:function double (:args 1)
                    (lds (reg a) (:arg 0))
                    (:op :add (reg a) (reg a))
                    (:return)))
                :backend 'callfoo-abi)
```

With `callfoo-abi` this renders as:

```
pushv # 21
call double
adds sp, # 1
hlt
double:
lds a, [ sp + 1 ]
add a, a
ret
```

## Items

| Item | Emits |
| --- | --- |
| `(:function NAME (OPTION...) ITEM...)` | The label, then the prologue. Options: `:args n`, `:locals n`, `:save (reg...)`, `:frame nil`. |
| `(:return)` | The epilogue and the return. |
| `(:call TARGET ARG... [:keep (reg...)])` | The [call sequence](#calls). |
| `(:push X)` `(:pop X)` | One push or pop, tracked in the frame. |
| `(:depth n)` | Sets the [tracked depth](#stack-depth) to `n`. Emits nothing. |

| Operand | Is |
| --- | --- |
| `(:arg i)` | Argument `i`: a register, or a stack slot. |
| `(:local i)` | Local `i`, a stack slot. |

`(:arg)`, `(:local)` and `(:return)` are valid only inside a `:function`, which
cannot nest. `:args` is required when the call is `:cleanup :callee` or
`:order :left-to-right`.

## Functions

The prologue pushes each `:save` register, which must be `:callee-saved`, then
allocates the locals. The locals are padded so the saves and locals fill a
multiple of the frame's `:alignment` cells. `(:return)` undoes both, then
returns; it needs the stack at its entry depth.

From the top of the stack, a frame holds the locals, the saved registers, the
return address, then the stack arguments. A slot is addressed by its distance from the top of the stack, through the
frame's `:slot` operand kind: `+n` when the stack grows down, `-1-n` when it
grows up.

`(:arg i)` for a register argument names that register, so an inner `(:call)`
overwrites it: copy it first or `:keep` it.

## Stack depth

Without a frame pointer, `(:arg i)` and `(:local i)` are addressed from the
stack pointer, so the lowering counts the cells the body has pushed. A function
without a frame pointer is checked three ways:

| Check | Error when |
| --- | --- |
| Labels | A label is defined at one depth and a [branch](backends.md#branches) instruction that names it is at another. |
| Raw stack instructions | An instruction, or an `(:op)` expansion form, is a backend's `:push`, `:pop`, `:alloc` or `:free` template: the same mnemonic, the parameters matching anything, the rest equal. `(:op :push ...)` is rejected too. |
| `(:depth n)` | `n` is negative, or it is outside a `:function`. |

An `(:op)` that declares [`:pushes` and `:pops`](backends.md#operations) is
accepted, and the depth follows its net effect:

```lisp
(ops (:push2 (a b) :pushes 2 (pushv a) (pushv b)))
```

```lisp
(:op :push2 (imm 1) (imm 2))   ; depth + 2
(lds (reg a) (:arg 0))         ; addressed two cells deeper
```

`(:depth n)` states the depth where the tracking cannot know it, such as after
a jump:

```lisp
(:function f ()
  (:push (imm 1))
  (call over)        ; at depth 1
  (:pop (reg b))
  (:depth 1)         ; `over` is reached from the call above
  (:label over)
  (:pop (reg b))
  (:return))
```

A [frame pointer](#frame-pointer) function does not depend on the depth and is
not checked.[^depth]

## Frame pointer

`(frame :pointer REG)` in the [backend](backends.md#frame) gives every function
a frame register. Slots are addressed from it, so `(:arg i)`, `(:local i)` and
`(:return)` do not depend on how many cells the body has pushed.

```
grows down     [fp + n]
  argument j   above the return address
  return addr
  saved regs   fp + 1 ... fp + S
  saved fp     fp + 0
  local i      fp - L + i
  pushes       below the locals, any number
```

| Step | Emits |
| --- | --- |
| Prologue | Push each `:save` register, `:enter`, then `:alloc` the locals. |
| Epilogue | `:leave`, pop the saves, then return. |

`:enter` pushes the frame pointer and points it at the top of the stack;
`:leave` restores the stack pointer from it and pops it. The saved frame pointer
counts towards the frame's `:alignment`. The frame pointer cannot be a `:save`
register. The backend's `:slot` kind takes an offset from the frame pointer, with
the same signs as from the stack pointer: `+n` when the stack grows down,
`-1-n` when it grows up.

### Opting out

`(:function f (:frame nil) ...)` skips `:enter` and `:leave` and the saved frame
pointer cell, and addresses slots from the stack pointer through the backend's
`(frame :stack-slot KIND)`, with the [stack depth](#stack-depth) tracked. Using a
slot without a `:stack-slot` kind is `items-malformed`. `:frame t` on a backend
with no `:pointer` is `items-malformed`; `:frame nil` there has no effect.

```lisp
(frame :pointer fp :slot fp-idx :stack-slot sp-idx)
```

```lisp
(:function leaf (:args 1 :frame nil)
  (lds (reg a) (:arg 0))    ; lds a, [ sp + 1 ]
  (:return))                ; ret
```

## Calls

`(:call f a b c)` emits, in order:

1. A push of each `:keep` register that is `:caller-saved`. A `:callee-saved`
   register is skipped; a `:return` register or a register with neither role is
   an error.
2. A push of each argument beyond the `call :args` registers, in the `:order`.
3. A `:move` of each register the call target reads into a free `:scratch`
   register, when an argument move would overwrite it. The call goes through
   the copy. With no free one, the call is `items-malformed`.
4. A `:move` into each argument register, ordered so no move overwrites a
   register a later one reads. A [cycle](#register-cycles) uses `:exchange`
   or a `:scratch` register.
5. The `:call`.
6. A `:free` of the stack arguments when `:cleanup` is `:caller`.
7. A pop of each kept register, in reverse.

With `call :args (b c)`, `(:call f (imm 1) (imm 2) (imm 3))` moves `1` into
`b` and `2` into `c` and pushes `3`; the callee finds its arguments `0` and `1`
in `b` and `c` and `2` on the stack.

### Register cycles

Arguments that swap registers form a cycle. With an `:exchange` operation, each
step swaps two registers of the cycle: one exchange for two registers, `n-1` for
`n`, and no scratch register.

```lisp
(:call f (reg c) (reg b))   ; call :args (b c), :exchange (x y) (xchg x y)
```

```
xchg b, c
call f
```

Without one, the first move's destination is copied into a free `:scratch`
register, and the moves that read it read the copy. A scratch register is free
when no argument register is it, no remaining move reads it and the call target
does not. With no free one, the call is `items-malformed`.

```lisp
(:call f (reg c) (reg b))   ; call :args (b c), registers :scratch (a)
```

```
movv a, b
movv b, c
movv c, a
call f
```

## Backend operations

Lowering emits these [operations](backends.md#operations) of the backend, and
a backend need only define those its programs use. Using one that is missing is
an `items-malformed`.

| Operation | Emitted for |
| --- | --- |
| `:push (x)` `:pop (x)` | Saves, kept registers, spilled arguments, `(:push)` `(:pop)`. |
| `:alloc (n)` `:free (n)` | Locals; caller clean-up. `n` is a positive count of cells. |
| `:move (dst src)` | Register arguments; a copy of a register the call target reads. |
| `:exchange (a b)` | A register-argument cycle, when defined. Swaps two registers. |
| `:call (f)` | `(:call ...)`. |
| `:return ()` | `(:return)`, or with `:cleanup :caller`. |
| `:return-pop (n)` | `(:return)` with `:cleanup :callee` and stack arguments. |
| `:enter ()` `:leave ()` | The prologue and epilogue of a [frame pointer](#frame-pointer). |

The backend also names the operand kind that writes a register, `(registers
:operand KIND)`, and the one that addresses a stack slot, `(frame :slot KIND)`.
The slot kind takes one value, the offset from the stack pointer, or from the
frame pointer with one.

```lisp
(defbackend callfoo-abi (:machine callfoo)
  (registers :return (a) :callee-saved (c d) :stack-pointer sp :operand reg)
  (call :args :stack :order :right-to-left :cleanup :caller)
  (frame :grows :down :slot sp-idx)
  (operands (reg call-reg) (imm call-imm) (sp-idx call-sp-idx) (sp call-sp))
  (ops (:push (x) (pushv x))
       (:pop (x) (popr x))
       (:move (d s) (movv d s))
       (:alloc (n) (subs (sp) (imm n)))
       (:free (n) (adds (sp) (imm n)))
       (:call (f) (call f))
       (:return () (ret))))
```

## Limitations

| Limitation | Ticket |
| --- | --- |
| A symbol spelled like a register is read as that register in an argument or the call target. | [#336](https://todo.sr.ht/~takeiteasy/lasm/336) |
| A raw instruction that writes the stack pointer, and matches no stack template or declared effect, is not seen. | [#340](https://todo.sr.ht/~takeiteasy/lasm/340) |

[^depth]: A label's depth is recorded at its definition and each reference's when the instruction is lowered; the two are compared at the end of the function, so a forward branch is checked. A name that is not a label of the body is ignored.
