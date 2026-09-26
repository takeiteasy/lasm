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
| `(:function NAME (OPTION...) ITEM...)` | The label, then the prologue. Options: `:args n`, `:locals n`, `:save (reg...)`. |
| `(:return)` | The epilogue and the return. |
| `(:call TARGET ARG... [:keep (reg...)])` | The [call sequence](#calls). |
| `(:push X)` `(:pop X)` | One push or pop, tracked in the frame. |

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

## Calls

`(:call f a b c)` emits, in order:

1. A push of each `:keep` register that is `:caller-saved`. A `:callee-saved`
   register is skipped; a `:return` register or a register with neither role is
   an error.
2. A push of each argument beyond the `call :args` registers, in the `:order`.
3. A `:move` into each argument register, ordered so no move overwrites a
   register a later one reads.
4. The `:call`.
5. A `:free` of the stack arguments when `:cleanup` is `:caller`.
6. A pop of each kept register, in reverse.

With `call :args (b c)`, `(:call f (imm 1) (imm 2) (imm 3))` moves `1` into
`b` and `2` into `c` and pushes `3`; the callee finds its arguments `0` and `1`
in `b` and `c` and `2` on the stack.

## Backend operations

Lowering emits these [operations](backends.md#operations) of the backend, and
a backend need only define those its programs use. Using one that is missing is
an `items-malformed`.

| Operation | Emitted for |
| --- | --- |
| `:push (x)` `:pop (x)` | Saves, kept registers, spilled arguments, `(:push)` `(:pop)`. |
| `:alloc (n)` `:free (n)` | Locals; caller clean-up. `n` is a positive count of cells. |
| `:move (dst src)` | Register arguments. |
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
| Register arguments that swap with each other are an error. | [#328](https://todo.sr.ht/~takeiteasy/lasm/328) |
| Without a frame pointer, the stack depth is tracked per item, not across labels or branches; a raw push in a body is not seen. | [#329](https://todo.sr.ht/~takeiteasy/lasm/329) |
| Every function of a frame-pointer backend has a frame pointer, even a leaf. | [#331](https://todo.sr.ht/~takeiteasy/lasm/331) |
