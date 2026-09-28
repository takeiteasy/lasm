# Static frames

A [`.lsp`](language.md) program keeps its locals and arguments at fixed
addresses, not in stack frames, so a machine with no SP-relative addressing (the
6502, CHIP-8) can be a target. The cost: no recursion.

```lisp
(defun square (n) (* n n))
(defun sum-of-squares (a b) (+ (square a) (square b)))
(defun main () (sum-of-squares 3 4))
```

```sh
lasm run static.lsp -m callfoo.lisp --backend callfoo-lang-abi --frames static
```

See [`tests/fixtures/cli/static.lsp`](../tests/fixtures/cli/static.lsp).

## Choosing

| Where | Spelling | Wins |
| --- | --- | --- |
| Command line | `--frames static\|stack` | first |
| Compile call | `:frames :static` or `:stack` | first |
| Program header | `(:program (:frames static))` | next |
| Backend | `(frame :static t)` in [`defbackend`](backends.md#frame) | last |

A backend with `(frame :static t)` and no stack operations only works in static
mode; a program can still ask for `stack` when the backend defines them.

## What a backend needs

None of `:get` `:set` `:alloc` `:free` `:push` `:pop`, nor the `-slot` variants.
A static slot is read with `:const` and `:peek`, and written with `:const` and
`:poke`, so those are needed, as is `:call`, `:return` and the rest of the
[backend requirements](language.md#backend-requirements). The registers and
`:save` of the stack convention are unused: no register is held across a call.

## How it works

| Stack frames | Static frames |
| --- | --- |
| A parameter or `let` variable is a frame slot | It is a labelled word, `sf`*function*`x`*n* |
| A call pushes its arguments | A call stores each into the callee's parameter word |
| A function allocates its slots on entry | Its words are reserved once, after the code |
| Slots are live for one call | Functions that never run together share addresses |[^layout]

A function's words start after those of every function that calls it, so a
function and its callees never overlap.

```
main    sfmainx0
sum     sfsumx0 sfsumx1
square  sfsquarex0          (after sum's)
```

An argument is stored straight into the callee's parameter as soon as it is
computed, unless a later argument may call a function, which could reuse that
address. Then it waits in a word of the caller's own frame until every argument
is ready.[^staging]

`(:var NAME)` in an [`(asm ...)`](language.md#inline-items) is the word's label,
like a global's, not a frame slot.

## Recursion

A cycle in the call graph, direct or through other functions, is a compile error
at the call that closes it.

```
fact.lsp:14:8: fact calls itself; static frames do not support recursion (in (fact (- n 1))) (function fact)
```

```
a calls b calls a is recursive; static frames do not support recursion
```

An `(asm ...)` that writes a function's label counts as a call to it.

## Computed calls

`(funcall (function F) ARG...)` is a plain call. A `funcall` through any other
value is a compile error.[^funcall]

## Limitations

| Limitation | Ticket |
| --- | --- |
| `funcall` through a computed target is an error. | [#419](https://todo.sr.ht/~takeiteasy/lasm/419) |
| A recursive function is an error; it cannot keep a stack frame. | [#420](https://todo.sr.ht/~takeiteasy/lasm/420) |
| No value is held across a call in a callee-saved register. | [#421](https://todo.sr.ht/~takeiteasy/lasm/421) |
| A global or static word takes two instructions to read or write, `:const` and `:peek`/`:poke`. | [#422](https://todo.sr.ht/~takeiteasy/lasm/422) |

[^layout]: Every function's frame is as large as its most slots at once, which
  its parameters, `let` variables and the temporaries that would have been
  pushed share out. A function's offset is the largest offset plus size among
  the functions that call it, and one that nobody calls starts at `0`. Every
  word is `.res` of one language word, so a wider word reserves more cells.

[^staging]: Only the arguments before the last one that may call are held; the
  rest are stored directly. A call, a `funcall` and any `(asm ...)` may call.

[^funcall]: The caller cannot know where a taken function's parameters are, and
  the call graph has no edge to it.
