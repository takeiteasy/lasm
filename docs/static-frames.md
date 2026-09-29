# Static frames

A [`.lsp`](language.md) program keeps its locals and arguments at fixed
addresses, not in stack frames, so a machine with no SP-relative addressing (the
6502, CHIP-8) can be a target. A function that calls itself keeps a stack frame, so
the backend needs stack operations only for recursion.

```lisp
(defun square (n) (* n n))
(defun sum-of-squares (a b) (+ (square a) (square b)))
(defun main () (sum-of-squares 3 4))
```

```sh
lasm run static.lsp -m callfoo.lisp --backend callfoo-lang-abi --frames static
```

See [`tests/fixtures/cli/static.lsp`](../tests/fixtures/cli/static.lsp), and
[`examples/chip8/host.lisp`](../examples/chip8/host.lisp), a backend that has no
other frames.

## Choosing

| Where | Spelling | Wins |
| --- | --- | --- |
| Command line | `--frames static\|stack` | first |
| Compile call | `:frames :static` or `:stack` | first |
| Program header | `(:program (:frames static))` | next |
| Backend | `(frame :static t)` in [`defbackend`](backends.md#frame) | last |

A backend with `(frame :static t)` and no stack operations only works in static
mode, and cannot run a [recursive](#recursion) program; a program can still ask
for `stack` when the backend defines them.

## What a backend needs

None of `:get` `:set` `:alloc` `:free` `:push` `:pop`, nor the `-slot` variants
(the `-label` variants take their place),
nor a `(frame :slot ...)` operand kind: a backend with no slot operand omits it.
A static slot is read with `:peek-label` and written with `:poke-label`, or
with `:const` then `:peek`/`:poke` when the backend has no such operations, so
those are needed, as is `:call`, `:return` and the rest of the
[backend requirements](language.md#backend-requirements). A `:callee-saved`
register holds a value across a call as with stack frames, and is saved in the
function's own frame, not with `:push` and `:pop`.

## How it works

| Stack frames | Static frames |
| --- | --- |
| A parameter or `let` variable is a frame slot | It is a labelled word, `sf`*function*`x`*n* |
| A call pushes its arguments | A call stores each into the callee's parameter word |
| A function allocates its slots on entry | Its words are reserved once, after the code; a recursive function [keeps stack frames](#recursion) |
| A `:callee-saved` register a function uses is pushed and popped | It is stored in an extra word of the function's frame, and loaded back before every `(:return)`[^saved] |
| Slots are live for one call | Functions that never run together share addresses |[^layout]
| A computed call passes its arguments on the stack | It stores them in a shared block, `sfx0`…, and the target's entry thunk copies them |

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

A function in a cycle of the call graph, calling itself directly or through other
functions, keeps a stack frame. Every other function keeps its static frame.

```lisp
(defun fact (n) (if (< n 2) 1 (* n (fact (- n 1)))))
(defun main () (fact 5))                                  ; main: static, fact: stack
```

Calls pass arguments the same way to both: the caller stores each into the
callee's parameter word. A stack function's prologue copies its words into frame
slots, so a recursive call can store the next arguments in them at once.[^cycles]

A backend needs `:get`, `:set`, `:alloc`, `:free`, `:push`, `:pop` and
`(frame :slot ...)` for that. Without them a cycle is a compile error at the call
that closes it.

```
fact.lsp:14:8: fact calls itself; a recursive function needs a stack frame, and the backend has no stack operations (in (fact (- n 1))) (function fact)
```

```
a calls b calls a is recursive; a recursive function needs a stack frame, and the backend has no stack operations
```

An `(asm ...)` that writes a function's label counts as a call to it. A
`funcall` through a computed target counts as a call to every function taken
with `(function F)` that takes as many arguments.[^edges]

## Computed calls

`(funcall E ARG...)` works for any `E`: an array element, a variable or a
computed value.

```lisp
(defarray ops ((function add) (function sub)))
(defun add (a b) (+ a b))
(defun sub (a b) (- a b))
(defun main () (funcall (aref ops 1) 9 4))
```

`(function F)` is F's entry thunk, `sf`*f*`e`. The caller stores its arguments
in the shared block, `sfx0`…, then calls the target. The thunk copies the block
into F's parameter words and falls through into F, which follows it.[^block] A
raw address, such as an `(asm ...)` label, receives its arguments in the block
the same way.

A computed call is linked only to the functions its target can be, for the
recursion check and the frame layout. A target the compiler cannot trace is
taken to be any entered function that takes as many arguments.

`(funcall (function F) ARG...)` and `(F ARG...)` are plain calls, with no thunk.

[^layout]: Every function's frame is as large as its most slots at once, which
  its parameters, `let` variables and the temporaries that would have been
  pushed share out. A function's offset is the largest offset plus size among
  the functions that call it, and one that nobody calls starts at `0`. Every
  word is `.res` of one language word, so a wider word reserves more cells.

[^saved]: The word follows the function's other slots, so a callee's frame never
  overlaps it. The register is stored at the function's start, and each restore
  loads it back with the accumulator left alone, so the return value survives.

[^cycles]: The call graph is compiled once with every frame static to find the
  cycles, then again with a stack frame for each function in one. The functions
  of a cycle share the words after those of every function that calls into it,
  and a static function they call lies beyond them.

[^staging]: Only the arguments before the last one that may call are held; the
  rest are stored directly. A call, a `funcall` and any `(asm ...)` may call.

[^edges]: The layout uses the same edges, so a function reached through a
  computed call never shares addresses with its caller.

[^block]: The block has as many words as the most arguments any computed call
  passes. An argument that waits for a later one that may call is held in the
  caller's frame first, as for a plain call, so a nested computed call cannot
  overwrite the block.
