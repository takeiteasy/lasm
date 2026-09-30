# Static frames

A [`.lsp`](language.md) program keeps its locals and arguments at fixed
addresses, not in stack frames, so a machine with no SP-relative addressing (the
6502, CHIP-8) can be a target. A function that calls itself keeps a stack frame, so
the backend needs stack operations only for recursion. A [`.lasm`](items.md#lasm-files)
function can keep its locals in [labelled words](#lasm-functions) too.

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
| `.lasm` function | `(:function f (:frames static\|stack))` | first |
| Command line | `--frames static\|stack` | next |
| Compile or assemble call | `:frames :static` or `:stack` | next |
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

## `.lasm` functions

A [`(:function ...)`](conventions.md#items) keeps its locals in labelled words when
its frames are static. `(:local i)` is the word `sf`*function*`x`*n*, addressed
with the backend's `(frame :label-slot KIND)`; *n* is `i` plus the argument words.

```lisp
(:function square-plus (:args 1 :locals 1)
  (:op :move (:local 0) (:arg 0))
  (:call multiply (:local 0) (:local 0))
  (:op :add (zp w0) (:local 0))
  (:return))
(:static-frames)
```

See [`demo.lasm`](../examples/6502/demo.lasm).

| Part | Behavior |
| --- | --- |
| Choosing | The function's `:frames`, then the [program's choice](#choosing). A backend with `(frame :static t)` and no `:alloc` is static unless told otherwise. |
| Words | One per argument past the registers, then `:locals`, then one per saved register, each `.res` of one language word. |
| Placement | At `(:static-frames)`, which holds every function's words, in RAM if the code is ROM. A function's words start after those of every function that calls it, so two that never run together [share addresses](#how-it-works). Without it, a function's words follow its code, so end the body with `(:return)`, and none are shared. |
| Arguments | `(:arg i)` is a register, or the word `sf`*function*`x`*n* for an argument past them. `(:call f ARG...)` stores each into that word, with no push. |
| `:save` | A saved register is stored in a word at entry and loaded back by `(:return)`. The backend needs `:poke-label` and `:peek-label`. |
| Interrupts | A function with `:interrupt t` is [an interrupt handler](#interrupts): its words lie apart from the main line's, and `(:return)` ends with the backend's `:return-interrupt`. |
| Halves | `(:lo (:local i))` and `(:hi (:local i))` are the cells of the word; the second is the label plus one. |
| Not allowed | `:frame t`, and `:alloc`, `:free`, `:enter` and `:leave`. |

A static function is not re-entrant. A call to itself, directly or through other
functions, is an `items-malformed` error at the call that closes the cycle; any
mention of a function's name in another's body, such as `(jmp f)`, counts as a call.
Use `:frames stack` for a function that recurses.

```
demo.lasm:9:5: a calls b calls a is recursive; a static function keeps its locals in fixed words, so give it :frames stack
```

## Interrupts

A function the machine runs at any time, an interrupt handler, takes
`:interrupt t` in a `.lasm` file, or `(declare (interrupt))` in a `.lsp` file.
Its words, and those of the functions it runs, lie apart from the main line's
and from other handlers', so an interrupt never overwrites a function it
interrupts.

```lisp
(:function irq (:interrupt t :locals 1)
  (:op :move (:local 0) (zp w0))
  (:call log (:local 0))
  (:return))
(:static-frames)
```

| Part | Behavior |
| --- | --- |
| Return | `(:return)` loads the saved registers back, then emits the backend's `:return-interrupt`, which the backend must define. |
| Arguments | None: `:args` is an error. |
| Words | The main line's first, then each handler's region in program order.[^regions] |
| Shared functions | A static function run from a handler and from the main line, or from two handlers, is an `items-malformed` error. Give it `:frames stack`. |
| Mentions | Naming a handler in other code, to load its address into a vector, is not a call. |
| Not covered | A handler written as a plain label, like `irq` in [`demo.lasm`](../examples/6502/demo.lasm), is outside the call graph and is not kept apart. |

```
demo.lasm:14:5: interrupt irq calls log, and main calls log; a static function is not re-entrant, so give it :frames stack
```

### `.lsp` handlers

A `defun` with no parameters and `(declare (interrupt))` first in its body is a
handler. The words are laid out as for a `.lasm` handler, and it returns with the
backend's `:return-interrupt`.

```lisp
(defvar ticks 0)
(defun irq ()
  (declare (interrupt))
  (set ticks (+ ticks 1)))
(defun main () ticks)
```

| Part | Behavior |
| --- | --- |
| Shared functions | A function run from a handler and from the main line, or from two handlers, keeps a stack frame, as a [recursive one](#recursion) does. The backend needs the stack operations, or it is a compile error. |
| Parameters | A shared function takes none: its argument words would be shared, so an interrupt between a caller's store and the call could overwrite them. It is a compile error. |
| Computed calls | A `funcall` of a computed target in a handler, or in a function it runs, is a compile error: it stores its arguments in a block the main line's computed calls share.[^block] |
| Not allowed | `main` as a handler, and parameters on one. |

```
irq.lsp:6:3: interrupt irq calls log, and main calls log; a function run from both keeps its arguments in words the two share, so it cannot take parameters
```

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
  the functions that call it, and one that nobody calls starts at `0`, or at the
  start of its [handler's region](#interrupts). Every word is `.res` of one language word, so a wider word reserves more cells.

[^regions]: A handler's region holds the words of the handler and of every
  function it calls, laid out as on the main line. The regions follow one another,
  so no address is shared between them.

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
