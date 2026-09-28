# Source language

A small Lisp-like language that compiles to [items](items.md) through a
[backend](backends.md), so one program targets any machine that has one. Values
are plain machine words: no tags, no heap, no garbage collection.

```lisp
(:program (:backend callfoo-lang-abi))

(defvar calls 0)

(defun fact (n)
  (set calls (+ calls 1))
  (if (< n 2)
      1
      (* n (fact (- n 1)))))

(defun main () (fact 5))
```

```sh
lasm run fact.lsp -m callfoo.lisp     # a is 120 when it halts
lasm compile fact.lsp -m callfoo.lisp # writes fact.lasm
```

See [`examples/cli/fact.lsp`](../examples/cli/fact.lsp).

## Program

A `.lsp` file holds top-level forms, in any order. A leading
`(:program (OPTION...))` takes the options of a [`.lasm` file](items.md#lasm-files).

| Form | Is |
| --- | --- |
| `(defun NAME (PARAM...) BODY...)` | A function returning its last form's value. |
| `(defvar NAME [INTEGER])` | A one-word global, `0` unless given. |
| `(defconstant NAME INTEGER)` | A compile-time integer. |
| `(defarray NAME SIZE)` | `SIZE` words reserved, uninitialised. |
| `(defarray NAME (VALUE...))` | Words initialised to `VALUE...`, each an integer, `(function F)`, or another `defconstant`/`defarray`/`defstring` name. |
| `(defstring NAME "TEXT")` | `TEXT`, one character a word, `0`-terminated. |
| `(defmacro NAME (PARAM... [&rest R]) BODY...)` | A compile-time macro; see [below](#macros). |
| `(defun-for-syntax NAME (PARAM... [&rest R]) BODY...)` | A compile-time helper function, callable from a macro's `BODY`. |

The program needs `(defun main () ...)`. The compiled program starts with a stub
that stores the globals' initial values, calls `main` and halts; the runner sets
the stack pointer. `defarray` and `defstring` data is part of the image, not the
stub, so it starts at those values every load.

A `defarray`/`defstring` name is its address, always, unlike a `defvar`'s name,
which is its value (peeked/poked through); it cannot be `set`. `(aref A I)`/
`(aset A I V)` index by word, `I` from `0`.

## Words wider than a cell

A word is one cell unless the backend's machine gives its stack pointer a
[`:width`](machine-model.md#stacks) wider than a cell (#167), in which case a
word spans that many cells, in the machine's own `:endian` order -- the same
split a stack slot already gets. `(defvar ...)`, `(defarray ...)` and
`(defstring ...)` all lay out by it: a `defvar` reserves a whole word, and
`(aref A I)`/`(aset A I V)` scale `I` by it, so an array or string strides
the same way regardless of word size.

An initialised `(defarray NAME (VALUE...))` or `(defstring NAME "TEXT")` is
laid out with `.cell` for a one-cell word and
[`.emit`](directives.md#emit) at the word's width otherwise, for any word size.
A backend whose stack instructions count cells rather than slots sets
[`(frame :offsets :cells :counts :cells)`](conventions.md#slots-and-cells).

Raw `peek`/`poke` and a manual address computed with `+` always count
cells, not words -- only `aref`/`aset` scale by the word size. The
backend's `:peek`/`:poke` operations (below) must move a whole word for
`peek`/`poke` to agree with `aref`/`aset` on what one element is.

## Expressions

Every expression has a value; a name is a parameter, `let` variable, global or
constant.

| Form | Value |
| --- | --- |
| `5`, `-5` | The integer. |
| `(set NAME E)` | `E`, stored in a variable or global. |
| `(let ((V E)...) BODY...)` | The last body form. Each `E` sees the earlier `V`. |
| `(if C A [B])` | `A` or `B`; `0` with no `B`. |
| `(while C BODY...)` | `0`. |
| `(progn E...)` | The last `E`, or `0`. |
| `(and E...)` `(or E...)` | The deciding value; stops at the first false, or true, one. |
| `(not E)` | `1` if `E` is `0`, else `0`. |
| `(peek ADDR)` `(poke ADDR V)` | The word at `ADDR`; `V`, stored there. |
| `(peek-byte ADDR)` `(poke-byte ADDR V)` | The byte at `ADDR`; `V`, stored there; see [below](#arrays-strings-and-byte-access). |
| `(aref A I)` `(aset A I V)` | The cell at index `I` of array/string `A`; `V`, stored there. |
| `(return [E])` | Exits the function with `E`, or `0`; see [below](#return).[^return] |
| `(function F)` | `F`'s address, a value; see [below](#function-values). |
| `(funcall E ARG...)` | Calls through `E`'s value; see [below](#function-values). |
| `(F ARG...)` | A call to `defun` `F`. |
| `(asm ITEM...)` | The accumulator; see [inline items](#inline-items). |

A value is true unless it is `0`.

| Operators | Meaning |
| --- | --- |
| `+ - * / mod` | Arithmetic; `+` `-` `*` take two or more operands, `(- x)` negates. |
| `logand logior logxor shl shr` | Bitwise. |
| `= /= < > <= >=` | Two operands; `1` or `0`. |

Symbols are compared by name, ignoring case.

## Return

`(return [E])` exits the enclosing function with `E`'s value, or `0` with no
`E`.

```lisp
(defun clamp (n limit)
  (if (< n limit) (return n))
  limit)
```

## Function values

`(function F)` is `F`'s address, a value like any other -- stored in a
variable, put in a `defarray`, or called through. `(funcall E ARG...)` calls
through `E`'s value: a literal `(function F)` compiles the same direct call
`(F ARG...)` does, arity-checked at compile time; any other `E` computes a
target checked only at the call.

```lisp
(defun add-one (x) (+ x 1))
(defun double (x) (* x 2))
(defarray ops ((function add-one) (function double)))

(defun apply-op (index n) (funcall (aref ops index) n))
```

Needs the backend's `:call` to have a clause for a register target (#365,
[backend requirements](#backend-requirements)).

## Arrays, strings and byte access

`(defarray NAME SIZE)` and `(defarray NAME (VALUE...))` declare an array;
`(defstring NAME "TEXT")` a `0`-terminated string, one character a word. Both
are addressed data in the image, not a stub-initialised `defvar`: the name is
always its address, and `(aref A I)`/`(aset A I V)` index a word of it (a
cell, unless the backend's [word is wider](#words-wider-than-a-cell)), `I`
from `0`.

```lisp
(defstring greeting "hi")

(defun sum (s len)
  (let ((total 0) (i 0))
    (while (< i len)
      (set total (+ total (aref s i)))
      (set i (+ i 1)))
    total))
```

`(peek-byte ADDR)`/`(poke-byte ADDR V)` are the backend's optional
`:peek-byte`/`:poke-byte`, byte-addressing `ADDR` as the machine defines it --
for a machine whose registers are wider than its cells. Using one without the
backend operation is a compile error naming the form.

## Macros

A `defmacro` call, in an expression or at top level, is replaced by its
`BODY` evaluated at compile time, with each parameter bound to the call's own
argument, unevaluated -- a form, not its value. `BODY` most often builds its
result with a quasiquoted template.

```lisp
(defmacro inc (v) `(set ,v (+ ,v 1)))
(defmacro unless (c &rest body) `(if ,c 0 (progn ,@body)))
(defmacro swap (a b) `(let ((tmp ,a)) (set ,a ,b) (set ,b tmp)))

(defmacro double-or-inc (n) (if (integerp n) `(+ ,n ,n) `(+ ,n 1)))
```
`` `TEMPLATE `` is TEMPLATE with each `,FORM` replaced by FORM's value and each
`,@FORM` (only as a list element) spliced in, FORM's value being a list.
`double-or-inc` decides its expansion from its own argument: doubling it when
it's a literal integer the macro can already see, incrementing it otherwise
(so it still works on a non-literal argument, computed at run time).

A trailing `&rest R` binds the remaining arguments as a list. A parameter is
substituted wherever it appears, so using it twice in the template runs its
argument twice.

A `let` the template writes gets fresh binding names, so `(swap tmp y)`
above doesn't confuse the macro's own `tmp` with the caller's: only a
template's own `let` names are protected this way, and a name the template
otherwise refers to free is still resolved where the macro is used, same as
any other name. A binding name written as `,NAME` (its value, computed, not
the template's own) is used as given, unrenamed. A macro call also works at
top level, where it can expand to `defun`, `defvar`, `defconstant`,
`defmacro`, or a `(progn DEF...)` of them.

Inside `(asm ...)`, `,FORM` and `,@FORM` substitute like anywhere else in the
template, so a constant, a register name or a `(:var NAME)` can all be
computed; everything written literally, including a register operand such as
`(reg a)`, is left alone.

### The compile-time evaluator

`BODY`'s forms run over plain data: integers, strings, symbols and lists.
`()` is false; anything else, including `t`, is true.

| Form | Does |
| --- | --- |
| `(quote FORM)`, `'FORM` | FORM itself, unevaluated. |
| `` (quasiquote FORM) ``, `` `FORM `` | FORM as a template; see above. |
| `(if TEST THEN [ELSE])` | THEN when TEST is true, else ELSE (`()` if omitted). |
| `(let ((NAME VALUE)...) BODY...)`, `let*` | As the language's own `let`, but at compile time. |
| `(progn FORM...)` | Each FORM in order; the last one's value. |
| `car`, `cdr`, `cons`, `list`, `append`, `length` | List operations; `car`/`cdr` of `()` is `()`. |
| `null`, `consp`, `symbolp`, `integerp` | Type predicates. |
| `eq`, `equal` | `eq` compares a pair of symbols by name, never by identity. |
| `+`, `-`, `*`, `=`, `<`, `>` | Integer arithmetic and comparison, each over any number of arguments. |
| `gensym` | A symbol no source text can spell, for a template to bind without capturing anything (`(gensym PREFIX)` names it, for reading a macro's own compile-time errors). |
| `error` | Signals a compile error naming the macro's call. |

`(defun-for-syntax NAME (PARAM... [&rest R]) BODY...)` is a compile-time
helper: unlike a macro's, its own arguments are evaluated before the call, so
it can recurse over a value a macro has already computed.

```lisp
(defun-for-syntax sum-of (xs) (if (null xs) 0 (+ (car xs) (sum-of (cdr xs)))))
(defmacro total (&rest xs) (sum-of xs))

(defun main () (total 1 2 3))          ; 6, added up at compile time
```

See [`examples/cli/macros.lsp`](../examples/cli/macros.lsp).[^macros]

## Inline items

`(asm ITEM...)` puts [items](items.md) in the function. `(:var NAME)` inside an
item is that variable's [frame slot](conventions.md#functions), or a global's
label.

```lisp
(defun main ()
  (let ((x 9))
    (asm (:op :const (reg a) 3)
         (:op :set (:var x) (reg a)))
    x))                                   ; 3
```

## Backend requirements

The backend's first `:return` register is the accumulator, and the first
`:scratch` or `:caller-saved` register that is neither it nor the frame pointer
holds a right operand. The rest of `:scratch`, `:caller-saved` and
`:callee-saved`, less those two and the frame pointer, are a pool the compiler
draws from to hold a left operand while a non-leaf right operand computes,
instead of the stack.[^codegen] This needs every language operation to write
only its destination register: nothing else may change while one of these
holds a value across other code. It also needs `(registers :operand KIND)`
and `(frame :slot KIND)`, and defines these [operations](backends.md#language-operations)
for the forms a program uses. A missing one is a compile error naming the form.

| Operation | Does |
| --- | --- |
| `:const (r v)` | `r` = integer or label. |
| `:get (r slot)` `:set (slot r)` | Reads and writes a frame slot. |
| `:peek (d a)` `:poke (a s)` | A whole word at the address in a register (#368). These take register names, so a template can put one in a bracket operand. |
| `:peek-byte (d a)` `:poke-byte (a s)` | As `:peek`/`:poke`, a byte; needed only by `peek-byte`/`poke-byte` (#366). |
| `:jump (target)` `:branch-zero (r target)` | Jump; jump when `r` is `0`. |
| `:halt ()` | Stops the machine. |
| `:add :sub :mul :div :mod :and :or :xor :shl :shr (d s)` | `d` = `d` op `s`. |
| `:eq :ne :lt :gt :le :ge (d s)` | `d` = `1` or `0`. |
| `:add-imm (d v)` `:add-slot (d slot)`, and the same for every operation above | Optional: `d` = `d` op an integer or label, or op a frame slot. |

An operator whose right operand is a constant, array or `(function F)` uses
the `-imm` variant, and one whose right operand is a parameter or `let`
variable uses the `-slot` variant, when the backend defines it. Any other
operand, or a backend without the variant, loads the operand into a register
first.[^variants]

```lisp
(ops (:add (d s) (add d s))
     (:add-imm (d v) (addri d (imm v)))
     (:add-slot (d slot) (addrs d slot)))
;; (+ x 1)  ->  :get a x, :add-imm a 1
;; (+ x y)  ->  :get a x, :add-slot a y
```

It also defines the operations [call lowering](conventions.md#backend-operations)
uses: `:push :pop :move :alloc :free :call :return`. `:push` and `:move` accept a
frame slot as a source, which is how a call passes its arguments. `:call` needs
an [operand-kind clause](backends.md#operand-kind-clauses) for a register
target to compile `funcall` on a computed value (#365); one clause for the
usual label call and another for a register are typical.

[`callfoo-lang-abi`](../examples/cli/callfoo.lisp) is a complete example;
[`callfoo-lang-fp-abi`](../examples/cli/callfoo-fp.lisp) uses a frame pointer;
[`widefoo-lang-abi`](../examples/cli/widefoo.lisp) has registers wider than
its cells (#368).

## Names

A function `add-one` is the label `fnaddz2dzone`; a global is `gv...`, an array
`ar...`, a string `st...`, and control flow uses `lbl1`, `lbl2`. Every name is
alphanumeric, so it cannot be a register alias or mnemonic. Two names that make
the same label are a compile error.

## Errors

`program-compile-error` carries the message, the form and the function, and
(from `read-source`/`read-source-from-string`) reports `FILE:LINE:COLUMN` with
the source line and a caret, as an assembly error does:

```
fact.lsp:2:7: unknown variable y (in (+ x y)) (function helper)
2 |   (+ x y))
  |       ^
```

`compile-program` on plain forms has no position to report. A source file is
read without evaluation, as [items files](items.md#lasm-files) are, so `'`,
`#` syntax and unknown packages are errors.

## Functions

| Function | Does |
| --- | --- |
| `(compile-program forms &key backend)` | Returns the items. |
| `(read-source path)` `(read-source-from-string text)` | Returns an `items-program` whose items are the source forms. |
| `(compile-source program &key backend)` | Returns an `items-program` of the compiled items; `backend` overrides the program's. |
| `(compile-source-file path &key backend)` | Reads and compiles. |
| `(assemble-source-file path &key backend machine lexer origin memory)` | Compiles and assembles as `assemble-items-file` does. |
| `(write-items-program program stream)` | Writes a `.lasm` file `read-items` reads back. |

The [command line](cli.md#source-programs) takes `.lsp` files.

## Limitations

| Limitation | Ticket |
| --- | --- |
| A register `%CC-TAKE` picks from the callee-saved pool for a single call site costs a save/restore even when the stack would have been as cheap. | [#376](https://todo.sr.ht/~takeiteasy/lasm/376) |
| An `(asm ...)` in an operand always falls back to the stack: asm has no declared clobber list, so any register could be unsafe. | [#377](https://todo.sr.ht/~takeiteasy/lasm/377) |
| A constant or variable *left* operand never uses an immediate or slot variant, even for a commutative operator. | [#388](https://todo.sr.ht/~takeiteasy/lasm/388) |
| `if`/`while`/`and`/`or` compare into the accumulator, then branch on it, rather than branching on the comparison directly. | [#375](https://todo.sr.ht/~takeiteasy/lasm/375) |
| `funcall`'s arity is checked only when the target is a literal `(function F)`; through a variable, a wrong argument count is not caught. | [#378](https://todo.sr.ht/~takeiteasy/lasm/378) |
| `defstring` is one character a word; no packed (several-per-word) strings. | [#379](https://todo.sr.ht/~takeiteasy/lasm/379) |
| A macro's own `let` names are hygienic, but a name it refers to free can still be captured by a caller's `let`. | [#382](https://todo.sr.ht/~takeiteasy/lasm/382) |
| A quasiquote template can't nest another quasiquote inside it. | [#383](https://todo.sr.ht/~takeiteasy/lasm/383) |
| The compile-time evaluator's operators are a minimal set: no strings, `apply`, `mapcar`, `and`/`or`/`cond`. | [#384](https://todo.sr.ht/~takeiteasy/lasm/384) |

[^codegen]: A binary operator's operands go into the accumulator and the
  temporary register in whichever order avoids the stack (#364): a leaf (an
  integer, or a parameter, `let` variable, global or constant) loads directly
  with `:const`/`:get`/`:peek`, and when the right operand is not a leaf but
  the left is an integer, a constant, or a local the right cannot change, the
  right is compiled first and the left loads afterwards. Otherwise the left
  operand moves into a register from the pool while the right computes into
  the accumulator, then moves back (#373): a `:scratch`/`:caller-saved`
  register when the right operand has no call, since a call is the only
  thing it could do that such a register does not survive; a
  `:callee-saved` one, added to the function's `:save`, when it calls a
  function; the stack, as before #373, when the pool has none free or the
  right operand reaches an `(asm ...)`, which could target any register
  directly. A call evaluates each argument into its own frame slot, then
  passes those slots. A register argument is copied to a slot on entry, so
  the body never reads an argument register another call clobbers.

[^variants]: `not` compares with `:eq-imm 0`. A global is not a slot, and reads
  through `:peek`, so it loads first. The variants take the same operand a
  load would: `:add-slot`'s `slot` is the operand `:get` takes, and
  `:add-imm`'s `v` is the one `:const` takes. The example backend
  [`callfoo-lang-abi`](../examples/cli/callfoo.lisp) defines them for `:add`,
  `:sub`, `:eq` and `:lt` only; `:mul` and the rest load first.

[^macros]: A fresh `let` name or `gensym` is an uninterned symbol whose
  printed name has a space, which no source symbol can spell. `nil` and `t`
  are self-evaluating, like Common Lisp's; every other symbol not bound by
  a `let` or a parameter is unbound. A function body is expanded once every
  top-level form (including every `defmacro`/`defun-for-syntax`, wherever it
  sits in the file) is registered, so a function can use a macro defined
  later in the file; a macro used at top level needs its own `defmacro`
  earlier in the file, same as a function needs a global defined before it's
  read. A program is limited to 10000 total macro expansions, which also
  catches a macro that expands into a call to itself, and 1,000,000
  compile-time evaluation steps across every macro and `defun-for-syntax`
  call, which catches a helper's runaway recursion; either raises a
  positioned error, as does recursing deep enough to exhaust the compiler's
  own stack. An error a macro's own code causes -- an unbound name, a wrong
  argument to `car` -- reports where that code is written, in the `defmacro`;
  one about a quasiquote's own list structure, which has no position of its
  own, reports the call instead.

[^return]: Pops any temporaries the compiler has pushed for an enclosing
  operator or `poke` since the function's entry, so the stack is back at its
  entry depth, then emits the function's ordinary exit. A register the
  allocator holds a value in needs nothing here: the function's ordinary
  exit restores every `:save` register regardless of how it is reached.
