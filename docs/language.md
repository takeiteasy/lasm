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

See [`tests/fixtures/cli/fact.lsp`](../tests/fixtures/cli/fact.lsp), and the
[CHIP-8 example](examples.md#chip-8) for a larger program.

## Program

A `.lsp` file holds top-level forms, in any order. A leading
`(:program (OPTION...))` takes the options of a [`.lasm` file](items.md#lasm-files),
[`:optimize`](#optimizing) and [`:frames`](static-frames.md#choosing).

| Form | Is |
| --- | --- |
| `(defun NAME (PARAM...) BODY...)` | A function returning its last form's value. |
| `(defvar NAME [INTEGER])` | A one-word global, `0` unless given. |
| `(defconstant NAME INTEGER)` | A compile-time integer. |
| `(defarray NAME SIZE)` | `SIZE` words reserved, uninitialised. |
| `(defarray NAME (VALUE...))` | Words initialised to `VALUE...`, each an integer, `(function F)`, or another `defconstant`/`defarray`/`defstring` name. |
| `(defstring NAME "TEXT")` | `TEXT`, one character a word, `0`-terminated. |
| `(defstring NAME "TEXT" :packed)` | `TEXT`, 8-bit characters packed into cells, `0`-terminated; see [below](#arrays-strings-and-byte-access). |
| `(defmacro NAME (PARAM... [&rest R]) BODY...)` | A compile-time macro; see [below](#macros). |
| `(defun-for-syntax NAME (PARAM... [&rest R]) BODY...)` | A compile-time helper function, callable from a macro's `BODY`. |

Comments are `;` to the end of the line and `#| ... |#` blocks, which nest.

The program needs `(defun main () ...)`. The compiled program starts with a stub
that stores the globals' initial values, calls `main` and halts; the runner sets
the stack pointer. `defarray` and `defstring` data is part of the image, not the
stub, so it starts at those values every load.

A `defarray`/`defstring` name is its address, always, unlike a `defvar`'s name,
which is its value (peeked/poked through); it cannot be `set`. `(aref A I)`/
`(aset A I V)` index by word, `I` from `0`.

## Words wider than a cell

A word is two [register-pair](register-pairs.md) halves wide when the backend
declares pairs. Otherwise it is one cell unless the backend's machine gives its stack pointer a
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
| `5`, `-5`, `#xFF`, `#b101`, `#o17` | The integer, in decimal, hexadecimal, binary or octal. |
| `(set NAME E)` | `E`, stored in a variable or global. |
| `(let ((V E)...) BODY...)` | The last body form. Each `E` sees the earlier `V`. |
| `(if C A [B])` | `A` or `B`; `0` with no `B`. |
| `(while C BODY...)` | `0`. |
| `(progn E...)` | The last `E`, or `0`. |
| `(and E...)` `(or E...)` | The deciding value; stops at the first false, or true, one. |
| `(not E)` | `1` if `E` is `0`, else `0`. |
| `(peek ADDR)` `(poke ADDR V)` | The word at `ADDR`; `V`, stored there. |
| `(peek-byte ADDR)` `(poke-byte ADDR V)` | The byte at `ADDR`; `V`, stored there; see [below](#arrays-strings-and-byte-access). |
| `(aref A I)` `(aset A I V)` | The word at index `I` of array/string `A`; `V`, stored there. |
| `(aref-byte S I)` `(aset-byte S I V)` | Character `I` of packed string `S`; `V`, stored there; see [below](#arrays-strings-and-byte-access). |
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
`(F ARG...)` does, arity-checked at compile time. Any other `E` computes its
target at run time, and its argument count must be one of the function values
that can reach it: those put in a `let` variable, global or parameter, in a
`defarray` element, or yielded by an `if`, `progn` or `let`. Otherwise it is a
compile error. Where that is not known, one some `(function F)` in the program
takes will do. An integer or `defconstant` target is a raw address and is not
checked.[^funcall-arity]

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

`(defstring NAME "TEXT" :packed)` stores as many 8-bit characters in a cell as
fit, `0`-terminated, in the memory's [`:endian`](machine-model.md#cell-width-and-the-assembler)
order: the first character in the low bits of a little-endian cell, the high
bits of a big-endian one. A character above `255` is a compile error. A compiled
`.lasm` file holds it as text, a [`.packz`](directives.md#pack--packz) directive.
`(aref-byte S I)`/`(aset-byte S I V)` read and write character `I`; on a machine
with 16-bit cells, `"abc"` is two cells and `(aref-byte S 1)` is `98`.

```lisp
(defstring greeting "hello" :packed)

(defun length (s)
  (let ((i 0))
    (while (aref-byte s i) (set i (+ i 1)))
    i))
```

`aref-byte` byte-addresses `S + I` through `:peek-byte`/`:poke-byte`, where
`S`'s byte address is the backend's optional `:byte-address` on `S`, or else
`S` times the characters a cell holds (`S` itself for 8-bit cells). A machine
whose cell holds one character and whose word is one cell needs neither: there
`aref-byte` is `aref`. A packed string's `aref` returns its raw cells.

`(aref-byte A I)`/`(aset-byte A I V)` also take a `defarray`, numbering the bytes of its
cells the same way.

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

Each expansion marks every name it writes, in a template or a `quote`, so the
macro's own names and the caller's never meet. A template's `let` or `defun`
parameter can't capture the caller's variable of the same name, and a caller's
`let` can't capture a free name the macro uses: a free name must be a global,
constant, array or string, or it's an `unknown variable` error.
`(unmark FORM)` returns FORM with every name unmarked, which reaches the
caller's own variable on purpose.[^hygiene] A name from `,FORM` (including a
`,NAME` binding name) is the caller's own.

```lisp
(defvar counter 10)
(defmacro bump () `(set counter (+ counter 1)))       ; the global, always
(defmacro bump-mine ()                                 ; the caller's
  `(set ,(unmark 'counter) (+ ,(unmark 'counter) 1)))

(defun main () (let ((counter 5)) (bump) (bump-mine) counter))   ; 6
```

A macro call also works at top level, where it can expand to `defun`,
`defvar`, `defconstant`, `defmacro`, or a `(progn DEF...)` of them. A
template can contain another quasiquote, so a macro can define a macro: an
inner `,X` or `,@X` stays literal, and `,,X` or `,@',X` reaches the outer
template, as in Common Lisp.

```lisp
(defmacro defadder (name n) `(defmacro ,name (x) `(+ ,x ,',n)))
(defadder add5 5)
(defun main () (add5 10))                             ; 15
```

A symbol passed through to the inner template is marked by the inner `quote`,
so `,(unmark ',n)` is how the inner macro reaches the caller's variable `n`.

Inside `(asm ...)`, `,FORM` and `,@FORM` substitute like anywhere else in the
template, so a constant, a register name or a `(:var NAME)` can all be
computed; everything written literally, including a register operand such as
`(reg a)`, is left alone.

### The compile-time evaluator

`BODY`'s forms run over plain data: integers, strings, symbols, lists and
functions. `()` is false; anything else, including `t`, is true.

| Form | Does |
| --- | --- |
| `(quote FORM)`, `'FORM` | FORM itself, each name in it marked; a `nil` in it is `()`. |
| `` (quasiquote FORM) ``, `` `FORM `` | FORM as a template; see above. |
| `(if TEST THEN [ELSE])` | THEN when TEST is true, else ELSE (`()` if omitted). |
| `(let ((NAME VALUE)...) BODY...)`, `let*` | As the language's own `let`, but at compile time. |
| `(progn FORM...)` | Each FORM in order; the last one's value. |
| `and`, `or`, `not` | Short-circuit, returning the deciding value; `not` gives `t` or `()`. |
| `(cond (TEST BODY...)...)` | The first clause whose TEST is true; a clause without BODY gives TEST's value. |
| `(when TEST BODY...)`, `unless` | BODY when TEST is true (`when`) or false (`unless`), else `()`. |
| `(lambda (PARAM... [&rest R]) BODY...)` | A function that sees the variables around it. |
| `(function NAME)`, `'NAME` | A builtin or `defun-for-syntax` helper as a value. |
| `funcall`, `apply`, `mapcar` | `(funcall F ARG...)`, `(apply F ARG... LIST)`, `(mapcar F LIST...)`, F a lambda or a name. |
| `car`, `cdr`, `cons`, `list`, `append`, `length` | List operations; `car`/`cdr` of `()` is `()`. |
| `reverse`, `nth`, `nthcdr`, `second`, `third`, `last`, `member`, `assoc` | More list operations; `member` and `assoc` compare with `equal`. |
| `null`, `consp`, `symbolp`, `integerp`, `stringp` | Type predicates. |
| `eq`, `equal` | `eq` compares a pair of symbols by name, never by identity. |
| `concat`, `string=`, `symbol-name`, `number-to-string` | String operations; `concat` takes strings only. |
| `(intern STRING)` | The symbol named STRING, marked like any name the macro writes. |
| `(unmark FORM)` | FORM with every name unmarked, so a name reaches the caller's variable. |
| `+`, `-`, `*`, `/`, `mod`, `min`, `max`, `logand`, `logior`, `ash` | Integer arithmetic; `/` truncates, and `ash` shifts by at most 64. |
| `=`, `/=`, `<`, `>`, `<=`, `>=` | Integer comparison, each over a run of arguments. |
| `gensym` | A symbol no source text can spell, for a template to bind without capturing anything (`(gensym PREFIX)` names it, for reading a macro's own compile-time errors). |
| `error` | Signals a compile error naming the macro's call. |

A macro body calls these operators and its helpers, never macros, so a macro
can share an operator's name: `(defmacro unless ...)` is a program's `unless`,
while `unless` in a body is the operator. A `defun-for-syntax` can't.

```lisp
(defmacro scaled (k &rest xs) `(+ ,@(mapcar (lambda (x) `(* ,k ,x)) xs)))
(defun main () (scaled 10 1 2 3))                     ; 60
```

`(defun-for-syntax NAME (PARAM... [&rest R]) BODY...)` is a compile-time
helper: unlike a macro's, its own arguments are evaluated before the call, so
it can recurse over a value a macro has already computed.

```lisp
(defun-for-syntax sum-of (xs) (if (null xs) 0 (+ (car xs) (sum-of (cdr xs)))))
(defmacro total (&rest xs) (sum-of xs))

(defun main () (total 1 2 3))          ; 6, added up at compile time
```

See [`tests/fixtures/cli/macros.lsp`](../tests/fixtures/cli/macros.lsp).[^macros]

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

`(asm (:clobbers REG...) ITEM...)` declares the registers the block writes; the
declaration is not emitted. The [register allocator](#backend-requirements)
holds a value across an `asm` in any other register, and the function saves
and restores a declared `:callee-saved` register for its caller. Without a declaration,
`asm` may write any register. The list is a promise: an `asm` that calls a
routine lists every register the routine changes.

```lisp
(asm (:clobbers b)
     (:op :const (reg b) 3))                  ; a value held in c survives
```

## Optimizing

`:optimize :size` (the default) emits the fewest instructions. `:optimize
:speed` also holds an operand across calls in a `:callee-saved` register that
call sites share when they run more than once a call, which costs two
instructions in the function's prologue and epilogue and saves memory accesses
at run time.[^speed] A program names it in its header; the `optimize` key or
`--optimize` overrides it.

```lisp
(:program (:backend callfoo-lang-abi :optimize speed))
```

| `:optimize` | Holds an operand across calls in |
| --- | --- |
| `:size` | a saved register inside a `while`, else the stack |
| `:speed` | a saved register inside a `while`, or shared by sites that together run more than once a call, else the stack |

```lisp
(defun main ()
  (+ (f 1) (f 2))                    ; :speed holds (f 1) in c
  (+ (f 3) (f 4)))                   ; and so does this one

(defun pick (x)
  (if x (+ (f 1) (f 2)) (+ (f 3) (f 4))))   ; one site runs per call: stack
```

```sh
lasm compile prog.lsp -m callfoo.lisp --backend callfoo-lang-abi --optimize speed
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
With [static frames](static-frames.md) it needs no `:get`, `:set`, `:alloc`,
`:free`, `:push` or `:pop`. With [register pairs](register-pairs.md) every one of
those registers is a pair.

| Operation | Does |
| --- | --- |
| `:const (r v)` | `r` = integer or label. |
| `:get (r slot)` `:set (slot r)` | Reads and writes a frame slot. |
| `:peek (d a)` `:poke (a s)` | A whole word at the address in a register (#368). These take register names, so a template can put one in a bracket operand. |
| `:peek-label (d label)` `:poke-label (label s)` | Optional: a word at a label, for a global or a [static frame](static-frames.md) slot. Without them, `:const` then `:peek`/`:poke`. |
| `:peek-byte (d a)` `:poke-byte (a s)` | As `:peek`/`:poke`, a byte; needed only by `peek-byte`/`poke-byte` and, on most machines, `aref-byte`/`aset-byte` (#366, #379). |
| `:byte-address (d)` | Optional: `d`, a cell address, becomes the byte address `:peek-byte` takes. Default: times the characters a cell holds. |
| `:jump (target)` `:branch-zero (r target)` | Jump; jump when `r` is `0`. |
| `:halt ()` | Stops the machine. |
| `:add :sub :mul :div :mod :and :or :xor :shl :shr (d s)` | `d` = `d` op `s`. |
| `:eq :ne :lt :gt :le :ge (d s)` | `d` = `1` or `0`. |
| `:add-imm (d v)` `:add-slot (d slot)`, and the same for every operation above | Optional: `d` = `d` op an integer or label, or op a frame slot. |
| `:branch-eq :branch-ne :branch-lt :branch-gt :branch-le :branch-ge (a b target)` | Optional: jump to `target` when `a` compares to `b` as the matching `:eq`...`:ge` does. |
| `:branch-lt-imm (a v target)` `:branch-lt-slot (a slot target)`, and the same for every branch above | Optional: as above, with an integer or label, or a frame slot, for `b`. |

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

A constant or variable *left* operand swaps to the right when that lets a
variant apply and the right operand has none: `(+ 1 (f y))` becomes
`(+ (f y) 1)`, and `(> 5 (f y))` becomes `(< (f y) 5)`. Only `+ * logand logior
logxor` and the comparisons swap. The swap happens only when evaluating the
right operand first cannot change the left one.[^swap]

A condition that is a comparison, or an `and`, `or` or `not` of conditions,
jumps on the backend's `:branch-` operation, when it defines the one it needs,
instead of computing `1` or `0` and testing that. A false condition jumps to
the `else` or the end of the loop, so `<` uses `:branch-ge`.[^branches] A
`:branch-cmp` must compare exactly as its `:cmp` does, signedness included.
A value with no comparison of its own that must jump when true uses
`:branch-ne-imm (a 0 target)` when the backend defines it.

A value-context `and` or `or` jumps on its comparisons, and on its nested
`not`, `and` and `or` operands, to a shared landing that loads `0` or `1`, when
that is shorter than computing each one.[^fusing]

```lisp
(ops (:branch-ge-imm (a v target) (bger a (imm v) target)))
;; (if (< x 5) A B)  ->  :get a x, :branch-ge-imm a 5 else, A, :jump end, else: B
```

It also defines the operations [call lowering](conventions.md#backend-operations)
uses: `:push :pop :move :alloc :free :call :return`. `:push` and `:move` accept a
frame slot as a source, which is how a call passes its arguments. `:call` needs
an [operand-kind clause](backends.md#operand-kind-clauses) for a register
target to compile `funcall` on a computed value (#365); one clause for the
usual label call and another for a register are typical.

[`callfoo-lang-abi`](../tests/fixtures/cli/callfoo.lisp) is a complete example;
[`callfoo-lang-fp-abi`](../tests/fixtures/cli/callfoo-fp.lisp) uses a frame pointer;
[`widefoo-lang-abi`](../tests/fixtures/cli/widefoo.lisp) has registers wider than
its cells (#368).

## Names

A function `add-one` is the label `fnaddz2dzone`; a global is `gv...`, an array
`ar...`, a string `st...`, a [static frame](static-frames.md) word `sf...x0`,
`sf...x1`, and control flow uses `lbl1`, `lbl2`. Every name is
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
read without evaluation, as [items files](items.md#lasm-files) are, so `#`
syntax other than `#x`/`#b`/`#o` integers and `#| ... |#` comments, and unknown
packages, are errors.

## Functions

| Function | Does |
| --- | --- |
| `(compile-program forms &key backend optimize frames)` | Returns the items. `optimize` is [`:size` or `:speed`](#optimizing); `frames` is [`:static` or `:stack`](static-frames.md#choosing). |
| `(read-source path)` `(read-source-from-string text)` | Returns an `items-program` whose items are the source forms. |
| `(compile-source program &key backend optimize frames)` | Returns an `items-program` of the compiled items; `backend`, `optimize` and `frames` override the program's. |
| `(compile-source-file path &key backend optimize frames)` | Reads and compiles. |
| `(assemble-source-file path &key backend machine lexer origin memory optimize frames)` | Compiles and assembles as `assemble-items-file` does. |
| `(write-items-program program stream)` | Writes a `.lasm` file `read-items` reads back. |

The [command line](cli.md#source-programs) takes `.lsp` files.

## Limitations

| Limitation | Ticket |
| --- | --- |
| `funcall` through a function's return value is checked only against every function value's arity. | [#403](https://todo.sr.ht/~takeiteasy/lasm/403) |
| `funcall` through a taken function's parameter, an escaped array's element or a computed target is checked only against every function value's arity. | [#404](https://todo.sr.ht/~takeiteasy/lasm/404) |

With several arities taken, a wrong one that another function has is not caught in these cases.

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
  function inside a `while` or the function already saves that register,
  since a first save costs a push and a pop that the stack would not (#376),
  or, with [`:optimize :speed`](#optimizing), when sites that together run more than once a call share it;
  the stack when the pool has none free. An `(asm ...)` in the right operand
  rules out the registers it declares in `:clobbers`, or every register when
  it declares none (#377).
  A call evaluates each argument into its own frame slot, then
  passes those slots. A register argument is copied to a slot on entry, so
  the body never reads an argument register another call clobbers.

[^funcall-arity]: The check runs once every function is compiled, so a function
  value taken after the call still counts. `(function F)` in a function body or
  a `defarray` adds `F`'s argument count to the program-wide set. A variable,
  parameter or array element holds the function values put in it, and whatever
  else flows in from another variable or an `if`, `progn` or `let` value; any
  other value, such as a computed one or an `if` with no `else`, makes it
  unknown, and a call through it uses the program-wide set.

  - A parameter holds what each direct call passes. A function taken with
    `(function F)`, or whose label an `(asm ...)` spells, has callers not seen,
    so its parameters are unknown.
  - An element of a `defarray` holds its initial value (`0` holds nothing) and
    what a constant-index `aset` puts in it; a computed index reads or writes
    every element. An array named other than as the base of `aref` or `aset`,
    even by a local of the same name, has unknown elements.
  - A global that starts non-zero, or a variable named by `(:var NAME)` in an
    `(asm ...)`, is unknown.

[^variants]: `not` compares with `:eq-imm 0` when its value is used. A global is not a slot, and reads
  through `:peek`, so it loads first. The variants take the same operand a
  load would: `:add-slot`'s `slot` is the operand `:get` takes, and
  `:add-imm`'s `v` is the one `:const` takes. The example backend
  [`callfoo-lang-abi`](../tests/fixtures/cli/callfoo.lisp) defines them for `:add`,
  `:sub`, `:eq` and `:lt` only; `:mul` and the rest load first.

[^swap]: Safe means the left operand is a constant, array, `(function F)` or
  a parameter or `let` variable the right operand neither sets nor reaches
  through an `(asm ...)` block. A global never swaps: a call or `poke` in the
  right operand could change it.

[^branches]: An `and`, `or` and `not` in a condition jump between their
  operands and produce no value. A comparison whose `:branch-cmp` the backend
  lacks, and any other condition, computes a value and uses `:branch-zero`.
  [`callfoo-lang-abi`](../tests/fixtures/cli/callfoo.lisp) defines all eighteen
  branch operations.

[^fusing]: The landing costs a `:jump` and a `:const`. Each operand before the
  last saves its estimated cost of computing a value and jumping on it, less
  the cost of jumping on it directly; the operands fuse when the savings total
  more than the landing costs. The estimate counts the loads of leaf operands
  and the variant each operation uses (#392). On `callfoo-lang-abi`, `(< x 3)`
  is `:get` `:lt-imm` `:branch-zero` against `:get` `:branch-ge-imm`, saving one
  instruction in an `and`, so it takes three operands before the last to fuse;
  `(>= x 3)` has no `:ge-imm`, loads `3` too, and saves two, so two do. A comparison with a
  `:cmp` variant but no `:branch-cmp` one saves nothing. In an `or` each saves
  one more without `:branch-ne-imm`, since a jump on a true value then costs
  two. `(not x)` saves one with `:branch-ne-imm` and nothing without it, and
  one more without `:eq-imm` and `:eq-slot`. An operand that jumps is one the
  estimate says saves something, and an `or` operand must also give only `0`
  or `1`, since the landing loads `1`: `(or (and a b) ...)` keeps computing
  `b`'s value.

[^speed]: Each function compiles twice. The first pass counts the call sites
  that would claim each `:callee-saved` register. A site counts 1, halved by
  each enclosing `if` arm and by each `and`/`or` operand after the first, and
  multiplied by 4 inside each `while`. The second pass lets a site outside a
  loop claim a register counted more than 1: each run saves a push and a pop, and saving the register
  costs one pair per call.

[^hygiene]: A mark is a number kept on a fresh uninterned copy of each name
  a quasiquote template writes; a local variable is looked up by name plus
  mark, and a function, global or constant by name alone. A quasiquote, a
  `quote` and `intern` mark names; a name from `,FORM` is unmarked. `unmark`
  gives each name the mark of the macro's own call: none when written in
  source, or the enclosing macro's when written in its template, so a macro
  called from another macro's template reaches that template's variables.
  A template inside a template gets its own mark in place of the outer one
  (marks do not stack), so a macro an outer macro defines reaches a variable
  the outer template binds only through `unmark`; a plain name there is an
  `unknown variable` error.

[^macros]: A `gensym` is an uninterned symbol whose
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
