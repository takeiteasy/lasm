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

The program needs `(defun main () ...)`. The compiled program starts with a stub
that stores the globals' initial values, calls `main` and halts; the runner sets
the stack pointer.

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
| `(F ARG...)` | A call to `defun` `F`. |
| `(asm ITEM...)` | The accumulator; see [inline items](#inline-items). |

A value is true unless it is `0`.

| Operators | Meaning |
| --- | --- |
| `+ - * / mod` | Arithmetic; `+` `-` `*` take two or more operands, `(- x)` negates. |
| `logand logior logxor shl shr` | Bitwise. |
| `= /= < > <= >=` | Two operands; `1` or `0`. |

Symbols are compared by name, ignoring case.

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
holds a right operand.[^codegen] It needs `(registers :operand KIND)` and
`(frame :slot KIND)`, and defines these [operations](backends.md#language-operations)
for the forms a program uses. A missing one is a compile error naming the form.

| Operation | Does |
| --- | --- |
| `:const (r v)` | `r` = integer or label. |
| `:get (r slot)` `:set (slot r)` | Reads and writes a frame slot. |
| `:peek (d a)` `:poke (a s)` | Memory at the address in a register. These take register names, so a template can put one in a bracket operand. |
| `:jump (target)` `:branch-zero (r target)` | Jump; jump when `r` is `0`. |
| `:halt ()` | Stops the machine. |
| `:add :sub :mul :div :mod :and :or :xor :shl :shr (d s)` | `d` = `d` op `s`. |
| `:eq :ne :lt :gt :le :ge (d s)` | `d` = `1` or `0`. |

It also defines the operations [call lowering](conventions.md#backend-operations)
uses: `:push :pop :move :alloc :free :call :return`. `:push` and `:move` accept a
frame slot as a source, which is how a call passes its arguments.

[`callfoo-lang-abi`](../examples/cli/callfoo.lisp) is a complete example;
[`callfoo-lang-fp-abi`](../examples/cli/callfoo-fp.lisp) uses a frame pointer.

## Names

A function `add-one` is the label `fnaddz2dzone`; a global is `gv...`, and control
flow uses `lbl1`, `lbl2`. Every name is alphanumeric, so it cannot be a register
alias or mnemonic. Two names that make the same label are a compile error.

## Errors

`program-compile-error` carries the message, the form and the function:
`unknown variable y (in (+ x y)) (function helper)`. A source file is read
without evaluation, as [items files](items.md#lasm-files) are, so `'`, `#` syntax and
unknown packages are errors.

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
| Errors have no line or column. | [#362](https://todo.sr.ht/~takeiteasy/lasm/362) |
| No early `return`. | [#363](https://todo.sr.ht/~takeiteasy/lasm/363) |
| Code is naive: intermediates go through the stack. | [#364](https://todo.sr.ht/~takeiteasy/lasm/364) |
| No function values or indirect calls. | [#365](https://todo.sr.ht/~takeiteasy/lasm/365) |
| No arrays, strings or sub-word access. | [#366](https://todo.sr.ht/~takeiteasy/lasm/366) |
| No macros. | [#367](https://todo.sr.ht/~takeiteasy/lasm/367) |
| A word is one cell. | [#368](https://todo.sr.ht/~takeiteasy/lasm/368) |

[^codegen]: A binary operator compiles its left operand, pushes the accumulator,
  compiles the right operand into the accumulator, moves it to the temporary
  register, pops the left operand back and applies the operation. A call
  evaluates each argument into its own frame slot, then passes those slots. A
  register argument is copied to a slot on entry, so the body never reads an
  argument register another call clobbers.
