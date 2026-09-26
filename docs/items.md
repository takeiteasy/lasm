# Items

An items program is a list of s-expressions instead of source text, so a
compiler can emit it without formatting assembly. `assemble-items` assembles
it against a [backend](backends.md) or a machine.

```lisp
(assemble-items '((:label main)
                  (push (imm 21))
                  (:op :call double)
                  (adds (sp) (imm 1))
                  (hlt)
                  (:label double)
                  (lds (reg a) (sp-idx 1))
                  (:op :add (reg a) (reg a))
                  (:op :return))
                :backend 'callfoo-abi)
```

The result is an `assembly`. Its source is the text the items [render](#rendering)
as, so listings, diagnostics and [snapshots](snapshots.md) work as for any source.

## Items

| Item | Is |
| --- | --- |
| `(:label NAME)` | A label. |
| `(:directive NAME EXPR...)` | A directive; the `.` is optional. A string argument is a string literal. |
| `(:op NAME ARG...)` | A backend [operation](backends.md#operations), expanded to instructions. |
| `(MNEMONIC OPERAND...)` | An instruction. |
| `(:function ...)` `(:call ...)` `(:return)` `(:push X)` `(:pop X)` `(:depth n)` | Lowered from the backend's [calling convention](conventions.md). |

## Operands

| Operand | Is |
| --- | --- |
| `(KIND value...)` | A backend [operand kind](backends.md#operand-kinds). |
| `(:mode MODE value...)` | A mode named directly; works without a backend. |
| An expression | A bare value, as in `call 6`. |
| `(:arg i)` `(:local i)` | A [frame slot](conventions.md#functions) of the enclosing function. |
| `(:force OPERAND)` | `OPERAND`, a `:mode` or kind, with its mode [forced](#forcing-a-variant). |

The values fill the mode's `expr` holes in order. A mode with a `one-of`
takes the chosen alternative's name before that alternative's values:
`(:mode ld-mode a ind b)`.

## Expressions

| Form | Is |
| --- | --- |
| `5`, `-5` | An integer. |
| `name`, `"Name"` | A label or register alias. |
| `(+ a b c)` | A binary operator chain: any of `+ - * / % << >> & "\|" ^ && "\|\|" == != < > <= >=`. |
| `(- a)` | A unary `- + ~ ! < >`. |
| `(bank label)` | A function operator of the [lexer](lexer.md). |

A binary or unary form renders in parentheses, so nesting never depends on
precedence.

## Names

A symbol names by its downcased name, so `main` and `MAIN` are the label `main`.
A symbol with mixed case, such as `|Main|`, and a string keep their case.

## Checked operands

`items-operand-mismatch` is signalled when:

- an operand written with a kind or `:mode` does not match its mode, as in
  `(reg 5)` where `reg` needs a register alias. This is checked before
  assembling, by `assemble-items` and `render-items`; or
- the assembler reads an operand as a different alternative than the one it
  names. `(:mode abs-mode a)` written as `[a]` is read as the register form.
  This is checked after assembling, against the alternative the assembler
  chose for that operand, including nested `one-of` selections and ties that
  declaration order decides.[^check]

Choosing between a mnemonic's variants by width is not a mismatch:
`(:mode absolute 5)` assembles as zero-page when the mnemonic has one. See
[Forcing a variant](#forcing-a-variant) to prevent it.

`items-malformed` is signalled for an item that is not well formed: an unknown
operation or kind, a wrong argument count, a keyword where a value belongs.
Both are `items-error`s; `items-error-detail` and `items-error-item` give the
message and the item.

## Forcing a variant

`(:force OPERAND)` renders the mnemonic with the mode's
[suffix](modes.md#forcing-a-mode-with-a-mnemonic-suffix), so the assembler uses
exactly that mode.

```lisp
(lda (:mode absolute 5))          ; lda 5    zero-page, by width
(lda (:force (:mode absolute 5))) ; lda.w 5  absolute
```

`items-malformed` is signalled when the `(:force)` is not the instruction's
only operand, its mode has no `:suffix`, the mode is not one of the mnemonic's
variants (a `one-of` alternative, for example), or the lexer has no mode suffix
separator. A forced value that does not fit the mode wraps as in source.[^wrap]

## Sizing

`(items-size items &key backend machine lexer origin memory assume)` returns the
cells `items` occupy, from their first cell to the end of the last, without
encoding. A label the items never define is allowed.

```lisp
(items-size '((br elsewhere) (nop)) :machine 'm)                    ; => 4
(items-size '((br elsewhere) (nop)) :machine 'm :assume :narrowest) ; => 3
```

| `:assume` | An operand naming a label the items do not define is sized at |
| --- | --- |
| `:widest` (default) | Its widest variant: a fit is safe whatever the label's value. |
| `:narrowest` | Its narrowest variant. |

A label the items define is sized as `assemble-items` sizes it, so a branch that
reaches its target stays short. `.res` space and `.org` gaps count. `:origin`
and `:memory` are `assemble-items`'s.

## Rendering

`(render-items items :backend b)` returns the source text. Assembling it gives
the same cells as `assemble-items`.

```
main:
push # 21
call double
adds sp, # 1
```

## `.lasm` files

A `.lasm` file holds one `(:program (OPTION...) ITEM...)` form.

```lisp
(:program (:backend callfoo-abi)
  (:label main)
  (push (imm 21))
  (:op :call double)
  (hlt))
```

| Option | Value |
| --- | --- |
| `:backend` | Backend name. |
| `:machine` | Machine name, when there is no backend. |
| `:origin` | Start address. |
| `:memory` | Memory element. |
| `:lexer` | Lexer name. |

The file is untrusted. It is read with the same restricted reader as
[snapshots](snapshots.md#reading): nothing is evaluated, symbols are never
interned, `#` syntax and quoting are rejected, and nesting and numbers are
bounded. Anything else signals `items-malformed`.

| Function | Does |
| --- | --- |
| `(read-items path)` | Returns an `items-program`: `items-program-items` `-backend` `-machine` `-origin` `-memory` `-lexer`. |
| `(read-items-from-string text)` | The same for a string. |
| `(assemble-items-file path &key backend machine lexer origin memory)` | Reads and assembles; a key overrides the file's option. `.include` resolves beside the file. |
| `(assemble-items items &key backend machine lexer origin memory file)` | Assembles a list of items. |
| `(items-size items &key backend machine lexer origin memory assume)` | Returns the [size](#sizing) in cells. |
| `(render-items items &key backend machine lexer)` | Returns the source text. |

The [command line](cli.md#items-programs) assembles `.lasm` files.

## Limitations

| Limitation | Ticket |
| --- | --- |
| A forced mode wraps a value that does not fit it, silently. | [#334](https://todo.sr.ht/~takeiteasy/lasm/334) |
| There is no language above items. | [#319](https://todo.sr.ht/~takeiteasy/lasm/319) |

[^check]: The assembler records each `one-of` pick as a token span, exposed as
  [`listing-line-choices`](listing.md#chosen-alternatives). A whole operand
  also loses to a variant of the mnemonic that matches its syntax more
  specifically. Instructions from macros, `.rept` and `.include` are not
  checked.

[^wrap]: `(:force (:mode zero-page 300))` assembles the byte 44.
