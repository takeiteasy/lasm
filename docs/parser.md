# Statement grammar and expression parser

`parse` turns source into statements. `parse-expression` builds expression
ASTs for [addressing modes](modes.md) and directives.

```lisp
(parse "loop: lda #$10
        bne loop")
```

## Statement grammar

```text
line      := [label] [mnemonic [operands]]
          |  [label] identifier "=" expression
label     := identifier label-suffix
operands  := operand ("," operand)*
```

Blank and comment-only lines produce no statement. `name = value` becomes
`.equ name, value`. A mnemonic suffix such as `lda.w` is stored separately
from the base mnemonic; a directive such as `.byte` stays whole. Commas
inside parentheses or brackets do not split operands.

```lisp
(parse string &key (lexer 'default) file)
;; => (values statement-list source-unit)
```

A `statement` keeps both comma-split `operands` and the complete
`operand-tokens` run. Mode matching uses the complete run because a mode
pattern can contain literal commas. `:file` names source in diagnostics.

## Expression parser

```lisp
(parse-expression tokens &key (start 0) (end (length tokens)))
;; => (values ast next-index)
```

The parser stops at the first token outside the expression, so a mode can
parse one `expr` hole and continue matching its remaining literals.

### Precedence

| Low to high | Operators |
| ---: | --- |
| 1 | `||` |
| 2 | `&&` |
| 3 | `<` `>` `<=` `>=` `==` `!=` |
| 4 | `|` |
| 5 | `^` |
| 6 | `&` |
| 7 | `<<` `>>` |
| 8 | `+` `-` |
| 9 | `*` `/` `%` |
| 10 | prefix `-` `+` `~` `!` `<` `>` |
| 11 | number, label, `*`, function call, parenthesized expression |

Binary operators are left-associative. Comparisons and logical operations
return `1` or `0`. `&&` and `||` short-circuit. Prefix `<` and `>` take
the low and next 8-bit byte; use `lowcell` and `highcell` for widths based
on the target memory cell.

### Function operators

The default lexer recognizes these `name(expr)` functions:

| Function | Value |
| --- | --- |
| `bank(label)` | Bank containing the label. |
| `bank(*)` | Bank of the current address. |
| `lowcell(expr)`, `highcell(expr)` | Low or next memory-cell-width bits. |
| `defined(name)` | `1` if the name is defined, else `0`. |
| `mem(addr)` | Available in debugger expressions. |

`bank` requires a label in a banked region. `defined` takes a name, not an
expression. See [Banked output](banked-output.md),
[Conditional assembly](conditionals.md), and
[Debugger](debugger.md#conditional-breakpoints).

### Location counter

A bare `*` where an expression starts means the current address; after a
value it means multiplication. A lexer can declare another location-counter
token. The assembler supplies the address when evaluating the AST; see
[Assembler](assembler.md#location-counter).[^syntax]

### AST nodes

| Node | Represents |
| --- | --- |
| `expr-number` | Numeric literal. |
| `expr-label` | Label reference, including a local-label flag. |
| `expr-location` | Location counter. |
| `expr-index` | `NAME[expr]`, a banked register cell or stack slot. Parsed only in debugger expressions.[^index] |
| `expr-unary` | Prefix operator or function. |
| `expr-binary` | Binary operator. |

Labels remain symbolic until assembly. `eval-expr-constant` rejects them;
see [Assembler](assembler.md#eval-expr).

## Conditions

`parse-failure` reports a malformed token stream with line and column.
`parse` can also signal `lex-error`. Source excerpts come from
[Diagnostics](diagnostics.md).

## Limitations

Directive grammar is covered by [Directives](directives.md); the parser
does not apply directive actions or resolve labels.

[^syntax]: With the default lexer, `%101` is a binary literal. Put space
  after the modulo operator before a right operand starting with `0` or
  `1`. For example, `13 % 5` evaluates to `3`.

[^index]: `NAME[expr]` parses only while `*indexed-names*` is true, which the
  debugger binds. Elsewhere `[` ends the expression so addressing-mode
  patterns such as `expr "[" reg "]"` keep matching. `eval-expr` reads the
  cell through `*index-reader*`, and signals when none is bound.
