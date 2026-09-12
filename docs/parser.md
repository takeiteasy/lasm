# Statement grammar & expression parser

A fixed line/statement grammar, shared across every target machine, plus a
precedence-climbing (Pratt) expression parser reusable at every operand
`expr` hole an addressing mode declares.

```lisp
(parse "loop: lda #$10, x
        sta $2000
        bne loop")
```

## Scope

This stops at the AST. Label references stay symbolic (`expr-label`) and
operand token runs are handed back unparsed as raw tokens rather than
expressions. `parse-expression` is the piece later stages call directly:
every addressing mode declared with `defmode` matches against it via
`match-operand-mode`/`try-match-operand-mode` — see [Addressing
modes](modes.md). Resolving a label reference against a symbol table is the
[Assembler](assembler.md)'s job (`eval-expr`); `eval-expr-constant` folds
constant expressions with no label support at all.

## Statement grammar

```
line      := [label-def] [mnemonic [operands]]
label-def := identifier label-suffix
operands  := operand ("," operand)*
```

One `statement` per source line; blank and comment-only lines produce none.
A label with no mnemonic is a legal statement (a label on its own line). A
comma inside a parenthesized group does not split operands.

```lisp
(defstruct statement label mnemonic operands operand-tokens line)
(defstruct operand tokens)   ; raw token run — a simple-vector
```

`operands` is the comma-split list above — a general statement-grammar
product (a future comma-separated directive, e.g. `.byte 1, 2, 3`, is the
likely consumer), but not what addressing-mode matching uses, even for a
multi-operand instruction ([Instructions, "Repeated `(operand ...)`
subclauses"](instructions.md)): those
reach their several operands through a multi-hole `defmode` pattern instead,
whose own literal commas (e.g. a two-register mode's `expr "," expr`) would
be unmatchable against one already-comma-split `operand` at a time.
`operand-tokens` is every token after the mnemonic, commas included,
uncommitted to any comma split: a mode's own pattern can include a literal
comma (e.g. `indexed-x`'s `expr "," "X"`, [Addressing modes](modes.md)), so
matching against `operands` instead would make such a mode unmatchable.
`match-operand-mode`/`try-match-operand-mode` take `operand-tokens`, not
`operands`.

`(parse string &key (lexer 'default))` tokenizes `string` with `lexer` and
returns a list of `statement`. Signals `lex-error` or `parse-failure`.

## Expression parser

`(parse-expression tokens &key (start 0) (end (length tokens)))` parses one
expression out of a token vector between `start` and `end`, returning
`(values ast next-index)` — so a caller matching an addressing-mode pattern
against an operand's token run can parse just its `expr` hole and continue
from where it left off (e.g. `parse-expression` stops cleanly at a `,` it
doesn't recognize as an operator).

### Precedence (lowest-binding first, all left-associative)

| Level | Operators |
|---|---|
| 1 | `\|` |
| 2 | `^` |
| 3 | `&` |
| 4 | `<<` `>>` |
| 5 | `+` `-` |
| 6 | `*` `/` |
| 7 | prefix `-` `+` `~` `<` `>` |
| 8 | primary: number, label, `*` (location counter), `( expr )` |

Prefix `<expr` / `>expr` are 6502-style low-/high-byte operators. **Known
future collision:** if a later milestone adds comparison operators, `<`/`>`
will need disambiguating from this prefix use — not a concern for M1, which
has no comparisons.

A bare `*` in primary position is the location-counter symbol (`expr-location`
below, #15) rather than multiplication: `%parse-primary` only reaches that
position where an operand is expected, so `lda *+2` (location counter plus 2)
and `lda 2*3` (still multiplication, since `2` is already a complete left
operand by the time `*` is seen) both parse as intended with no lexer
change.

### AST nodes

```lisp
(defstruct expr-number value)
(defstruct expr-label name localp)    ; NAME unresolved
(defstruct expr-location)             ; the "*" location-counter symbol (#15)
                                       ; -- no slots; it IS the value
(defstruct expr-unary op operand)     ; op: :neg :pos :lognot :lo :hi
(defstruct expr-binary op left right) ; op: :pipe :caret :amp :shl :shr
                                       ;     :plus :minus :star :slash
```

`expr-label-localp` is set from the lexer's `local-label-prefix` (`token-localp`,
[Lexer](lexer.md)) — true for an identifier starting with that prefix (`.` for
the default lexer), regardless of what characters follow. The parser only
tags the reference; scoping it to its nearest enclosing global label is the
[Assembler](assembler.md)'s job (#16) — `eval-expr` and the symbol table
themselves stay flat.

`expr-location` folds to an address, not a symbol-table lookup: `eval-expr`
takes it from a `:pc` argument the assembler passes at both layout and encode
time (its own address at that point), never from `symbols`. See
[Assembler](assembler.md#location-counter) for how each context (an
instruction, a `.byte`/`.word` element, `.org`'s operand) supplies it.

## Conditions

`parse-failure` (a subtype of `lasm-syntax-error`) is signalled on a
malformed token stream: an empty operand, a missing mnemonic, unbalanced
parentheses, a trailing binary operator, or any other token the grammar
doesn't expect.

## Follow-ups not covered here

- Directive grammar (`.org`, `.byte`/`.word`, `defdirective`).
