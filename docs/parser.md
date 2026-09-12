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

`operands` is the comma-split list above — kept for a possible future
multi-operand instruction, but not what addressing-mode matching uses.
`operand-tokens` is every token after the mnemonic, commas included,
uncommitted to any comma split: a mode's own pattern can include a literal
comma (e.g. `indexed-x`'s `expr "," "X"`, [Addressing modes](modes.md)), so
matching against one comma-delimited `operand` at a time would make such a
mode unmatchable. `match-operand-mode`/`try-match-operand-mode` take
`operand-tokens`, not `operands`.

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
| 8 | primary: number, label, `( expr )` |

Prefix `<expr` / `>expr` are 6502-style low-/high-byte operators. **Known
future collision:** if a later milestone adds comparison operators, `<`/`>`
will need disambiguating from this prefix use — not a concern for M1, which
has no comparisons.

### AST nodes

```lisp
(defstruct expr-number value)
(defstruct expr-label name localp)    ; NAME unresolved
(defstruct expr-unary op operand)     ; op: :neg :pos :lognot :lo :hi
(defstruct expr-binary op left right) ; op: :pipe :caret :amp :shl :shr
                                       ;     :plus :minus :star :slash
```

`expr-label-localp` is a heuristic (the name's first character is not
alphabetic — true for the default lexer's `.`-prefixed local labels), not a
descriptor-aware scoping check. The [Assembler](assembler.md) does not act
on it either — every label, local or not, shares one flat symbol table;
scoping a local label to its enclosing global label is a separate ticket
(#16).

## Conditions

`parse-failure` (a subtype of `lasm-syntax-error`) is signalled on a
malformed token stream: an empty operand, a missing mnemonic, unbalanced
parentheses, a trailing binary operator, or any other token the grammar
doesn't expect.

## Follow-ups not covered here

- Directive grammar (`.org`, `.byte`/`.word`, `defdirective`).
- A location-counter symbol in expressions (`*` or `$` for "current PC") —
  needs a syntax decision, since both candidate spellings collide with
  existing tokens.
- Local-label scoping (binding a `.loop` reference to its enclosing global
  label) — #16; see [Assembler](assembler.md).
