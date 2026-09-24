# Lexer

`deflexer` defines the source syntax for comments, numbers, identifiers,
strings, labels, and operand suffixes. A `default` lexer is available.

```lisp
(deflexer sixtyfoo-syntax
  (comment-styles (";" :line) ("/*" "*/" :block))
  (number-formats (:hex "$" "0x") (:bin "%" "0b") (:dec :default))
  (label-suffix ":")
  (local-label-prefix ".")
  (string-delim "\"")
  (ident-chars :alnum "_.")
  (mode-suffix-separator ".")
  (hole-prefix-separator ":"))
```

Use `find-lexer-descriptor` to retrieve a lexer, or pass its name as
`:lexer` to `tokenize` and `parse`.

## Clauses

| Clause | Meaning |
| --- | --- |
| `comment-styles` | Line or block comments. |
| `number-formats` | Hex, binary, octal, decimal, or character literals; one default. |
| `label-suffix` | Marker after a label definition. |
| `local-label-prefix` | Prefix identifying local labels. |
| `string-delim` | Quoted strings with escapes. |
| `ident-chars` | `:alnum` plus extra allowed identifier characters. |
| `line-continuation` | Join physical lines without a newline token. |
| `mode-suffix-separator` | Separator for forced modes such as `lda.w`. |
| `hole-prefix-separator` | Separator for a forced operand such as `#w:5`. |
| `function-operators` | Named expression calls such as `bank(...)`. |
| `location-counter` | Extra spelling for the current address. |

A local-label prefix and mode suffix separator must also be allowed by
`ident-chars`. Hole prefixes accept the label suffix, `#`, or `=` as their
separator. `*` always remains a location counter. See
[Addressing modes](modes.md) and [Parser](parser.md).

## Tokens

```lisp
(tokenize string &key (lexer 'default))
```

Returns a vector ending with `:eof`.

| Slot | Contents |
| --- | --- |
| `type` | Identifier, number, string, punctuation, location counter, label suffix, newline, or EOF. |
| `value` | Parsed string, integer, or keyword. |
| `text` | Source spelling. |
| `line`, `column` | One-based source position. |
| `localp` | Whether an identifier starts with the local-label prefix. |

Newline tokens matter to the line-oriented [parser](parser.md). `%` starts
a binary literal only when immediately followed by `0` or `1`; write
`13 % 5` for modulo. Punctuation includes brackets and `#` so addressing
mode patterns can match them.

## Conditions

Malformed strings, comments, character literals, numeric prefixes, or
unknown characters signal `lex-error` with position and source context.
See [Diagnostics](diagnostics.md).
