# Lexer

`deflexer` declares a *parameterized* surface syntax: comment styles,
number-literal prefixes, label suffix, local-label prefix, string delimiter,
identifier character class, and line continuation. Different fantasy CPUs
can each declare their own dialect rather than sharing one hard-coded
assembly syntax.

```lisp
(deflexer sixtyfoo-syntax
  (comment-styles (";" :line) ("/*" "*/" :block))
  (number-formats (:hex "$" "0x") (:bin "%" "0b") (:dec :default) (:char "'"))
  (label-suffix ":")
  (local-label-prefix ".")
  (string-delim "\"")
  (ident-chars :alnum "_.")
  (line-continuation "\\"))
```

`deflexer` registers a `lexer-descriptor` under `NAME`, retrievable with
`find-lexer-descriptor` and usable as the `:lexer` argument to `tokenize`/
`parse`. A ready-to-use `default` lexer (the clauses shown above, minus the
block comment style) is defined in `lexer.lisp` so callers need not define
their own for a conventional dialect.

## Clauses

- `(comment-styles (start [end] kind)...)` — `kind` is `:line` (runs to end
  of line) or `:block` (runs to `end`; unterminated signals `lex-error`).
- `(number-formats (name prefix...)... (name :default))` — `name` is one of
  `:hex` `:bin` `:oct` `:dec` (mapped internally to radix 16/2/8/10) or
  `:char`. Exactly one format should be marked `:default`, matched by bare
  digits with no prefix. `:char` reads a single character literal following
  its prefix (e.g. `'A` → 65) — there is no closing delimiter.
- `(label-suffix string)` — e.g. `":"`. A `nil`/omitted clause disables
  label definitions.
- `(local-label-prefix string)` — e.g. `"."`. It only matters if it's also
  listed in `ident-chars`, so that `.loop` lexes as one identifier rather than
  an error. Every `:identifier` token starting with this prefix is flagged
  `localp` (below); the [Assembler](assembler.md#local-label-scoping-16)
  scopes such a name to its nearest preceding non-local label (#16).
- `(string-delim string)` — enables string literals, with `\n`, `\t`, and
  `\<char>` (literal `<char>`) escapes.
- `(ident-chars :alnum extra-chars-string)` — `:alnum` is currently the only
  supported base class; `extra-chars-string` lists additional allowed
  characters (e.g. `"_."`). An identifier's first character must satisfy
  this too (so `_foo` and `.loop` are legal identifiers when `"_."` is
  listed).
- `(line-continuation string)` — e.g. `"\\"`. A continuation sequence at
  end of line, followed by an optional newline, is consumed without
  producing a `:newline` token, joining the next line onto the current one.

## Tokens

`(tokenize string &key (lexer 'default))` returns a `simple-vector` of
`token` structs, terminated by one `:eof` token. Each token has:

| Slot | Meaning |
|---|---|
| `type` | `:identifier` `:number` `:string` `:punctuation` `:label-suffix` `:newline` `:eof` |
| `value` | parsed value — string (identifier/string), integer (number), keyword (punctuation/label-suffix) |
| `text` | verbatim source text |
| `line`, `column` | 1-based source position |
| `localp` | `:identifier` only — T if `text` starts with `local-label-prefix` (#16) |

`:newline` tokens are significant — the grammar in
[Statement grammar & expression parser](parser.md) is line-oriented.

Punctuation tokens carry a keyword `value`: `:plus :minus :star :slash :amp
:pipe :caret :tilde :shl :shr :lparen :rparen :comma :lt :gt :hash`.
Two-character operators (`<<`, `>>`) win maximal munch over their
single-character prefixes. `#` has no meaning to the lexer itself — it is
recognized so addressing-mode literal patterns (e.g. 6502-style immediate
`#expr`) have a token to match against once `defmode` exists (M2).

## Conditions

`lex-error` (a subtype of `lasm-syntax-error`, itself a subtype of
`lasm-error`) is signalled on malformed input — unterminated string/block
comment/character literal, a numeric prefix with no digits following, or an
unrecognized character. It carries `lasm-syntax-error-message`,
`-line`, and `-column`.

## Known limitations / deferred to later tickets

- No `%` modulo operator is defined in the expression grammar (see
  [Parser](parser.md)), so `%` is reserved as a binary-literal prefix only;
  this sidesteps `%1010` vs. `a % b` ambiguity without a lexer special case.
- The lexer does not distinguish labels from directives — `.org` and
  `.loop` both lex as one `:identifier` token; the parser decides based on
  what follows (label-suffix or not). Directive grammar itself is M2.
