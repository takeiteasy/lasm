# Lexer

`deflexer` declares a *parameterized* surface syntax: comment styles,
number-literal prefixes, label suffix, local-label prefix, string delimiter,
identifier character class, line continuation, and mode-suffix separator.
It can also define an alternate location-counter spelling.
Different fantasy CPUs can each declare their own dialect rather than
sharing one hard-coded assembly syntax.

```lisp
(deflexer sixtyfoo-syntax
  (comment-styles (";" :line) ("/*" "*/" :block))
  (number-formats (:hex "$" "0x") (:bin "%" "0b") (:dec :default) (:char "'"))
  (label-suffix ":")
  (local-label-prefix ".")
  (string-delim "\"")
  (ident-chars :alnum "_.")
  (line-continuation "\\")
  (mode-suffix-separator ".")
  (hole-prefix-separator ":"))
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
- `(mode-suffix-separator string)` — e.g. `"."` (the `default` lexer's
  setting). Separates a mnemonic from a forced addressing-mode suffix (e.g.
  the `.w` in `lda.w`; see [Addressing modes, "Forcing a mode with a
  mnemonic suffix"](modes.md#forcing-a-mode-with-a-mnemonic-suffix)).
  Every character of it must already be listed in `ident-chars`, the same
  way `local-label-prefix` must be — otherwise `lda.w` would split into two
  tokens at the lexer level and the parser would never see one run to split
  a suffix off of; `deflexer` signals an error rather than let that surface
  later as a baffling "no addressing mode matches this operand". A
  `nil`/omitted clause disables mode-suffix syntax entirely — a dotted
  mnemonic is then just an ordinary (if unusual) identifier.
- `(hole-prefix-separator string)` — e.g. `":"` (the `default` lexer's
  setting). Separates a suffix name from the operand hole it forces, as in
  `#w:5` (see [Addressing modes, "Forcing one hole with a
  prefix"](modes.md#forcing-one-hole-with-a-prefix)). It must be the
  `label-suffix`, `"#"`, or `"="`; `deflexer` rejects any other spelling. A
  `nil`/omitted clause disables hole prefixes.
- `(function-operators (spelling operator)...)` — the `default` lexer sets
  `("bank" :bank) ("lowcell" :lowcell) ("highcell" :highcell) ("defined" :defined)`. Each
  spelling names one of the [function operators](parser.md#function-operators).
  An identifier matching a spelling (any case) and followed by `(` lexes as
  a `:function-operator` token whose `value` is the operator keyword;
  otherwise it is an ordinary identifier. Spellings must be valid
  identifiers under `ident-chars` and unique. An omitted clause disables
  every function operator.
- `(location-counter string)` — one optional nonempty punctuation spelling,
  such as `"$"` or `"."`. It denotes the current address when it is a
  complete token. `$FF` remains a hex number and `.loop` remains an
  identifier. `*` remains available in every lexer. Operator (including any
  prefix of one, such as `!`), comment, string, and label delimiters cannot
  be used as the alias.

## Tokens

`(tokenize string &key (lexer 'default))` returns a `simple-vector` of
`token` structs, terminated by one `:eof` token. Each token has:

| Slot | Meaning |
|---|---|
| `type` | `:identifier` `:number` `:string` `:punctuation` `:location-counter` `:label-suffix` `:newline` `:eof` |
| `value` | parsed value — string (identifier/string), integer (number), keyword (punctuation/location counter/label suffix) |
| `text` | verbatim source text |
| `line`, `column` | 1-based source position |
| `localp` | `:identifier` only — T if `text` starts with `local-label-prefix` (#16) |

`:newline` tokens are significant — the grammar in
[Statement grammar & expression parser](parser.md) is line-oriented.

Punctuation tokens carry a keyword `value`: `:plus :minus :star :slash :percent :amp
:pipe :caret :tilde :shl :shr :lparen :rparen :lbracket :rbracket :comma :lt
:gt :le :ge :eq :ne :andand :oror :bang :hash :equals`. Two-character
operators (`<<`, `>>`, `<=`, `>=`, `==`, `!=`, `&&`, `||`) win maximal munch over their single-character
prefixes. `#` has no meaning to the lexer itself
— it is recognized so addressing-mode literal patterns (e.g. 6502-style
immediate `#expr`) have a token to match against once `defmode` exists (M2).
`[`/`]` (`:lbracket`/`:rbracket`) likewise have no meaning to the expression
parser — they exist so an addressing-mode pattern can match an indirect
`"[" expr "]"` operand form (see [Addressing modes, "Per-operand
modes"](modes.md#per-operand-modes)). `=` (`:equals`) likewise has no
meaning to the expression parser (it's absent from both its binary-operator
and unary-operator tables) — it exists only so the statement grammar
(`parser.md`) can recognize `name = value` as sugar for `.equ name, value`
(#35, see [Directives](directives.md#equ)).

`%` begins a binary literal only when immediately followed by `0` or `1`.
Otherwise it produces a `:percent` punctuation token for modulo. Write
`13 % 5` for modulo with a numeric right operand; `13%1` tokenizes its `%1`
as a binary literal. Configured `%` comments still take priority.

## Conditions

`lex-error` (a subtype of `lasm-syntax-error`, itself a subtype of
`lasm-error`) is signalled on malformed input — unterminated string/block
comment/character literal, a numeric prefix other than `%` with no digits, or an
unrecognized character. It carries `lasm-syntax-error-message`,
`-line`, and `-column`, plus (once caught alongside the source text
`tokenize` was given, which `with-source-context` attaches automatically) a
`-source` rendered as an excerpt with a caret by `diagnostic-text` — see
[Diagnostics](diagnostics.md).
