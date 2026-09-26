# Addressing modes

`defmode` defines an operand pattern. `definstruction` connects that pattern
to an encoding and semantics. See [Per-operand modes](operand-modes.md) for
`one-of` patterns and independently selected holes.

```lisp
(defmode immediate "#" expr :width 1)
(defmode zero-page expr :width 1 :suffix "z")
(defmode absolute expr :suffix "w")
(defmode relative expr :width 1 :relative t)
```

## `defmode`

```lisp
(defmode NAME pattern-element...
  [:width n] [:signed t] [:relative t] [:suffix "s"] [:strict t])
```

A malformed definition signals `mode-definition-error`.

| Part | Meaning |
| --- | --- |
| String literal | Match token text, ignoring case. |
| `expr` | Parse an expression. |
| `(expr :register NAME)` | Require an alias from a register bank. |
| `(expr :signed t)` | Sign-extend this hole on decode. |
| `(expr :relative t)` | Encode this hole as an offset from the next instruction. |
| `(one-of mode...)` | Match one of several patterns; see [Per-operand modes](operand-modes.md). |

A mode may contain only literals and have no expression hole. Built-in modes
are `immediate`, `zero-page`, `absolute`, `indexed-x`, `indirect-y`,
`relative`, and `stack-relative`. Modes must be defined before an instruction
uses them. A mode is global unless it is [machine-local](#machine-local-modes).

| Option | Meaning |
| --- | --- |
| `:width n` | Default operand width in memory cells. |
| `:signed t` | Use a signed range and sign extension. |
| `:relative t` | Encode a signed PC-relative offset; implies `:signed t`. |
| `:suffix "s"` | Allow a mnemonic suffix such as `lda.w`. |
| `:strict t` | Signal `assembly-error` instead of wrapping an out-of-range value. |

### Declare narrower modes before wider ones

Modes with the same syntax compete by value. Declare the narrow mode first:

```lisp
(definstruction sixtyfoo lda
  (modes (zero-page (opcode #xA5))
         (absolute (opcode #xAD)))
  (semantics (set! a (mref machine 'ram operand))))
```

The assembler starts unknown labels narrow and widens them if needed.
A more specific pattern wins before value selection; see
[Assembler](assembler.md#choosing-a-mode).

### Width

`(operand :mode)` uses the mode's `:width` or the target memory's address
width rounded to cells. A mode with several `expr` holes needs an operand
field for each hole. Hole options can override mode-wide signedness or
relativeness; see [Per-operand modes](operand-modes.md#per-hole-attributes).

## Signed operands

`:signed t` makes the assembler test the signed range and makes decode
sign-extend the value. A signed mode need not be relative:

```lisp
(defmode signed-immediate "#" expr :width 1 :signed t)
```

The rule also applies to [word fields](word-instructions.md#signed-word-fields).

## PC-relative modes

A relative operand names an absolute target in source. The assembler stores
the signed offset from the address after the instruction:

```text
offset = target - (instruction address + instruction size)
```

The offset counts memory cells. Overflow signals `assembly-error` rather
than wrapping. The emulator sign-extends the operand before semantics runs;
`(set! pc (+ pc operand))` branches from the next instruction. See
[Assembler](assembler.md#pc-relative-offsets).

## Stack-relative addressing

`stack-relative` matches `expr "," "S"`. Its expression is an unsigned
offset from the top of the machine's fixed stack: `0` names the top, `1`
the cell below it.

```lisp
(definstruction hybridfoo lda
  (modes stack-relative)
  (encoding (opcode #xA3) (operand :mode))
  (semantics (set! a (stack-ref operand))))
```

See [`hybrid.lisp`](../examples/hybrid.lisp).

## Forcing a mode with a mnemonic suffix

A mode's `:suffix` lets source select it explicitly:

```lisp
lda.z target   ; force zero-page
lda.w target   ; force absolute
```

The assembler restricts selection to that mode. An unknown suffix, an
unsupported mode for the instruction, or mismatched syntax signals
`assembly-error`. An ordinary forced mode wraps an out-of-range value by
default; a relative mode still checks its offset. The suffix separator is a
[lexer](lexer.md) setting. See [`complete.lisp`](../examples/complete.lisp).

## Forcing one hole with a prefix

A hole prefix selects a `one-of` alternative or a word-field variant:

```lisp
seta #w:5   ; force an extra-word variant
pick rb:3   ; force an alternative with :suffix "rb"
```

The lexer controls the separator. Prefixes affect only the marked hole;
several can be combined. See [Per-operand modes](operand-modes.md#forcing-one-hole)
and [Word-encoded instructions](word-instructions.md#variant-suffixes).

## Matching

| Function | Result |
| --- | --- |
| `match-operand-mode tokens mode` | First AST, all hole ASTs, choices, and named selections; signals `parse-failure` on mismatch. |
| `try-match-operand-mode tokens mode` | Match data without signalling; includes prefixes, ties, a specificity score, and the token span of each `one-of` alternative matched. |

Pass the whole `statement-operand-tokens` run. The parsed `operands` list is
already split at top-level commas, but a mode may include commas as literal
pattern elements. See [Parser](parser.md) and
[Per-operand modes](operand-modes.md#matching-and-backtracking).

## Machine-local modes

`(defmode (NAME (:machine M)) ...)` defines a mode only machine `M` and its
`:extends` descendants see. It shadows a global mode of the same name, so
machines can use one name with different syntax:

```lisp
(defmode indexed-x expr "," "X")                       ; global
(defmode (indexed-x (:machine sixtyfoo)) "(" expr ",X)") ; sixtyfoo only
```

| Rule | Behavior |
| --- | --- |
| Lookup order | `M`'s local modes, its ancestors' local modes, then global modes. |
| Definition | `M` must already be defined with `defmachine`; otherwise `mode-definition-error`. |
| `one-of` | Alternative names resolve in the machine being defined, assembled, or disassembled.[^scope] |
| `:suffix` | Must be unique among the modes a machine sees. |
| Redefinition | Warns as [below](#redefining-a-mode), only for machines that see the redefined mode. |

`find-mode-descriptor` and `find-mode-by-suffix` take an optional machine name.

[^scope]: `*mode-scope*` holds the machine name. An instruction's own mode is fixed when its `definstruction` is compiled; a child machine that shadows a mode a `one-of` names changes how inherited instructions match that alternative.

## Redefining a mode

Redefining a mode updates the hole counts and option keys of modes that
reference it through `one-of`. When the shape changes, DEFMODE re-validates
those modes, innermost first, and signals a `stale-mode` warning for each
that no longer validates. Instructions already compiled keep their old
shapes; one more `stale-mode` warning lists them, and re-evaluating their
`definstruction` forms updates them. Redefining a mode with an identical
shape is silent.

## Note on operand binding

A mode only parses syntax. `operand` holds the decoded integer; semantics
chooses whether to use it directly or read memory. See
[Instructions](instructions.md#note-on-operand-binding).
