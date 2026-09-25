# Per-operand modes

`(one-of mode...)` lets each operand hole match one of several patterns.
The selected alternative can control encoding and semantics when the
instruction stores a discriminator. See [Addressing modes](modes.md) for
basic `defmode` options.

```lisp
(defmode a-reg expr)
(defmode a-ind "[" expr "]")
(defmode a-lit "#" expr)
(defmode pair (one-of a-reg a-ind a-lit) ","
              (one-of a-reg a-ind a-lit))
```

Each hole selects independently: `5, [10]` and `#5, 10` both match `pair`.
A `one-of` needs at least two previously declared alternatives.

## Named choice slots

`(one-of (SLOT mode...))` names a selection, including one with no
expression hole:

```lisp
(defmode fixed-or-register
  (one-of (fixed-kind stack-pointer program-counter) a-reg))
```

Matching returns named selections alongside the hole-aligned `choices`
record. A selection is the alternative's name, or its path for a varying
nested alternative: `(slot stk pop)`. Encodings can use a named slot to
distinguish fixed alternatives.

## Matching and backtracking

The matcher tries alternatives against the complete remaining pattern.
A complete match with more literal tokens wins; register-qualified holes
break the next tie. Declaration order breaks any remaining tie and reports
`ambiguous-alternative`.

```lisp
(defmode plain expr)
(defmode marked expr "X")
(defmode followed (one-of plain marked) "," "Y")
```

`5 X, Y` selects `marked` because `plain` leaves `X` unmatched. An
alternative with the same syntax as another requires its own `:suffix`;
without a prefix, the first declared one wins.

### Register-qualified holes

`(expr :register reg)` accepts a direct alias of banked register `reg`.
A plain `expr` still accepts numbers, labels, and compound expressions.
With the same surrounding literals, a register-qualified match outranks a
plain expression match. See [Machine model](machine-model.md#defmachine).

## Encoding a selection

A `one-of` changes accepted syntax, but an instruction must encode which
alternative matched if decode or semantics need to recover it.

| Encoding | Discriminator |
| --- | --- |
| Cell-encoded, one hole | [Hole-selected sub-opcode](instructions.md#variant-choice-m-sub-s--hole-selected-sub-opcode). |
| Cell-encoded, several holes | [Sub-opcode table](instructions.md#sub-opcode-table). |
| Cell-encoded, hole-less slot | [Slot participant](instructions.md#slot-participants). |
| Word-encoded | [`choice`-selected field](word-instructions.md#choice-selected-fields). |

Without a discriminator, disassembly renders the first alternative and
`choice-case` needs an `otherwise` branch. With one, decode preserves the
selection for [disassembly](disassembler.md) and
[`choice-case`](semantics.md#choice-case).

## Varying hole counts across alternatives

Alternatives can have different expression-hole counts, such as `reg` and
`[reg + offset]`. The longer alternative declares its extra fields with
`for-choice`:

```lisp
(for-choice (src a-idx) (operand off :trailing-word))
```

The base hole selects the alternative. A cell-encoded instruction needs a
sub-opcode selector; a word-encoded instruction needs a `choice` field.
Several varying elements select independently. See
[Word-encoded instructions](word-instructions.md#extra-holes-with-for-choice)
and [`subvarying.lisp`](../examples/subvarying.lisp).

### Nested varying alternatives

A keyed alternative inside another `one-of` is selected by a tree with one
subkey per keyed `one-of` it contains: `(choice (ind ind-idx))`. A `one-of` is
keyed when its options differ in hole count (varying) or in a
[per-hole attribute](#keyed-nested-alternatives).
`for-choice` uses the same key to name extra holes, and `choice-case` can
inspect the outer or inner selection. See
[`nestvarying.lisp`](../examples/nestvarying.lisp).[^nested]

An inner option with no hole, such as `POP`, needs a named outer `one-of`;
the slot records the pick as a tree:

```lisp
(defmode stk (one-of pop idx))
(defmode src (one-of (src-slot reg stk)))
```

See [`slotvarying.lisp`](../examples/slotvarying.lisp).

#### Several varying `one-of`s in one alternative

Name each keyed `one-of` with a slot. The option is a tree with a subkey
for each, and `for-choice` and `choice-case` address one `one-of` by its slot:

```lisp
(defmode pair (one-of (lhs abs idx)) "," (one-of (rhs abs idx)))
(defmode any (one-of pair lit))

(choice (pair idx abs))                              ; lhs = idx, rhs = abs
(for-choice (src pair lhs idx) (operand loff :width 1))
(for-choice (src pair rhs idx) (operand roff :width 1))
(choice-case (src pair lhs) (abs ...) (idx ...))
```

Extra operands land at the position of their own `one-of`, so an operand
declared after the nested alternative keeps its binding. See
[`nesttree.lisp`](../examples/nesttree.lisp).

When the alternative's minimum shape has more holes than the operand's base,
`(for-choice (src pair) ...)` declares the excess. Those operands land at
their pattern position, between the extras of the `one-of`s:

```lisp
(defmode any (one-of pair lit))     ; lit is "#" expr, one hole
(for-choice (src pair) (operand rv :width 1))   ; pair's second hole
```

See [`nestexcess.lisp`](../examples/nestexcess.lisp).

## Per-hole attributes

A plain hole can override mode defaults:

```lisp
(defmode branch-and-value
  (expr :relative t) "," (expr :signed t))
```

An alternative can also set its own `:strict`, `:signed`, `:width`, or
`:relative` value:

| Attribute | Effect | Discriminator needed when alternatives disagree? |
| --- | --- | --- |
| `:strict t` | Reject an out-of-range value at assembly. | No. |
| `:signed t` | Sign-extend on decode. | Yes. |
| `:width n` | Set the encoded cell count. | Yes, for cell encoding with `(operand :mode)`. |
| `:relative t` | Encode an offset from the next instruction. | Yes. |

A cell-encoded hole uses a sub-opcode selector when a decode-time
attribute differs. A word-encoded hole uses `choice` field variants;
per-hole `:width` is unavailable there because the field determines size.
An explicit `(operand :width n)` overrides an alternative's width.
`:relative t` implies signedness. See
[Diagnostics](diagnostics.md#strict-operand-range) and
[Assembler](assembler.md#pc-relative-offsets).

Inner alternatives of a [nested `one-of`](#nested-varying-alternatives)
can declare all four; each hole takes the attribute of the alternative that
owns it, and an alternative's own `:width` or `:strict` applies to holes of
its nested `one-of` that declare none:

```lisp
(defmode near expr :width 1 :signed t)
(defmode far "[" expr "," expr "]" :width 2 :strict t)
(defmode ind (one-of near far))
(defmode any (one-of ind lit))
```

#### Keyed nested alternatives

A nested `one-of` whose alternatives differ in hole count *or* in one of
these attributes is keyed: its pick is a subkey of the option tree,
`(choice (ind near))`. `ind` above has the same hole count either way, and
its picks still decide width and signedness. `:signed`, `:relative` and
`:width` need a selector for the hole, as at the top level, so decode
recovers them; `:strict` does not. See
[`nestkeyed.lisp`](../examples/nestkeyed.lisp).

## Forcing one hole

A mode alternative with `:suffix "rb"` can be selected with a prefix such
as `pick rb:3`. A word-field variant may have its own suffix; combined
prefixes select the alternative first and then the variant. See
[Addressing modes](modes.md#forcing-one-hole-with-a-prefix).

## Limitations

- A `one-of` with hole-less alternatives needs a slot on it. A
  cell-encoded one must be selected by a
  [sub-opcode table](instructions.md#slot-participants).
- An alternative selected by a tree cannot declare width, signedness,
  relative, suffix, or strict options itself; declare them on its holes or
  inner alternatives.[^nested]
- A selector lists every tree key, even when a difference such as `:strict`
  does not matter to it
  ([#278](https://todo.sr.ht/~takeiteasy/lasm/278)).
- An alternative with one varying `one-of` and further keyed ones repeats its
  extras in a `for-choice` for each keyed pick
  ([#279](https://todo.sr.ht/~takeiteasy/lasm/279)).

[^nested]: A tree lists the selected alternative at each keyed level:
  `(a (b c))` picks `b` inside `a`, then `c` inside `b`. A bare outer name
  does not identify a unique shape. A nested `one-of` whose alternatives
  agree on hole count and attributes is not keyed; a selection through it
  reports its outer alternative only.
