# Word-encoded instructions

An `instruction-word` machine packs opcode and operand fields into a word,
followed by any extra cells. An operand names a field of the selected layout.
See [Instructions](instructions.md) for `definstruction`, semantics, and
cell-encoded instructions.

```lisp
(operand [NAME] :field FIELD-NAME
  [(variant (range LO HI) inline [:bias N])
   (variant :else (extra-word :escape N [:cells K] [:endian ORDER]))]*)
```

| Form | Encoding |
| --- | --- |
| No `variant` | Value occupies the field directly. |
| `(range LO HI) inline` | Values in the range occupy the field; `:bias` adjusts stored bits. |
| `:else (extra-word ...)` | Escape code occupies the field; value follows in extra cells. |
| `(operand [NAME] :trailing-word [:cells K] [:endian ORDER])` | Unconditional value in extra cells; see [Extra holes](#extra-holes-with-for-choice). |

`:cells` sets an extra value's width in cells and defaults to the instruction
word's width. Extra values follow field order unless the machine declares
`extra-word-order`. Every expression hole needs a field or a trailing-word
operand. Overlapping stored ranges and escape codes fail at declaration
time.[^word]

```lisp
(definstruction wordfoo seta
  (modes wimm)
  (encoding (opcode 1)
    (operand value :field src
      (variant (range -1 30) inline :bias 1)
      (variant :else (extra-word :escape #x3ff))))
  (semantics (set! a value)))
```

The assembler chooses the inline form when it fits, otherwise the extra
word. See [`word.lisp`](../examples/word.lisp) and
[Assembler](assembler.md#choosing-a-mode).

## Per-instruction layouts

`(layout NAME)` selects a named `instruction-word` layout for this
instruction. Each `:field` and `field-value` name resolves within that
layout. Without `(layout ...)`, the default layout applies. Distinct
instructions sharing an opcode can use different layouts when their encoded
bit patterns remain distinguishable. See
[Machine model](machine-model.md#defmachine) and
[`chip8word.lisp`](../examples/chip8word.lisp).

## Cell order

Cells of the instruction word and of each extra value are stored in the
memory's `:endian` order by default. Two overrides take any `:endian` order
(`:little`, `:big` or `(outer inner group)`, see
[Machine model](machine-model.md#cell-width-and-the-assembler)):

| Override | Applies to |
| --- | --- |
| `(instruction-word :width n :endian ORDER ...)` | The instruction word, its extra values and every `(layout ...)`. |
| `:endian ORDER` on `extra-word` or `:trailing-word` | That one extra value. |

```lisp
(instruction-word :width 16 :endian :big (field opcode 4) (field src 12))
(variant :else (extra-word :escape #xfff :cells 2 :endian :little))
```

The extra value's setting wins over the instruction word's, which wins over
the memory's. `.word` and other data directives keep the memory's order. See
[`word-endian.lisp`](../examples/word-endian.lisp).[^endian]

## Fixed field values

`field-value` fixes a named field without an operand hole:

```lisp
(definstruction chip8wordfoo cls
  (encoding (opcode 0) (layout nnn) (field-value nnn #xe0))
  (semantics nil))
```

It can distinguish instructions sharing an opcode. A field cannot be both
fixed and assigned to an operand, and its value must fit. See
[`chip8word.lisp`](../examples/chip8word.lisp).

## Fallback instructions

A word instruction with `(fallback)` accepts a broader set of bit patterns
than specific instructions at the same opcode. Decode tries the specific
instructions first; the fallback receives the remaining words.

```lisp
(definstruction chip8wordfoo sys
  (modes wnnn)
  (encoding (opcode 0) (layout nnn) (fallback)
    (operand addr :field nnn))
  (semantics nil))
```

Its accepted patterns must strictly contain each overlapping instruction's
patterns. Equal or partial overlap signals `opcode-conflict`. Assembling a
fallback word that decodes as a specific instruction signals
`assembly-error`.

## Signed word fields

A `:signed t` mode encodes values as two's-complement values and decodes
them with sign extension. This applies to inline and extra-word fields;
`:relative` also implies signedness. See
[Addressing modes](modes.md#signed-operands).

## PC-relative operands

A relative operand names an absolute target. The assembler encodes its
signed offset from the next instruction, measured in **cells**. An offset
outside an inline range can use an extra-word variant; without one, it
signals `assembly-error`. See [Assembler](assembler.md#pc-relative-offsets).

## Choice-selected fields

A field variant can select by the syntax of a `one-of` alternative instead
of the value alone:

```lisp
(operand NAME :field FIELD-NAME
  (variant (choice MODE) inline :range (LO HI) [:bias N])
  (variant (choice MODE) (extra-word :escape N [:cells K])))
```

An inline `choice` needs an explicit range. A `choice` extra word is emitted
whenever its alternative matches. A field may mix `choice` variants with
value-selected variants when exactly one alternative is unclaimed; the
value-selected variants then belong to that alternative. Each alternative
must decode unambiguously.[^choices]

## Variant suffixes

Add `:suffix "name"` to a variant to force it with an operand prefix:

```lisp
(operand value :field src
  (variant (range -1 30) inline :bias 1)
  (variant :else (extra-word :escape #x3ff) :suffix "w"))
```

`seta #w:5` uses the extra word even when 5 fits inline. A forced inline
variant errors if the value does not fit. See
[Addressing modes](modes.md#forcing-one-hole-with-a-prefix).

## Aliased variants

`:alias t` lets two `choice` alternatives share one encoding. One variant
remains canonical: decode and disassembly report its syntax. Aliased escape
variants share the same escape value and extra-cell width; aliased inline
variants share the same range and bias. Their holes must also agree on
width, signedness, relative behavior, and any extra-hole encoding.[^aliases]

## Extra holes with `for-choice`

When a `one-of` alternative adds expression holes, `for-choice` supplies
those holes' operand fields:

```lisp
(operand src :field a
  (variant (choice a-reg) inline :range (0 7))
  (variant (choice a-idx) inline :range (0 7) :bias #x10))
(for-choice (src a-idx) (operand off :trailing-word))
```

`src` identifies the base operand and `a-idx` the longer alternative.
`:trailing-word` adds an unconditional extra value; it accepts `:cells k` and
[`:endian ORDER`](#cell-order).
The short form `(for-choice a-idx ...)` works when the alternative uniquely
identifies the varying element. The longer alternative's extra names are
available inside its `choice-case` branch.[^varying]

Multiple varying elements use separate `for-choice` groups. Nested
alternatives use tree keys such as `(choice (outer inner))` and
`(for-choice (src outer inner) ...)`; an alternative with several varying
`one-of`s takes one group per slot, `(for-choice (src outer lhs inner) ...)`.
An inner option with no hole pins a field with
`(for-choice (slot outer inner) (field-value f n))`. See
[Addressing modes](operand-modes.md#nested-varying-alternatives).

A named alternative with no holes can use `(for-choice (slot alternative)
(field-value mode 27))` to pin a field. Cell-encoded machines use ordinary
`:width` or `:mode` fields for extra holes, with a sub-opcode selector on
the base hole.

## Signed choice fields

A `choice` variant takes signedness from its selected mode. Decode
sign-extends the field or extra word before removing any bias. Alternatives
with different signedness need `choice` variants for every alternative;
value-selected fallback cannot identify which signedness to decode.


[^word]: Inline values are stored after applying `:bias`; decode subtracts
  it. Escape codes cannot overlap inline stored ranges. Extra-word width is
  per variant. The opcode and each named field must fit their declared bit
  widths. Fields use the selected layout's bit positions.

[^choices]: `choice` names an alternative of the hole's `one-of` mode. Each
  selected range or escape must fit its field, and stored encodings cannot
  overlap unless they are valid aliases. A matched choice whose value fits
  no selected variant signals `assembly-error`.

[^endian]: `:endian` orders *cells*. Byte order inside a wide cell in binary
  output still follows the memory's `:endian`. A `(layout ...)` cannot set
  its own `:endian`, because decode reads the word before it knows the layout.
  Aliased extra words must agree on `:endian`.

[^aliases]: Exactly one matching variant is canonical. Value-selected
  variants cannot use `:alias`. An alias with a different hole shape or
  partially overlapping stored range is invalid.

[^varying]: `for-choice` groups must cover each longer alternative with the
  right number of fields. The descriptor for a shorter alternative binds
  missing extra names to `nil`. Several varying elements select
  independently; their combinations must remain distinguishable at decode.
