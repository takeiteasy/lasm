# Diagnostics

Program-source errors carry positions through `lasm-syntax-error`.
`diagnostic-text` renders them with source context. See
[Conditions](conditions.md) for the condition hierarchy and readers.

## `diagnostic-text`

```lisp
(diagnostic-text condition &key source)
```

A positioned error shows its file or line, message, source text, and a
caret when that text is available:

```text
line 3, column 5: ldx: operand "(#5),Y" matches no addressing mode
3 | ldx (#5),Y
  |     ^
```

`:source` overrides source stored on the condition. Printing a syntax
condition normally calls the same renderer. Without a position or source,
the unavailable parts are omitted.

## Source propagation: `with-source-context`

`with-source-context` attaches source text to a syntax error that has none,
without changing its type or stopping propagation. File-backed statements
keep their file. An error in a macro shows the invocation as the primary
location and its body location as secondary detail.

```lisp
(with-source-context source
  (assemble-statements statements :machine 'sixtyfoo))
```

## Mode-mismatch diagnostics

A mismatched operand names the instruction, echoes its operand text, and
lists accepted mode syntax. Forced mode and hole prefixes also report
accepted forms. A `one-of` displays alternatives separated by `|`.

## Mode-selection ambiguity

When equally specific modes tie on encoded width, assembly warns with
`ambiguous-mode` and selects the first declared mode. Modes that differ in
width resolve through normal [mode selection](assembler.md#choosing-a-mode)
without a warning.

```lisp
(defmode mode-a expr :width 1)
(defmode mode-b expr :width 1)
;; An instruction using both warns when either could encode the operand.
```

Warnings occur after layout settles. A forced mode suffix selects the mode
without this warning. Use `handler-bind` and `muffle-warning` to handle
warnings in a caller.

## Alternative ambiguity

A `one-of` alternative wins by literal tokens, then by register-qualified
holes. A remaining tie warns with `ambiguous-alternative` and chooses the
first declared alternative. A hole prefix chooses explicitly and suppresses
the tie. See [Per-operand modes](operand-modes.md#matching-and-backtracking).

## Strict operand range

Ordinary values wrap to their encoded width by default. Use a strict mode
or bind the global switch to turn an overflow into `assembly-error`:

```lisp
(defmode strict-imm "#" expr :width 1 :strict t)

(let ((*strict-operand-range* t))
  (assemble source :machine 'sixtyfoo))
```

| Setting | Applies to |
| --- | --- |
| Mode `:strict t` | Operands selected through that mode. |
| Alternative `:strict t` | The matching `one-of` hole. |
| `*strict-operand-range*` | Every operand, including modes without `:strict`. |

Checks use each selected field's signed or unsigned range. Relative
offsets, `:register` indices, and `choice`-selected word fields already
signal on overflow regardless of strict settings. A per-mode setting only
applies if that mode is selected; the global switch covers every fallback
selection. See [Assembler](assembler.md#choosing-a-mode) and
[Word-encoded instructions](word-instructions.md#choice-selected-fields).

## Shadowed fallback encoding

Assembling a fallback instruction to bits that decode as a more specific
instruction signals `assembly-error`:

```text
SYS: this encoding decodes as CLS
```

## Opcode conflicts

`definstruction` signals `opcode-conflict` when two descriptors at one
opcode cannot be distinguished at decode. Cell-encoded instructions can
share an opcode with distinct sub-opcodes; word-encoded instructions can
share one when their fields are distinguishable or a valid fallback is
declared. See [Instructions](instructions.md#opcode-to-descriptor-decode).
