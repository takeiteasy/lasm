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

## Unknown mnemonic

An unregistered mnemonic signals `unknown-mnemonic`, positioned at its line
like other assembly errors and naming the include file or macro body it came
from. It is also an `unknown-instruction`.

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
| Alternative `:strict t` | The holes of the matching `one-of` alternative, including an inner alternative of a varying nested `one-of`. |
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

## Definition errors

A malformed `defmachine`, `definstruction`, `defmode`, `deflexer` or
`defdirective` signals a `definition-error`, distinct from
`lasm-syntax-error`, which reports a program's own source.

| Condition | Signalled by |
| --- | --- |
| `machine-definition-error` | `defmachine` |
| `instruction-definition-error` | `definstruction` and its `semantics` body |
| `mode-definition-error` | `defmode` |
| `lexer-definition-error` | `deflexer` |
| `directive-definition-error` | `defdirective` |

`definition-error-message` holds the text and `definition-error-name` the
name being defined. `opcode-conflict` is an `instruction-definition-error`.

```lisp
(handler-case (eval '(defmode bad (one-of only-one)))
  (definition-error (c) (definition-error-name c)))   ; => BAD
```

Errors from a word-encoded machine's `semantics` surface on the
first execution of the instruction.[^definition] Under `compile-file`, SBCL
reports them as `compiled-program-error`; see
[Limitations](#limitations).

## Usage errors

Misusing the library API, rather than a definition or a program's source,
signals a `usage-error`. `usage-error-message` holds the text.

| Condition | Signalled by |
| --- | --- |
| `debugger-usage-error` | Debugger commands and API: bad targets, indices, counts, banks |
| `disassembler-usage-error` | Missing `:machine`, `:start` or `:count`, bad data regions |
| `output-usage-error` | Cell widths, byte counts and ranges that a format cannot hold |
| `emulator-usage-error` | `load-program`, `run*`, clock speed, devices, memory resolution |
| `lookup-error` | `unknown-machine`, `unknown-mode`, `unknown-lexer`; `lookup-error-name` names the missing definition |

`debug-command` reports any `lasm-error` as `Error: ...` text. A definer
that names an unregistered machine or mode signals its own
`*-definition-error` instead of the lookup error. A lambda-list mismatch in a
definer form, such as an unknown keyword, is a definition error too.

## Opcode conflicts

`definstruction` signals `opcode-conflict` when two descriptors at one
opcode cannot be distinguished at decode. Cell-encoded instructions can
share an opcode with distinct sub-opcodes; word-encoded instructions can
share one when their fields are distinguishable or a valid fallback is
declared. See [Instructions](instructions.md#opcode-to-descriptor-decode).

## Limitations

- `compile-file` of a malformed definition loses the condition type; it is
  tracked in [ticket 258](https://todo.sr.ht/~takeiteasy/lasm/258).

[^definition]: Word-encoded semantics compile lazily on first use. The
  compile step re-signals the typed condition, so the caller sees the same
  `instruction-definition-error` as at definition time.
