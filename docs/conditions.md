# Conditions

Reference for every condition type `#:lasm` signals and its readers. A
public condition type's slot readers are public too — catch a condition
with `handler-case`/`handler-bind` and every slot below is readable without
reaching into `lasm::`. `tests/package.lisp` walks the condition hierarchy
via the MOP and checks this table against the export list automatically.

Each stage's own docs describe *when* and *why* a condition is signalled —
[Machine model](machine-model.md#conditions) for storage, devices and
interrupts; [Diagnostics](diagnostics.md) for syntax errors, warnings and
opcode conflicts; [Instructions](instructions.md),
[Assembler](assembler.md), [Macros](macros.md) for the rest. This page is
just the reader table.

| Condition | Parent | Readers |
|---|---|---|
| `lasm-error` | `error` | — |
| `storage-error` | `lasm-error` | `storage-error-machine`, `storage-error-name` |
| `unknown-storage` | `storage-error` | — |
| `address-out-of-range` | `storage-error` | `address-out-of-range-address` |
| `memory-write-protected` | `storage-error` | `memory-write-protected-address` |
| `stack-overflow` | `storage-error` | — |
| `stack-underflow` | `storage-error` | — |
| `stack-index-out-of-range` | `storage-error` | `stack-index-out-of-range-index` |
| `register-index-out-of-range` | `storage-error` | `register-index-out-of-range-index` |
| `no-such-device` | `lasm-error` | `no-such-device-machine`, `no-such-device-index` |
| `lasm-trap` | `lasm-error` | `lasm-trap-tag`, `lasm-trap-data` |
| `interrupt-queue-full` | `lasm-error` | `interrupt-queue-full-machine` |
| `snapshot-error` | `lasm-error` | `snapshot-error-detail` |
| `snapshot-version-mismatch` | `snapshot-error` | — |
| `snapshot-machine-mismatch` | `snapshot-error` | — |
| `snapshot-malformed` | `snapshot-error` | — |
| `snapshot-device-unknown` | `snapshot-error` | — |
| `lasm-syntax-error` | `lasm-error` | `lasm-syntax-error-message`, `lasm-syntax-error-line`, `lasm-syntax-error-column`, `lasm-syntax-error-file`, `lasm-syntax-error-source`, `lasm-syntax-error-definition-line`, `lasm-syntax-error-definition-file`, `lasm-syntax-error-definition-source` |
| `lex-error` | `lasm-syntax-error` | — |
| `parse-failure` | `lasm-syntax-error` | — |
| `unresolved-location` | `lasm-error` | — |
| `unresolved-label` | `lasm-syntax-error` | `unresolved-label-name` |
| `unknown-instruction` | `lasm-error` | `unknown-instruction-machine`, `unknown-instruction-mnemonic`, `unknown-instruction-opcode` |
| `no-matching-choice` | `lasm-error` | `no-matching-choice-machine`, `no-matching-choice-instruction`, `no-matching-choice-operand`, `no-matching-choice-choice` |
| `opcode-conflict` | `lasm-error` | `opcode-conflict-machine`, `opcode-conflict-opcode`, `opcode-conflict-mnemonic`, `opcode-conflict-other-mnemonic`, `opcode-conflict-reason` |
| `macro-error` | `lasm-syntax-error` | — |
| `include-error` | `lasm-syntax-error` | — |
| `assembly-error` | `lasm-syntax-error` | — |
| `lasm-warning` | `warning` | `lasm-warning-message`, `lasm-warning-line` |
| `ambiguous-mode` | `lasm-warning` | `ambiguous-mode-mnemonic`, `ambiguous-mode-chosen`, `ambiguous-mode-alternatives` |

`signed-range-out-of-field` is not in this table: it's internal to
`definstruction`'s own field-range checking, never escapes to a caller, and
defines no readers.
