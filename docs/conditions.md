# Conditions

LASM condition types and their public readers. Catch a condition with
`handler-case` or `handler-bind`; use the reader names below to inspect it.

For context, see [Machine model](machine-model.md#conditions),
[Diagnostics](diagnostics.md), [Assembler](assembler.md), and
[Macros](macros.md).

| Condition | Parent | Readers |
|---|---|---|
| `lasm-error` | `error` | — |
| `storage-error` | `lasm-error`, `runtime-location` | `storage-error-machine`, `storage-error-name` |
| `runtime-location` | — | `runtime-location-pc`, `runtime-location-listing-line`, `runtime-location-source-text` |
| `unknown-storage` | `storage-error` | — |
| `address-out-of-range` | `storage-error` | `address-out-of-range-address` |
| `memory-write-protected` | `storage-error` | `memory-write-protected-address` |
| `stack-overflow` | `storage-error` | — |
| `stack-underflow` | `storage-error` | — |
| `stack-index-out-of-range` | `storage-error` | `stack-index-out-of-range-index` |
| `stack-pointer-out-of-range` | `storage-error` | `stack-pointer-out-of-range-value` |
| `bank-out-of-range` | `storage-error` | `bank-out-of-range-bank` |
| `register-index-out-of-range` | `storage-error` | `register-index-out-of-range-index` |
| `no-such-device` | `lasm-error`, `runtime-location` | `no-such-device-machine`, `no-such-device-index` |
| `lasm-trap` | `lasm-error`, `runtime-location` | `lasm-trap-tag`, `lasm-trap-data` |
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
| `definition-error` | `lasm-error` | `definition-error-message`, `definition-error-name` |
| `machine-definition-error` | `definition-error` | — |
| `instruction-definition-error` | `definition-error` | — |
| `mode-definition-error` | `definition-error` | — |
| `lexer-definition-error` | `definition-error` | — |
| `directive-definition-error` | `definition-error` | — |
| `usage-error` | `lasm-error` | `usage-error-message` |
| `debugger-usage-error` | `usage-error` | — |
| `disassembler-usage-error` | `usage-error` | — |
| `output-usage-error` | `usage-error` | — |
| `emulator-usage-error` | `usage-error` | — |
| `lookup-error` | `usage-error` | `lookup-error-name` |
| `unknown-machine` | `lookup-error` | — |
| `unknown-mode` | `lookup-error` | — |
| `unknown-lexer` | `lookup-error` | — |
| `opcode-conflict` | `instruction-definition-error` | `opcode-conflict-machine`, `opcode-conflict-opcode`, `opcode-conflict-mnemonic`, `opcode-conflict-other-mnemonic`, `opcode-conflict-reason` |
| `macro-error` | `lasm-syntax-error` | — |
| `include-error` | `lasm-syntax-error` | — |
| `conditional-error` | `lasm-syntax-error` | — |
| `assembly-error` | `lasm-syntax-error` | — |
| `assertion-error` | `assembly-error` | — |
| `unknown-mnemonic` | `assembly-error`, `unknown-instruction` | `unknown-instruction-machine`, `unknown-instruction-mnemonic` |
| `lasm-warning` | `warning` | `lasm-warning-message`, `lasm-warning-line`, `lasm-warning-file` |
| `ambiguous-mode` | `lasm-warning` | `ambiguous-mode-mnemonic`, `ambiguous-mode-chosen`, `ambiguous-mode-alternatives` |
| `ambiguous-alternative` | `ambiguous-mode` | `ambiguous-alternative-hole`, `ambiguous-alternative-slot` |
| `stale-mode` | `lasm-warning`, `style-warning` | `stale-mode-mode`, `stale-mode-dependents`, `stale-mode-instructions` |
