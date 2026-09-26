;;;; package.lisp
;;;; Package definition for lasm

(defpackage #:lasm
  (:use #:cl)
  (:shadow #:push #:pop)
  (:export
   #:lasm ; the fiveam test suite name, defined in tests/suites.lisp

   ;; NOTE: PUSH and POP are shadowed here (distinct from CL:PUSH/CL:POP)
   ;; because SBCL's package locks forbid MACROLET from locally rebinding a
   ;; CL:-package symbol, even lexically. Elsewhere in this package's own
   ;; source (storage.lisp, machine.lisp), plain list push/pop uses CL:PUSH/
   ;; CL:POP explicitly. See docs/semantics.md.
   ;; Conditions
   #:lasm-error
   #:storage-error
   #:storage-error-machine
   #:storage-error-name
   #:unknown-storage
   #:address-out-of-range
   #:address-out-of-range-address
   #:memory-write-protected
   #:memory-write-protected-address
   #:privilege-violation
   #:privilege-violation-access
   #:privilege-violation-address
   #:privilege-violation-kind
   #:privilege-violation-mask
   #:privilege-violation-info
   #:privilege-violation-required
   #:privilege-violation-current
   #:privilege-level
   #:stack-overflow
   #:stack-underflow
   #:stack-index-out-of-range
   #:stack-index-out-of-range-index
   #:stack-pointer-out-of-range
   #:stack-pointer-out-of-range-value
   #:register-index-out-of-range
   #:register-index-out-of-range-index
   #:bank-out-of-range
   #:bank-out-of-range-bank
   #:no-such-device
   #:no-such-device-machine
   #:no-such-device-index
   #:interrupt-queue-full
   #:interrupt-queue-full-machine
   #:lasm-trap
   #:lasm-trap-tag
   #:usage-error
   #:usage-error-message
   #:debugger-usage-error
   #:disassembler-usage-error
   #:output-usage-error
   #:emulator-usage-error
   #:lookup-error
   #:lookup-error-name
   #:unknown-machine
   #:unknown-mode
   #:unknown-lexer
   #:unknown-backend
   #:backend-definition-error
   #:definition-error
   #:with-definition-errors
   #:definition-error-message
   #:definition-error-name
   #:machine-definition-error
   #:instruction-definition-error
   #:mode-definition-error
   #:lexer-definition-error
   #:directive-definition-error
   #:runtime-location
   #:runtime-location-pc
   #:runtime-location-listing-line
   #:runtime-location-source-text
   #:runtime-location-label
   #:lasm-trap-data
   #:lasm-syntax-error
   #:lasm-syntax-error-message
   #:lasm-syntax-error-line
   #:lasm-syntax-error-column
   #:lasm-syntax-error-source
   #:lasm-syntax-error-file
   #:lasm-syntax-error-definition-file
   #:lasm-syntax-error-definition-source
   #:lex-error
   #:parse-failure
   #:unresolved-location

   ;; Diagnostics (#74)
   #:diagnostic-text
   #:with-source-context
   #:lasm-warning
   #:lasm-warning-message
   #:lasm-warning-line
   #:lasm-warning-file
   #:stale-backend
   #:ambiguous-mode
   #:ambiguous-mode-mnemonic
   #:ambiguous-mode-chosen
   #:ambiguous-mode-alternatives
   #:ambiguous-alternative
   #:ambiguous-alternative-hole
   #:ambiguous-alternative-slot
   #:*strict-operand-range*

   ;; Machine definition
   #:defmachine
   #:make-machine
   #:reset
   #:find-machine-descriptor
   #:machine-descriptor
   #:machine-descriptor-name
   #:machine-descriptor-elements
   #:storage-element
   #:storage-element-name
   #:storage-element-kind
   #:storage-element-width
   #:storage-element-count
   #:storage-element-depth
   #:storage-element-addr-width
   #:storage-element-cell-width
   #:storage-element-regions
   #:memory-region
   #:memory-region-p
   #:memory-region-name
   #:memory-region-start
   #:memory-region-end
   #:memory-region-kind
   #:memory-region-banks
   #:memory-region-on-write
   #:memory-region-read
   #:memory-region-write
   #:memory-region-device
   #:memory-region-device-index
   #:machine-descriptor-instruction-word
   #:instruction-word-layout
   #:instruction-word-layout-p
   #:instruction-word-layout-width
   #:instruction-word-layout-width-cells
   #:instruction-word-layout-fields
   #:instruction-word-layout-name
   #:instruction-word-layout-alternates
   #:instruction-word-layout-extra-word-order
   #:instruction-word-field
   #:instruction-word-layout-named
   #:machine-descriptor-clock-speed
  #:machine-descriptor-reset-pc
   #:machine-cycles
   #:machine-descriptor-devices
   #:device-descriptor
   #:device-descriptor-p
   #:device-descriptor-name
   #:device-descriptor-id
   #:device-descriptor-version
   #:device-descriptor-manufacturer
   #:device-descriptor-init
   #:device-descriptor-tick
   #:device-descriptor-receive
   #:device-descriptor-detach
   #:device-descriptor-save
   #:device-descriptor-load
   #:device-descriptor-read
   #:bind-region
   #:unbind-region
   #:device-descriptor-write
   #:device
   #:device-p
   #:device-index
   #:device-state
   #:machine-descriptor-interrupts
   #:machine-descriptor-parent
   #:machine-descriptor-undefined-opcode
   #:machine-descriptor-properties
   #:machine-descriptor-property
   #:machine-property
   #:interrupt-descriptor
   #:interrupt-descriptor-p
   #:interrupt-descriptor-vector
   #:interrupt-descriptor-message
   #:interrupt-descriptor-save
   #:interrupt-descriptor-stack-name
   #:interrupt-descriptor-queue-depth
   #:interrupt-descriptor-on-overflow
   #:interrupt-descriptor-mask-when
   #:interrupt-descriptor-mask-flag
   #:interrupt-descriptor-cycles
   #:interrupt-descriptor-drop-on-zero-vector
   #:interrupt-descriptor-stack-kind
   #:machine-descriptor-stack-pointers
   #:stack-pointer-descriptor
   #:stack-pointer-descriptor-p
   #:stack-pointer-descriptor-register
   #:stack-pointer-descriptor-memory
   #:stack-pointer-descriptor-grows
   #:stack-pointer-descriptor-width
   #:stack-pointer-descriptor-bounds

   ;; Storage accessors
   #:sref
   #:signed-value
   #:regref
   #:mref
   #:mpeek
   #:current-bank
   #:bank-peek
   #:stack-push
   #:stack-pop
   #:stack-depth
   #:stack-ref
   #:stack-pointer
   #:sp-push
   #:sp-pop
   #:sp-ref
   #:flag
   #:register-alias-at
   #:wrap-value

   ;; Devices (#108)
   #:attach-device
   #:detach-device
   #:device-at
   #:device-count
   #:find-device
   #:device-info
   #:device-send
   #:tick-devices
   #:device-signal
   #:machine-devices
   #:machine-interrupt-hook
   #:machine-access-hook

   ;; Snapshots (#112)
   #:machine-snapshot
   #:restore-snapshot
   #:write-snapshot
   #:read-snapshot
   #:snapshot-assembly
   #:+snapshot-version+
   #:snapshot-error
   #:snapshot-error-detail
   #:snapshot-version-mismatch
   #:snapshot-machine-mismatch
   #:snapshot-malformed
   #:snapshot-device-unknown
   #:snapshot-unwritable

   ;; Backends and items (#113)
   #:defbackend
   #:find-backend
   #:backend-descriptor
   #:backend-descriptor-p
   #:backend-descriptor-name
   #:backend-descriptor-machine
   #:backend-descriptor-registers
   #:backend-descriptor-call
   #:backend-descriptor-frame
   #:backend-descriptor-operands
   #:backend-descriptor-ops
   #:backend-descriptor-op-effects
   #:backend-descriptor-branches
   #:backend-stack-writers
   #:backend-register
   #:backend-expand-op
   #:assemble-items
   #:assemble-items-file
   #:items-size
   #:render-items
   #:read-items
   #:read-items-from-string
   #:items-program
   #:items-program-p
   #:items-program-items
   #:items-program-backend
   #:items-program-machine
   #:items-program-origin
   #:items-program-memory
   #:items-program-lexer
   #:items-error
   #:items-error-detail
   #:items-error-item
   #:items-malformed
   #:items-operand-mismatch
   #:read-restricted-form

   ;; Interrupts (#109)
   #:signal-interrupt
   #:deliver-pending-interrupt
   #:machine-interrupt-pending-count
   #:map-pending-interrupts
   #:machine-interrupt-depth

   ;; Semantics vocabulary
   #:with-machine
   #:with-machine-bindings
   #:set!
   #:push
   #:pop
   #:set-flags!
   #:trap
   #:idle
   #:interrupt-return
   #:zero?
   #:bit-set?
   #:page-crossed?
   #:elapse

   ;; Lexer
   #:deflexer
   #:find-lexer-descriptor
   #:tokenize
   #:token
   #:token-p
   #:token-type
   #:token-value
   #:token-text
   #:token-line
   #:token-column
   #:token-localp

   ;; Parser / expression AST
   #:parse
   #:parse-expression
   #:statement
   #:statement-p
   #:statement-label
   #:statement-label-localp
   #:statement-mnemonic
   #:statement-operands
   #:statement-mode-suffix
   #:statement-line
   #:statement-definition-line
   #:statement-source-unit
   #:operand
   #:operand-p
   #:operand-tokens
   #:expr-number
   #:expr-number-p
   #:expr-number-value
   #:expr-string
   #:expr-string-p
   #:expr-string-value
   #:expr-label
   #:expr-label-p
   #:expr-label-name
   #:expr-label-localp
   #:expr-location
   #:expr-location-p
   #:expr-unary
   #:expr-unary-p
   #:expr-unary-op
   #:expr-unary-operand
   #:expr-binary
   #:expr-binary-p
   #:expr-binary-op
   #:expr-binary-left
   #:expr-binary-right

   ;; Directives
   #:defdirective
   #:find-directive-descriptor
   #:directive-descriptor
   #:directive-descriptor-p
   #:directive-descriptor-name
   #:directive-descriptor-arity
   #:directive-descriptor-action
   #:directive-descriptor-width

   ;; Macros
   #:preprocess
   #:conditional-error
   #:include-error
   #:macro-error
   #:macro-descriptor
   #:macro-descriptor-p
   #:macro-descriptor-name
   #:macro-descriptor-params
   #:macro-descriptor-body
   #:*max-macro-depth*

   ;; Addressing modes
   #:defmode
   #:find-mode-descriptor
   #:mode-descriptor
   #:mode-descriptor-p
   #:mode-descriptor-name
   #:mode-descriptor-pattern
   #:mode-descriptor-width
   #:mode-descriptor-relativep
   #:mode-descriptor-signedp
   #:mode-descriptor-strictp
   #:mode-descriptor-suffix
   #:find-mode-by-suffix
   #:match-operand-mode
   #:try-match-operand-mode

   ;; Instructions
   #:definstruction
   #:find-instruction
   #:find-instruction-variants
   #:find-instruction-by-opcode
   #:find-instruction-descriptors-by-opcode
   #:eval-expr-constant
   #:encode-instruction
   #:execute-instruction
   #:unresolved-label
   #:unresolved-label-name
   #:unknown-instruction
   #:unknown-mnemonic
   #:unknown-instruction-machine
   #:unknown-instruction-mnemonic
   #:unknown-instruction-opcode
   #:no-matching-choice
   #:no-matching-choice-machine
   #:no-matching-choice-instruction
   #:no-matching-choice-operand
   #:no-matching-choice-choice
   #:opcode-conflict
   #:opcode-conflict-machine
   #:opcode-conflict-opcode
   #:opcode-conflict-mnemonic
   #:opcode-conflict-other-mnemonic
   #:opcode-conflict-reason
   #:instruction-descriptor
   #:instruction-descriptor-p
   #:instruction-descriptor-name
   #:instruction-descriptor-machine
   #:instruction-descriptor-mode
   #:instruction-descriptor-opcode
   #:instruction-descriptor-sub-opcode
   #:instruction-descriptor-operand-widths
   #:instruction-descriptor-operand-names
   #:instruction-descriptor-operand-registers
   #:instruction-descriptor-total-operand-width
   #:instruction-descriptor-semantics-fn
   #:instruction-descriptor-cycles
   #:instruction-descriptor-variable-cycles
   #:instruction-descriptor-word-fields
   #:instruction-descriptor-word-alternatives
   #:instruction-descriptor-extra-cells
   #:instruction-descriptor-word-layout
   #:instruction-descriptor-size
   #:word-field-choice
   #:word-field-choice-p
   #:word-field-choice-width
   #:word-field-choice-shift
   #:word-field-choice-kind
   #:word-field-choice-bias
   #:word-field-choice-range
   #:word-field-choice-escape
   #:word-field-choice-choice
   #:word-field-choice-signedp
   #:eval-expr

   ;; Assembler
   #:assemble
   #:assemble-statements
   #:assemble-file
   #:assembly
   #:assembly-p
   #:assembly-cells
   #:assembly-banks
   #:assembly-bank-image
   #:bank-image
   #:bank-image-p
   #:bank-image-region
   #:bank-image-bank
   #:bank-image-origin
   #:bank-image-cells
   #:assembly-cell-width
   #:assembly-origin
   #:assembly-symbols
   #:assembly-symbol-info
   #:assembly-listing
   #:assembly-source
   #:assembly-error
   #:assertion-error
   #:listing-line
   #:listing-line-p
   #:listing-line-address
   #:listing-line-size
   #:listing-line-line
   #:listing-line-definition-line
   #:listing-line-kind
   #:listing-line-descriptor
   #:listing-line-choices
   #:listing-line-file
   #:listing-line-region
   #:listing-line-bank
   #:symbol-info
   #:symbol-info-p
   #:symbol-info-name
   #:symbol-info-qualified-name
   #:symbol-info-scope
   #:symbol-info-kind
   #:symbol-info-localp
   #:symbol-info-value
   #:symbol-info-line
   #:symbol-info-file
   #:symbol-info-definition-line
   #:symbol-info-definition-file
   #:symbol-info-region
   #:symbol-info-bank
   #:lasm-syntax-error-definition-line

   ;; Emulator
   #:load-program
   #:step-machine
   #:run
   #:run-for-cycles
   #:run-for-duration
   #:machine-elapsed-seconds

   ;; Idle/sleep (#110)
   #:wake-machine
   #:machine-idle-p

   ;; Decoder (#21)
   #:decode-instruction-at
   #:machine-cell-reader
   #:vector-cell-reader
   #:machine-peek-reader

   ;; Disassembler (#21)
   #:disassemble-cells
   #:disassemble-assembly
   #:disassemble-memory
   #:disassembly-text
   #:print-disassembly
   #:disassembly-line
   #:disassembly-line-p
   #:disassembly-line-address
   #:disassembly-line-size
   #:disassembly-line-cells
   #:disassembly-line-cell-width
   #:disassembly-line-descriptor
   #:disassembly-line-values
   #:disassembly-line-choices
   #:disassembly-line-label
   #:disassembly-line-text

   ;; Listing (#25)
   #:listing-text
   #:print-listing
   #:listing-line-at
   #:assembly-label-at
   #:machine-label-at
   #:label-offset-text
   #:machine-listing-line
   #:listing-line-source-text
   #:machine-program
   #:listing-lines-for-source-line
   #:*listing-max-cells-shown*

   ;; Binary output (#79)
   #:assembly-bytes
   #:bytes-to-cells
   #:write-binary
   #:hex-text
   #:write-intel-hex

   ;; CLI (#80)
   #:run-cli

   ;; Symbol table (#37)
   #:assembly-symbol
   #:assembly-symbols-list
   #:assembly-data-regions
   #:assembly-symbol-groups
   #:symbols-text
   #:print-symbols

   ;; Debugger (#76)
   #:make-debug-session
   #:debug-session
   #:debug-session-p
   #:debug-session-machine
   #:debug-session-assembly
   #:breakpoint
   #:breakpoint-p
   #:breakpoint-id
   #:breakpoint-address
   #:breakpoint-label
   #:breakpoint-condition
   #:watchpoint
   #:watchpoint-p
   #:watchpoint-id
   #:watchpoint-name
   #:watchpoint-index
   #:watchpoint-address
   #:watchpoint-bank
   #:watchpoint-access
   #:watchpoint-label
   #:watch-hit
   #:watch-hit-p
   #:watch-hit-watchpoint
   #:watch-hit-access
   #:watch-hit-old
   #:watch-hit-new
   #:debug-watch
   #:debug-unwatch
   #:debug-watchpoints
   #:debug-break
   #:debug-unbreak
   #:debug-breakpoints
   #:debug-step
   #:debug-step-cycles
   #:debug-step-back
   #:debug-reverse-continue
   #:debug-reverse-continue-to
   #:debug-continue
   #:debug-continue-to
   #:debug-state-text
   #:debug-memory-text
   #:debug-banks-text
   #:debug-set
   #:debug-write
   #:debug-set-bank
   #:debug-where-text
   #:debug-command
   #:debugger-repl))
