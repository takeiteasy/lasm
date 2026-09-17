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
   #:unknown-storage
   #:address-out-of-range
   #:stack-overflow
   #:stack-underflow
   #:stack-index-out-of-range
   #:stack-index-out-of-range-index
   #:lasm-trap
   #:lasm-syntax-error
   #:lasm-syntax-error-message
   #:lasm-syntax-error-line
   #:lasm-syntax-error-column
   #:lasm-syntax-error-source
   #:lex-error
   #:parse-failure
   #:unresolved-location

   ;; Diagnostics (#74)
   #:diagnostic-text
   #:with-source-context
   #:lasm-warning
   #:lasm-warning-message
   #:lasm-warning-line
   #:ambiguous-mode
   #:ambiguous-mode-mnemonic
   #:ambiguous-mode-chosen
   #:ambiguous-mode-alternatives
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
   #:machine-descriptor-instruction-word
   #:instruction-word-layout
   #:instruction-word-layout-p
   #:instruction-word-layout-width
   #:instruction-word-layout-width-cells
   #:instruction-word-layout-fields
   #:instruction-word-layout-name
   #:instruction-word-layout-alternates
   #:instruction-word-field
   #:instruction-word-layout-named
   #:machine-descriptor-clock-speed
   #:machine-cycles

   ;; Storage accessors
   #:sref
   #:signed-value
   #:mref
   #:stack-push
   #:stack-pop
   #:stack-depth
   #:stack-ref
   #:flag
   #:wrap-value

   ;; Semantics vocabulary
   #:with-machine
   #:with-machine-bindings
   #:set!
   #:push
   #:pop
   #:set-flags!
   #:trap
   #:zero?
   #:bit-set?

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
   #:operand
   #:operand-p
   #:operand-tokens
   #:expr-number
   #:expr-number-p
   #:expr-number-value
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
   #:expand-macros
   #:macro-error
   #:macro-descriptor
   #:macro-descriptor-p
   #:macro-descriptor-name
   #:macro-descriptor-params
   #:macro-descriptor-body
   #:*max-macro-expansion-rounds*

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
   #:unknown-instruction-machine
   #:unknown-instruction-mnemonic
   #:unknown-instruction-opcode
   #:no-matching-choice
   #:no-matching-choice-machine
   #:no-matching-choice-instruction
   #:no-matching-choice-operand
   #:no-matching-choice-choice
   #:instruction-descriptor
   #:instruction-descriptor-p
   #:instruction-descriptor-name
   #:instruction-descriptor-machine
   #:instruction-descriptor-mode
   #:instruction-descriptor-opcode
   #:instruction-descriptor-sub-opcode
   #:instruction-descriptor-operand-widths
   #:instruction-descriptor-operand-names
   #:instruction-descriptor-total-operand-width
   #:instruction-descriptor-semantics-fn
   #:instruction-descriptor-cycles
   #:instruction-descriptor-word-fields
   #:instruction-descriptor-word-alternatives
   #:instruction-descriptor-extra-words
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
   #:assembly
   #:assembly-p
   #:assembly-cells
   #:assembly-cell-width
   #:assembly-origin
   #:assembly-symbols
   #:assembly-symbol-info
   #:assembly-listing
   #:assembly-source
   #:assembly-error
   #:listing-line
   #:listing-line-p
   #:listing-line-address
   #:listing-line-size
   #:listing-line-line
   #:listing-line-kind
   #:listing-line-descriptor
   #:symbol-info
   #:symbol-info-p
   #:symbol-info-name
   #:symbol-info-qualified-name
   #:symbol-info-scope
   #:symbol-info-kind
   #:symbol-info-localp
   #:symbol-info-value
   #:symbol-info-line

   ;; Emulator
   #:load-program
   #:step-machine
   #:run
   #:run-for-cycles
   #:run-for-duration
   #:machine-elapsed-seconds

   ;; Decoder (#21)
   #:decode-instruction-at
   #:machine-cell-reader
   #:vector-cell-reader

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
   #:disassembly-line-descriptor
   #:disassembly-line-values
   #:disassembly-line-choices
   #:disassembly-line-label
   #:disassembly-line-text

   ;; Listing (#25)
   #:listing-text
   #:print-listing
   #:listing-line-at
   #:listing-lines-for-source-line
   #:*listing-max-cells-shown*

   ;; Symbol table (#37)
   #:assembly-symbol
   #:assembly-symbols-list
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
   #:debug-break
   #:debug-unbreak
   #:debug-breakpoints
   #:debug-step
   #:debug-continue
   #:debug-continue-to
   #:debug-state-text
   #:debug-memory-text
   #:debug-where-text
   #:debug-command
   #:debugger-repl))
