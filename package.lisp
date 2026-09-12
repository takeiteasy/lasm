;;;; package.lisp
;;;; Package definition for lasm

(defpackage #:lasm
  (:use #:cl)
  (:shadow #:push #:pop)
  (:export
   #:lasm ; the fiveam test suite name, defined in tests.lisp

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
   #:lasm-trap
   #:lasm-syntax-error
   #:lasm-syntax-error-message
   #:lasm-syntax-error-line
   #:lasm-syntax-error-column
   #:lex-error
   #:parse-failure

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

   ;; Storage accessors
   #:sref
   #:signed-value
   #:mref
   #:stack-push
   #:stack-pop
   #:stack-depth
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

   ;; Parser / expression AST
   #:parse
   #:parse-expression
   #:statement
   #:statement-p
   #:statement-label
   #:statement-mnemonic
   #:statement-operands
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
   #:expr-unary
   #:expr-unary-p
   #:expr-unary-op
   #:expr-unary-operand
   #:expr-binary
   #:expr-binary-p
   #:expr-binary-op
   #:expr-binary-left
   #:expr-binary-right

   ;; Addressing modes
   #:defmode
   #:find-mode-descriptor
   #:mode-descriptor
   #:mode-descriptor-p
   #:mode-descriptor-name
   #:mode-descriptor-pattern
   #:mode-descriptor-width
   #:mode-descriptor-relativep
   #:match-operand-mode
   #:try-match-operand-mode

   ;; Instructions
   #:definstruction
   #:find-instruction
   #:find-instruction-variants
   #:find-instruction-by-opcode
   #:eval-expr-constant
   #:encode-instruction
   #:execute-instruction
   #:unresolved-label
   #:unresolved-label-name
   #:unknown-instruction
   #:unknown-instruction-machine
   #:unknown-instruction-mnemonic
   #:unknown-instruction-opcode
   #:instruction-descriptor
   #:instruction-descriptor-p
   #:instruction-descriptor-name
   #:instruction-descriptor-machine
   #:instruction-descriptor-mode
   #:instruction-descriptor-opcode
   #:instruction-descriptor-operand-width
   #:instruction-descriptor-semantics-fn
   #:instruction-descriptor-cycles
   #:eval-expr

   ;; Assembler
   #:assemble
   #:assemble-statements
   #:assembly
   #:assembly-p
   #:assembly-bytes
   #:assembly-origin
   #:assembly-symbols
   #:assembly-error

   ;; Emulator
   #:load-program
   #:step-machine
   #:run))
