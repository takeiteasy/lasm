# Macros

A `.macro`/`.endm` block defines a reusable group of statements. Define the
macro before its first invocation.

```asm
.macro addconst dst, k=1
    lda dst
    adc #k
    sta dst
.endm

addconst cell       ; uses k=1
addconst cell, 5    ; uses k=5
```

See the runnable [`macros.lisp` example](../examples/macros.lisp).

## Syntax

| Form | Meaning |
|---|---|
| `.macro name param...` | Start a definition. Parameters are identifiers. |
| `.endm` | End the definition. |
| `name arg...` | Expand the body with the supplied arguments. |
| `param=expression` | Set a default for a trailing parameter. |

Commas between parameters are optional unless a default is present. Required
parameters precede parameters with defaults. A default is inserted as written;
it does not expand references to other parameters.[^defaults]

## Substitution

A matching identifier in the body is replaced by the argument's tokens. Other
tokens stay as written. Pass `5` to `adc #k`: the body supplies `#`, so passing
`#5` produces invalid `##5`.

An invocation label names the start of the expanded body, including an empty
body.[^label] Each invocation gives body-defined labels and `.equ` names unique
names. Caller arguments keep their spelling.

```asm
.macro countdown
.loop:  dex
        bne .loop
.endm

start:  countdown
        countdown   ; each .loop is distinct
```

## Conditional and nested macros

A macro body can contain balanced [conditional blocks](conditionals.md).
A macro can be defined in a kept `.if` branch. Nested invocations work up to
`*max-macro-depth*` (256 by default). Recursive macros need a terminating
condition; unbounded recursion signals `macro-error`.

```asm
.macro rep n
.if n > 0
    nop
    rep n-1
.endif
.endm
rep 3
```

A [forced addressing mode](modes.md#forcing-a-mode-with-a-mnemonic-suffix) can
appear in the body. A suffix on the macro invocation itself signals
`macro-error`.

## Errors

`macro-error` covers malformed definitions, duplicate names, wrong argument
counts, nested definitions, and excessive expansion depth. Errors in expanded
statements report the invocation and the body line.[^source]

[^defaults]: Arguments are comma-separated expressions. Defaults are token runs,
    so they can contain expressions such as `1 + 2`.
[^label]: An invocation label is emitted before the body. A local invocation
    label uses the scope at the call site, even if the body starts with a global
    label.
[^source]: Listings and symbols use the outermost invocation's source line.
    The body definition location (`symbol-info-definition-file` and
    `-definition-line`) is retained separately for diagnostics and symbols.
