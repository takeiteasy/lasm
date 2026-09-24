# Assertions

`.assert` and `.error` stop assembly with a message. Both signal
`assertion-error`, a subclass of `assembly-error`.

```asm
.org $8000
    ...
.assert * <= $C000, "program overflows ROM"

.ifndef CONFIG
.error "CONFIG must be defined"
.endif
```

## `.assert expr[, "message"]`

Checks `expr` after layout, so it can use labels, `*` and `bank(...)`. A zero
value signals `assertion-error` with the message, or `assertion failed` when
none is given. `*` is the address of the `.assert` line. A `.set` name has the
value in effect at the line. `defined(name)` tests the final symbol table, so
a name defined anywhere counts.

An `.assert` occupies no address and emits nothing. A label on the line binds
to the current address. A mode suffix is an error, as is a second operand
that is not a quoted string.

## `.error "message"`

Signals `assertion-error` as soon as the line is reached in an emitting
region, so an `.error` in a skipped branch does nothing. It fires before layout,
so an earlier-in-file `.error` is reported ahead of any layout problem. The
operand must be a single quoted string.

Use it in the unsupported branch of a conditional, where `.if` conditions
cannot be checked with `.assert`. See [Conditional assembly](conditionals.md).

## Handling

`assertion-error` carries the line of the directive, or the invocation line
when it is inside a [macro](macros.md). Both directives need a lexer with a
`string-delim`, which the default lexer has.
