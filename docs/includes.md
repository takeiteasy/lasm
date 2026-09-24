# Includes

`.include "path"` splices another source file's statements in at that point,
so shared `.equ` constants and `.macro` libraries can live in their own
files. It is not a `defdirective` — like [`.macro`](macros.md), it spans a
range of statements, which no directive action can express — and is handled by
`expand-includes` (`include.lisp`), which `assemble` runs after `parse` and
before macro expansion.

```asm
.include "defs.asm"

start:  countdown iterations
```

Because includes are spliced before macros are collected, an included file can
define `.macro` blocks and `.equ` constants that the including file uses, and
an `.include` inside a `.macro` body is spliced into that body.

See [`examples/include/`](../examples/include/include.lisp) for a runnable
version.

## Syntax

The operand is a single quoted path, so the active lexer must declare a
`string-delim` (the default lexer does). `.include` is matched
case-insensitively; a mode suffix is an error. A label on the line binds to the
address of the first included statement.

## Path resolution

A path resolves against the directory of the file that names it, so a nested
include is relative to its own includer. For source given as a string to
`assemble`, the base is `*default-pathname-defaults*`; `assemble-file` uses the
file's own directory.

## Inside `.if`

An `.include` inside `.if` is always read, even when the branch is skipped;
see [Conditional assembly](conditionals.md#macros-and-includes).

## Nesting and cycles

Includes nest. A file that (directly or indirectly) includes itself signals
`include-error` naming the chain. A file included twice, without a cycle, is
processed twice.

## Conditions

`include-error` (a `lasm-syntax-error`) is signalled for a malformed operand, a
mode suffix, a target that does not exist (carrying the `.include` line), a
circular include, and an `.include` left unexpanded in the statements passed to
`assemble-statements` — call `(expand-includes statements :lexer lexer)` first.
A missing top-level file given to `assemble-file` is the ordinary CL
`file-error`.

## Entry point

```lisp
(expand-includes STATEMENTS &key (lexer 'default))
```

Returns `statements` with every `.include` replaced by the named file's
statements, parsed with `lexer`.

Diagnostics use the file and source line that contain the error. A macro
invocation reports its call location and names its body location separately.
Listings show each included file inline at its `.include` line, including
blank and non-emitting lines; repeated includes appear at each occurrence.
