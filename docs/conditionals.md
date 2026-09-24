# Conditional assembly

`.if`, `.elseif`, `.else` and `.endif` include or skip a block of statements
based on a constant expression. Like [`.macro`](macros.md) and
[`.include`](includes.md), they span a range of statements, so they are
handled by `preprocess` (`preprocess.lisp`) rather than `defdirective`.

```asm
.equ debug, 1

.if debug && size > 2
    nop
.elseif debug
    nop
    nop
.else
    hlt
.endif
```

## Syntax

| Line | Meaning |
|---|---|
| `.if expr` | start a block; the branch is kept when `expr` is nonzero |
| `.elseif expr` | kept when no earlier branch was and `expr` is nonzero |
| `.else` | kept when no earlier branch was |
| `.endif` | end the block |

Conditions use the full [expression syntax](parser.md), including the
comparison and logical operators. Directive names match case-insensitively.
`.if` and `.elseif` take exactly one expression; a mode suffix is an error.
Blocks nest. A label on a conditional line binds as a label-only statement
when the enclosing region is kept.

## Evaluation

Conditions fold before layout, against the constants defined above them:
`.equ`, `.set` and `name = value`. A `.set` value is the one in effect at the
condition. A condition cannot use a label, `*` or `bank(...)`, or a constant
built from them; these signal `conditional-error`, as does a name that is not
defined above.

A skipped branch is never evaluated, so it may refer to anything. Within a
kept condition, `&&` and `||` short-circuit, so `0 && undefined` is fine.

## Macros and includes

Includes, macros and conditionals resolve in one pass, in source order. A
skipped branch is never interpreted: an `.include` in it is not read (the file
need not exist), a `.macro` in it is not defined, and an invocation in it is
not checked. A kept branch may contain either.

- A `.if` block and its `.endif` must be in the same macro body or the same
  source file.
- A `.if` inside a macro body sees the substituted arguments.
- A macro must be defined above its first invocation.
