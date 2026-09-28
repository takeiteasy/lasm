# Examples

Each example is an ASDF system depending on `:lasm`, with its own test system.

| System | Front end | Shows |
| --- | --- | --- |
| `:dcpu16` | Lisp DSL | A DCPU-16 v1.7 machine: `defmachine`, operand modes, word-encoded instructions, interrupts and devices. |

## Loading

ASDF must find the system. Any one of these works, from the repository root:

```lisp
(asdf:load-asd #p"examples/dcpu16/dcpu16.asd")

(cl:push #p"examples/dcpu16/" asdf:*central-registry*)

(ql:register-local-projects)   ; when LASM is in quicklisp/local-projects
```

Then load the system:

```lisp
(asdf:load-system :dcpu16)
```

## Testing

```lisp
(asdf:test-system :dcpu16)
```

`(asdf:test-system :lasm)` runs each example's tests too, in a separate Lisp
process.
