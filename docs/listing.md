# Listing and source map

`assemble`/`assemble-statements` retain the address↔statement mapping
computed during layout. This page covers listing rendering and address↔line
lookups. See [Assembler](assembler.md) for layout.

```lisp
(let ((a (assemble source :machine 'sixtyfoo)))
  (print-listing a))
```

See [`examples/listing.lisp`](../examples/listing.lisp) for a runnable
version.

## The retained mapping: `assembly-listing` / `assembly-source`

`ASSEMBLY` retains:

- `assembly-listing` — a list of `listing-line`, ascending by address, one
  per address-occupying statement (an instruction, a `.byte`/`.word`, or a
  `.res`; a `.org`, `.equ`, or `.set` occupies no address and contributes none).
- `assembly-source` — the original source string `assemble` was given, or
  `nil` when the `assembly` came from `assemble-statements` called directly
  with no `:source`.

Each listing entry also has `listing-line-file`, the source path for
file-backed input, or `nil` for in-memory source.

```lisp
(defstruct listing-line
  address    ; where this statement starts
  size       ; cells occupied
  line       ; 1-based invocation/source line
  file       ; source path, or nil
  definition-line ; macro body line, or nil
  kind       ; :instruction | :emit | :reserve
  descriptor)  ; the chosen INSTRUCTION-DESCRIPTOR, :instruction only
```

A `listing-line` stores no cells of its own — `listing-text` slices them out
of `assembly-cells` on demand, at `[address - origin, address - origin +
size)`, which is always in bounds even for a trailing `.res`.

`assemble-statements` accepts `:source` directly for a caller building
`statements` by hand; `assemble` passes its own `source` argument through
automatically, so the common path needs nothing extra to get a listing with
source lines attached.

## Lookup

```lisp
(listing-line-at assembly address)             ; => listing-line, or NIL
(listing-lines-for-source-line assembly line &key file) ; => list of listing-line
```

`listing-line-at` is the address→line direction and always returns at most
one entry — an address belongs to at most one statement's
`[address, address + size)` run, or falls in a gap (a forward `.org`'s pad)
and returns `nil`.

`listing-lines-for-source-line` is the reverse direction, and returns a
**list**, because one source line can emit several statements. Expanded
statements use the outermost invocation's line; repeated calls therefore
have distinct lookups. `listing-line-definition-line` gives the body line
that emitted each entry, or `nil` for ordinary statements. Macro definition
lines have no listing entries of their own. With no `:file`, lookup selects
the top-level source. Pass a path to `:file` to select an included file;
repeated includes return all entries from that file and line.

`listing-line-at` is a linear scan over `assembly-listing` — fine at the
program sizes LASM currently targets; an address-indexed structure is a
follow-up if that ever matters.

## `assembly-data-regions`

```lisp
(assembly-data-regions assembly) ; => ((start . end) ...)
```

The cell ranges (`end` exclusive) that `assembly`'s `.byte`, `.word` and
`.res` statements occupy, ascending, with adjacent runs merged. Empty when
the assembly has no listing. [`disassemble-assembly`](disassembler.md#data-regions)
passes these as its `:data-regions` by default.

## Rendering: `listing-text` / `print-listing`

```lisp
(listing-text assembly &key stream)  ; => string, or writes to STREAM
(print-listing assembly &key stream) ; listing-text to *standard-output* by default
```

Three columns: address, encoded cells (hex), and the original source line,
one row per source line when `assembly-source` is present:

```
0000      EA            start: nop
0001      A2 0A         ldx #10
0003      01 02 03      .byte 1,2,3
0006      EA            end: nop
```

A source line with no `listing-line` at all — a comment, a label-only line,
`.org`, `.equ`, `.set` — still renders, with blank address/cells columns, so the
listing is complete rather than silently skipping non-code lines. A macro
invocation line renders once per emitted statement, each with its own address
and cells. The source column shows the invocation as written.
Macro definition lines render with blank address and cells columns.
For file-backed input, the source column carries `path:line | text`.
Included files appear immediately after their `.include` line, recursively,
and each occurrence is rendered separately.

With `assembly-source` absent (`assemble-statements` called with no
`:source`), `listing-text` degrades to an entry-ordered listing with no
source column at all — there is no source text to index into.

The cell hex field is sized from `assembly-cell-width` —
`(ceiling cell-width 4)` digits per cell — so a 16-bit-cell machine
(DCPU-16-shaped, see [Machine model](machine-model.md#cell-width-and-the-assembler))
renders full 4-digit cells rather than truncating to 2. A long cell run (a
sizeable `.res`) is elided to `... ` after the first several cells, to keep
one entry to one line.

## Symbol table

`#37`: `assembly-symbols` (a flat name → value table, see
[Assembler](assembler.md)) mixes plain global names and qualified
`"global.local"` local names in one namespace, and — since `.equ` — plain
labels and computed constants in one set of values, with no way to tell
either apart except guessing from the string or the value. `assembly-symbol-
info` (also on `assembly`, built alongside `assembly-symbols`) tags every
entry with the metadata `assembly-symbols` can't carry, captured once at
bind time rather than recovered afterward:

```lisp
(defstruct symbol-info
  name            ; unqualified spelling, e.g. ".next"
  qualified-name  ; assembly-symbols key, e.g. "loop.next"
  scope           ; enclosing global label's name, or NIL
  kind            ; :label | :equ | :set
  localp
  value           ; same value as assembly-symbols' entry
  line)           ; defining statement's source line
```

A `.set` name appears once, with its final value, kind `:set`, and the last
assignment's source line. Earlier uses retain their source-order values in
the encoded cells.

Query functions built on it:

```lisp
(assembly-symbol assembly name &key scope)       ; => symbol-info, or NIL
(assembly-symbols-list assembly &key kind scope) ; => list of symbol-info
(assembly-symbol-groups assembly)                ; => ((global . locals) ...)
```

`assembly-symbol` looks up a single name, qualifying it against `scope` the
same way the assembler would (so `(assembly-symbol a ".next" :scope "loop")`
finds what `loop: .next:` bound). `assembly-symbols-list` returns every
symbol, optionally filtered to one `kind` (`:label`/`:equ`/`:set`) and/or one
`scope` (pass `nil` for top-level symbols — globals and top-level assignments).
`assembly-symbol-groups` is the grouped view a listing wants: a leading
`(nil . symbols)` bucket for every top-level symbol, then one
`(global-name . symbols)` entry per global that has at least one local, the
global's own `symbol-info` heading its list:

```lisp
(assembly-symbol-groups a)
=> ((nil       . (#<equ bufsize=16>))
    ("start"  . (#<label start=$8000> #<label start.loop=$8003>))
    ("delay"  . (#<label delay=$800a> #<label delay.loop=$800c>)))
```

Both `assembly-symbols-list` and `assembly-symbol-groups` order symbols by
their `symbol-info-line`, then by binding order within a line. An assignment's
value has no address meaning. Symbols defined by macros use the outermost
invocation line; `symbol-info-definition-line` gives their body line.

Every function above degrades to `NIL`/empty rather than erroring when
`assembly-symbol-info` itself is `NIL` (e.g. an `assembly` built by some
other path that never populated it).

### Rendering: `symbols-text` / `print-symbols`

```lisp
(symbols-text assembly &key stream)  ; => string, or writes to STREAM
(print-symbols assembly &key stream) ; symbols-text to *standard-output* by default
```

A grouped dump, parallel to `listing-text`/`print-listing`: top-level symbols
first, then each global with its locals indented underneath, each row naming
the symbol, its value (hex for a label, decimal for an assignment), and its kind.
See [`examples/pc-and-scopes.lisp`](../examples/pc-and-scopes.lisp) for a
runnable version.

This same tagging is what lets the disassembler (see
[Disassembler](disassembler.md)) substitute a real label even when an `.equ`
happens to share its value, instead of the line-start-restricted mitigation
it used before `symbol-info` existed.

## Scope

This covers retaining and rendering the mapping `assemble` already computes
internally, plus the scope/kind-tagged symbol table above. It does not cover:

- **Runtime diagnostics naming a source line** (e.g. a trap or an
  out-of-range access reporting "line 12") — the emulator holds no
  reference to the `assembly` it was loaded from, so wiring one in is a
  separate design decision, tracked as a follow-up.
- **A CLI `--listing` flag** — anticipated by the M7 roadmap once both this
  and [the disassembler](disassembler.md) (#21) exist, not part of this.
