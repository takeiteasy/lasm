# Listing and source map

An `assembly` keeps its address-to-source mapping and symbol details.

```lisp
(let ((a (assemble source :machine 'sixtyfoo)))
  (print-listing a)
  (print-symbols a))
```

See [`listing.lisp`](../examples/listing.lisp).

## The retained mapping: `assembly-listing` / `assembly-source`

`assembly-listing` contains one entry per instruction, emitted data, or
reserved range, ordered by address. Each entry records address, size,
source file and line, kind, and the chosen instruction descriptor when
applicable. `.org`, `.equ`, and `.set` occupy no cells and add no entry.

`assembly-source` holds source text when known. `assemble-statements`
accepts `:source` for caller-built statements; `assemble` supplies it.

## Lookup

```lisp
(listing-line-at assembly address &key region bank)
(listing-lines-for-source-line assembly line &key file)
```

`listing-line-at` finds the statement occupying an address, or `nil` in a
gap. `:region` and `:bank` select banked output. The reverse lookup returns
a list because a macro invocation or repeated include can emit several
entries for one source line. A listing entry keeps the macro body line as
`listing-line-definition-line`.

### Runtime lookup

```lisp
(machine-listing-line machine address &key memory assembly)
(listing-line-source-text line assembly)
```

`machine-listing-line` returns the entry for an address in a live machine.
It prefers the bank mapped at that address, then the main image, and
defaults `assembly` to `machine-program`. `listing-line-source-text` reads
the entry's line from its own included file when it has one. Runtime
conditions use both; see [Error locations](emulator.md#error-locations).

## `assembly-data-regions`

```lisp
(assembly-data-regions assembly &key region bank)
```

Returns merged, ascending `(start . end)` cell ranges for `.byte`, `.word`,
and `.res`, with exclusive ends. `disassemble-assembly` treats them as data
by default; see [Disassembler](disassembler.md#data-regions).

## Rendering: `listing-text` / `print-listing`

```lisp
(listing-text assembly &key stream)
(print-listing assembly &key stream)
```

A listing shows address, encoded cells, and source:

```text
0000      EA            start: nop
0001      A2 0A         ldx #10
0003      01 02 03      .byte 1,2,3
```

Non-emitting source lines stay visible with empty address and cell columns.
Included files appear after their `.include` line; macro invocations show
each emitted statement. Without `assembly-source`, the listing shows
entries without a source column. Wide cells use enough hex digits for the
machine's cell width.

## Symbol table

`assembly-symbols` maps names to final values. `assembly-symbol-info` adds
each symbol's kind (`:label`, `:equ`, `:set`), scope, readable qualified
name, value, and source line. A local label can share visible spelling with
a global because its scope remains distinct.

```lisp
(assembly-symbol assembly name &key scope)
(assembly-symbols-list assembly &key kind scope)
(assembly-symbol-groups assembly)
```

`assembly-symbol` returns one `symbol-info` or `nil`; use `:scope` to
resolve a local name such as `.loop`. The list function filters by kind or
scope. Groups place top-level symbols first, then each global with its
locals. A `.set` entry holds its last value and assignment line. See
[Assembler](assembler.md#local-label-scoping).

### Rendering: `symbols-text` / `print-symbols`

```lisp
(symbols-text assembly &key stream)
(print-symbols assembly &key stream)
```

The grouped rendering names each symbol, its value, and its kind.
The [disassembler](disassembler.md) uses symbol kind to distinguish labels
from assignments with the same value.

## Limitations

- Address lookup scans the listing linearly. An indexed lookup may help
  larger programs.
