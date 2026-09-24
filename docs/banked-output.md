# Banked output

`.bank` places assembled output in a specific bank of a
[banked region](machine-model.md#bank-switching).

```
        nop                 ; main image
        .bank 2
        .org $4000          ; bank 2 of the region at $4000
far:    hlt
        .byte 7, 8
        .org $0100          ; outside any banked region: main image
```

See [`examples/banks.lisp`](../examples/banks.lisp) for a runnable version.

## `.bank`

`.bank N` selects bank `N` for later output. Output whose address lies in a
banked region of the target memory is placed in that bank of the region;
everything else stays in the main image. There is no closing directive.

- `N` must fold to a constant. It must be a valid bank of every region the
  output lands in.
- While a bank is selected, `.org` may move backward and never moves the
  assembly's `origin`.
- A statement may not cross the edge of a banked region. Overlapping output
  in one bank signals `assembly-error`.
- Signals `assembly-error` on a memory with no banked region.

## `bank(label)`

`bank(label)` is an expression operator that folds to the bank a label was
defined in.

```
        bnk #bank(far)      ; switch to the bank holding far
        jmp far
        .bank 2
        .org $4000
far:    hlt
```

The label must lie in a banked region; a main-image label or an
`.equ`/`.set` name signals `assembly-error`. The operator's spelling is the
lexer's [`bank-operator`](lexer.md#clauses) clause.

## `bank(*)`

`bank(*)` folds to the bank in effect at the current address, for code that
needs its own bank without a label.

```
        .bank 2
        .org $4000
        bnk #bank(*)        ; 2
        .byte bank(*)       ; 2
```

The address must lie in a banked region while a bank is selected; otherwise it
signals `assembly-error`.

## Bank images

```lisp
(assembly-banks assembly)                  ; => list of bank-image
(assembly-bank-image assembly region bank) ; => bank-image or nil
```

`assembly-cells` and `assembly-origin` remain the main image. A `bank-image`
holds `region`, `bank`, `origin` (the region's start) and `cells` (the whole
region window, zero-filled where nothing was placed). `assembly-banks` is
ordered by region, then bank.

A label defined under a selected bank records it in `symbol-info-region` and
`symbol-info-bank`. A `listing-line` records where its output went in
`listing-line-region` and `listing-line-bank`; both are `nil` for the main
image.

## Listings and symbols

Banked addresses render as `BB:AAAA` (bank in hex) in
[`listing-text` and `symbols-text`](listing.md).
`(listing-line-at assembly address :region r :bank b)` and
`(assembly-data-regions assembly :region r :bank b)` select one bank's
entries; without them only main-image entries match.

## Output files

`assembly-bytes`, `write-binary`, `hex-text` and `write-intel-hex` write the
physical layout: the main image, then each banked region's banks from 0 to the
highest one used, each padded to the region's size. `:bank N` (with `:region`
when several regions have output) writes that bank alone; Intel HEX records
then start at the region's address. See [Binary output](binary-output.md).

## Loading

`load-program` with such an assembly loads the main image as usual, then fills
every bank image without changing the mapping. The PC comes from the main
image's origin. See [Emulator](emulator.md#load-program).

## Disassembly

```lisp
(disassemble-assembly assembly :machine m :bank n [:region r])
(disassemble-assembly assembly :machine m :bank :all)
```

`:bank n` decodes that bank's image at the region's addresses, over the
addresses its listing entries cover. `:bank :all` returns the main image's
lines followed by every bank image's. Each `disassembly-line` records its
`region` and `bank` (both `nil` for the main image), and `disassembly-text`
starts each bank image with `.bank N` and `.org ADDR`, so the whole program
round-trips as one source. A main-image line may not follow bank lines.

```lisp
(disassembly-text (disassemble-assembly a :machine m :bank :all)
                  :origin (assembly-origin a))
```

Each image uses only the labels defined in it. As in the main image, a label
not on a decoded line start renders as an address.

## Tools

- The CLI writes one bank with `assemble --bank N`; see
  [Command line](cli.md).
- The debugger shows and switches banks; see [Debugger](debugger.md#banks).
