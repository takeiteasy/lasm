# Binary output

`write-binary` and `write-intel-hex` write an `assembly`'s cells to a
standalone file for tools outside the Lisp image: an external emulator, a ROM
flasher, a hex viewer.

```lisp
(let ((a (assemble-file "prog.asm" :machine 'sixtyfoo)))
  (write-binary a "prog.bin")
  (write-intel-hex a "prog.hex"))
```

## Cells to bytes

An 8-bit cell is one byte. A wider cell splits into bytes ordered by the
memory element's `:endian` ([Machine model](machine-model.md)): `:little`
writes the low byte at the lower offset. `:machine` (with `:memory` when the
machine has several memory elements) supplies the endianness, or pass `:endian`
directly. A cell wider than 8 bits with neither signals. A memory element with
a grouped order such as `(:big :little 2)` orders the bytes inside each cell by
its inner order (`:little` here).

## Packing

`:packing` chooses how a cell width that is not a multiple of 8 is written.
For 12-bit cells `#xABC #x123`:

| `:packing` | `:endian :big` | `:endian :little` |
|---|---|---|
| `:pad` (default) | `0A BC 01 23` | `BC 0A 23 01` |
| `:bits` | `AB C1 23` | `BC 3A 12` |

- `:pad` zero-extends each cell to `ceil(cell-width / 8)` bytes.
- `:bits` lays the cells end to end as one bitstream, high bit first for
  `:big` and low bit first for `:little`. The last byte is zero-padded.

The two agree when the width is a multiple of 8. `:bits` always needs an
endian source unless the width is 8; `:pad` needs one only for cells wider than
8 bits.[^bits]

Under `:bits` the last byte can hold up to 7 padding bits. Reading a
narrow-cell image back, such as 4-bit cells, decodes that padding as extra zero
cells unless the count is given: `:count N` on `bytes-to-cells`, or
`--cells N` on `lasm disassemble`. `N` must account for every byte, apart from
the padding.

```sh
lasm assemble examples/cli/twelve.asm -m examples/cli/twelve.lisp --packing bits
```

Gaps left by `.org` and `.res` are already zero-filled in the cells, so the
output is one contiguous run starting at the assembly's `origin`.

## Banks

An assembly with [banked output](banked-output.md) writes the main image
followed by each banked region's banks, 0 up to the highest used, each padded
to the region's size. `:bank N` writes only that bank (`:region` names the
region when several have output); Intel HEX records then start at the
region's address.

## Intel HEX

Records carry up to 16 data bytes and are followed by an end-of-file record.
An extended linear address record is emitted wherever the upper 16 address
bits change, so programs past 64K are written correctly. A program extending
past the 32-bit range signals.

HEX addresses count bytes. On a machine with `:cell-width 16` the record
address of a cell is `2 * (cell address)`, so it differs from the address its
labels resolve to. With `:packing :bits` the start is `origin * cell-width / 8`,
and a start that is not a whole byte signals.

## Entry points

```lisp
(assembly-bytes ASSEMBLY &key machine memory endian bank region (packing :pad))
(bytes-to-cells BYTES CELL-WIDTH &key (endian :little) (packing :pad) count)
(write-binary ASSEMBLY PATH &key machine memory endian bank region (packing :pad))
(hex-text ASSEMBLY &key stream machine memory endian bank region (packing :pad))
(write-intel-hex ASSEMBLY PATH &key machine memory endian bank region (packing :pad))
```

`assembly-bytes` returns the byte vector both writers use; `bytes-to-cells` is
its inverse. `write-binary` and `write-intel-hex` replace an existing file and
return `path`. `hex-text` returns the HEX text as a string, or writes it to
`stream` and returns `nil`.

[^bits]: Without `:bank`, the banks are concatenated as cells, so under
    `:bits` a bank boundary falls mid-byte when `region size * cell-width` is
    not a multiple of 8. `bytes-to-cells` accepts up to 7 trailing padding
    bits and signals on a whole leftover byte.
