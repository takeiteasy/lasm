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

An 8-bit cell is one byte. A wider cell splits into `cell-width / 8` bytes,
ordered by the memory element's `:endian` ([Machine model](machine-model.md)):
`:little` writes the low byte at the lower offset. `:machine` (with `:memory`
when the machine has several memory elements) supplies the endianness, or pass
`:endian` directly. A wide cell with neither signals. A memory element with a
grouped order such as `(:big :little 2)` orders the bytes inside each cell by
its inner order (`:little` here).

A `cell-width` that is not a multiple of 8 signals for both formats.

Gaps left by `.org` and `.res` are already zero-filled in the cells, so the
output is one contiguous run starting at the assembly's `origin`.

## Intel HEX

Records carry up to 16 data bytes and are followed by an end-of-file record.
An extended linear address record is emitted wherever the upper 16 address
bits change, so programs past 64K are written correctly. A program extending
past the 32-bit range signals.

HEX addresses count bytes. On a machine with `:cell-width 16` the record
address of a cell is `2 * (cell address)`, so it differs from the address its
labels resolve to.

## Entry points

```lisp
(assembly-bytes ASSEMBLY &key machine memory endian)
(bytes-to-cells BYTES CELL-WIDTH &key (endian :little))
(write-binary ASSEMBLY PATH &key machine memory endian)
(hex-text ASSEMBLY &key stream machine memory endian)
(write-intel-hex ASSEMBLY PATH &key machine memory endian)
```

`assembly-bytes` returns the byte vector both writers use; `bytes-to-cells` is
its inverse. `write-binary` and `write-intel-hex` replace an existing file and
return `path`. `hex-text` returns the HEX text as a string, or writes it to
`stream` and returns `nil`.
