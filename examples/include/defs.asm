; Shared constants and macros, pulled in by main.asm.
iterations = 10

.macro countdown n
        ldx #n
.loop:  dex
        bne .loop
.endm
