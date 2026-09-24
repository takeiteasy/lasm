; Shared constants and macros, pulled in by main.asm.
.ifndef DEFS_LOADED             ; include guard: safe to include twice
DEFS_LOADED = 1
iterations = 10

.macro countdown n
        ldx #n
.loop:  dex
        bne .loop
.endm
.endif
