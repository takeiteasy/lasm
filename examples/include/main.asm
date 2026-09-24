.include "defs.asm"
.include "defs.asm"             ; skipped by the guard, no duplicate macro

.ifndef iterations
.error "defs.asm must define iterations"
.endif

start:  countdown iterations
        sta $1000
        hlt
end:

.assert end <= $20, "program too large"
