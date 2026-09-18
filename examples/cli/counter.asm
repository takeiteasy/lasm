count:  ldx #10         ; x = 10
.loop:  dex             ; x -= 1
        bne .loop       ; loop while x != 0
        sta $1000
        hlt
