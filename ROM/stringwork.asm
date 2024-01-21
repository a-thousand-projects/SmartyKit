
PRINTSTR:
        ; Address of string in  lda,ldy
        ldx #0

PRINTLOOP:
        ;lda (strPtrLo), y
        lda WELCOME,x
        beq PRINTFIN ; If is Zero then Finished
wt:     BIT DSP         ; bit (B7) cleared yet?
        BMI wt        ; No, wait for display.
        sta DSP
        inx
        jmp PRINTLOOP
PRINTFIN:
        jmp RESET
WELCOME:     
        .byte "G-Monitor",$0