ca65 V2.19 - Git 4944c92
Main file   : SmartyKit1_ROM.asm
Current file: SmartyKit1_ROM.asm

000000r 1                         .feature c_comments
000000r 1               /*  SmartyKit 1 - ROM source
000000r 1                *  http://www.smartykit.io/
000000r 1                *  Copyright (C) 2020, Sergey Panarin <sergey@smartykit.io>
000000r 1                *
000000r 1                   This program is free software: you can redistribute it and/or modify
000000r 1                   it under the terms of the GNU General Public License as published by
000000r 1                   the Free Software Foundation, either version 3 of the License, or
000000r 1                   (at your option) any later version.
000000r 1                   This program is distributed in the hope that it will be useful,
000000r 1                   but WITHOUT ANY WARRANTY; without even the implied warranty of
000000r 1                   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
000000r 1                   GNU General Public License for more details.
000000r 1                   You should have received a copy of the GNU General Public License
000000r 1                   along with this program.  If not, see <https://www.gnu.org/licenses/>.
000000r 1               */
000000r 1               
000000r 1                         .setcpu "6502"
000000r 1                         .segment "PICTURE"
000000r 1  88 A8 50 07  Woz:      .byte $88, $a8, $50, $07, $61, $92, $94, $67
000004r 1  61 92 94 67  
000008r 1                       ;  .byte "8*8 Pixel Art picture end", $0d, $00
000008r 1               
000008r 1                         .code
000000r 1  EA                     nop
000001r 1                         .segment "C000"
000000r 1  EA                     nop
000001r 1               
000001r 1                         .segment "E000"
000000r 1                         ;.include "a1basic.asm"
000000r 1                         ;.include "stringwork.asm"
000000r 1               
000000r 1                         .segment "F000"
000000r 1  EA                     nop
000001r 1                         ;Woz face
000001r 1                        ; .include "Apple30th_Woz.asm"
000001r 1               
000001r 1                         .segment "F800"
000000r 1  EA                     nop
000001r 1                         ;Test from Apple-1 Operation Manual – printing all ASCII symbols in a loop
000001r 1                        ; .include "TestFromManual.asm"
000001r 1               
000001r 1                         .segment "FA00"
000000r 1  EA                     nop
000001r 1                         ;Power-On Self Test (POST)
000001r 1                        ; .include "POST.asm"
000001r 1               
000001r 1                          .segment "FC00"
000000r 1  EA                      nop
000001r 1                         ;;printing 8x8 picture in the center with '*'
000001r 1                        ; .include "8x8art.asm"
000001r 1                         .include "stringwork.asm"
000001r 2               
000001r 2               PRINTSTR:
000001r 2                       ; Address of string in  lda,ldy
000001r 2  A2 00                ldx #0
000003r 2               
000003r 2               PRINTLOOP:
000003r 2                       ;lda (strPtrLo), y
000003r 2  BD rr rr             lda WELCOME,x
000006r 2  F0 0C                beq PRINTFIN ; If is Zero then Finished
000008r 2  2C 12 D0     wt:     BIT DSP         ; bit (B7) cleared yet?
00000Br 2  30 FB                BMI wt        ; No, wait for display.
00000Dr 2  8D 12 D0             sta DSP
000010r 2  E8                   inx
000011r 2  4C rr rr             jmp PRINTLOOP
000014r 2               PRINTFIN:
000014r 2  4C 00 FF             jmp RESET
000017r 2               WELCOME:
000017r 2  47 2D 4D 6F          .byte "G-Monitor",$0
00001Br 2  6E 69 74 6F  
00001Fr 2  72 00        
000021r 2               
000021r 1               
000021r 1                         .segment "FD00"
000000r 1                         ;.include "POST.asm"
000000r 1  EA                     nop
000001r 1                         ;Printing 'Hello, World!'
000001r 1                         .include "HelloWorld.asm"
000001r 2               ;printing 'Hello, World!'
000001r 2               ;More about 'Hello, World!' program: https://en.wikipedia.org/wiki/%22Hello,_World!%22_program
000001r 2  A2 00        	LDX #00
000003r 2               PRINT_CHAR:
000003r 2  BD rr rr     	LDA HelloWorldText, X
000006r 2  F0 07        	BEQ END_PRINT	;end printing at the end of the string (\n=0)
000008r 2  20 EF FF     	JSR ECHO
00000Br 2  E8           	INX
00000Cr 2  4C rr rr     	JMP PRINT_CHAR
00000Fr 2               END_PRINT:
00000Fr 2  4C 00 FF     	JMP RESET	;return to Woz Monitor
000012r 2               
000012r 2               HelloWorldText:
000012r 2  0D 48 65 6C  	.byte $0d, "Hello, World!", $0d, "This is SmartyKit 1.", $0d, $0d, $00
000016r 2  6C 6F 2C 20  
00001Ar 2  57 6F 72 6C  
000038r 2               
000038r 1               
000038r 1                         .segment "FF00"
000000r 1                         .include "Woz_Monitor.asm"
000000r 2               ;  The WOZ Monitor for the Apple 1
000000r 2               ;  Written by Steve Wozniak in 1976
000000r 2               
000000r 2               
000000r 2               ; Page 0 Variables
000000r 2               
000000r 2               XAML            = $24           ;  Last "opened" location Low
000000r 2               XAMH            = $25           ;  Last "opened" location High
000000r 2               STL             = $26           ;  Store address Low
000000r 2               STH             = $27           ;  Store address High
000000r 2               L               = $28           ;  Hex value parsing Low
000000r 2               H               = $29           ;  Hex value parsing High
000000r 2               YSAV            = $2A           ;  Used to see if hex value is given
000000r 2               MODE            = $2B           ;  $00=XAM, $7F=STOR, $AE=BLOCK XAM
000000r 2               
000000r 2               
000000r 2               ; Other Variables
000000r 2               
000000r 2               IN              = $0200         ;  Input buffer to $027F
000000r 2               KBD             = $D010         ;  PIA.A keyboard input
000000r 2               KBDCR           = $D011         ;  PIA.A keyboard control register
000000r 2               DSP             = $D012         ;  PIA.B display output register
000000r 2               DSPCR           = $D013         ;  PIA.B display control register
000000r 2               
000000r 2                              .org $FF00
00FF00  2                              .export RESET
00FF00  2               
00FF00  2               
00FF00  2  D8           RESET:          CLD             ; Clear decimal arithmetic mode.
00FF01  2  58                           CLI
00FF02  2  A0 7F                        LDY #$7F        ; Mask for DSP data direction register.
00FF04  2  8C 12 D0                     STY DSP         ; Set it up.
00FF07  2  A9 A7                        LDA #$A7        ; KBD and DSP control register mask.
00FF09  2  8D 11 D0                     STA KBDCR       ; Enable interrupts, set CA1, CB1, for
00FF0C  2  8D 13 D0                     STA DSPCR       ; positive edge sense/output mode.
00FF0F  2  C9 DF        NOTCR:          CMP #'_'+$80    ; "_"?
00FF11  2  F0 13                        BEQ BACKSPACE   ; Yes.
00FF13  2  C9 9B                        CMP #$9B        ; ESC?
00FF15  2  F0 03                        BEQ ESCAPE      ; Yes.
00FF17  2  C8                           INY             ; Advance text index.
00FF18  2  10 0F                        BPL NEXTCHAR    ; Auto ESC if > 127.
00FF1A  2  A9 DC        ESCAPE:         LDA #'\'+$80    ; "\".
00FF1C  2  20 EF FF                     JSR ECHO        ; Output it.
00FF1F  2  A9 8D        GETLINE:        LDA #$8D        ; CR.
00FF21  2  20 EF FF                     JSR ECHO        ; Output it.
00FF24  2  A0 01                        LDY #$01        ; Initialize text index.
00FF26  2  88           BACKSPACE:      DEY             ; Back up text index.
00FF27  2  30 F6                        BMI GETLINE     ; Beyond start of line, reinitialize.
00FF29  2  AD 11 D0     NEXTCHAR:       LDA KBDCR       ; Key ready?
00FF2C  2  10 FB                        BPL NEXTCHAR    ; Loop until ready.
00FF2E  2  AD 10 D0                     LDA KBD         ; Load character. B7 should be �1�.
00FF31  2  99 00 02                     STA IN,Y        ; Add to text buffer.
00FF34  2  20 EF FF                     JSR ECHO        ; Display character.
00FF37  2  C9 8D                        CMP #$8D        ; CR?
00FF39  2  D0 D4                        BNE NOTCR       ; No.
00FF3B  2  A0 FF                        LDY #$FF        ; Reset text index.
00FF3D  2  A9 00                        LDA #$00        ; For XAM mode.
00FF3F  2  AA                           TAX             ; 0->X.
00FF40  2  0A           SETSTOR:        ASL             ; Leaves $7B if setting STOR mode.
00FF41  2  85 2B        SETMODE:        STA MODE        ; $00=XAM $7B=STOR $AE=BLOK XAM
00FF43  2  C8           BLSKIP:         INY             ; Advance text index.
00FF44  2  B9 00 02     NEXTITEM:       LDA IN,Y        ; Get character.
00FF47  2  C9 8D                        CMP #$8D        ; CR?
00FF49  2  F0 D4                        BEQ GETLINE     ; Yes, done this line.
00FF4B  2  C9 AE                        CMP #'.'+$80    ; "."?
00FF4D  2  90 F4                        BCC BLSKIP      ; Skip delimiter.
00FF4F  2  F0 F0                        BEQ SETMODE     ; Yes. Set STOR mode.
00FF51  2  C9 BA                        CMP #':'+$80    ; ":"?
00FF53  2  F0 EB                        BEQ SETSTOR     ; Yes. Set STOR mode.
00FF55  2  C9 D2                        CMP #'R'+$80    ; "R"?
00FF57  2  F0 3B                        BEQ RUN         ; Yes. Run user program.
00FF59  2  86 28                        STX L           ; $00-> L.
00FF5B  2  86 29                        STX H           ; and H.
00FF5D  2  84 2A                        STY YSAV        ; Save Y for comparison.
00FF5F  2  B9 00 02     NEXTHEX:        LDA IN,Y        ; Get character for hex test.
00FF62  2  49 B0                        EOR #$B0        ; Map digits to $0-9.
00FF64  2  C9 0A                        CMP #$0A        ; Digit?
00FF66  2  90 06                        BCC DIG         ; Yes.
00FF68  2  69 88                        ADC #$88        ; Map letter "A"-"F" to $FA-FF.
00FF6A  2  C9 FA                        CMP #$FA        ; Hex letter?
00FF6C  2  90 11                        BCC NOTHEX      ; No, character not hex.
00FF6E  2  0A           DIG:            ASL
00FF6F  2  0A                           ASL             ; Hex digit to MSD of A.
00FF70  2  0A                           ASL
00FF71  2  0A                           ASL
00FF72  2  A2 04                        LDX #$04        ; Shift count.
00FF74  2  0A           HEXSHIFT:       ASL             ; Hex digit left, MSB to carry.
00FF75  2  26 28                        ROL L           ; Rotate into LSD.
00FF77  2  26 29                        ROL H           ;  Rotate into MSD�s.
00FF79  2  CA                           DEX             ; Done 4 shifts?
00FF7A  2  D0 F8                        BNE HEXSHIFT    ; No, loop.
00FF7C  2  C8                           INY             ; Advance text index.
00FF7D  2  D0 E0                        BNE NEXTHEX     ; Always taken. Check next char for hex.
00FF7F  2  C4 2A        NOTHEX:         CPY YSAV        ; Check if L, H empty (no hex digits).
00FF81  2  F0 97                        BEQ ESCAPE      ; Yes, generate ESC sequence.
00FF83  2  24 2B                        BIT MODE        ; Test MODE byte.
00FF85  2  50 10                        BVC NOTSTOR     ;  B6=0 STOR 1 for XAM & BLOCK XAM
00FF87  2  A5 28                        LDA L           ; LSD�s of hex data.
00FF89  2  81 26                        STA (STL,X)     ; Store at current �store index�.
00FF8B  2  E6 26                        INC STL         ; Increment store index.
00FF8D  2  D0 B5                        BNE NEXTITEM    ; Get next item. (no carry).
00FF8F  2  E6 27                        INC STH         ; Add carry to �store index� high order.
00FF91  2  4C 44 FF     TONEXTITEM:     JMP NEXTITEM    ; Get next command item.
00FF94  2  6C 24 00     RUN:            JMP (XAML)      ; Run at current XAM index.
00FF97  2  30 2B        NOTSTOR:        BMI XAMNEXT     ; B7=0 for XAM, 1 for BLOCK XAM.
00FF99  2  A2 02                        LDX #$02        ; Byte count.
00FF9B  2  B5 27        SETADR:         LDA L-1,X       ; Copy hex data to
00FF9D  2  95 25                        STA STL-1,X     ; �store index�.
00FF9F  2  95 23                        STA XAML-1,X    ; And to �XAM index�.
00FFA1  2  CA                           DEX             ; Next of 2 bytes.
00FFA2  2  D0 F7                        BNE SETADR      ; Loop unless X=0.
00FFA4  2  D0 14        NXTPRNT:        BNE PRDATA      ; NE means no address to print.
00FFA6  2  A9 8D                        LDA #$8D        ; CR.
00FFA8  2  20 EF FF                     JSR ECHO        ; Output it.
00FFAB  2  A5 25                        LDA XAMH        ; �Examine index� high-order byte.
00FFAD  2  20 DC FF                     JSR PRBYTE      ; Output it in hex format.
00FFB0  2  A5 24                        LDA XAML        ; Low-order �examine index� byte.
00FFB2  2  20 DC FF                     JSR PRBYTE      ; Output it in hex format.
00FFB5  2  A9 BA                        LDA #':'+$80    ; ":".
00FFB7  2  20 EF FF                     JSR ECHO        ; Output it.
00FFBA  2  A9 A0        PRDATA:         LDA #$A0        ; Blank.
00FFBC  2  20 EF FF                     JSR ECHO        ; Output it.
00FFBF  2  A1 24                        LDA (XAML,X)    ; Get data byte at �examine index�.
00FFC1  2  20 DC FF                     JSR PRBYTE      ; Output it in hex format.
00FFC4  2  86 2B        XAMNEXT:        STX MODE        ; 0->MODE (XAM mode).
00FFC6  2  A5 24                        LDA XAML
00FFC8  2  C5 28                        CMP L           ; Compare �examine index� to hex data.
00FFCA  2  A5 25                        LDA XAMH
00FFCC  2  E5 29                        SBC H
00FFCE  2  B0 C1                        BCS TONEXTITEM  ; Not less, so no more data to output.
00FFD0  2  E6 24                        INC XAML
00FFD2  2  D0 02                        BNE MOD8CHK     ; Increment �examine index�.
00FFD4  2  E6 25                        INC XAMH
00FFD6  2  A5 24        MOD8CHK:        LDA XAML        ; Check low-order �examine index� byte
00FFD8  2  29 07                        AND #$07        ; For MOD 8=0
00FFDA  2  10 C8                        BPL NXTPRNT     ; Always taken.
00FFDC  2  48           PRBYTE:         PHA             ; Save A for LSD.
00FFDD  2  4A                           LSR
00FFDE  2  4A                           LSR
00FFDF  2  4A                           LSR             ; MSD to LSD position.
00FFE0  2  4A                           LSR
00FFE1  2  20 E5 FF                     JSR PRHEX       ; Output hex digit.
00FFE4  2  68                           PLA             ; Restore A.
00FFE5  2  29 0F        PRHEX:          AND #$0F        ; Mask LSD for hex print.
00FFE7  2  09 B0                        ORA #'0'+$80    ; Add "0".
00FFE9  2  C9 BA                        CMP #$BA        ; Digit?
00FFEB  2  90 02                        BCC ECHO        ; Yes, output it.
00FFED  2  69 06                        ADC #$06        ; Add offset for letter.
00FFEF  2  2C 12 D0     ECHO:           BIT DSP         ; bit (B7) cleared yet?
00FFF2  2  30 FB                        BMI ECHO        ; No, wait for display.
00FFF4  2  8D 12 D0                     STA DSP         ; Output character. Sets DA.
00FFF7  2  60                           RTS             ; Return.
00FFF8  2               
00FFF8  2  40            NMI:           RTI             ; simple Interrupt Service Routine(ISR)
00FFF9  2  40            IRQ:           RTI             ; simple Interrupt Service Routine(ISR)
00FFFA  2               
00FFFA  2               
00FFFA  2               
00FFFA  1               
00FFFA  1                         .segment "VECTORS"
00FFFA  1                         ; Interrupt Vectors
00FFFA  1  F8 FF                  .WORD NMI            ; NMI
00FFFC  1  00 FF                  .WORD RESET     ; RESET (starting point in Woz Monitor) or POST (test)
00FFFE  1  F9 FF                  .WORD IRQ            ; BRK/IRQ
010000  1               
010000  1               
010000  1               
