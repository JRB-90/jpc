.feature force_range
.debuginfo +
.setcpu "65c02"
.macpack longbranch
.list on

; Defines to go here
VIA1       	    =	$6000       ; The base address of the 6522 Versatile Interface Adapter is $A000.
V1_PB           = 	VIA1        ; Its port B is at that address.
V1_PA           =   VIA1+1      ; Its port A is at address $6001.
V1_DDRB         =   VIA1+2      ; Its data-direction register for port B is at $6002.
V1_DDRA         =   VIA1+3      ; Its data-direction register for port A is at $6003.
V1_T1CL         =   VIA1+4      ; Its timer-1 latch's low  byte is at $6004.
V1_T1CH         =   VIA1+5      ; Its timer-1 latch's high byte is at $6005.
V1_T1LL         =   VIA1+6      ; Its timer-1 counter's low  byte is at $6006.
V1_T1LH         =   VIA1+7      ; Its timer-1 counter's high byte is at $6007.
V1_T2CL         =   VIA1+8      ; Its timer-2 counter's low  byte is at $6008.
V1_T2CH         =   VIA1+9      ; Its timer-2 counter's high byte is at $6009.
V1_SR           =   VIA1+10     ; The shift register is at $600A.
V1_ACR          =   VIA1+11     ; The auxiliary  control register is at $600B.
V1_PCR          =   VIA1+12     ; The peripheral control register is at $600C.
V1_IFR          =   VIA1+13     ; The interrupt  flag  register is at $600D.
V1_IER          =   VIA1+14     ; The interrupt enable register is at $600E.
V1_PANH         =   VIA1+15     ; Same as PA except without handshaking

ACIA1           =   $5000       ; Base address of 6551 ACIA uart chip
A1_DATA         =   ACIA1       ; Transmit/Recieve buffer and data register
A1_STATUS       =   ACIA1+1     ; Status register or reset if write
A1_COMMAND      =   ACIA1+2     ; Command register
A1_CONTROL      =   ACIA1+3     ; Control register

VRAM            =   $2000       ; Base address for VRAM

.feature org_per_seg
.segment "ZEROPAGE"
.zeropage
.org $0000

.include "zeropage.asm"

; Main code
.segment "CODE"
.org $8000

Main:
    ;JMP COLD_START
    JMP StartJasic

    JMP Main

HandleVIA1Interupt:
    LDA V1_T2CL             ; Reset Timer1 interupt flag
    LDA #$01
    STA Timer2_Exp
    RTS

HandleACIA1Interupt:
    LDA A1_DATA
    STA A1_DATA
    RTS

SetupVIA1:
    LDA #$00                ; Reset time1 counter
    STA Timer2_Exp
    STA V1_DDRB             ; Set all of Port B to inputs
    LDA #$FF                ; Set all of Port A to outputs
    STA V1_DDRA
    LDA #$AA
    STA V1_PA
    LDA #%00000000          ; Set Timer 2 to one shot mode
    STA V1_ACR
    LDA #%10100000          ; Set up interupt enables
    STA V1_IER
    RTS

SetupACIA1:
    STA A1_STATUS           ; Software reset
    LDA #%00001011          ; Enable DTR, disable receive IRQ, disable transmit IRQ
    STA A1_COMMAND          ; Disable echo mode, disable parity
    LDA #%00011111          ; Setup intenal divider Baud control @ 19,200 Baud
    STA A1_CONTROL          ; 1 Stop Bit, 8 Data Bits
    RTS

Reset:
    LDX #$FF
    TXS                     ; Set stack pointer
    LDA #$00                ; Reset count
    STA Timer2_Exp
    STA CurrentChar
    STA CursorCol
    STA CursorRow
    JSR CalcNewCursorPos
    JSR ClearScreen
    CLI                     ; enable interupts

    JSR SetupVIA1
    JSR SetupACIA1
    JSR Wait50ms
    
    JMP Main

IRQ:
    PHA                     ; Store registers on the stack
    PHX
    PHY
CheckVIA1:
    LDA V1_IFR
    ;AND #%10000000
    ;BNE CheckACIA1
    JSR HandleVIA1Interupt
CheckACIA1:
    LDA A1_STATUS
    ;AND #%10000000
    ; BNE InteruptsDone
    ; JSR HandleACIA1Interupt
InteruptsDone:
    PLY                     ; Restore registers from stack
    PLX
    PLA
    RTI

NMI:
    RTI

.include "math.asm"
.include "video.asm"
.include "jasic.asm"
.include "time.asm"

; Interupt vectors
.segment "VECTORS"
.org $FFFA
	.word	NMI
	.word	Reset
	.word	IRQ
