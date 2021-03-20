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
    LDA Timer1_Count
    CMP #$C8
    BCC Not1Sec
    INC Output_Leds
    LDA #$00
    STA Timer1_Count
    LDA #$4A
    STA A1_DATA
Not1Sec:
    LDA Output_Leds
    STA V1_PA

    JSR ProcessAscii
    INC CurrentChar

    JMP Main

SetupVIA1:
    LDA #$00                ; Reset time1 counter
    STA Timer1_Count
    LDA #$FF                ; Set all of Port A to outputs
    STA V1_DDRA
    LDA #%01000000          ; Set Timer 1 to free running mode
    STA V1_ACR
    LDA #%11000000          ; Set up interupt enables
    STA V1_IER
    LDA #$50                ; Load into low and high T1 counter 50,000
    STA V1_T1CL             ; This will create interupts every 50ms
    LDA #$C3
    STA V1_T1CH
    RTS

SetupACIA1:
    STA A1_STATUS
    LDA #%00011011
    STA A1_COMMAND
    LDA #%00011110
    STA A1_CONTROL
    RTS

Reset:
    LDX #$FF
    TXS                     ; Set stack pointer
    LDA #$00                ; Reset count
    STA Timer1_Count
    STA Output_Leds
    STA CurrentChar
    STA CursorCol
    STA CursorRow
    JSR CalcNewCursorPos
    JSR SetupVIA1
    JSR SetupACIA1
    CLI                     ; enable interupts
    JMP Main

IRQ:
    PHA                     ; Store registers on the stack
    PHX
    PHY
    LDA V1_T1CL             ; Reset Timer1 interupt flag
    INC Timer1_Count
    PLY                     ; Restore registers from stack
    PLX
    PLA
    RTI

NMI:
    RTI

.include "math.asm"
.include "video.asm"

; Interupt vectors
.segment "VECTORS"
.org $FFFA
	.word	NMI
	.word	Reset
	.word	IRQ
