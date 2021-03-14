.debuginfo +
.setcpu "65c02"
.macpack longbranch
.list on

; Defines to go here

.feature org_per_seg
.segment "ZEROPAGE"
.zeropage
.org $0000

; Zeropage variable reservations to go here

; Main code
.segment "CODE"
.org $8000

Main:
    LDA #$77
    STA $2222
    LDA #$00
    LDA $2222
    STA $3333
    CMP #$77
    BNE NotEq
    NOP
    NOP
    NOP
    NOP
    NOP
NotEq:
    JMP Main

Reset:
    LDX #$FF
    TXS                     ; Set stack pointer
    CLI                     ; enable interupts
    JMP Main

IRQ:
    RTI

NMI:
    RTI

; Interupt vectors
.segment "VECTORS"
.org $FFFA
	.word	NMI
	.word	Reset
	.word	IRQ
