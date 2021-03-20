MUL8:           ; A*256 + X = FAC1 * FAC2
    LDA #$00
    LDX #$08
    CLC
m0:
    BCC m1
    CLC
    ADC FAC2
m1:     
    ROR
    ROR FAC1
    DEX
    BPL m0
    LDX FAC1
    RTS