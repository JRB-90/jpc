WaitTimerExp:
	LDA Timer2_Exp
    BEQ WaitTimerExp
    LDA #$00
    STA Timer2_Exp
	RTS

Wait100us:
    PHA
	LDA #$16
    STA V1_T2CL
    LDA #$00
    STA V1_T2CH
    JSR WaitTimerExp
    PLA
    RTS

Wait150us:
    PHA
	LDA #$46
    STA V1_T2CL
    LDA #$00
    STA V1_T2CH
    JSR WaitTimerExp
    PLA
    RTS

Wait200us:
    PHA
    LDA #$78
    STA V1_T2CL
    LDA #$00
    STA V1_T2CH
    JSR WaitTimerExp
    PLA
    RTS

Wait4500us:
    PHA
	LDA #$92
    STA V1_T2CL
    LDA #$11
    STA V1_T2CH
    JSR WaitTimerExp
    PLA
    RTS

Wait50ms:
    PHA
	LDA #$4E
    STA V1_T2CL
    LDA #$C3
    STA V1_T2CH
    JSR WaitTimerExp
    PLA
    RTS
