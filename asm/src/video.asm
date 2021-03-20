; This file contains all the video/screen interaction routines

ProcessNewChar:
    ; Here I need to check for control characters and react
    ; I envision having support for: null (0), backspace (8),
    ; line feed (9), form feed (12), carriage return (13)
    
    LDA CurrentChar
    CMP #$00
    BEQ ProcessNull
    CMP #$08
    BEQ ProcessBKSP
    CMP #$09
    BEQ ProcessLF
    CMP #$0C
    BEQ ProcessFF
    CMP #$0D
    BEQ ProcessCR
    JMP ProcessAscii

ProcessNull:
    JMP ProcessAscii
ProcessBKSP:
    RTS
ProcessLF:
    LDA #$00
    STA CursorCol
    INC CursorRow
    JMP ProcessNewPos
ProcessFF:
    RTS
ProcessCR:
    LDA #$00
    STA CursorCol
    INC CursorRow
    JMP ProcessNewPos
ProcessAscii:
    LDA CurrentChar
    LDX #$00
    STA (CursorPos,x)
    INC CursorCol
    LDA CursorCol
    CMP #$50
    BNE ProcessNewPos
    LDA #$00
    STA CursorCol
    INC CursorRow
    LDA CursorRow
    CMP #$3C
    BNE ProcessNewPos
    LDA #$00
    STA CursorRow
ProcessNewPos:
    JSR CalcNewCursorPos
ProcessNewCharEnd:
    RTS

CalcNewCursorPos:
    CLC
    LDA CursorCol
    CMP #$50
    BCC ColLessLimit
    LDA #$4F
    STA CursorCol
ColLessLimit:
    LDA #$50
    STA FAC1
    LDA CursorRow
    STA FAC2
    JSR MUL8
    STX CursorPos
    ADC #$20
    STA CursorPos+1
    CLC
    TXA
    ADC CursorCol
    STA CursorPos
    BCC CalcNewCursorPosEnd
    INC CursorPos+1
CalcNewCursorPosEnd:
    RTS