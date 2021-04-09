; This file contains all the video/screen interaction routines

; Clears the currently selected row
ClearLine:
    LDA #$00
    STA CursorCol
    JSR CalcNewCursorPos
    LDY #$00
ClearLinePos:
    LDA #$00
    STA (CursorPos), y
    INY
    CPY #$50
    BNE ClearLinePos
    INC CursorRow
    JSR CalcNewCursorPos
    RTS

; Clears the whole screen
ClearScreen:
    LDA #$20
    STA CursorPos+1
    LDA #$00
    STA CursorPos+0
    STA CursorCol
    STA CursorRow
    LDY #$00
ClearScreenPos:
    LDA CurrentChar
    STA (CursorPos), y
    CPY #$FF
    BNE NoRollover
    INC CursorPos+1
NoRollover:
    INY
    LDA CursorPos+1
    CMP #$33
    BNE ClearScreenPos
    LDA #$00
    STA CursorCol
    STA CursorRow
    JSR CalcNewCursorPos
    RTS

; Processes a received character as a dumb terminal would
; Character to process is stored in zero page variable 'CurrentChar'
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
    PHA
    LDA #$00
    STA CurrentChar
    JSR ClearScreen
    PLA
    RTS
ProcessCR:
    LDA #$00
    STA CursorCol
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