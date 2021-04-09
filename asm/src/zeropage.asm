; This file contains all zeropage variable reservations

;================= JASIC ZP =================

;.include "jasiczp.asm"

;================== JOS ZP ==================

.org $00E2

CursorPos:
    .res 2

; Screen buffer variables
CursorCol:
    .res 1
CursorRow:
    .res 1
CurrentChar:
    .res 1

DebugFlag:
    .res 1

; Math routine variables
FAC1:
    .res 1
FAC2:
    .res 1

; Timer variables
Timer2_Exp:
	.res 1

; Placeholders
Placeholder1:
    .res 1
Placeholder2:
    .res 1

TempReg1:
	.res 1
CharToPrint:
	.res 1