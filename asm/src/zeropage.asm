; This file contains all zeropage variable reservations

; LED counter variables
Timer1_Count:
	.res 1
Output_Leds:
    .res 1

; Screen buffer variables
CursorCol:
    .res 1
CursorRow:
    .res 1
CursorPos:
    .res 2
CurrentChar:
    .res 1

; Math routine variables
FAC1:
    .res 1
FAC2:
    .res 1