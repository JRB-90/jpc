# jpc
6502 Based HDL Computer

Simple computer created in verilog and based on a 6502 cpu and peripherals.

# Memory Map
$0000 -> $00FF  Zero Page
$0100 -> $01FF  Stack
$0200 -> $1FFF  General Purpos RAM
$2000 -> $3FFF  Screen Buffer
$5000 -> $5001  6551 ACIA UART Chip
$6000 -> $600F  6522 VIA I/O Chip
$8000 -> $FFF9  Program ROM
$FFFA -> $FFFF  Interupt Vectors