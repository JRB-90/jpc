ECHO OFF
iverilog -v -o J6502_System.vvp -I ..\rtl -I ..\rtl\6502 ..\rtl\J6502_System.v
vvp J6502_System.vvp
PAUSE