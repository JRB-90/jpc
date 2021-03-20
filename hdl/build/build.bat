ECHO OFF
iverilog -v -o J6502_System.vvp -I ..\rtl -I ..\rtl\6502 -I ..\rtl\6522 -I ..\rtl\6551 -I ..\rtl\video -DFAKE_BRAM ..\rtl\J6502_System.v
vvp J6502_System.vvp
PAUSE