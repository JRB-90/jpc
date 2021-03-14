ECHO OFF
ECHO Beginning build
ca65 ..\src\main.asm -o jos.o
ld65 -C ..\src\jos.cfg jos.o -o jos.bin
ECHO Compile and linking OK
.\BinToMif.exe jos.bin jos.mif
xcopy /y jos.mif ..\..\hdl\rtl\memory\jos.mif
ECHO Copied memory init file to project
ECHO Built successfully./
PAUSE