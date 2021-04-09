ECHO OFF
ECHO Beginning build

ca65 -D cbmbasic2 ..\src\main.asm -o jos.o
IF %ERRORLEVEL% NEQ 0 (
  ECHO Failed to compile
  EXIT ERROR
)

ld65 -C ..\src\jos.cfg jos.o -o jos.bin
IF %ERRORLEVEL% NEQ 0 (
  ECHO Failed to link
  EXIT ERROR
)

ECHO Compile and linking OK

.\BinToMif.exe jos.bin jos.mif
IF %ERRORLEVEL% NEQ 0 (
  ECHO Failed to convert binary
  EXIT ERROR
)

xcopy /y jos.mif ..\..\hdl\rtl\memory\jos.mif

ECHO Copied memory init file to project
ECHO Built successfully