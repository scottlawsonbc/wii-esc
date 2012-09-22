@ECHO OFF

SET AS="%ProgramFiles%\AvrAssembler2\avrasm2.exe"
SET AS_FLAGS= -I "%ProgramFiles%\AvrAssembler2\AppNotes"
SET OUT=bin

%AS% %AS_FLAGS%  -S "%OUT%\labels.tmp" -fI -W+ie -o "%OUT%\ESCBL1.hex" -d "%OUT%\AVRootloader.obj" -m "%OUT%\AVRootloader.map" -l "%OUT%\AVRootloader.lst" "AVRootloader.asm"
