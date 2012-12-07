@ECHO OFF

SET AS="%ProgramFiles%\AvrAssembler2\avrasm2.exe"
SET AS_FLAGS= -I "%ProgramFiles%\AvrAssembler2\AppNotes"
SET OUT=bin

%AS% %AS_FLAGS%  -S "%OUT%\labels.tmp" -fI -W+ie -o "%OUT%\USBLinker.hex" -d "%OUT%\USBLinker.obj" -m "%OUT%\USBLinker.map" -l "%OUT%\USBLinker.lst" "USBLinker.asm"
