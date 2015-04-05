# Introduction #

The wii-esc firmware is written in assembly language. Any change in hardware and input configuration needs a reassembling of the source code.


# Required Software #

On Windows AvrAssembler2 is used for assembling. Officially it's not a standalone program and only available as a component of Atmel's [AVR Studio](http://www.atmel.com/avrstudio/). Because AVR Studio is a very comprehensive software package, here is a non offical link for the [AvrAssembler2](http://www.vfx.hu/avr/download/avrasm2.zip) standalone version.


# Setting up AvrAssembler2 #

It should be checked if the path of the AvrAssembler2 installation matches the settings in `_`make.cmd:

```
SET AS="%ProgramFiles%\Atmel\AVR Tools\AvrAssembler2\avrasm2.exe"
SET AS_FLAGS= -I "%ProgramFiles%\Atmel\AVR Tools\AvrAssembler2\AppNotes"
```


# Assembling #

Assembling is done with the batch files named after the hardware confguration files. These files execute `_`make.cmd with the selected hardware and input configuration as parameter. For example, this is the content of Plush30A.cmd:

```
call _make.cmd Plush30A pwm_rc_200
```

It assembles a firmware with the hardware configuration file Plush30A.inc and input configuration file pwm\_rc\_200.inc. To use the input configuration file pwm\_fast\_250.inc it must be changed to:

```
call _make.cmd Plush30A  pwm_fast_250
```

This is the output of a succesfull assembly:

```
AVRASM: AVR macro assembler 2.1.42 (build 1796 Sep 15 2009 10:48:36)
Copyright (C) 1995-2009 ATMEL Corporation

[builtin](2): Including file 'C:\Programme\Atmel\AVR Tools\AvrAssembler2\AppNotes\m8def.inc'
[builtin](3): Including file 'core\..\input\pwm_rc_200.inc'
[builtin](4): Including file 'core\..\hw\Plush30A.inc'
core\..\hw\Plush30A.inc(1): Including file 'core\common.inc'
core\bldc.asm(22): Including file 'core\ppm_light.inc'
core\bldc.asm(1348): Including file 'core\str_zc_filter.inc'

ATmega8 memory use summary [bytes]:
Segment   Begin    End      Code   Data   Used    Size   Use%
---------------------------------------------------------------
[.cseg] 0x000000 0x000830   2076     20   2096    8192  25.6%
[.dseg] 0x000060 0x0000d6      0    118    118    1024  11.5%
[.eseg] 0x000000 0x000000      0      0      0     512   0.0%

Assembly complete, 0 errors. 0 warnings
```

The assembled hex files are stored in the ./bin directory and can be flashed as described in the [Firmware Flashing](FirmwareFlashing.md) and BootLoader page.