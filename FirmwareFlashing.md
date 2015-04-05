# Introduction #

The wii-esc firmware is flashed with an In-System Programming (ISP) adapter. The Atmel standard ISP connection is a six wire interface:

_Atmel ISP headers (image taken from [th9x](http://code.google.com/p/th9x/wiki/installation_de?wl=en-US#Connecting_the_Th9x_with_the_Programmer) project)_
![http://wii-esc.googlecode.com/svn/wiki/hw/isp_atmel_6_10_pin.jpg](http://wii-esc.googlecode.com/svn/wiki/hw/isp_atmel_6_10_pin.jpg)


There are a lot of different ISP programmers available for Atmel AVR devices. This howto is based on the Atmel AVR910 compatible programming adapter in combination with the software avrdude.



# Boot Loader Consideration #

With some ESCs (HK/Mystery Blue Series), it's possible to use a boot loader. This makes updating the firmware a lot of easier and faster. Once installed, the firmware can be flashed via the servo plug. The usage of the boot loader is described on the BootLoader page. However, the installation of the boot loader also requires an ISP connection once.


# Connecting the ISP adapter #

There are several ways in making an ISP connection. The following pictures are showing some examples:

_Soldered ISP connection_
![http://wii-esc.googlecode.com/svn/wiki/hw/isp_soldered_1.jpg](http://wii-esc.googlecode.com/svn/wiki/hw/isp_soldered_1.jpg)

_Soldered ISP connection (image taken from [rcgroups.com](http://www.rcgroups.com/forums/showthread.php?t=1513678))_
![http://wii-esc.googlecode.com/svn/wiki/hw/isp_nek_soldered_2.jpg](http://wii-esc.googlecode.com/svn/wiki/hw/isp_nek_soldered_2.jpg)

_Needle board_
![http://wii-esc.googlecode.com/svn/wiki/hw/isp_needle_board_1.jpg](http://wii-esc.googlecode.com/svn/wiki/hw/isp_needle_board_1.jpg)

_Clothes pin (image taken from [rcgroups.com](http://www.rcgroups.com/forums/showthread.php?t=1513678))_
![http://wii-esc.googlecode.com/svn/wiki/hw/isp_jonnyc67_clothes_pin_1.jpg](http://wii-esc.googlecode.com/svn/wiki/hw/isp_jonnyc67_clothes_pin_1.jpg)

The way of makeing the connection mostly depends on the availability and arrangement of the ISP pads and if a boot loader can be used for updating. The pin assignment for the ISP connections of some ESCs can be found on the Supported ESCs page.



# Using avrdude #

## 1. Erasing the AVR ##
This command performs a chip erase. All lockbits are cleared and the ROM and EEPROM is erased.
```
avrdude.exe -C avrdude.conf -v -p m8 -P com1 -c avr910 -e 
```

## 2. Flashing ##
This command is used for flashing the firmware or boot loader to the ESC.
```
avrdude.exe -C avrdude.conf -v -p m8 -P com1 -c avr910 -U flash:w:Mystery20A_nFET.hex
```
The example above is for a HK BlueSeries ESC.
```
avrdude.exe -C avrdude.conf -v -p m8 -P com1 -c avr910 -U flash:w:ESCBL.hex
```
The example above is for the boot loader.

## 3. Setting the Fuses ##
This will set the proper fuses. The different fuse settings can be found on the Supported ESCs page.
```
avrdude.exe -C avrdude.conf -v -p m8 -P com1 -c avr910 -U lfuse:w:0x3f:m -U hfuse:w:0xcf:m
```
The example above is for a HK BlueSeries ESC with boot loader installed.



# Testing the ESC #

To make sure everything works as expected, the first power connection after flashing should be done with a current limited power source. For example, this can be a regulated power supply or a 12V / ~20W car bulb in series with the ESC.