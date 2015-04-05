# Introduction #

The bootloader is a small piece of software that can be flashed onto the ESC MCU. It allows you to upload firmware to the board without external ISP programmer. The Bootloaders used with ESC's usually communicates with host computer using single-wire communication protocol, which allows updating firmware, using only existing servo connector.

When you reset the ESC board, it runs the bootloader (if present). The bootloader then waits a few moments for commands or data to arrive from the the computer. Usually, this is a firmware that the bootloader writes to the flash memory on the ATmega8 chip. A few seconds later, the bootloader launches the newly-uploaded program. If no data arrives from the computer, the bootloader launches whatever program was last uploaded onto the chip. If the chip is still "virgin" the bootloader is the only program in memory and will start itself again.


# Why are we using a bootloader? #

The use of a bootloader allows us to avoid the use of external ISP  programmers. (Burning the bootloader onto the chip, however, requires one of these external programmers.)

# Not using a bootloader #

To update firmware using bootloader, you need to supply power to the MCU. Without additional arrangements, it is only possible using main battery plug. On other hand, ISP programmers usually supply power to target MCU through ISP connector, which allows flashing without main battery

# Bootloader Activation #

Normally, after reset or power-up ATmega8 MCU starts execution for zero address. Programming of the BOOTRST can change that. This fuse tells the chip to jump to the bootloader section (at higher address) after a reset or power up. There are another 2 fuse bits, related to the bootloader: BOOTSZ0, BOOTSZ1.  Those 2 bits define  bootloader size (address to start). The usual value is 2kb bootloader, which means both bits should be programmed. (Start adress 0x0c00).

# Bootloader types #

There are 2 commonly used bootloaders used with ESC boards:

## AvrootLoader ##

Universal high speed bootloader for AVR microcontrollers. It uses simple 1-wire serial adapter with one resistor on Tx line.

  * Main Site: http://www.mikrocontroller.net/topic/avr-bootloader-mit-verschluesselung
  * Pre-compilled binary: http://wii-esc.googlecode.com/svn/release/_boot/ESCBL1.hex
  * Protocol: Custom-binary


## USB Linker ##

This bootloader was created by Simon.K, author of the RapidESC firmware. It is designed to utilize of-the-shelf "Turnigy USB Linker"

  * Main Site: https://github.com/sim-/tgy
  * Pre-compilled binary: http://wii-esc.googlecode.com/svn/release/_boot/USBLinker.hex
  * Protocol: STK500v2




# Bootloader Interesting facts #

  * Programming BOOTRST is actually safe even without flashing bootloader (in case user program is smaller than 6k). The MCU just NOP-sliding to the user program.
  * Flashing bootloader, but not setting BOOTRST may appear to be working. The MCU starts from 0, but because of clean flash it nop-sliding to the bootloader. But would stop working as soon, as you flashed actual firmware.