# About #

This firmware designed as a replacement for many commercially available ESC designs based on the AVR MCU. It implements scalar sensor less method to drive Brushless Motor by detecting BEMF zero-crossing instants. The goal of this project is to create firmware most suitable to use in multi-rotors, using cheap and commercially available hardware.

### Features: ###
  * Fastest possible power response.
  * Up to 4000 steps of resolution.
  * Low noise with comparatively high efficiency (Sigma-delta modulator, instead of fixed frequency PWM)
  * Linear power response. (completely no "bump" at 100%)
  * Jitter-free input PWM measurement without harware assisted input capture.
  * Accepts any PWM update rate
  * Sync recovery.
  * Safe stall detection.
  * Complimentary PWM support (AKA: active freewheeling, active rectification)
  * Fixed throttle end-points. No need to calibrate. (since version 2.0.9 it is also possible to calibrate end-points using stick programming procedure)
  * Automatic oscillator calibration.
  * Enhanced PPM filter, preventing accidental motor startup (when FC is rebooted, for example)
  * Configurable. The configuration parameters are stored in EEPROM. The Wii-ESC flash tool has visual parameters editor. No more stick programming.
  * Modularity. The high-level implementation is separated from actual hardware with HAL layer.
  * Portability. The firmware is written in C++, which means it can be easilly ported to different platform.

### Supported Hardware: ###
For complete mapping betwen targets and real hardware, it is possible to use [RapidESC Database](http://wiki.openpilot.org/display/Doc/RapidESC+Database). Currently tested targets:
  * bs.hex
  * bs\_nfet.hex
  * bs40a.hex
  * kda.hex
  * qynx.hex
  * rb50a.hex
  * rct30nfs.hex
  * rct45nfs.hex
  * tgy.hex
  * tp.hex
  * tp\_nfet.hex
### Latest releases: ###
  * Firmware: **[2.1.1](https://googledrive.com/host/0Bzk5pgkwmev-b3ZLaWdQOEJGWUk/wii-esc-2014-02-20.zip)***** Flash Tool:**[0.7](http://wii-esc.googlecode.com/files/wii_esc_flash_v07_win32.exe) 

### ESC reflashing procedure: ###
  * [Project Wiki](http://code.google.com/p/wii-esc/wiki/FirmwareFlashing)
  * [RCGroups Thread](http://www.rcgroups.com/forums/showthread.php?t=1513678)
  * [{goblin} blog (Russian)](http://forum.rcdesign.ru/blogs/43414/blog15262.html)

For simplified firmware update, it is possible to use the following tools:
  * [KKmulticopter Flash Tool](http://lazyzero.de/kkflashtool) - it supports both Wii-ESC and RapidESC. This tool downloads latest revision of firmware directly from SVN
  * [Wii-ESC Flash Tool](http://code.google.com/p/wii-esc/downloads/list). Simple small and portable flash tool for Win32. It also downloads latest revision from SVN and features visual EEPROM parameters editor.

### FAQ: ###
http://code.google.com/p/wii-esc/wiki/WiiESCv20FAQ

### Results ###

![http://wii-esc.googlecode.com/svn/wiki/Media/BC-3536-11%20750KV_Wii-ESC_SimonK_HK40A-bs_12%20APC.png](http://wii-esc.googlecode.com/svn/wiki/Media/BC-3536-11%20750KV_Wii-ESC_SimonK_HK40A-bs_12%20APC.png)

![http://wii-esc.googlecode.com/svn/wiki/Media/BC-3536-11%20750KV_Wii-ESC_SimonK_HK40A-bs_13%20APC.png](http://wii-esc.googlecode.com/svn/wiki/Media/BC-3536-11%20750KV_Wii-ESC_SimonK_HK40A-bs_13%20APC.png)

<a href='http://www.youtube.com/watch?feature=player_embedded&v=n_g2s1nclUM' target='_blank'><img src='http://img.youtube.com/vi/n_g2s1nclUM/0.jpg' width='425' height=344 /></a>

# Acknowledgments: #
  * [Bernhard Konze](http://home.versanet.de/~b-konze) for original ideas.
  * [h3ifri](http://code.google.com/u/h3ifri) for a great job creating Wiki documentation.
  * [MultiWii Comunity](http://www.multiwii.com/forum/) for support ideas and encouragement!
  * [LazyZero](http://lazyzero.de/kkflashtool) For adding support in "KKmulticopter Flash Tool"
  * [Eugene Mufel](http://forum.rcdesign.ru/member.php?u=119169) for running performance comparision tests

# Project Statistics: #
&lt;wiki:gadget url="http://www.ohloh.net/p/605617/widgets/project\_basic\_stats.xml" height="220" width="355" border="1"/&gt;

# Donations: #

Wii-ESC free to use under the GNU v3.0 License. Feel free to use, copy and modify it as you wish!
I have spent a lot of time to make this software as good as possible. If you feel that this software has been beneficial to you please show your support by donating. This will be greatly appreciated and will help me to continue experements.

[![](https://www.paypalobjects.com/en_AU/i/btn/btn_donate_SM.gif)](https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=ziss%40mail%2eru&lc=AU&item_name=wii%2desc&currency_code=USD&bn=PP%2dDonationsBF%3abtn_donate_SM%2egif%3aNonHosted)