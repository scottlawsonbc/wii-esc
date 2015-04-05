## About ##

The wii-esc firmware for brushless DC motor controllers written by D. Kayukov is a further development of the well known [quax](http://home.versanet.de/~b-konze) firmware by B. Konze. It was started with the goal to increase the input resolution and optimize the response time for better flight stability of multi copters.

During the development the two different quax versions (Type 17a / 18a) have been merged into one and a lot of additional changes (improved startup, zero crossing filer, complementary PWM...) were committed. Different hardware is now selected with hardware dependent configuration files.

Although it's called wii-esc ([MultiWii](http://www.multiwii.com/) project), this firmware works with every PWM capable flight controller, e.g. [OpenPilot](http://www.openpilot.org) CopterControl.


## Getting Started ##

These are the basic steps to update an ESC with the wii-esc firmware:

  1. Identify the ESC ([Supported ESCs](SupportedEscs.md))
  1. Flash the ESC ([Firmware Flashing](FirmwareFlashing.md))
  1. Configure firmware ([User Level Configuration](UserLevelConfig.md))