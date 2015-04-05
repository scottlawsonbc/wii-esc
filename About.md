# Introduction #
The wii-esc firmware for brushless DC motor controllers written by D. Kayukov is a further development of the well known [quax](http://home.versanet.de/~b-konze) firmware by B. Konze. It was started with the goal to increase the input resolution and optimize the response time for better flight stability of multi copters.

During the development the two different quax versions (Type 17a / 18a) have been merged into one and a lot of additional changes (improved startup, zero crossing filer, complementary PWM...) were committed. Different hardware is now selected with hardware dependent configuration files.
# Project Goals #
  * **Modularity.**
    * Hardware specific configuration files (includes).
    * Input specific includes
  * **Precision**
    * Software PWM generator module is optimised to have linear output
    * Extended PWM range support for Arduino generated PWM.
  * **Reliability.**
    * Improved zero-crossing filter
    * Implemented lost zero-crossing recovery algorithm.
  * **Safety.**
    * Improved motor stall detection
  * **Minimal changes**
    * Usually it is not required to change fuses, cut traces, etc.
# Project Name #
Although it's called wii-esc ([MultiWii](http://www.multiwii.com/) project), this firmware works with every PWM capable flight controller, e.g. [OpenPilot](http://www.openpilot.org) CopterControl.