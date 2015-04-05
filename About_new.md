# Introduction #
The wii-esc firmware for sensorles brushless DC motor controllers written by D. Kayukov is a further development of the well known [quax](http://home.versanet.de/~b-konze) firmware by B. Konze. It was started with the goal to increase the input resolution and optimize the response time for better flight stability of multi copters.
Initial idea was to resolve couple of issues of original firmware (which looked insignificant at that time) and add some small features. This goal was archived with V 1 release at the end of the 2011. The main highlights:
  * Improved startup
  * "Penalty" based non-linear zero crossing filer (which currently also used projects: RapidESC and BLHeli)
  * Complimentary PWM support
  * Sync recovery.
  * Good power linearity.

# V2 #
V2 started as prototype project for testing of overheads of the "prothothreads": C hackery library for implementing cooperative multi-tasking in restricted environment. Originally I was using spare ESC as an prototyping board to test idea generating PWM without any hardware, except "systick" timer. After couble of successfull tests, I have added commutation routines, then ZC detector and then, sudenlly, I had a pretty much working ESC firmware..

# History #

## August, 2011 ##
Project started as a merge different quax versions (Type 17a / 18a) into one codebase and various optimizations in software PWM generation routines.

## Sep 22, 2011 ##
Project moved to the Google code.

## Sep 30, 2011 ##
After some reports of burned FETs in crashes, timing strategy was reviewed: The ZC timeout window was reduced to the 120 deg, to prevent locking to the PWM harmonics.

## Oct 5, 2011 ##
First version of the Sync recovery algorithm.

## Nov 20, 2011 ##
"Penalty" based non-linear zero crossing filer, dynamically adjusted to the RPM.

## Dec 15, 2011 ##
"Official" V 1.0 release date.


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