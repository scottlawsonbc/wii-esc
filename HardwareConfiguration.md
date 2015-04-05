# Hardware Configuration #

The files in the ./hw directory are used to configure the hardware. In most cases, the standard parameters should work very well. However, it's possible that some ESC/motor/propeller combinations need some fine tuning.

The upper part of this file is used to adapt the hardware to the firmware. It should be left alone, unless there is no suitable hardware file. The interesting part starts with the "Parameters" section.



## Parameters Section ##


#### MOT\_BRAKE ####
**_(yes|no)_** – Enables the motor brake. It's commonly disabled on multi copters.


#### READ\_CALIBRATION ####
**_(yes|no)_** – On boards without external crystal, the internal clock source (8 MHz) must be used. Due to manufacturing tolerances, it's possible to calibrate it with an external program (e.g. PonyProg). The calibration value is stored in the EEPROM and can be used by this firmware. It should be disabled on boards with external resonator or when OVERCLOCK is defined.


#### OVERCLOCK ####
**_(yes|no)_** – The calibration register can also be used to over clock the processor. This is for boards without external crystal and makes the internal clock to run at about 16 MHz. It should to be disabled on boards with external resonator or when READ\_CALIBRATION is defined.

It is not recommended to use this option with ArduinoPWM input method (or should carefully be tested on the ground) because this method has low tolerance margins and the overclocked RC generator working on 16Mhz has significant variations. The usual symptoms are:
  * Esc not arming.
  * Stops at full throttle (it is hard to diagnose with MWC as it reserves some throttle, it is better to use simple arduino sketch to test it).
Also over clocked RC oscillator has lower thermal stability.


#### F\_CPU ####
**_(8000000|16000000 `[`Hz`]`)_** – This parameter must be set to the actual system clock in Hz. If OVERCLOCK is defined, this must be set to 160000000.


#### HIGH\_SIDE\_PWM ####
**_(yes|no)_** – Generate PWM on high side switches. This mode can be used with boards with nFETs on high side and BEMF circuit without virtual neural point (neural point is the half of the bus voltage). Currently there are no commercially available boards with such schematic.


#### COMP\_PWM ####
**_(yes|no)_** – Generate PWM on both sides symmetrically (complimentary PWM, active rectification, active free run).

This mode has the following advantages:
  * The current decays not through freewheeling diode but through FET. Which usually means less heat and higher efficiency.
  * The motor is actively breaking
It should only be used on all nFET boards.


#### CHARGE\_BOOTSTRAP ####
**_(yes|no)_** – For boards with nFETs on high side, it is necessary to pre-charge bootstrap capacitors before we can control high side switches. This should only be used on all nFET boards with charge pump on high side.



## Power Settings Section ##


#### PCT\_PWR\_MIN ####
**_(0-100 `[`%`]`)_** – Sets the minimal power (pulse length) needed to start the motor. If the pulse length is below this value, the motor is always switched off. It can be used to configure idle throttle.

Example: With MIN\_RC\_PULS=1100 (input configuration) the pulse length used to calculate the throttle is 1900us – 1110us = 800us. If  PCT\_PWR\_MIN=5, the pulse length must be above 1100us + 800 `*` 0,05 = 1140us to start the motor.

Warning: It is not recommended to drop it lower than 5%.



## Power Curve Section ##


#### PWR\_CURVE\_POINT ####
**_(01|02, 400-20,000 `[`1/min`]`, 0-100 `[`%`]`)_** – This limits the power in dependence of the electrical rotations. It's used to reduce false zero crossing detections and for preventing the ESC from destruction due to unnatural throttle jumps. Currently there are two curve points supported.

Example: PWR\_CURVE\_POINT(01, 3500, 25) limits the power to 25% as long as the electrical rotations are below 3500 per minute.



## Start-up Settings Section ##

#### RPM\_STEP\_INITIAL ####
**_(200-10,000 `[`1/min`]`)_** – Initially when there is no back EMF, it starts commutation with RPM\_STEP\_INITIAL speed and accelerates up to RPM\_STEP\_MAX. For a good start it's need to provide enough power to rotate motor (with load) at least at RPM\_STEP\_INITIAL and the motor should be able to produce detectable back EMF somewhere inside this range.


#### RPM\_STEP\_MAX ####
**_(200-10,000 `[`1/min`]`)_** – See RPM\_STEP\_INITIAL.


#### PCT\_PWR\_STARTUP ####
**_(0-100 `[`%`]`)_** – Minimal power used for bringing the motor from starting into running mode. This can be increased if the motor struggles during start-up.


#### PCT\_PWR\_MAX\_STARTUP ####
**_(0-100 `[`%`]`)_** – Maximal power used for bringing the motor from starting into running mode. This can be increased if the motor struggles during start-up. It must be enough to rotate faster than RPM\_START\_MIN\_RPM.


#### RPM\_START\_MIN\_RPM ####
**_(400-20,000 `[`1/min`]`)_** – Minimum electrical rotation speed for the transition from starting into running mode. If PCT\_PWR\_MAX\_STARTUP is to low and/or RPM\_START\_MIN\_RPM to high the motor will never switch into running mode.


#### ENOUGH\_GOODIES ####
**_(1-255 `[`-`]`)_** – Number of successful electrical rotations before checking RPM\_START\_MIN\_RPM for the transition from starting into running mode.



## Run Settings Section ##

#### RPM\_RUN\_MIN\_RPM ####
**_(400-20,000 `[`1/min`]`)_** – Below this amount of electrical rotations per minute the firmware exits from running mode. There are two cases:
  * If current power is lower than PCT\_PWR\_MAX\_STARTUP it goes into starting mode.
  * If higher, it resets (most probably motor stalled).



## General Notes ##

  * All RPM parameters are electrical RPM. This is the equation: RPM<sub>electrical</sub> = (RPM<sub>mechanical</sub> `*` poles) / 2. The parameters are evaluated only if motor RPM is lower than 20,000. This is done to minimize computation time on high RPM.
  * The firmware assumes that RUN\_MIN\_RPM < RPM\_RUN\_RANGE\_01 < RPM\_RUN\_RANGE\_02.
  * Setting RPM\_RUN\_RANGE\_01 and/or RPM\_RUN\_RANGE\_02 too high may affect flying performance.
  * The RPM\_STEP values are evaluated using 17bit software timer, that means that they have minimum limit in ~200. All other RPM values are evaluated using 16bit AVR timer, so the minimum limit is ~400.
  * The option yes/no means #define/#undef.