# Introduction #

This page is collection of answers from different forums and threads. Main intention is to collect knowelege base here and than transform it to proper documentation later on.


## Q: What is the difference (meaning of) RCP\_MAX and RCP\_FULL? ##

  * RCP\_MIN/RCP\_MAX - valid range, everything outsude is rejected
  * RCP\_START - where to start (with min. power)
  * RCP\_FULL - where to have full power
  * RCP\_DEADBAND - deadband for startup

## Q: Do you have a link to compile/flashing instructions for v2 ##

The easiest way to build:
  * Download Code::Blocks (in downloads)
  * Open project wii-esc-ng.cbp
  * Setup path to avr-gcc in Code::Blocks

## Q: I did notice on an MT3506 and a Turnigy/Keda 2213 1050KV motor that the throttle seems to have some bumps, with either normal or complementary PWM. ##

I would guess you have BEMF filter caps still in place. It does not like them any more

## Q: What is Sigma-Delta modulation and how such resolution is archived with /8 timer pre-scaler ##

Generally concept is simple: [http://en.wikipedia.org/wiki/Pulse-density\_modulation](http://en.wikipedia.org/wiki/Pulse-density_modulation). So, there are 1 bit first order SDM. Due the fact that it has integrator, it is not necessary to sample it precisely. It even benefitial to add some noise to the sampling. So SDM generation scheduled as Idle task using prothothreads, without any timer or interrupt. This also allows to naturally sync SDM generation and Analogue comparator sampling. Qantizer value can be any as soon as the integrator not overflowing. So currently all measured range passed directly to the SDM without any transformation, which is ~4000 points with extended range, 1600 with standard one.

Benefits of this approach:
  * Lower switching losses as FET's have enough time to properly open/close (currently min on/off ~4us)
  * More linear power curve by the same reason.
  * Lower noise in working range. Yes, the base frequency drops to 1khz at 99.99% but it is not really critical as commutation noises are much higher there.

## Q: What is done to increase dynamic response of the system? ##

Yes I have 7.5° blanking time and 2 taps FIR instead of IIR for timing. 2 taps FIR still needed because ZC detection in LH and HL transitions is not symmetrical.

## Q: How Can I activate complimentary PWM during compilation? ##

Add "#define COMP\_PWM" in config.h

## Q: hi ziss.. im interested in wii-esc fws.. and would be interested to know what is the downside of wii-esc? ##

Bright side:
  1. up to 4000 steps of resolution.
  1. low noise with comparatively high efficiency (Sigma-delta modulator, instead of fixed frequency PWM)
  1. linear power response. (completely no "bump" at 100%)
  1. Sync recovery.
  1. Safe stall detection.
  1. Complimentary PWM support

dark side:
  1. no EPA, throttle endpoints are fixed.
  1. no reverse
  1. probably lower max RPM.
  1. require removing BEMF capacitors.

## Q: What is better for BS20A wii-esc Complementary or Low side PWM? ##

Short answer:
Complimentary PWM allows faster rotor deceleration, putting energy back to the battery.

Long answer:
This has an interesting
effect of having the motor speed more closely and more quickly track the
duty cycle even without any active braking or closed-loop controlling.