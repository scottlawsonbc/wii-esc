.include "common.inc"

#define Typ "Plush 6a"

;*********************
; PORT B definitions *
;*********************
;#define                7     
;#define                6     
#define  DbgLED         5;    ;o (sck stk200 interface) 
;#define                4        (miso stk200 interface)
;#define                3        (mosi stk200 interface) 
#define  AnFET          2     ;o
#define  CnFET          1     ;o
#define  BnFET          0     ;o

#define  INIT_PB        0
#define  DIR_PB         (1<<AnFET)+(1<<BnFET)+(1<<CnFET)+(1<<DbgLED)
#define  BRAKE_PB       (1<<AnFET)+(1<<BnFET)+(1<<CnFET)

#define DbgLEDOn        sbi     PORTB, DbgLED
#define DbgLEDOff       cbi     PORTB, DbgLED

;*********************
; PORT C definitions *
;*********************
#define INIT_PC         0
#define DIR_PC          0
#define BRAKE_PC        0

;*********************
; PORT D definitions *
;*********************  
;#define                7     ;i <plus> comparator input (AIN0)
#define c_comp          6     ;i common comparator input (AIN0)
#define ApFET           5     ;o
#define CpFET           4     ;o
#define BpFET           3     ;o
#define rcp_in          2     ;i r/c pulse input

#define INIT_PD         0
#define DIR_PD          (1<<ApFET)+(1<<BpFET)+(1<<CpFET)
#define BRAKE_PD        0

;*********************
; ADC definitions    *
;*********************
#define mux_a           5     ; ADC5 phase input
#define mux_c           4     ; ADC4 phase input
;#define                3     ; ADC3
#define mux_b           2     ; ADC2
#define temp_adc        1     ; ADC1 temperature control input 
#define accu_adc        0     ; ADC0 voltage control input 

;*********************
; FET Control        *
;*********************
#define ApFET_on        sbi     PORTD, ApFET
#define ApFET_off       cbi     PORTD, ApFET
#define AnFET_on        sbi     PORTB, AnFET
#define AnFET_off       cbi     PORTB, AnFET

#define BpFET_on        sbi     PORTD, BpFET
#define BpFET_off       cbi     PORTD, BpFET
#define BnFET_on        sbi     PORTB, BnFET
#define BnFET_off       cbi     PORTB, BnFET

#define CpFET_on        sbi     PORTD, CpFET
#define CpFET_off       cbi     PORTD, CpFET
#define CnFET_on        sbi     PORTB, CnFET
#define CnFET_off       cbi     PORTB, CnFET

;*************************
; Comparator definitions *
;*************************
.macro AcInit
  ACMultiplexed
.endmacro

.macro AcPhaseA
  ACChannel mux_a
.endmacro

.macro AcPhaseB
  ACChannel mux_b
.endmacro

.macro AcPhaseC
  ACChannel mux_c
.endmacro

;*************************
; Parameters             *
;*************************
#undef     MOT_BRAKE      
#undef     READ_CALIBRATION
#define    F_CPU               8000000           ; 8Mhz internal RC oscilator 
#define    TIMING_ADV          0
           
;*************************
; Power settings         *
;*************************
#define    PCT_PWR_MIN         3                 ; Min power 3%

;*************************
; Power curve            *
;*************************
PWR_CURVE_POINT(01, 3500, 25)
PWR_CURVE_POINT(02, 7000, 50)

;*************************
; Startup settings       *
;*************************
#define    RPM_STEP_INITIAL    90
#define    RPM_STEP_MAX        180
#define    PCT_PWR_STARTUP     14
#define    PCT_PWR_MAX_STARTUP 20
#define    RPM_START_MIN_RPM   4000
#define    ENOUGH_GOODIES      20

;*************************
; Run settings           *
;*************************
#define    RPM_RUN_MIN_RPM     3000


