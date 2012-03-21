.include "common.inc"

#define Typ "Mystery 20"


;*********************
; PORT B definitions *
;*********************
;#define                7     
;#define                6     
#define DbgLED          5;    (sck stk200 interface) 
;#define                4     (miso stk200 interface)
;#define                3     (mosi stk200 interface) 
;#define                2     
;#define                1     
#define CnFET           0

#define INIT_PB         0
#define DIR_PB          (1<<CnFET)+(1<<DbgLED)
#define BRAKE_PB        (1<<CnFET)

#define DbgLEDOn        sbi     PORTB, DbgLED
#define DbgLEDOff       cbi     PORTB, DbgLED

;*********************
; PORT C definitions *
;*********************
#define AnRef           1
#define CpFET           3
#define BnFET           4
#define BpFET           5  

#define INIT_PC         (1<<AnRef)
#define DIR_PC          (1<<AnRef)+(1<<CpFET)+(1<<BnFET)+(1<<BpFET)
#define BRAKE_PC        (1<<BnFET)

;*********************
; PORT D definitions *
;*********************  
;#define                7     ;
#define c_comp          6     ;i common comparator input (AIN0)
#define AnFET           5     ;o
#define ApFET           4     ;o
;#define                3     ;
#define rcp_in          2     ;i r/c pulse input
;#define                1     ;
;#define                0     ;

#define INIT_PD         0
#define DIR_PD          (1<<ApFET)+(1<<AnFET)
#define BRAKE_PD        (1<<AnFET)

;*********************
; ADC definitions    *
;*********************
#define mux_c           0     ; ADC0
#define mux_a           6     ; ADC6
#define mux_b           7     ; ADC7
#define ACCU_MUX        2     ; ADC2 voltage control input 

;*********************
; FET Control        *
;*********************
#define ApFET_on        sbi     PORTD, ApFET
#define ApFET_off       cbi     PORTD, ApFET
#define AnFET_on        sbi     PORTD, AnFET
#define AnFET_off       cbi     PORTD, AnFET

#define BpFET_on        sbi     PORTC, BpFET
#define BpFET_off       cbi     PORTC, BpFET
#define BnFET_on        sbi     PORTC, BnFET
#define BnFET_off       cbi     PORTC, BnFET

#define CpFET_on        sbi     PORTC, CpFET
#define CpFET_off       cbi     PORTC, CpFET
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
#define    F_CPU               16000000          ; 16Mhz oscilator 
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
#define    RPM_STEP_INITIAL    155
#define    RPM_STEP_MAX        200
#define    PCT_PWR_STARTUP     18
#define    PCT_PWR_MAX_STARTUP 25
#define    RPM_START_MIN_RPM   4200
#define    ENOUGH_GOODIES      20

;*************************
; Run settings           *
;*************************
#define    RPM_RUN_MIN_RPM     3200
