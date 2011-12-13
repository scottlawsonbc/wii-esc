.include "common.inc"

#define Typ "Qynx 20a"

;*********************
; PORT B definitions *
;*********************
#define    CpFET           1      ; Aansturing FET plus
#define    BpFET           2      ; Aansturing FET plus
#define    ApFET           3      ; Aansturing FET plus

#define    INIT_PB         (1<<ApFET) + (1<<BpFET) + (1<< CpFET)
#define    DIR_PB          (1<<ApFET) + (1<<BpFET) + (1<< CpFET)
#define    BRAKE_PB        0

;*********************
; PORT C definitions *
;*********************
#define    AnFET           0      ; Aansturing FET massa
#define    BnFET           1      ; Aansturing FET massa 
#define    CnFET           2      ; Aansturing FET massa

#define    INIT_PC         0
#define    DIR_PC          (1<<AnFET) + (1<<BnFET) + (1<< CnFET)
#define    BRAKE_PC        (1<<AnFET) + (1<<BnFET) + (1<< CnFET)

;*********************
; PORT D definitions *
;*********************  
#define    DbgLED          2      ; LED
#define    rcp_in          3      ; INT1 PPM input
#define    c_comp          6

#define    INIT_PD         (1<<DbgLED) ; Led staat uit
#define    DIR_PD          (1<<DbgLED)
#define    BRAKE_PD        0

#define    DbgLEDOn        sbi     PORTD, DbgLED
#define    DbgLEDOff       cbi     PORTD, DbgLED

;*********************
; ADC definitions    *
;*********************
#define    mux_a           3      ;
#define    mux_b           4      ;
#define    mux_c           5      ;

;*********************
; FET Control        *
;*********************
#define ApFET_on           cbi     PORTB, ApFET
#define ApFET_off          sbi     PORTB, ApFET
#define AnFET_on           sbi     PORTC, AnFET
#define AnFET_off          cbi     PORTC, AnFET

#define BpFET_on           cbi     PORTB, BpFET
#define BpFET_off          sbi     PORTB, BpFET
#define BnFET_on           sbi     PORTC, BnFET
#define BnFET_off          cbi     PORTC, BnFET

#define CpFET_on           cbi     PORTB, CpFET
#define CpFET_off          sbi     PORTB, CpFET
#define CnFET_on           sbi     PORTC, CnFET
#define CnFET_off          cbi     PORTC, CnFET

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
#define    PCT_PWR_STARTUP     14
#define    PCT_PWR_MAX_STARTUP 20
#define    RPM_START_MIN_RPM   4200
#define    ENOUGH_GOODIES      20

;*************************
; Run settings           *
;*************************
#define    RPM_RUN_MIN_RPM     3200
