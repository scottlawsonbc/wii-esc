#define _include_ppm_inc_

#define    POWER_RANGE   250
#define    MIN_RC_PULS   18

; falling->rising intervals
#define    MAX_INT_FR    25000
#define    MIN_INT_FR    14

; rising->falling intervals
#define    MAX_INT_RF    2200
#define    MIN_INT_RF    14

.macro EvaluatePWC
                sbrs    flags1, RC_PULS_UPDATED
                rjmp    eval_rc_exit
                lds     temp1, new_rcpuls_l
                lds     temp2, new_rcpuls_h
                cbr     flags1, (1<<RC_PULS_UPDATED) ; rc impuls value is read out
eval_rc_p00:    lsr     temp2   
                ror     temp1
                lsr     temp2   
                ror     temp1
                lsr     temp2   
                ror     temp1
.if CLK_SCALE==2
                lsr     temp2   
                ror     temp1
.endif
                mov     temp3, temp1
                cpi     temp3, POWER_RANGE - 1
                brcs    eval_rc_p10
                ldi     temp3, POWER_RANGE - 1
eval_rc_p10:    
                mov     ZH, temp3
eval_rc_exit:                
                ret
.endmacro
