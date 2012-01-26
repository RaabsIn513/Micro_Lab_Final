
;**********************************************************************
;                                                                     *
;    Filename:	Gentry_N_LAB_5_Interrupts.asm                         *
;    Date:        2/14/11                                             *
;    File Version: 1                                                  *
;                                                                     *
;    Author:       Noah Gentry                                        *
;    Company:    University of Cincinnati                             *
;                                                                     *
;                                                                     *
;**********************************************************************
;                                                                     *
;    Notes:   This is a program whose end result is the same as       *
;           Lab 4, but with improved handling of the A2D, Button,     *
;           and TMR0 with interrupts.                                 *
;                                                                     *
;                                                                     *
;                                                                     *
;**********************************************************************

#include <p16F887.inc>
	__CONFIG    _CONFIG1, _LVP_OFF & _FCMEN_OFF & _IESO_OFF & _BOR_OFF & _CPD_OFF & _CP_OFF & _MCLRE_OFF & _PWRTE_ON & _WDT_OFF & _INTRC_OSC_NOCLKOUT
	__CONFIG    _CONFIG2, _WRT_OFF & _BOR21V

     cblock     0x20
;create variables intermediate handling of interrupts
A2D_Average		 ; define a variable to hold the Analog to Digital average
A2D_Current		 ; define variable for current A2D reading
Direction 		 ; define a variable to hold the direction
Flip			 ; degine a variable to hold a value to flip the direction (on a button push)
; define variables, these come from tutorial 11!
Display_Current          ; define a variable to display current
Display_Average	; define a variable to display average
Queue:8          ; 8 bytes to hold last 8 entries
Delay:2          ; counter to limit delay
RunningSum:2     ; sum of last 8 entries
Round:2          ; divide by 8 and round.
temp
     endc

;This comes from tutorial 10, setup the interrupt registers!
; Flag Definitions
     cblock 0x70     ; put these up in unbanked RAM
W_Save
STATUS_Save
     endc
     
     org 0
     goto      Start

; ISR, Interrupt Service Routine, begins here. 
     org 4
ISR:   
     movwf     W_Save              ; Save context
     movf      STATUS,w
     movwf     STATUS_Save 
	 bcf	   STATUS, RP0
	 bcf	   STATUS, RP1

       
; look at push button flag
	 btfsc 	   INTCON,RBIF
	 goto 	   ServicePushButton

; look at Timer0 interrupt flag
     btfsc     INTCON,T0IF
     goto      ServiceTimer0

; look at the A2D interrupt flag
     btfsc     PIR1,ADIF           ; Uncomment to check the ADC (routine needed)
     goto      ServiceA2D

     goto      ExitISR   


; Now we have to setup a service routine for the push button, Timer0, and the A2D. 

; I setup a service routine for the push button, following similar structure as the other service routines
ServicePushButton:
	 bcf	   STATUS, RP0
	 bcf 	   STATUS, RP1

	 btfsc	   Flip,0
	 comf 	   Read
	 comf	   Flip
; 10 microseconds of debounce
	 nop
	 nop
	 nop
	 nop
	 nop
	 nop
	 nop
	 nop
	 nop
	 nop
; here read port, clear interrupt, then go to exitISR
	 movfw	   PORTB
	 bcf 	   INTCON,RBIF
	 goto 	   ExitISR

; setup service routine for Timer0, following tutorial 10
ServiceTimer0:
     bcf       STATUS,RP0          ; Ensure ISR executes in Register Bank 0
     bcf       STATUS,RP1
     bcf       INTCON,T0IF         ; clear the interrupt flag. (must be done in software)

;here insert function to read A2D...
Read:
	 btfsc 	   Read,0
	 goto	   Average
	 goto	   Current

Current:
	 clrf	   A2D_Current 
	 movf	   ADRESH,w			; read the A2D
	 movwf	   A2D_Current	    ; save the current value
	 movwf	   Display_Current	; load current value into display_current
	 call 	   Filter			; send it to filter
	 movwf	   A2D_Average		; save the filtered value
	 goto 	   LoadTMR0
	 
	 
Average:
	 movfw	   A2D_Average		; move average into working
	 movwf	   Display_Average	; 
	 goto 	   LoadTMR0

LoadTMR0:
	 movlw 	   0x00
	 movwf	   TMR0

	 goto 	   ExitISR
ServiceA2D:
	 bcf	   STATUS,RP0
	 bcf	   STATUS,RP1
	 bcf	   PIR1,ADIF
; follow code from tutorial 11 here, then load into A2D_average, which holds the average
	 movf	   ADRESH,w
	 CALL	   Filter
	 movwf	   A2D_AVERAGE
	 bsf	   ADCON0,GO
	
	 goto 	   ExitISR 
;Follow code from tutorial 10, and use this to restore context after the interrupt.
ExitISR:
     movf      STATUS_Save,w       ; Restore context
     movwf     STATUS
     swapf     W_Save,f            ; swapf doesn't affect Status bits, but MOVF would
     swapf     W_Save,w
     retfie

; Setup initiliaze subroutine. We have A LOT of stuff to initialize. 
Init: 
	 bsf       STATUS,RP0     ; select Register Bank 1
	 bcf	   STATUS,RP1
; Setup TMR0, Variable Pot, Push Button, LEDs, and A2D. Follow code from tutorial 9, 10, 11.
 	 movlw     b'00000111'    ; configure Timer0.  Sourced from the Processor clock;
     movwf     OPTION_REG     ; Maximum Prescaler
     movlw     0xFF
     movwf     TRISA          ; Make PortA all input
     movlw     0x01
     movwf     TRISB          ; Make RB0 pin input (switch)
     clrf      TRISD          ; Make PortD all output
; Dark Spot    
	 ;movlw     0x7F           
     movwf	   Display
	 clrf	   Direction
     bsf	   Flip,0		  ; look for zero at the button
   	 
	 bsf	   STATUS,RP0
	 bcf       STATUS,RP1          ; select Register Bank 3
     movlw     0x00                ; Left Justified, Vdd-Vss referenced
	 movwf 	   ADCON1
	 bsf	   STATUS,RP0
	 bsf 	   STATUS,RP1
	 movlw	   0xFF
     movwf     ANSEL
     movlw     0x00
     movwf     ANSELH              ; PortB pins are digitial (important as RB0 is switch)
     bcf       STATUS,RP0          ; back to Register Bank 0
     bcf       STATUS,RP1
     
     movlw     0x41
     movwf     ADCON0         ; configure A2D for Fosc/8, Channel 0 (RA0), and turn on the A2D module  
     
; setup interrupts
	 movlw	   b'11100000'
	 movwf 	   INTCON
	 bsf	   STATUS,RP0
	 bcf	   STATUS,RP1
	 bsf	   IOCB,0		; interrupt on change
	 bsf 	   INTCON,RBIE	; interrupts portB
	 bsf	   PIE1,ADIF	; A2D interrupt

	 bcf	   STATUS,RP0
	 bcf	   STATUS,RP1
	 CALL	   FilterInit	;follow tutorial 11
	 bsf	   ADCON0,GO

	 movlw	   0x7F
	 movwf	   Display 
; this returns to start +1 
	 retlw 	   0 			 

; Main Program 
Start:
     call 	   Init  		  ; call init subroutine
MainLoop:
     movf      Display,w      ; Copy the display to the LEDs
     movwf     PORTD

	 goto	   MainLoop

; follow code from tutorial 11 here after. This gets the A2D averaging taken care of. 
FilterInit:
     movlw     Queue
     movwf     FSR
     clrf      RunningSum
     clrf      RunningSum+1
     clrf      Queue
     clrf      Queue+1
     clrf      Queue+2
     clrf      Queue+3
     clrf      Queue+4
     clrf      Queue+5
     clrf      Queue+6
     clrf      Queue+7
     return

Filter:
     movwf     temp           ; save 
     
     movf      INDF,w         ; subtract the current out of the sum
     subwf     RunningSum,f
     btfss     STATUS,C       ; was there a borrow?
     decf      RunningSum+1,f ; yes, take it from the high order byte
     
     movf      temp,w
     movwf     INDF           ; store in table
     addwf     RunningSum,f   ; Add into the sum
     btfsc     STATUS,C
     incf      RunningSum+1,f
     
     incf      FSR,f
     movf      FSR,w
     xorlw     Queue+8        ; did it overflow?
     movlw     Queue          ; preload Queue base address (Does not affect the flags)
     btfsc     STATUS,Z
     movwf     FSR            ; yes: reset the pointer
;MAStraightline  ; 53 instructions, 55 cycles including call and & return
     bcf       STATUS,C       ; clear the carry
     rrf       RunningSum+1,w
     movwf     Round+1
     rrf       RunningSum,w   ; divide by 2 and copy to a version we can corrupt
     movwf     Round
     
     bcf       STATUS,C       ; clear the carry
     rrf       Round+1,f
     rrf       Round,f        ; divide by 4

     bcf       STATUS,C       ; clear the carry
     rrf       Round+1,f
     rrf       Round,f        ; divide by 8
     
     btfsc     STATUS,C       ; use the carry bit to round
     incf      Round,f          
     movf      Round,w        ; load Wreg with the answer
     return     

end
