;**********************************************************************
;                                                                     *
;    Filename:      Lab5_TurnIn.asm                                   *
;    Date:          2/13/11                                           *
;    File Version:  9999                                              *
;                                                                     *
;    Author:        Robert Hildebrandt                                *
;    Company:       Student                                           *
;                                                                     *
;                                                                     *
;**********************************************************************
;                                                                     *
;    Files required: P16F877.INC                                      *
;                                                                     *
;                                                                     *
;                                                                     *
;**********************************************************************
;                                                                     *
;    Notes: This program has the ability to recognize and handle      *
;           three interrupt sources. The end functinality implemented *
;           on the PICkit2 dev board was the change in rotation speed *
;           via the potentiometer & change in the direction of        *
;           rotation of the 8 LEDs attached to the board              *
;**********************************************************************

#include <p16F887.inc>
	__CONFIG    _CONFIG1, _LVP_OFF & _FCMEN_OFF & _IESO_OFF & _BOR_OFF & _CPD_OFF & _CP_OFF & _MCLRE_OFF & _PWRTE_ON & _WDT_OFF & _INTRC_OSC_NOCLKOUT
	__CONFIG    _CONFIG2, _WRT_OFF & _BOR21V

    cblock     0x20
;--SYMBOL-----|--ADDRESS--|--COMMENT--
Display_Save  ;  0x020
Dice_Display  ;  0x021
Dice		  ;  0x022
DiceLow		  ;  0x023
DiceHi		  ;  0x024
Dice_Final	  ;  0x025
Display       ;  0x026      Define a variable to hold the diplay
State	 	  ;  0x027
Toggle		  ;  0x028
LCD_E		  ;	 0x029		LCD enable aka "clk" aka latch, we must toggle this pin to show new data
LCD_RS		  ;	 0x030      LCD Command/Data Select (0/1) 
LCD_RW		  ;  0x031		LCD read/write (0/1) we will be using read only...
LCD_Data	  ;	 0x032		Full blown 8 bit data
Delay1
Delay2
curLabelDone  ;  0x
avgLabelDone
tempChar
A2D_Average		 ; define a variable to hold the Analog to Digital average
A2D_Current		 ; define variable for current A2D reading
Current_Char1
Current_Char2
Avg_Char1
Avg_Char2
Display_Current          ; define a variable to display current
Display_Average	; define a variable to display average
Queue:8          ; 8 bytes to hold last 8 entries
RunningSum:2     ; sum of last 8 entries
Round:2          ; divide by 8 and round.
temp
    endc

; Interrupt Context registers
    cblock 0x70     ; put these up in unbanked RAM
;--SYMBOL-----|--ADDRESS--|--COMMENT--
W_Save		  ;  0x070		For saving working register
STATUS_Save   ;  0x071		For saving the status word
    endc

    org 0
    goto      init

; -- INTERUPT SERVICE ROUTINE --
     org 4
ISR:   
    ; -- Pre-ISR 
	movwf     W_Save              ; Save context
    movf      STATUS,w			   ; move STATUS file to working
    movwf     STATUS_Save		   ; move working to STATUS_Save

	; -- Recognize the Timer0 Interrupt flag
    btfsc     INTCON,T0IF		  ; if the Timer0 Interupt flag (T0IF) was set
    goto      ServiceTimer0		  ; goto that routine.
       
	goto      ExitISR          

ServiceTimer0:
    bcf       STATUS,RP0          ; Ensure ISR executes in Register Bank 0
    bcf       STATUS,RP1
    bcf       INTCON,T0IF         ; clear the interrupt flag. (must be done in software)
	; ----------------------------------------
	; When Timer0 over flows, get the ADC value
	; and update cur-val and avg-val on the screen
	; ----------------------------------------
	clrf	  A2D_Current
	bsf       ADCON0,GO_DONE      ; start conversion
    btfss     ADCON0,GO_DONE      ; this bit will change to zero when the conversion is complete
    goto      $-1
	 
	movf	  ADRESH,w			; read the A2D
	movwf	  A2D_Current	    ; save the current value
	call 	  Filter			; send it to filter
	movwf	  A2D_Average		; save the filtered value
	movfw	  A2D_Average
	call	  LCD_WavgVal
	movfw	  A2D_Current
	call	  LCD_WcurVal

	goto	  LoadNewTMR0

	LoadNewTMR0:
		movlw	  0x00
		movwf	  TMR0

    bcf       INTCON,T0IF         ; clear the interrupt flag. (must be done in software)

	goto	ExitISR

; ----- Before returning to the next instruction after the interupt
; ----- we must restore what we were using. This section will always
; ----- be the same regardless of what interupt we were serving...               
ExitISR:
    movf      STATUS_Save,w ; Restore context
    movwf     STATUS
    swapf     W_Save,f      ; swapf doesn't affect Status bits, but MOVF would
    swapf     W_Save,w
    retfie					; return from interupt and allow other to occur (interupt enable)
 
DelayW:
	 movwf	   Delay1
	 movwf	   Delay2
	 call	   Delay
	 return
Delay:
     decfsz    Delay1,f
     goto      Delay
     decfsz    Delay2,f
     goto      Delay
	 return
   
init:
	;-----------------------------------------
	;--------- Initializations ---------------
	;--PORTS--
	;  PORTD:   Output, connected to data pins
	; 		    on the HD44780 display
	;  PORTE:   Output, connected to the control
	;		    pins on the HD44780. E0 = LCD_E,
	;		    E1 = LCD_R/W, E2 = LCD_RS, 
	;		    E3 = NC
	;  PORTA:   Input, RA1 connected to 
	;		    microphone
	;  PORTB:   Input, RB0 connected to button
	;--TIMERS--
	;  TMR0:    1:255 prescaller, interrupt
	;           enabled. Used to trigger events
	;--Display
	;  HD44780: Interface specifications 
	; 			shall be met for correct
	; 			operation
	;--Variables
	;	curLabelDone - 1 if the current label 
	;				   has been written
	;	avgLabelDone - 1 if the average label
	;				   has been written
	;-----------------------------------------
	bsf	 	  STATUS,RP0
	bsf	      STATUS,RP1
	clrf	  ANSEL



	;<PORTD>
	;<PORTE>
	bsf		  STATUS,RP0	; bank 1
	bcf		  STATUS,RP1
	clrf 	  TRISD 		;
	clrf	  TRISE

	bcf		  STATUS,RP0	; bank 0
	bcf		  STATUS,RP1	;
	clrf 	  PORTD 		;
	clrf	  PORTE	
	;</PORTD>
	;</PORTE>
	;<PORTB>
    bsf	   	STATUS,RP0		; select Register Bank 3	
	bsf     STATUS,RP1    
    movlw   0x00
    movwf   ANSELH			; PortB pins are digitial (important as RB0 is switch)
	movlw   0x00                ; Left Justified, Vdd-Vss referenced
	movwf 	ADCON1
	bsf	    STATUS,RP0
	bsf 	STATUS,RP1
	movlw	0xFF
	movwf   ANSEL
	movlw   0x00
	movwf   ANSELH              ; PortB pins are digitial (important as RB0 is switch)
	bcf     STATUS,RP0          ; back to Register Bank 0
	bcf     STATUS,RP1
	
	movlw     B'10000101'
	movwf     ADCON0         ; configure A2D for Fosc/8, Channel 0 (RA1), and turn on the A2D module  

    bcf     STATUS,RP0		; back to Register Bank 0
    bcf     STATUS,RP1
	;</PORTB>
	;<Interrupts>
	 movlw	   b'11100000'
	 movwf 	   INTCON
	 bsf	   STATUS,RP0
	 bcf	   STATUS,RP1
	 ;bsf	   IOCB,0		; interrupt on change
	 ;bsf 	   INTCON,RBIE	; interrupts portB
	 ;bsf	   PIE1,ADIF	; A2D interrupt

	 bcf	   STATUS,RP0
	 bcf	   STATUS,RP1
	 call	   FilterInit	;follow tutorial 11
	 bsf	   ADCON0,GO
	;</Interrupts>

	;<LCD>
	call	  LCD_init	
	;</LCD>

	;<state>
	clrf      State
	bcf		  State,0
    bsf       Toggle,0      ; Looking for a 0 on the button
	clrf	  curLabelDone
	clrf	  avgLabelDone	
	bcf		  curLabelDone,0; initialially clear
	bcf		  avgLabelDone,0
    ;</state>

	 bsf       STATUS,RP0     ; select Register Bank 1
	 bcf	   STATUS,RP1
; Setup TMR0, Variable Pot, Push Button, LEDs, and A2D. Follow code from tutorial 9, 10, 11.
 	 movlw     b'00000111'    ; configure Timer0.  Sourced from the Processor clock;
     movwf     OPTION_REG     ; Maximum Prescaler
     movlw     0xFF
     movwf     TRISA          ; Make PortA all input
     movlw     0x01
     movwf     TRISB          ; Make RB0 pin input (switch)

	 bcf       STATUS,RP0     ; select Register Bank 0
	 bcf	   STATUS,RP1

	goto	MainLoop

MainLoop:
	

	
	goto      MainLoop

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


LCD_init:
	;---------------------------------------------------
	;-- LCD setup	[Port D config to digital outputs]
	;-- PortE relationship to LCD is:
	;-- |	E3	|	E2	|	E1	|	E0	|
	;-- |	NC	|LCD_RS	|LCD_RW |LCD_E	|
	;---------------------------------------------------
	movlw	  0x19 			; Delay 18.5 ms (Datasheet)
	call	  DelayW		;

	;---------------------------------------------------
	; Use 8 bit mode, 2 lines and the std 5*8 font size
	;---------------------------------------------------
	clrf	  PORTE			; -\ Toggle LCD_E
	movlw	  B'00000001'	; --
	movwf	  LCD_E 
	movwf	  PORTE
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	movlw	  B'00111000'	; -- 8 bit mode, 2 Lines, 5*8 font, 
	movwf	  LCD_Data		; -- Send to LCD
	movwf	  PORTD
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	clrf	  PORTE			; --
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	clrf	  PORTD			; -/
	

	movlw	  0x09			; Delay 6.1 ms
	call	  DelayW

	;---------------------------------------------------
	; Use 8 bit mode, 2 lines and the std 5*8 font size
	;---------------------------------------------------
	clrf	  PORTE			; -\ Toggle LCD_E
	movlw	  B'00000001'	; --
	movwf	  LCD_E 
	movwf	  PORTE
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	movlw	  B'00111000'	; -- 8 bit mode, 2 Lines, 5*8 font, 
	movwf	  LCD_Data		; -- Send to LCD
	movwf	  PORTD
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	clrf	  PORTE			; --
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	clrf	  PORTD			; -/
	

	movlw	  0x09			; Delay 6.1 ms
	call	  DelayW

	;---------------------------------------------------
	; Use 8 bit mode, 2 lines and the std 5*8 font size
	;---------------------------------------------------
	clrf	  PORTE			; -\ Toggle LCD_E
	movlw	  B'00000001'	; --
	movwf	  LCD_E 
	movwf	  PORTE
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	movlw	  B'00111000'	; -- 8 bit mode, 2 Lines, 5*8 font, 
	movwf	  LCD_Data		; -- Send to LCD
	movwf	  PORTD
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	clrf	  PORTE			; --
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	clrf	  PORTD			; -/
	
	movlw	  0x02			; Delay 777 us
	call	  DelayW

	;---------------------------------------------------
	; Cursor to the home position
	;---------------------------------------------------	
	clrf	  PORTE			; -\ Toggle LCD_E
	movlw	  B'00000001'	; --
	movwf	  LCD_E 
	movwf	  PORTE
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	movlw	  B'00000010'	; -- Cursor home 
	movwf	  LCD_Data		; -- Send to LCD
	movwf	  PORTD
	call	  DelayW		; -- delay 777us
	clrf	  PORTE			; --
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	clrf	  PORTD			; -/

	movlw	  0x02			; Delay 777 us
	call	  DelayW

	;---------------------------------------------------
	; Cursor the display
	; (may not be necessary but its good practice)
	;---------------------------------------------------
	clrf	  PORTE			; -- Toggle LCD_E
	movlw	  B'00000001'	; --
	movwf	  LCD_E 
	movwf	  PORTE
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us 
	movlw	  B'00000001'	; -\ Clear Display
	movwf	  LCD_Data
	movwf	  PORTD			; -- Send to LCD
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	clrf	  PORTE			; --
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	clrf	  PORTD			; -/

	movlw	  0x02			; Delay 777 us
	call	  DelayW	

	;---------------------------------------------------
	; Turn Display on, Cursor on, Blink on
	;---------------------------------------------------
	clrf	  PORTE			; -\ Toggle LCD_E
	movlw	  B'00000001'	; --
	movwf	  LCD_E 
	movwf	  PORTE
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	movlw	  B'00001100'	; -- Display on, Cursor off, Blink off
	movwf	  LCD_Data
	movwf	  PORTD			; -- Send to LCD
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	clrf	  PORTE			; --
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	clrf	  PORTD			; -/

	movlw	  0x02			; Delay 777 us
	call	  DelayW	

	;---------------------------------------------------
	; Increase cursor posistion as we print chars
	; Don't scroll our screen left or right.. confusing!
	;---------------------------------------------------
	clrf	  PORTE			; -\ Toggle LCD_E
	movlw	  B'00000001'	; --
	movwf	  LCD_E 
	movwf	  PORTE
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us 
	movlw	  B'00000110'	; -- Increase cursor position, scroll off
	movwf	  LCD_Data
	movwf	  PORTD			; -- Send to LCD
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	clrf	  PORTE
	call	  DelayW		; -- delay 777 us
	clrf	  PORTD			; -/

	call LCD_WcurLabel	    ; Write "cur-val:"
	call LCD_WavgLabel		; Write "avg-val:"

	return

LCD_ClearScreen:
	;---------------------------------------------------
	; Clear Screen and cursor home...
	;---------------------------------------------------
	; Clear Screen
	;---------------------------------------------------
	clrf	  PORTE			; -\ Toggle LCD_E
	movlw	  B'00000001'	; --
	movwf	  LCD_E 
	movwf	  PORTE
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	movlw	  B'00000001'	; -\ Clear Screen
	movwf	  LCD_Data
	movwf	  PORTD			; -- Send to LCD
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	clrf	  PORTE
	call	  DelayW
	clrf	  PORTD	
	;---------------------------------------------------
	; Cursor home
	;---------------------------------------------------
	clrf	  PORTE			; -\ Toggle LCD_E
	movlw	  B'00000001'	; --
	movwf	  LCD_E 
	movwf	  PORTE
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	movlw	  B'00000010'	; -\ Cursor home
	movwf	  LCD_Data
	movwf	  PORTD			; -- Send to LCD
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	clrf	  PORTE
	call	  DelayW
	clrf	  PORTD	
	
	return

LCD_WriteHello:
	;---------------------------------------------------
	; Write "Hello" for funnsies
	;---------------------------------------------------
	clrf	  PORTE			; -\ Toggle LCD_E
	movlw	  B'00000101'	; -- & enable write...
	movwf	  LCD_E 
	movwf	  PORTE
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	movlw	  B'01001000'	; -- 'H'
	movwf	  LCD_Data
	movwf	  PORTD			; -- Send to LCD
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	clrf	  PORTE
	call	  DelayW
	clrf	  PORTD	

	clrf	  PORTE			; -\ Toggle LCD_E
	movlw	  B'00000101'	; -- & enable write...
	movwf	  LCD_E 
	movwf	  PORTE
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	movlw	  B'01100101'	; -- 'e'
	movwf	  LCD_Data
	movwf	  PORTD			; -- Send to LCD
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	clrf	  PORTE
	call	  DelayW
	clrf	  PORTD	

	clrf	  PORTE			; -\ Toggle LCD_E
	movlw	  B'00000101'	; -- & enable write...
	movwf	  LCD_E 
	movwf	  PORTE
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	movlw	  B'01101100'	; -- 'l'
	movwf	  LCD_Data
	movwf	  PORTD			; -- Send to LCD
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	clrf	  PORTE
	call	  DelayW
	clrf	  PORTD	

	clrf	  PORTE			; -\ Toggle LCD_E
	movlw	  B'00000101'	; -- & enable write...
	movwf	  LCD_E 
	movwf	  PORTE
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	movlw	  B'01101100'	; -- 'l'
	movwf	  LCD_Data
	movwf	  PORTD			; -- Send to LCD
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	clrf	  PORTE
	call	  DelayW
	clrf	  PORTD	

	clrf	  PORTE			; -\ Toggle LCD_E
	movlw	  B'00000101'	; -- & enable write...
	movwf	  LCD_E 
	movwf	  PORTE
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	movlw	  B'01101111'	; -- 'o'
	movwf	  LCD_Data
	movwf	  PORTD			; -- Send to LCD
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	clrf	  PORTE
	call	  DelayW
	clrf	  PORTD	
	
	return

LCD_WriteW:
	;---------------------------------------------------
	; Write the ascii equivalent of what is 
	; held in the working reg.
	;---------------------------------------------------
	movwf	  tempChar		; save passed in arg..

	clrf	  PORTE			; -\ Toggle LCD_E
	movlw	  B'00000101'	; -- & enable write...
	movwf	  LCD_E 
	movwf	  PORTE
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	movfw	  tempChar		; -- bring back the char..
	movwf	  LCD_Data
	movwf	  PORTD			; -- Send to LCD
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	clrf	  PORTE
	call	  DelayW		; -- delay 777 us
	clrf	  PORTD			; -/
	movlw	  0x00

	return

LCD_WcurVal
	;---------------------------------------------------
	; Write Current Value
	; move the cursor to the proper location 
	; in order to write the current sound 
	; level. line 1, col 9
	;---------------------------------------------------
	movwf	  tempChar		; save passed in value
	
	clrf	  PORTE			; -\ Toggle LCD_E
	movlw	  B'00000001'	; -- & enable write...
	movwf	  LCD_E 
	movwf	  PORTE
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	movlw	  B'10001000'	; -- DDRAM Address to 8
	movwf	  LCD_Data
	movwf	  PORTD			; -- Send to LCD
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	clrf	  PORTE
	call	  DelayW
	clrf	  PORTD	
	
	movfw	  tempChar		; recall passed in value
	movwf	  Current_Char1 ; load value into Character1 variable
	swapf	  Current_Char1,w	; Swap the high nibble to the lowpart, then move to working.
	movwf	  Current_Char2	; load the swapped value into Character 2

	movfw	  Current_Char2	; load character1 into working register
	call	  CharConvert	; Call Character Conversion
	movwf	  Current_Char2	; Load the character conversion into Character1

	call	  LCD_WriteW	; call the LCD_WriteW function to write our converted character

	movfw	  Current_Char1	; load Character1 into the working register
	call	  CharConvert	; Call character conversion
	movwf	  Current_Char1 ; Load the character conversion into Character2

	call 	  LCD_WriteW	; call the LCD_WriteW function to write our converted character 	
	
	return

LCD_WcurLabel
	;---------------------------------------------------
	; Write "cur-val:" label on screen on line 1
	;---------------------------------------------------
	
	clrf	  PORTE			; -\ Toggle LCD_E
	movlw	  B'00000101'	; -- & enable write...
	movwf	  LCD_E 
	movwf	  PORTE
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	movlw	  B'01100011'	; -- 'c'
	movwf	  LCD_Data
	movwf	  PORTD			; -- Send to LCD
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	clrf	  PORTE
	call	  DelayW		; -- delay 777 us
	clrf	  PORTD			; -/

	clrf	  PORTE			; -\ Toggle LCD_E
	movlw	  B'00000101'	; -- & enable write...
	movwf	  LCD_E 
	movwf	  PORTE
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	movlw	  B'01110101'	; -- 'u'
	movwf	  LCD_Data
	movwf	  PORTD			; -- Send to LCD
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	clrf	  PORTE
	call	  DelayW		; -- delay 777 us
	clrf	  PORTD			; -/

	clrf	  PORTE			; -\ Toggle LCD_E
	movlw	  B'00000101'	; -- & enable write...
	movwf	  LCD_E 
	movwf	  PORTE
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	movlw	  B'01110010'	; -- 'r'
	movwf	  LCD_Data
	movwf	  PORTD			; -- Send to LCD
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	clrf	  PORTE
	call	  DelayW		; -- delay 777 us
	clrf	  PORTD			; -/

	clrf	  PORTE			; -\ Toggle LCD_E
	movlw	  B'00000101'	; -- & enable write...
	movwf	  LCD_E 
	movwf	  PORTE
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	movlw	  B'00101101'	; -- '-'
	movwf	  LCD_Data
	movwf	  PORTD			; -- Send to LCD
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	clrf	  PORTE
	call	  DelayW		; -- delay 777 us
	clrf	  PORTD			; -/

	clrf	  PORTE			; -\ Toggle LCD_E
	movlw	  B'00000101'	; -- & enable write...
	movwf	  LCD_E 
	movwf	  PORTE
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	movlw	  B'01110110'	; -- 'v'
	movwf	  LCD_Data
	movwf	  PORTD			; -- Send to LCD
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	clrf	  PORTE
	call	  DelayW		; -- delay 777 us
	clrf	  PORTD			; -/

	clrf	  PORTE			; -\ Toggle LCD_E
	movlw	  B'00000101'	; -- & enable write...
	movwf	  LCD_E 
	movwf	  PORTE
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	movlw	  B'01100001'	; -- 'a'
	movwf	  LCD_Data
	movwf	  PORTD			; -- Send to LCD
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	clrf	  PORTE
	call	  DelayW		; -- delay 777 us
	clrf	  PORTD			; -/

	clrf	  PORTE			; -\ Toggle LCD_E
	movlw	  B'00000101'	; -- & enable write...
	movwf	  LCD_E 
	movwf	  PORTE
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	movlw	  B'01101100'	; -- 'l'
	movwf	  LCD_Data
	movwf	  PORTD			; -- Send to LCD
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	clrf	  PORTE
	call	  DelayW		; -- delay 777 us
	clrf	  PORTD			; -/

	clrf	  PORTE			; -\ Toggle LCD_E
	movlw	  B'00000101'	; -- & enable write...
	movwf	  LCD_E 
	movwf	  PORTE
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	movlw	  B'00111010'	; -- ':'
	movwf	  LCD_Data
	movwf	  PORTD			; -- Send to LCD
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	clrf	  PORTE
	call	  DelayW		; -- delay 777 us
	clrf	  PORTD			; -/

	bsf		  curLabelDone,0
	
	return

LCD_WavgLabel
	;---------------------------------------------------
	; Write "avg-val:" label on screen on line 2
	;---------------------------------------------------

	clrf	  PORTE			; -\ Toggle LCD_E
	movlw	  B'00000001'	; -- & enable write...
	movwf	  LCD_E 
	movwf	  PORTE
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	movlw	  B'10101000'	; -- DDRAM Address to 64
	movwf	  LCD_Data
	movwf	  PORTD			; -- Send to LCD
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	clrf	  PORTE
	call	  DelayW
	clrf	  PORTD	
	
	clrf	  PORTE			; -\ Toggle LCD_E
	movlw	  B'00000101'	; -- & enable write...
	movwf	  LCD_E 
	movwf	  PORTE
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	movlw	  B'01100001'	; -- 'a'
	movwf	  LCD_Data
	movwf	  PORTD			; -- Send to LCD
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	clrf	  PORTE
	call	  DelayW		; -- delay 777 us
	clrf	  PORTD			; -/

	clrf	  PORTE			; -\ Toggle LCD_E
	movlw	  B'00000101'	; -- & enable write...
	movwf	  LCD_E 
	movwf	  PORTE
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	movlw	  B'01110110'	; -- 'v'
	movwf	  LCD_Data
	movwf	  PORTD			; -- Send to LCD
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	clrf	  PORTE
	call	  DelayW		; -- delay 777 us
	clrf	  PORTD			; -/

	clrf	  PORTE			; -\ Toggle LCD_E
	movlw	  B'00000101'	; -- & enable write...
	movwf	  LCD_E 
	movwf	  PORTE
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	movlw	  B'01100111'	; -- 'g'
	movwf	  LCD_Data
	movwf	  PORTD			; -- Send to LCD
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	clrf	  PORTE
	call	  DelayW		; -- delay 777 us
	clrf	  PORTD			; -/

	clrf	  PORTE			; -\ Toggle LCD_E
	movlw	  B'00000101'	; -- & enable write...
	movwf	  LCD_E 
	movwf	  PORTE
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	movlw	  B'00101101'	; -- '-'
	movwf	  LCD_Data
	movwf	  PORTD			; -- Send to LCD
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	clrf	  PORTE
	call	  DelayW		; -- delay 777 us
	clrf	  PORTD			; -/

	clrf	  PORTE			; -\ Toggle LCD_E
	movlw	  B'00000101'	; -- & enable write...
	movwf	  LCD_E 
	movwf	  PORTE
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	movlw	  B'01110110'	; -- 'v'
	movwf	  LCD_Data
	movwf	  PORTD			; -- Send to LCD
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	clrf	  PORTE
	call	  DelayW		; -- delay 777 us
	clrf	  PORTD			; -/

	clrf	  PORTE			; -\ Toggle LCD_E
	movlw	  B'00000101'	; -- & enable write...
	movwf	  LCD_E 
	movwf	  PORTE
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	movlw	  B'01100001'	; -- 'a'
	movwf	  LCD_Data
	movwf	  PORTD			; -- Send to LCD
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	clrf	  PORTE
	call	  DelayW		; -- delay 777 us
	clrf	  PORTD			; -/

	clrf	  PORTE			; -\ Toggle LCD_E
	movlw	  B'00000101'	; -- & enable write...
	movwf	  LCD_E 
	movwf	  PORTE
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	movlw	  B'01101100'	; -- 'l'
	movwf	  LCD_Data
	movwf	  PORTD			; -- Send to LCD
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	clrf	  PORTE
	call	  DelayW		; -- delay 777 us
	clrf	  PORTD			; -/

	clrf	  PORTE			; -\ Toggle LCD_E
	movlw	  B'00000101'	; -- & enable write...
	movwf	  LCD_E 
	movwf	  PORTE
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	movlw	  B'00111010'	; -- ':'
	movwf	  LCD_Data
	movwf	  PORTD			; -- Send to LCD
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	clrf	  PORTE
	call	  DelayW		; -- delay 777 us
	clrf	  PORTD			; -/

	bsf		  avgLabelDone,0
	
	return

LCD_WavgVal
	;---------------------------------------------------
	; Write Average Value
	; move the cursor to the proper location 
	; in order to write the current sound 
	; level. line 2, col 8
	;---------------------------------------------------
	movwf	  tempChar		; save passed in value
	
	clrf	  PORTE			; -\ Toggle LCD_E
	movlw	  B'00000001'	; -- & enable write...
	movwf	  LCD_E 
	movwf	  PORTE
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	movlw	  B'11001000'	; -- DDRAM Address to 
	movwf	  LCD_Data
	movwf	  PORTD			; -- Send to LCD
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	clrf	  PORTE
	call	  DelayW
	clrf	  PORTD	
	
	movfw	  tempChar		; recall passed in value
	movwf	  Avg_Char1 ; load value into Character1 variable
	swapf	  Avg_Char1,w	; Swap the high nibble to the lowpart, then move to working.
	movwf	  Avg_Char2	; load the swapped value into Character 2

	movfw	  Avg_Char2	; load character1 into working register
	call	  CharConvert	; Call Character Conversion
	movwf	  Avg_Char2	; Load the character conversion into Character1

	call	  LCD_WriteW	; call the LCD_WriteW function to write our converted character

	movfw	  Avg_Char1	; load Character1 into the working register
	call	  CharConvert	; Call character conversion
	movwf	  Avg_Char1 ; Load the character conversion into Character2

	call 	  LCD_WriteW	; call the LCD_WriteW function to write our converted character 	
	
	return

;org     0xf7                  ; force table to cross a 256 instruction boundary

CharConvert:
     andlw     0x0F                ; mask off invalid entries
     movwf     temp
     movlw     high TableStart     ; get high order part of the beginning of the table
     movwf     PCLATH
     movlw     low TableStart      ; load starting address of table
     addwf     temp,w              ; add offset
     btfsc     STATUS,C            ; did it overflow?
     incf      PCLATH,f            ; yes: increment PCLATH
     movwf     PCL                 ; modify PCL

; Here I modified a lookup table with the equivalent LCD HEX character values for binary values 0-15. 
; I have an excel file I created, which I will include in my writeup which shows the table and equivalent dice value, and the operations. 
; I then just made a simple 16 value lookup table, giving a HEX equivalent character value for each binary input. WOOT?!
TableStart:
     retlw     0x30             ; 0
     retlw     0x31             ; 1
     retlw     0x32             ; 2
     retlw     0x33             ; 3
     retlw     0x34             ; 4
     retlw     0x35             ; 5
     retlw     0x36             ; 6
     retlw     0x37             ; 7
     retlw     0x38             ; 8
     retlw     0x39             ; 9
     retlw     0x41             ; A
     retlw     0x42             ; B
	 retlw     0x43             ; C
     retlw     0x44             ; D
     retlw     0x45             ; E
     retlw     0x46             ; F

    
    end
     
