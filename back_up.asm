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
tempChar

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

;---; -- Recognize the Push button
	bcf		  STATUS,RP0
	bcf		  STATUS,RP1
	btfsc	  INTCON,RBIF			; Check if RB0 interrupt flag is set
	goto	  ServiceButton		  	; Go do some actions?

;---; -- Recognize the Timer0 Interrupt flag
    btfsc     INTCON,T0IF		  ; if the Timer0 Interupt flag (T0IF) was set
    goto      ServiceTimer0		  ; goto that routine.
       
	goto      ExitISR          

ServiceButton:
	bcf		  STATUS,RP0
	bcf		  STATUS,RP1

	; Toggle is used to counter-act the Interrupt On Change
	btfsc	  Toggle,0			  ; If Toggle is clear
	comf	  State				  ; Change the state
	comf	  Toggle			  ; Else ignore

	; -- Begin our intricate debounce routine... 
	nop
	nop
	nop
	nop
	nop
	nop							  ; 6 us
	nop							  ; 7 us
	nop							  ; 8 us
	nop							  ; 9 us
	nop							  ; 10 us

	movfw	  PORTB				  ; A bug exists in the PIC16F & PIC18F Families... joy
								  ; You must read port b prior to clearing the RBIF
	bcf		  INTCON,RBIF		  ; Clear the interrupt flag after debounce

	goto	  ExitISR

ServiceTimer0:
    bcf       STATUS,RP0          ; Ensure ISR executes in Register Bank 0
    bcf       STATUS,RP1
    bcf       INTCON,T0IF         ; clear the interrupt flag. (must be done in software)
       
	DetermineState:
    	bcf       STATUS,C            ; ensure the carry bit is clear
		movf	  Display,w
		movwf     Display_Save		  ; save off the current Display
    	btfsc     State,0			  ; IF(State == 1) if 0 skip to DisplayDice, else goto Rotate
    	goto      Rotate			  ; ELSE RotateLeft 
		goto	  DisplayDice		  ; RotateRight

	DisplayDice:
		; make the connection
		movfw	  Dice_Final		  ; Display the Dice value that we
		movwf	  Dice_Display		  ; saved when the button push occured. 
	goto	  ExitISR

	Rotate:
    	movfw	  Display_Save		  ; pick up where we left off with our rotation
		movwf	  Display
		rlf       Display,f           ; rotate in place
    	btfsc     STATUS,C            ; did it rotate out of the display
    	bsf       Display,0           ; yes, put it into bit 0
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
	bsf	 	  STATUS,RP0
	bsf	      STATUS,RP1
	clrf	  ANSEL

	bsf		  STATUS,RP0	; bank 1
	bcf		  STATUS,RP1
	clrf 	  TRISD 		;
	clrf	  TRISE

	bcf		  STATUS,RP0	; bank 0
	bcf		  STATUS,RP1	;
	clrf 	  PORTD 		;
	clrf	  PORTE	

	call	  LCD_init	

	; -- LEDs setup [Port D config to all outputs]  
	clrf      State
    bsf       Toggle,0      ; Looking for a 0 on the button
    
	; -- Set up PortB
    bsf	   	STATUS,RP0		; select Register Bank 3	
	bsf     STATUS,RP1    
    movlw   0x00
    movwf   ANSELH			; PortB pins are digitial (important as RB0 is switch)
    bcf     STATUS,RP0		; back to Register Bank 0
    bcf     STATUS,RP1

	bcf		STATUS,RP0
	bcf		STATUS,RP1
    movlw   B'11100000'		; global interrupts, pariphrial interrupts enable (PEIE) & TMR0 (T0IE),
    movwf   INTCON
	bsf		STATUS,RP0		; select register bank 1...
	bcf		STATUS,RP1		; to address the ADIF and PortB Interrupts
	bsf		INTCON,RBIE		; Enable the use of interrupts on PortB
    bsf		IOCB,0			; Interrupt On Change (IOC) enabled


	bcf		  State,0

	goto	MainLoop

LCD_init:
	; -- LCD setup	[Port A config to digital outputs]
	; -- PortE relationship to LCD is:
	; -- |	E3	|	E2	|	E1	|	E0	|
	; -- |	NC	|LCD_RS	|LCD_RW |LCD_E	|
	;		  2.) go through setup sequence for LCD

	movlw	  0x19 			; Delay 18.5 ms
	call	  DelayW		;

	clrf	  PORTE			; -- Toggle LCD_E
	movlw	  B'00000001'	; --
	movwf	  LCD_E 
	movwf	  PORTE
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	movlw	  B'00111000'	; -\ 8 bit mode, 2 Lines, 5*8 font, 
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

	clrf	  PORTE			; -- Toggle LCD_E
	movlw	  B'00000001'	; --
	movwf	  LCD_E 
	movwf	  PORTE
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	movlw	  B'00111000'	; -\ 8 bit mode, 2 Lines, 5*8 font, 
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

	clrf	  PORTE			; -- Toggle LCD_E
	movlw	  B'00000001'	; --
	movwf	  LCD_E 
	movwf	  PORTE
	movlw	  0x02
	call	  DelayW		; -- delay 777us
	movlw	  B'00111000'	; -\ 8 bit mode, 2 Lines, 5*8 font, 
	movwf	  LCD_Data		; -- Send to LCD
	movwf	  PORTD
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 uss
	clrf	  PORTE			; --
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	clrf	  PORTD			; -/
	
	movlw	  0x02			; Delay 777 us
	call	  DelayW

	clrf	  PORTE			; -- Toggle LCD_E
	movlw	  B'00000001'	; --
	movwf	  LCD_E 
	movwf	  PORTE
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	movlw	  B'00000010'	; -\ Cursor home 
	movwf	  LCD_Data		; -- Send to LCD
	movwf	  PORTD
	call	  DelayW		; -- delay 777us
	clrf	  PORTE			; --
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	clrf	  PORTD			; -/

	movlw	  0x02			; Delay 777 us
	call	  DelayW

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

	clrf	  PORTE			; -\ Toggle LCD_E
	movlw	  B'00000001'	; --
	movwf	  LCD_E 
	movwf	  PORTE
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	movlw	  B'00001111'	; -- Display on, Cursor on, Blink on
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

	clrf	  PORTE			; -\ Toggle LCD_E
	movlw	  B'00000001'	; --
	movwf	  LCD_E 
	movwf	  PORTE
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us 
	movlw	  B'00000110'	; -\ Increase cursor position, scroll off
	movwf	  LCD_Data
	movwf	  PORTD			; -- Send to LCD
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	clrf	  PORTE
	call	  DelayW
	clrf	  PORTD

	movlw	  0x02			; Delay 1.5ms
	call	  DelayW	

	clrf	  PORTE			; -\ Toggle LCD_E
	movlw	  B'00000001'	; --
	movwf	  LCD_E 
	movwf	  PORTE
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	movlw	  B'00000110'	; -\ Increase cursor position, scroll off
	movwf	  LCD_Data
	movwf	  PORTD			; -- Send to LCD
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	clrf	  PORTE
	call	  DelayW
	clrf	  PORTD

	return

LCD_ClearScreen:
	;-----------------------------------------
	; Clear Screen and cursor home...
	;-----------------------------------------
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
	;-----------------------------------------
	; Write the asci equivalent of what is 
	; held in the working reg.
	;-----------------------------------------
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
	call	  DelayW
	clrf	  PORTD	
	
	return

LCD_WcurVal
	;-----------------------------------------
	; Write Current Value
	; move the cursor to the proper location 
	; in order to write the current sound 
	; level. line 1, col 9
	;-----------------------------------------
	movwf	  tempChar		; save passed in value
	
	clrf	  PORTE			; -\ Toggle LCD_E
	movlw	  B'00000001'	; -- & enable write...
	movwf	  LCD_E 
	movwf	  PORTE
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	movlw	  B'10001001'	; -- DDRAM Address to 9
	movwf	  LCD_Data
	movwf	  PORTD			; -- Send to LCD
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	clrf	  PORTE
	call	  DelayW
	clrf	  PORTD	
	
	movfw	  tempChar		; recall passed in value
	call	  LCD_WriteW	
	
	return

LCD_WavgVal
	;-----------------------------------------
	; Write Average Value
	; move the cursor to the proper location 
	; in order to write the current sound 
	; level. line 2, col 9
	;-----------------------------------------
	movwf	  tempChar		; save passed in value
	
	clrf	  PORTE			; -\ Toggle LCD_E
	movlw	  B'00000001'	; -- & enable write...
	movwf	  LCD_E 
	movwf	  PORTE
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	movlw	  B'11001001'	; -- DDRAM Address to 73
	movwf	  LCD_Data
	movwf	  PORTD			; -- Send to LCD
	movlw	  0x02			; 
	call	  DelayW		; -- delay 777 us
	clrf	  PORTE
	call	  DelayW
	clrf	  PORTD	
	
	movfw	  tempChar		; recall passed in value
	call	  LCD_WriteW	
	
	return

MainLoop:

	call	  LCD_ClearScreen
	movlw	  0xFF
	call	  DelayW
	call	  DelayW
	call	  DelayW
	call	  DelayW
	call	  DelayW
	call	  DelayW
	movlw	  0x41
	call 	  LCD_WriteHello
	movlw	  0xFF
	call	  DelayW
	call	  DelayW
	call	  DelayW
	call	  DelayW
	call	  DelayW
	call	  DelayW
	
	goto      MainLoop
    
    end
     
