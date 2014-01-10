      list p=16f877                 ; list directive to define processor
      #include <p16f877.inc>        ; processor specific variable definitions
      __CONFIG _CP_OFF & _WDT_OFF & _BODEN_ON & _PWRTE_ON & _HS_OSC & _WRT_ENABLE_ON & _CPD_OFF & _LVP_OFF

      #include <lcd.inc>			   ;Import LCD control functions from lcd.asm
	  #include <rtc_macros.inc>

offset  EQU       0x20				;For pc interface
temp2    EQU       0x21

	cblock	0x22					;0x30 works
		Table_Counter
		lcd_tmp	
		lcd_d1
		lcd_d2
		com	
		dat	
		Number_of_boxes
		Pattern_Number_1
		Pattern_Number_2
		Pattern_Number_3
		Pattern_Number_Current			;stores the current pattern numerically
		Pattern_Counter
		First_Pattern					;stores patterns in terms of 1 for white and 0 for orange
		Second_Pattern
		Third_Pattern
		Fourth_Pattern
		Fifth_Pattern
		Sixth_Pattern
		Pattern_Current					;stores current pattern shape
		Counter							;Count for motors
		Box_Counter						;Count box position (1-6)
		TensMonth
		OnesMonth
		TensDay
		OnesDay
		TensHoursStarted					;Store start and finish times
		OnesHoursStarted
		TensMinutesStarted
		OnesMinutesStarted
		TensSecondsStarted
		OnesSecondsStarted
		TensHoursFinished
		OnesHoursFinished
		TensMinutesFinished
		OnesMinutesFinished
		TensSecondsFinished
		OnesSecondsFinished
		temp
		OSE								;Save elapsed times
		TSE
		OME
		TME
		OHE
		THE
		WhiteBalls
		OrangeBalls
		Address_Counter
		Read_Address_Counter
		Memory_Temp
		var
		Operation_Number
		Log_Number
;		First_Counter				;counter for bottom stepper rotations
;		Second_Counter				;counter for pauses between stepper rotations
;		Third_Counter				;counter for done one box
	endc

	udata_shr
	COUNTH	res 1	;const used in delay
	COUNTM	res	1	;const used in delay
	COUNTL	res	1	;const used in delay

#define	RS 	PORTD,2
#define	E 	PORTD,3
	
         ORG       0x0000     ;RESET vector must always be at 0x00
         goto      init       ;Just jump to the main code section.

		 ORG 0x0004
Interrupt_Code
;		btfsc		PCLATH,3			;check if on page 1
;		call		Switch_To_Page0
		btfss		INTCON,INTF 	  	;poll INTF flag to make sure interrupt comes from keypad
		goto 		$-1
		bcf 		INTCON,INTF	  	;clear flag for next interrupt
		btfsc		PORTE,1			;check if right sensor (orange)
		call		AddoneOrange
		btfsc		PORTA,3			;check if right sensor (white)
		call		AddoneWhite
		btfsc		PORTA,1			;check if microswitch
		bsf			PORTC,0
;		btfsc		PORTB,3	;check if should return to page 0
;		call		Switch_To_Page1	
		bcf			PORTB,3
		retfie


;Timer_Interrupt
;		bcf			INTCON,T0IF		;clear interrupt flag
;		incf		First_Counter	;move eight times then pause to wait for ball to fall
;		btfsc		First_Counter,3
;		goto		Pause
;		btfsc		PORTC,5			;If RC5 is high next pattern is 2 or 3
;		goto		Second_Or_Third
;		goto		Fourth_Or_First	;else next pattern is fourth or first
;
;Pause										;just wait a few cycles
;		btfss		Second_Counter,7		;counter to count cycles of nothing
;		goto		END_ISR
;		clrf		First_Counter
;		clrf		Second_Counter
;		incf		Third_Counter			;counts when 6 rotations are done
;		goto		END_ISR
;
;Second_Or_Third
;		btfsc		PORTC,1			;if RC1 third pattern is next
;		goto		Third_Position
;		goto		Second_Position	;else go to second pattern
;Fourth_Or_First
;		btfsc		PORTC,1			;if RC1 Fourth pattern is next
;		goto		Fourth_Position
;		goto		Start_Position	;else go to Start pattern	
;Start_Position
;		bsf			PORTC,5
;		bcf			PORTC,1
;		bcf			PORTE,0
;		bsf			PORTC,7
;		goto		END_ISR
;Second_Position
;		bsf			PORTC,5
;		bsf			PORTC,1
;		bcf			PORTE,0
;		bcf			PORTC,7
;		goto		END_ISR
;Third_Position
;		bcf			PORTC,5
;		bsf			PORTC,1
;		bsf			PORTE,0
;		bcf			PORTC,7
;		goto		END_ISR
;Fourth_Position
;		bcf			PORTC,5
;		bcf			PORTC,1
;		bsf			PORTE,0
;		bsf			PORTC,7
;		goto		END_ISR

;***************************************
; Look up tables
;***************************************
Enter_Boxes
		addwf	PCL,F
		dt		"Enter # of boxes", 0
Cont_Msg
		addwf	PCL,F
		dt		"* to continue", 0

Log_Information
		addwf     PCL,f
        dt        "2-Log info", 0	

InvalidBox
		addwf     PCL,f
        dt        "Invalid Input", 0	;changed to invalid input to be more generalized

Enter_Pattern
		addwf	PCL,F
		dt		"Enter Pattern ", 0

PatternsEntered
		addwf	PCL,F
		dt		"Patterns: ", 0

KPHexToChar										;changed to reflect new keypad
        addwf     PCL,f
        dt        "123*456#0", 0

OneToNine
		addwf     PCL,f
        dt        "0123456789", 0

Busy
		addwf     PCL,f
        dt        "*Busy*", 0

New_Operation
		addwf     PCL,f
        dt        "1-New Operation", 0

;***************************************
; Display macro
;***************************************
Display macro	Message
		local	loop_
		local 	end_
		clrf	Table_Counter	
		clrw	
	
loop_	movf	Table_Counter,W
		call 	Message
		xorlw	B'00000000' ;check WORK reg to see if 0 is returned
		btfsc	STATUS,Z
		goto	end_
		call	WR_DATA
		incf	Table_Counter,F
		goto	loop_
end_
		endm

;***************************************
;PageChange macro
;***************************************
Pagechange macro 	tableIndex
		   movwf 	var ; Save the index
		   movlw 	HIGH tableIndex
		   movwf 	PCLATH ; Store it for next jump
		   movf 	var, W
		   addlw 	LOW tableIndex ; Compute its Offset
		   btfsc 	STATUS, C
		   incf 	PCLATH, F ; If in next, PCLATH = PCLATH + 1
		   movwf 	PCL ; Write the correct address to PC
endm ; End of macro

Start_Msg
Pagechange labela
labela
dt "Any key to start", 0

Invalid_Pattern_Number
Pagechange labelb
labelb
dt "Invalid Pattern #", 0

Done_Msg
Pagechange labelc
labelc
dt "Done", 0

Press_Star
Pagechange labeld
labeld
dt " Press *", 0

Date_Finished
Pagechange labelj
labelj
dt "Date_Finished: ", 0

Patterns_Chosen
Pagechange labelk
labelk
dt "Patterns Selected: ", 0

White_Remaining
Pagechange labelg
labelg
dt "White Remaining: ", 0

Orange_Remaining
Pagechange labelh
labelh
dt "Orange Remaining: ", 0

Time_Taken
Pagechange labeli
labeli
dt "Time Elapsed: ", 0

Log
Pagechange labell
labell
dt "Log Information", 0

;***************************************
; Initilization of both LCD and RTC
;***************************************
init
Enabletimer0
; 		bcf STATUS,RP0 ; select bank 0
;		bsf INTCON,7 ;global interrupts enabled
;		bcf INTCON,1 ;clear the interrput flag
;		bsf STATUS,RP0
;		bsf OPTION_REG, INTEDG ;set it so that the interrput only occurs on the positive edge
;		bcf OPTION_REG,T0CS ;set internal clock source
;		bcf OPTION_REG,T0SE ;read on low to high edges
;		bcf OPTION_REG,PSA ;use prescaler
;		bsf OPTION_REG,PS0
;		bsf OPTION_REG,PS1 ;use a prescaler of 256
;		bsf OPTION_REG,PS2
;		bcf STATUS,RP0
        clrf      	INTCON         ; No interrupts
		;bsf			INTCON,T0IE	   ; Enable Timer 0 interrupt  clear later!
		bsf			INTCON,GIE	   ; global interrupts enable
		bsf			INTCON,INTE	   ; external interrupts enable
        bsf       	STATUS,RP0     ; select bank 1
        movlw 		b'00000110'
		movwf 		ADCON1 		   	;All digital input, reference voltage Vdd Vss
       	movlw     	b'01110011'    ; Set required keypad inputs
        movwf     	TRISB
		movlw		b'01011100'	   ; RC6 is input pin for RS232, RB0 interrupt for microswitch
								   ; RC3, RC4 used for RTC
        movwf     	TRISC          ; All port C is output
       	clrf      	TRISD          ; All port D is output
		clrf		TRISE			;All port E is output
       	movlw     	b'101000'    ; Set required PORTA inputs
       	movwf     	TRISA	

        bcf       	STATUS,RP0     ; select bank 0
		movlw 		b'11000001' 	;clock selected, ADC module turned on
		movwf 		ADCON0

        clrf      	PORTA
        clrf      	PORTB
        clrf      	PORTC
       	clrf      	PORTD
       	movlw		b'111111'			;Load First_Pattern, 1 for white 0 for orange
		movwf		First_Pattern
		movlw		b'000000'			;Load Second_Pattern 
		movwf		Second_Pattern
		movlw		b'000111'			;Load Third_Pattern 
		movwf		Third_Pattern
		movlw		b'101101'			;Load Fourth_Pattern 
		movwf		Fourth_Pattern
		movlw		b'010010'			;Load Fifth_Pattern 
		movwf		Fifth_Pattern
		movlw		b'010101'			;Load Sixth_Pattern 
		movwf		Sixth_Pattern
		clrf		Pattern_Counter		;Set Pattern Counter to 0
		movlw		d'8'				;Set Patterns to 0 as per keypad
		movwf		Pattern_Number_1
		movwf		Pattern_Number_2
		movwf		Pattern_Number_3
		clrf		Operation_Number
		clrf		Log_Number
		clrf		Read_Address_Counter

		 ;Set up I2C for communication
		 call 	   i2c_common_setup

		 ;rtc_resetAll
		 
		 ;Used to set up time in RTC, load to the PIC when RTC is used for the first time
		 ;call	   set_rtc_time
          
         call      InitLCD    ;Initialize the LCD (code in lcd.asm; imported by lcd.inc)

Main	
		call		RetrieveAddressCounter
		movf		Read_Address_Counter,W				;Check if at end	
		sublw		0xFF							;subtract address counter from 247  (13x19 + 1 to account for 0) 
		btfsc		STATUS,Z						;zero bit to see if 0
		call		Save0intoaddresscounter
		;call		show_RTC
		goto		StartingInterface
DoneInterface
		call		Set_Balls
		call		Show_Balls
		call		EnterBoxes
		call		EnterPattern
		call		ClrLCD
		Display		Busy				;show busy on lcd
		bcf			PCLATH,4			;Select page 1
		bsf			PCLATH,3	
		call		Loading
		bcf			PORTC,0				;prepare Movebox
		;call		zeropointone		;wait for debouncing
		call		MoveBox				;move last box
		bcf			PCLATH,4			;Select page 0
		bcf			PCLATH,3
		call		DoneAutonomous
		call		RetrieveLog
		bcf			PCLATH,4			;Select page 1
		bsf			PCLATH,3	
		goto		EndMenu

Show_Balls
		call		ClrLCD
		movlw		"W"
		call		WR_DATA
		movlw		":"
		call		WR_DATA
		movfw		WhiteBalls
		movwf		Memory_Temp
		call		WriteTheBalls
		call		Switch_Lines
		movlw		"O"
		call		WR_DATA
		movlw		":"
		call		WR_DATA
		movfw		OrangeBalls
		movfw		OrangeBalls
		movwf		Memory_Temp
		call		WriteTheBalls
		movlw		" "
		call		WR_DATA
		movlw		" "
		call		WR_DATA
		movlw		"P"
		call		WR_DATA
		movlw		"r"
		call		WR_DATA
		movlw		"e"
		call		WR_DATA
		movlw		"s"
		call		WR_DATA
		movlw		"s"
		call		WR_DATA
		movlw		" "
		call		WR_DATA
		movlw		"*"
		call		WR_DATA
		call		StarPressed
		return

WriteTheBalls
		movfw		Memory_Temp
		sublw		d'9'				;check if over nine
		btfss		STATUS,C			;will be set if negative
		goto		OverNineShow
		goto		UnderNineShow
		
OverNineShow		
		movlw		"1"
		call		WR_DATA
		movlw		d'10'
		subwf		Memory_Temp,F

UnderNineShow
		movfw		Memory_Temp
		call		OneToNine	
		call		WR_DATA
		return
		

Save0intoaddresscounter
		movlw		0xFF						;Initilializes address_counter to 0!!
		bcf			STATUS,RP0
		bsf			STATUS,RP1
		movwf		EEADR
		bcf			STATUS,RP1			;switch banks to get data
		movlw		0x00
		bsf			STATUS,RP1
		movwf		EEDATA
		bsf			STATUS,RP0
		bcf			EECON1,EEPGD
		bsf			EECON1,WREN
		movlw		0x55
		movwf		EECON2
		movlw		0xaa
		movwf		EECON2
		bsf			EECON1,WR
		btfsc		EECON1,WR
		goto		$-1
		bcf 		EECON1, WREN
		bcf			STATUS,RP0
		bcf			STATUS,RP1		
		bcf			PIR2,EEIF
		return

;***************************************
; LCD control
;***************************************
Switch_Lines
		movlw	B'11000000'
		call	WR_INS
		return

Pick_Log
Pagechange labele
labele
dt "Log 1,2 or 3", 0

ContMsg2
Pagechange labelf
labelf
dt "* to continue", 0

StartingInterface
		call	  ClrLCD
		Display	  New_Operation
		call	  Switch_Lines
		Display	  Log_Information
		btfss	  PORTB,1		;wait until input is available
		goto	  $-1
		btfsc	  PORTB,6
		goto	  InvalidOption
		btfsc	  PORTB,5
		goto	  InvalidOption	
		btfsc	  PORTB,4
		goto	  Log_Info
		btfsc	  PORTB,1		;wait until input is cleared
		goto	  $-1
		goto	  DoneInterface

InvalidOption
		call	   ClrLCD
		Display		InvalidBox
		call		Switch_Lines
		Display		Cont_Msg
		call		StarPressed
		goto		StartingInterface

Log_Info
		call		ClrLCD
		Display		Pick_Log
		call		Switch_Lines
		Display		ContMsg2
		clrf		PCLATH
ReadKeypad2
	    btfss		PORTB,1    		 	;Wait until data is available from the keypad
        goto		$-1 
		movlw		b'11001111'
		call		WR_INS
		swapf		PORTB,W    			;Read PortB<7:4> into W<3:0>
        andlw		0x0F
		movwf		Log_Number 			;Save LOG NUMBER 0,1, OR 2
        call    	KPHexToChar			;Convert keypad value to LCD character  	;(value is still held in W)
		call		WR_DATA
        btfsc		PORTB,1     		;Wait until key is released
        goto		$-1
		btfss		PORTB,1				;Check if * is pressed
		goto		$-1
		btfss		PORTB,4
		goto		ReadKeypad2
		btfss		PORTB,5
		goto		ReadKeypad2
		btfsc		PORTB,6
		goto		ReadKeypad2
		btfsc		PORTB,1     		;Wait until key is released
        goto		$-1
		goto		CheckValidInput		;Check if it is a valid box number

CheckValidInput
		btfsc		Log_Number,2			;If Log > 3
		goto		InvalidLogNumber
		btfss		Log_Number,0			;If Log > 3
		goto		Goodinput
		btfss		Log_Number,1			;If Log > 3
		goto		Goodinput
		goto		InvalidLogNumber
Goodinput
		call		RetrieveAddressCounter
GoodinputLoop
		movfw		Read_Address_Counter
		addlw		0x00				;check if at 0, Z == 0 if true
		btfsc		STATUS,Z			;loop around to 255
		call		Change_Read_Address_Counter_255
		movlw		d'17'
		subwf		Read_Address_Counter,F
		movlw		d'1'
		subwf		Log_Number,F
		btfsc		STATUS,C
		goto		GoodinputLoop		;loop until log number == 0	(log number - 1 is negative)		
		call		Retrieveinfo
		bcf			PCLATH,4			;Select page 1
		bsf			PCLATH,3
		goto		EndMenu
		
InvalidLogNumber
		call		ClrLCD
		Display		InvalidBox
		call		Switch_Lines
		Display		Cont_Msg
		call		StarPressed
		goto		Log_Info


Set_Balls
		call	  ClrLCD
		movlw	  "1"
		call	  WR_DATA
		movlw	  "-"
		call	  WR_DATA
		movlw	  "R"
		call	  WR_DATA
		movlw	  "e"
		call	  WR_DATA
		movlw	  "s"
		call	  WR_DATA
		movlw	  "e"
		call	  WR_DATA
		movlw	  "t"
		call	  WR_DATA
		movlw	  " "
		call	  WR_DATA
		movlw	  "B"
		call	  WR_DATA		
		movlw	  "a"
		call	  WR_DATA
		movlw	  "l"
		call	  WR_DATA
		movlw	  "l"
		call	  WR_DATA
		movlw	  "s"
		call	  WR_DATA
		call	  Switch_Lines
		movlw	  "2"
		call	  WR_DATA
		movlw	  "-"
		call	  WR_DATA
		movlw	  "L"
		call	  WR_DATA
		movlw	  "o"
		call	  WR_DATA
		movlw	  "a"
		call	  WR_DATA
		movlw	  "d"
		call	  WR_DATA		
		movlw	  " "
		call	  WR_DATA
		movlw	  "P"
		call	  WR_DATA
		movlw	  "r"
		call	  WR_DATA
		movlw	  "e"
		call	  WR_DATA
		movlw	  "v"
		call	  WR_DATA		
		movlw	  "i"
		call	  WR_DATA
		movlw	  "o"
		call	  WR_DATA
		movlw	  "u"
		call	  WR_DATA
		movlw	  "s"
		call	  WR_DATA
		btfss	  PORTB,1		;wait until input is available
		goto	  $-1
		btfsc	  PORTB,6
		goto	  InvalidOption2
		btfsc	  PORTB,5
		goto	  InvalidOption2	
		btfsc	  PORTB,4
		goto	  Retrieve_Balls
		goto	  Reset_Balls
	
Reset_Balls
		clrf	  WhiteBalls
		clrf	  OrangeBalls
		goto	  LoadBalls

Retrieve_Balls
		call	  RetrieveLog
		goto	  LoadBalls

LoadBalls
		btfsc	  PORTB,1		;wait until input is cleared
		goto	  $-1
		call	  ClrLCD
		movlw	  "L"
		call	  WR_DATA
		movlw	  "o"
		call	  WR_DATA
		movlw	  "a"
		call	  WR_DATA		
		movlw	  "d"
		call	  WR_DATA
		movlw	  " "
		call	  WR_DATA
		movlw	  "B"
		call	  WR_DATA
		movlw	  "a"
		call	  WR_DATA	
		movlw	  "l"
		call	  WR_DATA		
		movlw	  "l"
		call	  WR_DATA
		movlw	  "s"
		call	  WR_DATA
		call	  Switch_Lines
		Display	  Cont_Msg
		call	  StarPressed
		return

InvalidOption2
		call	   ClrLCD
		Display		InvalidBox
		call		Switch_Lines
		Display		Cont_Msg
		call		StarPressed
		goto		Set_Balls

RetrieveLog
		call		RetrieveAddressCounter
		movfw		Read_Address_Counter
		addlw		0x00				;check if at 0, Z == 0 if true
		btfsc		STATUS,Z			;loop around to 255
		call		Change_Read_Address_Counter_255
		movlw		d'17'
		subwf		Read_Address_Counter,F
		call		Retrieveinfo
		return

RetrieveAddressCounter
		movlw 		0xFF				;Get Address of last operation  FE works as opposed to FF
		bcf 		STATUS,RP0
		bsf 		STATUS,RP1 ;Selects Bank 2
;Address Defined by moving Read_Address_Counter into working register
		movwf 		EEADR
		bsf 		STATUS,RP0
		bcf 		EECON1,EEPGD ;point to data memory
		bsf 		EECON1,RD ; Starts reading operation
		bcf 		STATUS,RP0
		movfw 		EEDATA
		bcf 		STATUS,RP1 ; Selects Bank 0
		movwf 		Read_Address_Counter
		return

Change_Read_Address_Counter_255
		movfw		Read_Address_Counter
		addlw		d'238'
		movwf		Read_Address_Counter
		return	

Retrieveinfo
		;Put appropriate EEPROM values into registers
		call 		Read_EEPROM
		movfw 		Memory_Temp
		movwf 		Pattern_Number_1

		incf 		Read_Address_Counter		;Put appropriate EEPROM values into registers
		call 		Read_EEPROM
		movfw 		Memory_Temp
		movwf 		Pattern_Number_2

		incf 		Read_Address_Counter		;Put appropriate EEPROM values into registers
		call 		Read_EEPROM
		movfw 		Memory_Temp
		movwf 		Pattern_Number_3

		incf 		Read_Address_Counter		;Put appropriate EEPROM values into registers
		call 		Read_EEPROM
		movfw 		Memory_Temp
		movwf 		TensMonth

		incf 		Read_Address_Counter		;Put appropriate EEPROM values into registers
		call 		Read_EEPROM
		movfw 		Memory_Temp
		movwf 		OnesMonth

		incf 		Read_Address_Counter		;Put appropriate EEPROM values into registers
		call 		Read_EEPROM
		movfw 		Memory_Temp
		movwf 		TensDay

		incf 		Read_Address_Counter		;Put appropriate EEPROM values into registers
		call 		Read_EEPROM
		movfw 		Memory_Temp
		movwf 		OnesDay

		incf		Read_Address_Counter,F				;Save Date Finished
		call 		Read_EEPROM
		movfw 		Memory_Temp
		movwf 		TensHoursFinished

		incf		Read_Address_Counter,F				;Save Date Finished
		call 		Read_EEPROM
		movfw 		Memory_Temp
		movwf 		OnesHoursFinished

		incf		Read_Address_Counter,F				;Save Date Finished
		call 		Read_EEPROM
		movfw 		Memory_Temp
		movwf 		TensMinutesFinished

		incf		Read_Address_Counter,F				;Save Date Finished
		call 		Read_EEPROM
		movfw 		Memory_Temp
		movwf 		OnesMinutesFinished

		incf		Read_Address_Counter,F				;Save Date Finished
		call 		Read_EEPROM
		movfw 		Memory_Temp
		movwf 		TensSecondsFinished

		incf		Read_Address_Counter,F				;Save Date Finished
		call 		Read_EEPROM
		movfw 		Memory_Temp
		movwf 		OnesSecondsFinished

		incf 		Read_Address_Counter		;Put appropriate EEPROM values into registers
		call 		Read_EEPROM
		movfw 		Memory_Temp
		movwf 		TSE

		incf 		Read_Address_Counter		;Put appropriate EEPROM values into registers
		call 		Read_EEPROM
		movfw 		Memory_Temp
		movwf 		OSE

		incf		Read_Address_Counter,F				;Save Date Finished
		call 		Read_EEPROM
		movfw 		Memory_Temp
		movwf 		WhiteBalls

		incf		Read_Address_Counter,F				;Save Date Finished
		call 		Read_EEPROM
		movfw 		Memory_Temp
		movwf 		OrangeBalls
		incf		Read_Address_Counter,F	

		return

Read_EEPROM
		movfw 		Read_Address_Counter
		bcf 		STATUS,RP0
		bsf 		STATUS,RP1 ;Selects Bank 2
;Address Defined by moving Read_Address_Counter into working register
		movwf 		EEADR
		bsf 		STATUS,RP0
		bcf 		EECON1,EEPGD ;point to data memory
		bsf 		EECON1,RD ; Starts reading operation
		bcf 		STATUS,RP0
		movfw 		EEDATA
		bcf 		STATUS,RP1 ; Selects Bank 0
		movwf 		Memory_Temp
		return

AddoneWhite
		;call		ClrLCD
		call		zeropointonepg0
		call		zeropointonepg0
		call		zeropointonepg0
		call		zeropointonepg0
		incf		WhiteBalls			;add one and wait until clear
;		movlw		"W"
;		call		WR_DATA
;		movlw		":"
;		call		WR_DATA
		movfw		WhiteBalls
		movwf		Memory_Temp			;put balls in temp
		movfw		Memory_Temp
		sublw		d'19'				;check if over 19
		btfss		STATUS,C			;will be set if negative
		clrf		WhiteBalls			;reset if over 19
		movfw		WhiteBalls
		movwf		Memory_Temp			;put balls in temp
		movfw		Memory_Temp
		sublw		d'9'				;check if over nine
		btfss		STATUS,C			;will be set if negative
		goto		OverNine1
		goto		DoneChecking1

AddoneOrange
	;	call		ClrLCD
		call		zeropointonepg0		;need more delay for debouncing
		call		zeropointonepg0
		call		zeropointonepg0
		call		zeropointonepg0
		incf		OrangeBalls			;add one and wait until clear
;		movlw		"O"
;		call		WR_DATA
;		movlw		":"
;		call		WR_DATA
		movfw		OrangeBalls
		movwf		Memory_Temp			;put balls in temp
		movfw		Memory_Temp
		sublw		d'19'				;check if over 19
		btfss		STATUS,C			;will be set if negative
		clrf		OrangeBalls			;reset if over 19
		movfw		OrangeBalls
		movwf		Memory_Temp			;put balls in temp
		movfw		Memory_Temp
		sublw		d'9'				;check if over nine
		btfss		STATUS,C			;will be set if negative
		goto		OverNine1
		goto		DoneChecking1

;OverNineteen
;		clrf		Memory_Temp			;reset if over 19 somehow
;		goto		DoneChecking1
		
OverNine1		
;		movlw		d'1'
;		call		OneToNine
;		call		WR_DATA
		movlw		d'10'
		subwf		Memory_Temp,F

DoneChecking1
		movfw		Memory_Temp
		btfsc		PORTA,3
		goto		$-1
		btfsc		PORTE,1
		goto		$-1
		return

Switch_To_Page1
		bcf			PCLATH,4
		bsf			PCLATH,3
		return

point01pg0
			;24998 cycles
	movlw	0x87
	movwf	lcd_d1
	movlw	0x14
	movwf	lcd_d2
point01_0pg0
	decfsz	lcd_d1, f
	goto	$+2
	decfsz	lcd_d2, f
	goto	point01_0pg0
			;2 cycles
	goto	$+1
	return

zeropointonepg0
	movlw	0x1F
	movwf	lcd_d1
	movlw	0x4F
	movwf	lcd_d2
zeropointonepg0_0
	decfsz	lcd_d1, f
	goto	$+2
	decfsz	lcd_d2, f
	goto	zeropointonepg0_0

			;2 cycles
	goto	$+1
	return
;***************************************
; Setup RTC with time defined by user
;***************************************
;set_rtc_time
;
;		rtc_resetAll	;reset rtc
;
;		rtc_set	0x00,	B'10000000'
;
;		;set time 
;		rtc_set	0x06,	B'00010011'		; Year
;		rtc_set	0x05,	B'00000100'		; Month
;		rtc_set	0x04,	B'00000001'		; Date
;		rtc_set	0x03,	B'00000010'		; Day
;		rtc_set	0x02,	B'00100001'		; Hours
;		rtc_set	0x01,	B'01000000'		; Minutes
;		rtc_set	0x00,	B'00000000'		; Seconds
;		return
;
;show_RTC
;		;clear LCD screen
;		movlw	b'00000001'
;		call	WR_INS
;
;		;Get year
;		movlw	"2"				;First line shows 20**/**/**
;		call	WR_DATA
;		movlw	"0"
;		call	WR_DATA
;		rtc_read	0x06		;Read Address 0x06 from DS1307---year
;		movfw	0x77
;		call	WR_DATA
;		movfw	0x78
;		call	WR_DATA
;
;		movlw	"/"
;		call	WR_DATA
;
;		;Get month
;		rtc_read	0x05		;Read Address 0x05 from DS1307---month
;		movfw	0x77
;		call	WR_DATA
;		movfw	0x78
;		call	WR_DATA
;
;		movlw	"/"
;		call	WR_DATA
;
;		;Get day
;		rtc_read	0x04		;Read Address 0x04 from DS1307---day
;		movfw	0x77
;		call	WR_DATA
;		movfw	0x78
;		call	WR_DATA
;
;		movlw	B'11000000'		;Next line displays (hour):(min):(sec) **:**:**
;		call	WR_INS
;
;		;Get hour
;		rtc_read	0x02		;Read Address 0x02 from DS1307---hour
;		movfw	0x77
;		call	WR_DATA
;		movfw	0x78
;		call	WR_DATA
;		movlw			":"
;		call	WR_DATA
;
;		;Get minute		
;		rtc_read	0x01		;Read Address 0x01 from DS1307---min
;		movfw	0x77
;		call	WR_DATA
;		movfw	0x78
;		call	WR_DATA		
;		movlw			":"
;		call	WR_DATA
;		
;		;Get seconds
;		rtc_read	0x00		;Read Address 0x00 from DS1307---seconds
;		movfw	0x77
;		call	WR_DATA
;		movfw	0x78
;		call	WR_DATA
;		
;		call	OneS			;Delay for exactly one seconds and read DS1307 again
;		goto	show_RTC

;***************************************
; Delay 1s
;***************************************
OneS
		local	OneS_0
      movlw 0x10
      movwf COUNTH
      movlw 0x7A
      movwf COUNTM
      movlw 0x06
      movwf COUNTL

OneS_0
      decfsz COUNTH, f
      goto   $+2
      decfsz COUNTM, f
      goto   $+2
      decfsz COUNTL, f
      goto   OneS_0

      goto $+1
      nop
      nop
		return

StarPressed
		btfss		PORTB,1				;Check if * is pressed
		goto		StarPressed
		btfss		PORTB,4
		goto		$-1
		btfss		PORTB,5
		goto		StarPressed
		btfsc		PORTB,6
		goto		StarPressed
		btfsc		PORTB,1     		;Wait until key is released
        goto		$-1
		return

;***************************************
; Allows user to enter # of boxes (1-3)
;***************************************
EnterBoxes
		clrf		Pattern_Counter		;Set Pattern Counter to 0
DisplayEnterBoxes						;Display Enter # of boxes
		call		ClrLCD
		Display		Enter_Boxes	
		call		Switch_Lines
		Display		Cont_Msg

ReadKeypad
	    btfss		PORTB,1    		 	;Wait until data is available from the keypad
        goto		$-1 
		movlw		b'11001111'
		call		WR_INS
		swapf		PORTB,W    			;Read PortB<7:4> into W<3:0>
        andlw		0x0F
		movwf		Number_of_boxes 	;Save number of boxes
		incf		Number_of_boxes
        call    	KPHexToChar			;Convert keypad value to LCD character  	;(value is still held in W)
		call		WR_DATA
        btfsc		PORTB,1     		;Wait until key is released
        goto		$-1
		btfss		PORTB,1				;Check if * is pressed
		goto		$-1
		btfss		PORTB,4
		goto		ReadKeypad
		btfss		PORTB,5
		goto		ReadKeypad
		btfsc		PORTB,6
		goto		ReadKeypad
		btfsc		PORTB,1     		;Wait until key is released
        goto		$-1
		goto		CheckValidBox		;Check if it is a valid box number

CheckValidBox
		btfsc		Number_of_boxes,2			;If Number_of_boxes > 3
		goto		InvalidBoxNumber
		btfsc		Number_of_boxes,3			;If Number_of_boxes > 3
		goto		InvalidBoxNumber
		return
		
InvalidBoxNumber
		call		ClrLCD
		Display		InvalidBox
		call		Switch_Lines
		Display		Cont_Msg
		call		StarPressed
		goto		EnterBoxes

;*************************
; User enters patterns (1-6)
;***************************************		
EnterPattern
		incf		Pattern_Counter			;Count which pattern is being loaded
InvalidPatternStart
		call 		ClrLCD
		Display 	Enter_Pattern
		movf		Pattern_Counter,W    	;Read Pattern counter<3:0> into W<3:0>
		andlw		0x0F
		call    	OneToNine				;Display up to nine.
		call		WR_DATA
		call		Switch_Lines
		Display		Cont_Msg

;******************************************************************
; Continually reads keypad and updates pattern until '*' is pressed
;******************************************************************
PatternWaiting
		btfss		PORTB,1     			;Wait until data is available from the keypad
        goto		$-1 
		movlw		b'11001111'
		call		WR_INS
		swapf		PORTB,W    				;Read PortB<7:4> into W<3:0>
        andlw		0x0F

CheckValidPattern
		btfss		PORTB,4					
		goto		GoodPattern
		btfss		PORTB,5			
		goto		GoodPattern

InvalidPatternNumber
		call		ClrLCD
		Display		Invalid_Pattern_Number
		bcf			PCLATH,4
		bcf			PCLATH,3
		call		Switch_Lines
		Display		Cont_Msg
		call		StarPressed
		goto		InvalidPatternStart

GoodPattern
		btfsc		Pattern_Counter,0		;Check if on pattern 1 or 3
		call		UpdatePattern1
		btfss		Pattern_Counter,0		;Check if possibly on pattern 2
		call		UpdatePattern2
		movwf		Pattern_Number_Current
        call    	KPHexToChar 			;Convert keypad value to LCD character 	(value is still held in W)
		call		WR_DATA
        btfsc		PORTB,1    			 	;Wait until key is released
        goto		$-1						
		btfss		PORTB,1					;Check if '*' is pressed
		goto		$-1
		btfss		PORTB,4
		goto		PatternWaiting
		btfss		PORTB,5
		goto		PatternWaiting
		btfsc		PORTB,6
		goto		PatternWaiting
		btfsc		PORTB,1     		;Wait until key is released
        goto		$-1

;******************************************************************
; Checks to see if 
; # of patterns have been entered
;******************************************************************
CheckDone
		movf		Number_of_boxes,W		;move # of boxes to working register
		subwf		Pattern_Counter,W	
		btfss		STATUS,C
		goto		EnterPattern
		call		FinalMessages
		call		AnytoStart
		return

;*******************************************
; Displays the patterns the user has entered
;*******************************************
FinalMessages
        call 		ClrLCD
		Display		PatternsEntered
		movf		Pattern_Number_1,W
		call    	KPHexToChar 				;Display up to nine.
		call		WR_DATA
		movlw		","
		call		WR_DATA						;insert comma
		movf		Pattern_Number_2,W
		call    	KPHexToChar 				;Display up to nine.
		call		WR_DATA
		movlw		","
		call		WR_DATA						;insert comma
		movf		Pattern_Number_3,W
		call    	KPHexToChar					;Display up to nine.
		call		WR_DATA
		return

AnytoStart
		call		Switch_Lines
		Display		Start_Msg
	    btfss		PORTB,1    		 	;Wait until data is available from the keypad
        goto		$-1 
SaveTimeStarted
		rtc_read	0x00		;Read Address 0x01 from DS1307---sec
		movfw		0x77
		movwf		TensSecondsStarted
		movfw		0x78	
		movwf		OnesSecondsStarted
		rtc_read	0x01		;Read Address 0x01 from DS1307---min
		movfw		0x77
		movwf		TensMinutesStarted
		movfw		0x78	
		movwf		OnesMinutesStarted
		rtc_read	0x02		;Read Address 0x01 from DS1307---hrs
		movfw		0x77
		movwf		TensHoursStarted
		movfw		0x78	
		movwf		OnesHoursStarted
		clrf		Pattern_Counter		;Clear the pattern counter for autonomous part
;Enabletimer0
; 		bcf STATUS,RP0 ; select bank 0
;		bsf INTCON,7 ;global interrupts enabled
;		bcf INTCON,1 ;clear the interrput flag
;		bsf STATUS,RP0
;		bsf OPTION_REG, INTEDG ;set it so that the interrput only occurs on the positive edge
;		bcf OPTION_REG,T0CS ;set internal clock source
;		bcf OPTION_REG,T0SE ;read on low to high edges
;		bcf OPTION_REG,PSA ;use prescaler
;		bsf OPTION_REG,PS0
;		bsf OPTION_REG,PS1 ;use a prescaler of 256
;		bsf OPTION_REG,PS2
;		bcf STATUS,RP0
		return

;*************************************************************************************
; End message - Input:-, Output:-
;*************************************************************************************
DoneAutonomous
		call		ClrLCD
		movlw		"D"
		call		WR_DATA
		movlw		"o"
		call		WR_DATA
		movlw		"n"
		call		WR_DATA
		movlw		"e"
		call		WR_DATA
		;call		SaveOperationNumber
SaveEndTimes
		rtc_read	0x00		;Read Address 0x01 from DS1307---sec
		movfw		0x77
		movwf		TensSecondsFinished
		movfw		0x78	
		movwf		OnesSecondsFinished
		rtc_read	0x01		;Read Address 0x01 from DS1307---min
		movfw		0x77
		movwf		TensMinutesFinished
		movfw		0x78	
		movwf		OnesMinutesFinished
		rtc_read	0x02		;Read Address 0x01 from DS1307---hrs
		movfw		0x77
		movwf		TensHoursFinished
		movfw		0x78	
		movwf		OnesHoursFinished
		call		Switch_Lines
		Display		Cont_Msg
		call		StarPressed

Standby
		call		ClrLCD

		;Get year
		movlw	"2"				;First line shows 20**/**/**
		call	WR_DATA
		movlw	"0"
		call	WR_DATA
		rtc_read	0x06		;Read Address 0x06 from DS1307---year
		movfw	0x77
		call	WR_DATA
		movfw	0x78
		call	WR_DATA

		movlw	"/"
		call	WR_DATA

		;Get month
		rtc_read	0x05		;Read Address 0x05 from DS1307---month
		movfw	0x77
		movwf	TensMonth
		call	WR_DATA
		movfw	0x78
		movwf	OnesMonth
		call	WR_DATA

		movlw	"/"
		call	WR_DATA

		;Get day
		rtc_read	0x04		;Read Address 0x04 from DS1307---day
		movfw	0x77
		movwf	TensDay
		call	WR_DATA
		movfw	0x78
		movwf	OnesDay
		call	WR_DATA
		movlw	B'11000000'		;Next line displays (hour):(min):(sec) **:**:**
		call	WR_INS

		movfw		TensHoursFinished
		call		writenumber
		movfw		OnesHoursFinished
		call		writenumber
		movlw		":"
		call		WR_DATA
		movfw		TensMinutesFinished
		call		writenumber
		movfw		OnesMinutesFinished
		call		writenumber
		movlw		":"
		call		WR_DATA
		movfw		TensSecondsFinished
		call		writenumber
		movfw		OnesSecondsFinished
		call		writenumber
		Display		Press_Star
SaveTopEEPROM							;Save the patterns into EEPROM,and time elapsed
												;Save the patterns into EEPROM

;RetrieveAddressCounter
		movlw 		0xFF				;Get the ending Address of last operation
		bcf 		STATUS,RP0
		bsf 		STATUS,RP1 ;Selects Bank 2
;Address Defined by moving Read_Address_Counter into working register
		movwf 		EEADR
		bsf 		STATUS,RP0
		bcf 		EECON1,EEPGD ;point to data memory
		bsf 		EECON1,RD ; Starts reading operation
		bcf 		STATUS,RP0
		movfw 		EEDATA
		bcf 		STATUS,RP1 ; Selects Bank 0
		movwf 		Address_Counter		;Put into address counter
		
;		
		call 	WriteEEPROMTop
		movfw 	Pattern_Number_1
		movwf	Memory_Temp
		call	WriteEEPROMBottom

		incf	Address_Counter,F
		call 	WriteEEPROMTop
		movfw 	Pattern_Number_2
		movwf	Memory_Temp
		call	WriteEEPROMBottom

		incf	Address_Counter,F
		call 	WriteEEPROMTop
		movfw 	Pattern_Number_3
		movwf	Memory_Temp
		call	WriteEEPROMBottom

		incf	Address_Counter,F				;Save Date Finished
		call 	WriteEEPROMTop
		movfw 	TensMonth
		movwf	Memory_Temp
		call	WriteEEPROMBottom

		incf	Address_Counter,F
		call 	WriteEEPROMTop
		movfw 	OnesMonth
		movwf	Memory_Temp
		call	WriteEEPROMBottom

		incf	Address_Counter,F
		call 	WriteEEPROMTop
		movfw 	TensDay
		movwf	Memory_Temp
		call	WriteEEPROMBottom

		incf	Address_Counter,F
		call 	WriteEEPROMTop
		movfw 	OnesDay
		movwf	Memory_Temp
		call	WriteEEPROMBottom

		incf	Address_Counter,F				;Save Date Finished
		call 	WriteEEPROMTop
		movfw 	TensHoursFinished
		movwf	Memory_Temp
		call	WriteEEPROMBottom

		incf	Address_Counter,F
		call 	WriteEEPROMTop
		movfw 	OnesHoursFinished
		movwf	Memory_Temp
		call	WriteEEPROMBottom

		incf	Address_Counter,F
		call 	WriteEEPROMTop
		movfw 	TensMinutesFinished
		movwf	Memory_Temp
		call	WriteEEPROMBottom

		incf	Address_Counter,F
		call 	WriteEEPROMTop
		movfw 	OnesMinutesFinished
		movwf	Memory_Temp
		call	WriteEEPROMBottom

		incf	Address_Counter,F
		call 	WriteEEPROMTop
		movfw 	TensSecondsFinished
		movwf	Memory_Temp
		call	WriteEEPROMBottom

		incf	Address_Counter,F
		call 	WriteEEPROMTop
		movfw 	OnesSecondsFinished
		movwf	Memory_Temp
		call	WriteEEPROMBottom

OnesSecSub
		movfw		OnesSecondsStarted
		subwf		OnesSecondsFinished,W
		btfss		STATUS,C
		goto		onessecnegative
		movwf		OSE

TensSecSub
		movfw		TensSecondsStarted
		subwf		TensSecondsFinished,W
		btfss		STATUS,C
		goto		TensSecnegative
		movwf		TSE

OnesMinSub
		movfw		OnesMinutesStarted
		subwf		OnesMinutesFinished,W
		btfss		STATUS,C
		goto		OnesMinnegative
		movwf		OME

TensMinSub
		movfw		TensMinutesStarted
		subwf		TensMinutesFinished,W
		btfss		STATUS,C
		goto		TensMinnegative
		movwf		TME

OnesHrsSub
		movfw		OnesHoursStarted
		subwf		OnesHoursFinished,W
		btfss		STATUS,C
		goto		OnesHrsnegative
		movwf		OHE

TensHrsSub
		movfw		TensHoursStarted				;No way it can be negative unless 24
		subwf		TensHoursFinished,W
		btfss		STATUS,C
		goto		TensHrsnegative
		movwf		THE
		goto		endsubtract

TensHrsnegative
		movlw		d'24'
		addwf		TensHoursFinished,F
		goto		TensHrsSub
	
OnesHrsnegative
		decf		TensHoursFinished,F
		movlw		d'10'
		addwf		OnesHoursFinished,F
		goto		OnesHrsSub	

TensMinnegative
		decf		OnesHoursFinished,F
		movlw		d'6'
		addwf		TensMinutesFinished,F
		goto		TensMinSub	

OnesMinnegative
		decf		TensMinutesFinished,F
		movlw		d'10'
		addwf		OnesMinutesFinished,F
		goto		OnesMinSub	

TensSecnegative
		decf		OnesMinutesFinished,F
		movlw		d'6'
		addwf		TensSecondsFinished,F
		goto		TensSecSub

onessecnegative
		decf		TensSecondsFinished,F
		movlw		d'10'
		addwf		OnesSecondsFinished,F
		goto		OnesSecSub

endsubtract	

SaveBottomEEPROM
		incf	Address_Counter,F				;Save Time elapsed
		call 	WriteEEPROMTop
		movfw 	TSE
		movwf	Memory_Temp
		call	WriteEEPROMBottom

		incf	Address_Counter,F
		call 	WriteEEPROMTop
		movfw 	OSE
		movwf	Memory_Temp
		call	WriteEEPROMBottom

		incf	Address_Counter,F				;Save Balls Left
		call 	WriteEEPROMTop
		movfw 	WhiteBalls
		movwf	Memory_Temp
		call	WriteEEPROMBottom

		incf	Address_Counter,F
		call 	WriteEEPROMTop
		movfw 	OrangeBalls
		movwf	Memory_Temp
		call	WriteEEPROMBottom
		incf	Address_Counter,F

		clrw										;ensure both are cleared
		movf		Address_Counter,W				
		sublw		0xFF							;subtract address counter from 247  (13x19 + 1 to account for 0) 
		btfsc		STATUS,Z						;zero bit to see if 0
		clrf		Address_Counter


		call	SaveAddressCounter
		call		StarPressed
		return

SaveAddressCounter						;Save the addressCounter to always know where to go
		movlw		0xFF
		bcf			STATUS,RP0
		bsf			STATUS,RP1
		movwf		EEADR
		bcf			STATUS,RP1			;switch banks to get data
		movfw		Address_Counter
		bsf			STATUS,RP1
		movwf		EEDATA
		bsf			STATUS,RP0
		bcf			EECON1,EEPGD
		bsf			EECON1,WREN
		movlw		0x55
		movwf		EECON2
		movlw		0xaa
		movwf		EECON2
		bsf			EECON1,WR
		btfsc		EECON1,WR
		goto		$-1
		bcf 		EECON1, WREN
		bcf			STATUS,RP0
		bcf			STATUS,RP1		
		bcf			PIR2,EEIF
		return


WriteEEPROMTop
		movfw		Address_Counter
		bcf			STATUS,RP0
		bsf			STATUS,RP1
		movwf		EEADR
		bcf			STATUS,RP1			;switch banks to get data
		return

WriteEEPROMBottom
		movfw		Memory_Temp
		bsf			STATUS,RP1
		movwf		EEDATA
		bsf			STATUS,RP0
		bcf			EECON1,EEPGD
		bsf			EECON1,WREN
		movlw		0x55
		movwf		EECON2
		movlw		0xaa
		movwf		EECON2
		bsf			EECON1,WR
		btfsc		EECON1,WR
		goto		$-1
		bcf 		EECON1, WREN
		bcf			STATUS,RP0
		bcf			STATUS,RP1		
		bcf			PIR2,EEIF
		return

writenumber
		andlw		0x0F
		call		OneToNine
		call		WR_DATA
		return	

UpdatePattern1
		btfsc		Pattern_Counter,1	;See if its pattern 3
		goto		UpdatePattern3
		movwf		Pattern_Number_1 	;Save pattern 1
		return

UpdatePattern2
		btfss		Pattern_Counter,1	;Confirm pattern 2
		return							;return if not
		movwf		Pattern_Number_2 	;Save pattern 2
		return

UpdatePattern3
		movwf		Pattern_Number_3 	;Save pattern 3
		return

ButtonReleased
	    btfsc		PORTB,1    		 	;Wait until released
        goto		$-1 
		return


;******* LCD-related subroutines *******
lcdLongDelay
    movlw d'20'
    movwf lcd_d2
LLD_LOOP
    LCD_DELAY
    decfsz lcd_d2,f
    goto LLD_LOOP
    return

		ORG 0x0800		;Page 1.  See if this helps
;********************************************************************
; Loads one box of balls completely as specified by current pattern #
;********************************************************************
Loading
		call		LoadPatternNumber
		call		LoadPattern	
		call		LoadBox

;*************************************************************************************
; Loads the numerical pattern # - Input:Pattern_Counter, Output:Pattern_Number_Current
;*************************************************************************************
LoadPatternNumber	
		btfsc		Pattern_Counter,1		;Load the proper pattern
		goto		LoadPatternNumber3		;Remember pattern1 = 0, pattern2 = 1, pattern3 = 2 due to keypad setup
		btfss		Pattern_Counter,0		
		goto		LoadPatternNumber1
		goto		LoadPatternNumber2

LoadPatternNumber1
		movf		Pattern_Number_1,W
		movwf		Pattern_Number_Current
		return

LoadPatternNumber2
		movf		Pattern_Number_2,W
		movwf		Pattern_Number_Current
		return

LoadPatternNumber3
		movf		Pattern_Number_3,W
		movwf		Pattern_Number_Current
		return

;********************************************************
; Displays pattern # being outputted (FOR DEBUGGING ONLY) 
;********************************************************
LoadPattern
		btfss		Pattern_Number_Current,2		;Check if low pattern or high pattern
		goto		LowPattern
		goto		HighPattern

;*************************************************************************************
; Load the proper pattern shape - Input:Pattern_Number_Current, Output:Pattern_Current
;*************************************************************************************
LowPattern
		btfsc		Pattern_Number_Current,1		;Check if 3 should be loaded
		goto		LoadPattern3
		btfsc		Pattern_Number_Current,0
		goto		LoadPattern2
		goto		LoadPattern1

LoadPattern3
		movfw		Third_Pattern			;Put third shape into current shape
		movwf		Pattern_Current
		goto		DoneLoadingPattern

LoadPattern2
		movfw		Second_Pattern			;Put third shape into current shape
		movwf		Pattern_Current
		goto		DoneLoadingPattern

LoadPattern1
		movfw		First_Pattern			;Put third shape into current shape
		movwf		Pattern_Current
		goto		DoneLoadingPattern
	
HighPattern
		btfsc		Pattern_Number_Current,1		;Check if 6 should be loaded
		goto		LoadPattern6
		btfsc		Pattern_Number_Current,0
		goto		LoadPattern5
		goto		LoadPattern4

LoadPattern6
		movf		Sixth_Pattern,W			;Put third shape into current shape
		movwf		Pattern_Current
		goto		DoneLoadingPattern

LoadPattern5
		movf		Fifth_Pattern,W			;Put third shape into current shape
		movwf		Pattern_Current
		goto		DoneLoadingPattern

LoadPattern4
		movf		Fourth_Pattern,W			;Put third shape into current shape
		movwf		Pattern_Current
		goto		DoneLoadingPattern

DoneLoadingPattern
		return

;Could be looped later
;***************************************************
; Dispense 6 balls - Input:Pattern_Current, Output:-
;***************************************************
LoadBox
		movlw		D'6'
		movwf		Box_Counter

LoadBoxLoop
		decf		Box_Counter,F					;Decrement counter
		call		DispenseBall					;Dispense a ball of appropriate colour
		rlf			Pattern_Current,F
		call		CC_60					;Rotate to next slot			
		btfsc		Box_Counter,2					;Check if done
		goto		LoadBoxLoop				
		btfsc		Box_Counter,1
		goto		LoadBoxLoop
		btfsc		Box_Counter,0
		goto		LoadBoxLoop
		call		zeropointone					;wait for last ball
		bcf			PORTC,0
		btfsc		Pattern_Counter,0				;Check if on first box
		goto		NotFirstBox
		btfsc		Pattern_Counter,1
		goto		NotFirstBox
		call		MoveFirstBox
DoneMoving
		incf		Pattern_Counter
		movf		Number_of_boxes,W				;check if loaded enough boxes
		subwf		Pattern_Counter,W	
		btfss		STATUS,C
		goto		Loading
		return

NotFirstBox
		call		MoveBox
		goto		DoneMoving

MoveFirstBox
		bsf			PORTA,0					;Start Motor
		call		QS				; Pulse width modulation
		call		QS				;added this and the 0.01
		call		QS
		;call		point01
		bcf			PORTA,0
		call		zeropointone
		call		point06
		call		point01
		btfss		PORTC,0
		goto		MoveFirstBox
		return

;***********************************************************************
; Moves the loaded box - Input:Pattern_Counter, Output:Pattern_Counter++
;***********************************************************************
MoveBox
		bcf			PORTC,0					;make sure this is cleared
		bsf			PORTA,0					;Start Motor
;		call		zeropointone
;		call		zeropointone
;		call		zeropointone
;		call		zeropointone
;		call		zeropointone
;		call		zeropointone
		call		HalfS					;0.6 seconds works!!
		;call		HalfS					;put in an extra half S for boast
		;call		HalfS
		;call		HalfS
		;call		zeropointone
		bcf			PORTA,0
MoveBoxSlow
		bsf			PORTA,0					;Start Motor
		call		QS				; Pulse width modulation
		call		point06
		call		point06
		call		zeropointone;		call		zeropointone		;put this in
		bcf			PORTA,0
		call		QS			;point06 works!!
		call		QS
		call		QS
		call		zeropointone		
		btfss		PORTC,0
		goto		MoveBoxSlow
		return

;*************************************************************************************
; Dispenses a ball - Input:Pattern_Current, Output:-
;*************************************************************************************
DispenseBall
		btfss		Pattern_Current,5			;Check if white or orange ball should be dispensed
		goto		DispenseOrange
		goto		DispenseWhite

DispenseOrange
		call		SeparatorCC60						;Turn 60 degrees counter clockwise
		call		SeparatorClock60
		decf		OrangeBalls,F
		goto		DoneDispensing

DispenseWhite
		call		SeparatorClock60				;Turn 60 degrees clockwise
		call		SeparatorCC60	
		decf		WhiteBalls,F
		goto		DoneDispensing

;Replace RB0, with RA2
SeparatorClock60
		movlw		"2"
		movwf		Counter
SeparatorClock_Rotation
		decf		Counter
		bsf			PORTD,1
		bcf			PORTD,0
		bcf			PORTA,2
		bsf			PORTB,2
		call		Appropriate_Delay
		bsf			PORTD,1
		bsf			PORTD,0
		bcf			PORTA,2
		bcf			PORTB,2
		call		Appropriate_Delay
		bcf			PORTD,1
		bsf			PORTD,0
		bsf			PORTA,2
		bcf			PORTB,2
		call		Appropriate_Delay
		bcf			PORTD,1
		bcf			PORTD,0
		bsf			PORTA,2
		bsf			PORTB,2
		call		Appropriate_Delay
		btfsc		Counter,0
		goto		SeparatorClock_Rotation
		bsf			PORTD,1
		bcf			PORTD,0
		bcf			PORTA,2
		bsf			PORTB,2
		return

SeparatorCC60
		movlw		"2"
		movwf		Counter
SeparatorCC_Rotation
		decf		Counter
		bsf			PORTD,1
		bcf			PORTD,0
		bcf			PORTA,2
		bsf			PORTB,2
		call		Appropriate_Delay
		bcf			PORTD,1
		bcf			PORTD,0
		bsf			PORTA,2
		bsf			PORTB,2
		call		Appropriate_Delay
		bcf			PORTD,1
		bsf			PORTD,0
		bsf			PORTA,2
		bcf			PORTB,2
		call		Appropriate_Delay
		bsf			PORTD,1
		bsf			PORTD,0
		bcf			PORTA,2
		bcf			PORTB,2
		call		Appropriate_Delay
		bsf			PORTD,1
		bcf			PORTD,0
		bcf			PORTA,2
		bsf			PORTB,2
		call		Appropriate_Delay
		bcf			PORTD,1
		bcf			PORTD,0
		bsf			PORTA,2
		bsf			PORTB,2
		call		Appropriate_Delay
		bcf			PORTD,1
		bsf			PORTD,0
		bsf			PORTA,2
		bcf			PORTB,2
		call		Appropriate_Delay
		bsf			PORTD,1
		bsf			PORTD,0
		bcf			PORTA,2
		bcf			PORTB,2
		call		Appropriate_Delay
		bsf			PORTD,1
		bcf			PORTD,0
		bcf			PORTA,2
		bsf			PORTB,2
		return

CC_60
		decf		Counter
		bsf			PORTC,5
		bcf			PORTC,1
		bcf			PORTE,0
		bsf			PORTC,7
		call		Disk_Delay
		bsf			PORTC,5
		bsf			PORTC,1
		bcf			PORTE,0
		bcf			PORTC,7
		call		Disk_Delay
		bcf			PORTC,5
		bsf			PORTC,1
		bsf			PORTE,0
		bcf			PORTC,7
		call		Disk_Delay
		bcf			PORTC,5
		bcf			PORTC,1
		bsf			PORTE,0
		bsf			PORTC,7
		call		Disk_Delay
		bsf			PORTC,5
		bcf			PORTC,1
		bcf			PORTE,0
		bsf			PORTC,7
		call		Disk_Delay
		bsf			PORTC,5
		bsf			PORTC,1
		bcf			PORTE,0
		bcf			PORTC,7
		call		Disk_Delay
		bcf			PORTC,5
		bsf			PORTC,1
		bsf			PORTE,0
		bcf			PORTC,7
		call		Disk_Delay
		bcf			PORTC,5
		bcf			PORTC,1
		bsf			PORTE,0
		bsf			PORTC,7
		call		Disk_Delay
		bsf			PORTC,5
		bcf			PORTC,1
		bcf			PORTE,0
		bsf			PORTC,7
		return

Appropriate_Delay
		;call	zeropointone					;0.12s works
		;call	HalfS
		call	QS
		call	point06
		call	point01
		return

Disk_Delay
		call		zeropointone				;This works!!!
		return

DoneDispensing
;		btfss		Box_Counter,1			
;		goto		NormalDelay
;		btfss		Box_Counter,0			
;		goto		NormalDelay
;		call		HalfS					;extra delay at spot 3
;NormalDelay
		call		zeropointone
		call		zeropointone
		call		zeropointone
		call		zeropointone
		call		zeropointone
		call		zeropointone
		call		zeropointone
		call		zeropointone
		call		zeropointone
		call		zeropointone
		call		zeropointone
		return


;***************************************
; Delay 0.5s
;***************************************
HalfS	
	local	HalfS_0
      movlw 0x88
      movwf COUNTH
      movlw 0xBD
      movwf COUNTM
      movlw 0x03
      movwf COUNTL

HalfS_0
      decfsz COUNTH, f
      goto   $+2
      decfsz COUNTM, f
      goto   $+2
      decfsz COUNTL, f
      goto   HalfS_0

      goto $+1
      nop
      nop
		return

point06
			;79998 cycles
	movlw	0xDF
	movwf	lcd_d1
	movlw	0x2F
	movwf	lcd_d2
point06_0
	decfsz	lcd_d1, f
	goto	$+2
	decfsz	lcd_d2, f
	goto	point06_0

			;2 cycles
	goto	$+1
	return

; Delay = 0.01 seconds
; Clock frequency = 10 MHz

; Actual delay = 0.01 seconds = 25000 cycles
; Error = 0 %

point01
			;24998 cycles
	movlw	0x87
	movwf	lcd_d1
	movlw	0x14
	movwf	lcd_d2
point01_0
	decfsz	lcd_d1, f
	goto	$+2
	decfsz	lcd_d2, f
	goto	point01_0
			;2 cycles
	goto	$+1
	return

zeropointone
	movlw	0x1F
	movwf	lcd_d1
	movlw	0x4F
	movwf	lcd_d2
Delay_0
	decfsz	lcd_d1, f
	goto	$+2
	decfsz	lcd_d2, f
	goto	Delay_0

			;2 cycles
	goto	$+1
	return
;***************************************
; Delay 0.03s
;***************************************
QS	
			;19993 cycles
	movlw	0x9E
	movwf	lcd_d1
	movlw	0x10
	movwf	lcd_d2
QS_0
	decfsz	lcd_d1, f
	goto	$+2
	decfsz	lcd_d2, f
	goto	QS_0

			;3 cycles
	goto	$+1
	nop

			;4 cycles (including call)
	return

;*************************************************************************************
; End Data - Input:PORTB<6:4>, Output:-
;*************************************************************************************
EndMenu
		bcf			PCLATH,4
		bcf 		PCLATH,3    		;calls functions in Page 0 
		call		ClrLCD
		movlw		"1"
		call		WR_DATA
		movlw		"-"
		call		WR_DATA
		movlw		"P"
		call		WR_DATA
		movlw		"a"
		call		WR_DATA
		movlw		"t"
		call		WR_DATA
		movlw		"t"
		call		WR_DATA
		movlw		"e"
		call		WR_DATA
		movlw		"r"
		call		WR_DATA
		movlw		"n"
		call		WR_DATA
		movlw		"s"
		call		WR_DATA
		call		Switch_Lines
		movlw		"2"
		call		WR_DATA
		movlw		"-"
		call		WR_DATA
		movlw		"B"
		call		WR_DATA
		movlw		"a"
		call		WR_DATA
		movlw		"l"
		call		WR_DATA
		movlw		"l"
		call		WR_DATA
		movlw		"s"
		call		WR_DATA
		movlw		" "
		call		WR_DATA
		movlw		"3"
		call		WR_DATA
		movlw		"-"
		call		WR_DATA
		movlw		"M"
		call		WR_DATA
		movlw		"o"
		call		WR_DATA
		movlw		"r"
		call		WR_DATA
		movlw		"e"
		call		WR_DATA	
		bcf			PCLATH,4			;go back to Page 1
		bsf 		PCLATH,3  
	    btfss		PORTB,1    		 	;Wait until data is available from the keypad
        goto		$-1 
		goto		CheckvalidPage1 
GoodinputPage1
		btfsc		PORTB,4
		goto		BallsMenu
		btfsc		PORTB,5
		goto		SecondPageEndMenu
		goto		PatternsChosen

CheckvalidPage1 
		btfsc		PORTB,6			;If input > 3
		goto		InvalidPage1
		btfss		PORTB,4			;If input > 3
		goto		GoodinputPage1
		btfss		PORTB,5			;If input > 3
		goto		GoodinputPage1
		goto		InvalidPage1

InvalidPage1	
		call		InvalidPage
		bcf			PCLATH,4		;call page 1 things
		bsf			PCLATH,3
		goto		EndMenu

InvalidPage
		bcf			PCLATH,4		;call page 0 things
		bcf			PCLATH,3
		call		ClrLCD
		movlw		"I"
		call		WR_DATA
		movlw		"n"
		call		WR_DATA
		movlw		"v"
		call		WR_DATA
		movlw		"a"
		call		WR_DATA
		movlw		"l"
		call		WR_DATA
		movlw		"i"
		call		WR_DATA
		movlw		"d"
		call		WR_DATA
		movlw		" "
		call		WR_DATA
		movlw		"I"
		call		WR_DATA
		movlw		"n"
		call		WR_DATA
		movlw		"p"
		call		WR_DATA
		movlw		"u"
		call		WR_DATA
		movlw		"t"
		call		WR_DATA
		call		Switch_Lines
		movlw		"*"
		call		WR_DATA
		movlw		" "
		call		WR_DATA
		movlw		"t"
		call		WR_DATA
		movlw		"o"
		call		WR_DATA
		movlw		" "
		call		WR_DATA
		movlw		"c"
		call		WR_DATA
		movlw		"o"
		call		WR_DATA
		movlw		"n"
		call		WR_DATA
		movlw		"t"
		call		WR_DATA
		movlw		"i"
		call		WR_DATA
		movlw		"n"
		call		WR_DATA
		movlw		"u"
		call		WR_DATA
		movlw		"e"
		call		WR_DATA	
		call		StarPressed
		return


SecondPageEndMenu
		bcf			PCLATH,4
		bcf 		PCLATH,3    		;calls functions in Page 0 
		call		ClrLCD
		movlw		"1"
		call		WR_DATA
		movlw		"-"
		call		WR_DATA
		movlw		"D"
		call		WR_DATA
		movlw		"a"
		call		WR_DATA
		movlw		"t"
		call		WR_DATA
		movlw		"e"
		call		WR_DATA
		movlw		" "
		call		WR_DATA
		movlw		"E"
		call		WR_DATA
		movlw		"n"
		call		WR_DATA
		movlw		"d"
		call		WR_DATA
		movlw		"e"
		call		WR_DATA
		movlw		"d"
		call		WR_DATA
		call		Switch_Lines
		movlw		"2"
		call		WR_DATA
		movlw		"-"
		call		WR_DATA
		movlw		"T"
		call		WR_DATA
		movlw		"i"
		call		WR_DATA
		movlw		"m"
		call		WR_DATA
		movlw		"e"
		call		WR_DATA
		movlw		" "
		call		WR_DATA
		movlw		"3"
		call		WR_DATA
		movlw		"-"
		call		WR_DATA
		movlw		"M"
		call		WR_DATA
		movlw		"o"
		call		WR_DATA
		movlw		"r"
		call		WR_DATA
		movlw		"e"
		call		WR_DATA
		bcf			PCLATH,4			;go back to Page 1
		bsf 		PCLATH,3  
	    btfss		PORTB,1    		 	;Wait until data is available from the keypad
        goto		$-1 
		goto		CheckvalidPage2
GoodinputPage2
		btfsc		PORTB,4
		goto		DisplayTimeTaken
		btfsc		PORTB,5
		goto		ThirdPageEndMenu
		goto		DisplayDateEnded

CheckvalidPage2 
		btfsc		PORTB,6			;If input > 3
		goto		InvalidPage2
		btfss		PORTB,4			;If input > 3
		goto		GoodinputPage2
		btfss		PORTB,5			;If input > 3
		goto		GoodinputPage2
		goto		InvalidPage2

InvalidPage2	
		call		InvalidPage
		bcf			PCLATH,4		;call page 1 things
		bsf			PCLATH,3
		goto		SecondPageEndMenu

DisplayDateEnded
		bcf			PCLATH,4
		bcf 		PCLATH,3    		;calls functions in Page 0 
		call		ClrLCD
		call		ButtonReleased
		;Get year
		movlw		"2"				;First line shows 20**/**/**
		call		WR_DATA
		movlw		"0"
		call		WR_DATA
		movlw		"1"
		call		WR_DATA
		movlw		"3"
		call		WR_DATA
		movlw		"/"
		call		WR_DATA
		movfw		TensMonth
		call		WR_DATA
		movfw		OnesMonth
		call		WR_DATA
		movlw		"/"
		call		WR_DATA
		movfw		TensDay
		call		WR_DATA
		movfw		OnesDay
		call		WR_DATA
		movlw		B'11000000'		;Next line displays (hour):(min):(sec) **:**:**
		call		WR_INS
		movfw		TensHoursFinished
		call		writenumber
		movfw		OnesHoursFinished
		call		writenumber
		movlw		":"
		call		WR_DATA
		movfw		TensMinutesFinished
		call		writenumber
		movfw		OnesMinutesFinished
		call		writenumber
		movlw		":"
		call		WR_DATA
		movfw		TensSecondsFinished
		call		writenumber
		movfw		OnesSecondsFinished
		call		writenumber
		movlw		" "
		call		WR_DATA
		movlw		"P"
		call		WR_DATA
		movlw		"r"
		call		WR_DATA
		movlw		"e"
		call		WR_DATA
		movlw		"s"
		call		WR_DATA
		movlw		"s"
		call		WR_DATA
		movlw		" "
		call		WR_DATA
		movlw		"*"
		call		WR_DATA
		call		StarPressed			;Check if star is pressed
		call 		ButtonReleased
		bcf			PCLATH,4			;go back to Page 1
		bsf 		PCLATH,3 
		goto		SecondPageEndMenu	

ThirdPageEndMenu
		bcf			PCLATH,4
		bcf 		PCLATH,3    		;calls functions in Page 0 
		call		ClrLCD
		movlw		"1"
		call		WR_DATA
		movlw		"-"
		call		WR_DATA
		movlw		"N"
		call		WR_DATA
		movlw		"e"
		call		WR_DATA
		movlw		"w"
		call		WR_DATA
		movlw		" "
		call		WR_DATA
		movlw		"O"
		call		WR_DATA
		movlw		"p"
		call		WR_DATA
		movlw		"e"
		call		WR_DATA
		movlw		"r"
		call		WR_DATA
		movlw		"a"
		call		WR_DATA
		movlw		"t"
		call		WR_DATA
		movlw		"i"
		call		WR_DATA
		movlw		"o"
		call		WR_DATA
		movlw		"n"
		call		WR_DATA
		call		Switch_Lines
		movlw		"2"
		call		WR_DATA
		movlw		"-"
		call		WR_DATA
		movlw		"M"
		call		WR_DATA
		movlw		"o"
		call		WR_DATA
		movlw		"r"
		call		WR_DATA
		movlw		"e"
		call		WR_DATA
		bcf			PCLATH,4			;go back to Page 1
		bsf 		PCLATH,3  
	    btfss		PORTB,1    		 	;Wait until data is available from the keypad
        goto		$-1 
		goto		CheckvalidPage3
GoodinputPage3
		btfsc		PORTB,4
		goto		FourthPageEndMenu
		goto		NewOperation

CheckvalidPage3 
		btfsc		PORTB,6			;If input > 3
		goto		InvalidPage3
		btfsc		PORTB,5			;If input > 3
		goto		InvalidPage3
		goto		GoodinputPage3

InvalidPage3	
		call		InvalidPage
		bcf			PCLATH,4		;call page 1 things
		bsf			PCLATH,3
		goto		ThirdPageEndMenu

NewOperation
		bcf			PCLATH,4
		bcf 		PCLATH,3    		;calls functions in Page 0 
		lgoto		init				;init works better
		;lgoto		DoneInterface

FourthPageEndMenu
		bcf			PCLATH,4
		bcf 		PCLATH,3    		;calls functions in Page 0 
		call		ClrLCD
		movlw		"1"
		call		WR_DATA
		movlw		"-"
		call		WR_DATA
		movlw		"D"
		call		WR_DATA
		movlw		"o"
		call		WR_DATA
		movlw		"w"
		call		WR_DATA
		movlw		"n"
		call		WR_DATA
		movlw		"l"
		call		WR_DATA
		movlw		"o"
		call		WR_DATA
		movlw		"a"
		call		WR_DATA
		movlw		"d"
		call		WR_DATA
		movlw		" "
		call		WR_DATA
		movlw		"l"
		call		WR_DATA
		movlw		"o"
		call		WR_DATA
		movlw		"g"
		call		WR_DATA
		movlw		"s"
		call		WR_DATA
		call		Switch_Lines
		movlw		"2"
		call		WR_DATA
		movlw		"-"
		call		WR_DATA
		movlw		"M"
		call		WR_DATA
		movlw		"o"
		call		WR_DATA
		movlw		"r"
		call		WR_DATA
		movlw		"e"
		call		WR_DATA
		bcf			PCLATH,4			;go back to Page 1
		bsf 		PCLATH,3  
	    btfss		PORTB,1    		 	;Wait until data is available from the keypad
        goto		$-1 
		goto		CheckvalidPage4
GoodinputPage4
		btfsc		PORTB,4
		goto		EndMenu
		goto		DownloadLogs

CheckvalidPage4
		btfsc		PORTB,6			;If input > 3
		goto		InvalidPage4
		btfsc		PORTB,5			;If input > 3
		goto		InvalidPage4
		goto		GoodinputPage4

InvalidPage4	
		call		InvalidPage
		bcf			PCLATH,4		;call page 1 things
		bsf			PCLATH,3
		goto		FourthPageEndMenu

DownloadLogs
		bcf			PCLATH,4			;go back to Page 0
		bcf 		PCLATH,3  
		call		ClrLCD
		movlw		"*"
		call		WR_DATA
		movlw		"B"
		call		WR_DATA
		movlw		"u"
		call		WR_DATA
		movlw		"s"
		call		WR_DATA
		movlw		"y"
		call		WR_DATA
		movlw		"*"
		call		WR_DATA
;;************ initialize *******************
initPC  
		bcf	      STATUS,RP0			;bank0
		clrf	  Read_Address_Counter	;start at address 0
		bcf		  PCLATH,4		 ;back to page 0
		bcf		  PCLATH,3
		call	  Retrieveinfo	 ;get proper log
		bsf       STATUS,RP0     ; select bank 1
        clrf      TRISD

RS232Setup        
        ;Setup USART for RS232
        movlw     d'15'          ; BAUD rate 9600, assuming 10MHz oscillator
        movwf     SPBRG
        clrf      TXSTA          ; 8 bits data ,no,1 stop
        
        bcf       STATUS,RP0     ; select bank 0
        bsf       RCSTA,SPEN     ; Asynchronous serial port enable
        bsf       RCSTA,CREN     ; continuous receive

        bsf       STATUS,RP0     ; select bank 1
        bsf       TXSTA,TXEN     ; Transmit enable

;********** Send welcome message **********************
        bcf       STATUS,RP0     ; select bank 0
        clrf      offset         ; Reset offset to 0		
        
start   
		bcf			PCLATH,4			;go back to Page 1
		bsf 		PCLATH,3  
		movlw	  d'13'					;Have some white space
		call	  Send_Working
		movlw	  d'13'
		call	  Send_Working
		movlw	  d'13'
		call	  Send_Working
		movlw	  d'13'
		call	  Send_Working
		movlw	  "L"
		call	  Send_Working
		movlw	  "o"
		call	  Send_Working
		movlw	  "g"
		call	  Send_Working
		movlw	  " "
		call	  Send_Working
		movlw	  "I"
		call	  Send_Working
		movlw	  "n"
		call	  Send_Working
		movlw	  "f"
		call	  Send_Working
		movlw	  "o"
		call	  Send_Working
		movlw	  "r"
		call	  Send_Working
		movlw	  "m"
		call	  Send_Working
		movlw	  "a"
		call	  Send_Working
		movlw	  "t"
		call	  Send_Working
		movlw	  "i"
		call	  Send_Working
		movlw	  "o"
		call	  Send_Working
		movlw	  "n"
		call	  Send_Working

Date_of_End
		call	  NewLine
		clrf	  offset
DateEnd
		movlw	  "D"
		call	  Send_Working
		movlw	  "a"
	    call	  Send_Working
		movlw	  "t"
		call	  Send_Working
		movlw	  "e"
	    call	  Send_Working
		movlw	  " "
		call	  Send_Working
		movlw	  "F"
	    call	  Send_Working
		movlw	  "i"
		call	  Send_Working
		movlw	  "n"
	    call	  Send_Working
		movlw	  "i"
		call	  Send_Working
		movlw	  "s"
	    call	  Send_Working
		movlw	  "h"
		call	  Send_Working
		movlw	  "e"
	    call	  Send_Working
		movlw	  "d"
		call	  Send_Working
		movlw	  ":"
	    call	  Send_Working
		movlw	  " "
	    call	  Send_Working


Actual_Date
		movlw	  "2"
		call	  Send_Working
		movlw	  "0"
	    call	  Send_Working
		movlw	  "1"
		call	  Send_Working
		movlw	  "3"
	    call	  Send_Working
		movlw	  "/"
	    call	  Send_Working
		movfw	  TensMonth
		call	  Send_Working
		movfw	  OnesMonth
	    call	  Send_Working
		movlw	  "/"
		call	  Send_Working
		movfw	  TensDay
	    call	  Send_Working
		movfw	  OnesDay
	    call	  Send_Working
		movlw	  " "
		call	  Send_Working
		movfw	  TensHoursFinished
	    call	  Send_Working
		movfw	  OnesHoursFinished
	    call	  Send_Working
		movlw	  ":"
		call	  Send_Working
		movfw	  TensMinutesFinished
	    call	  Send_Working
		movfw	  OnesMinutesFinished
	    call	  Send_Working
		movlw	  ":"
		call	  Send_Working
		movfw	  TensSecondsFinished
	    call	  Send_Working
		movfw	  OnesSecondsFinished
	    call	  Send_Working
		movlw	  " "
		call	  Send_Working
		goto	  Patterns_Picked

NewLine
		movlw	  d'10'					;go to new line
		call	  Send_Working
		movlw	  d'13'					;reset carriage
		call	  Send_Working
		return

Send_Working
		movwf     TXREG          ; Send contents of W to RS232
        bsf       STATUS,RP0     ; Go to bank with TXSTA
        btfss     TXSTA,1        ; check TRMT bit in TXSTA (FSR) until TRMT=1
        goto      $-1
        bcf       STATUS,RP0     ; Go back to bank 0
		return

Send_Working_Number
		addlw	  d'48'
		call	  Send_Working
		return

Patterns_Picked
		clrf	  offset
		movlw	  "P"
	    call	  Send_Working
		movlw	  "a"
	    call	  Send_Working
		movlw	  "t"
	    call	  Send_Working
		movlw	  "t"
	    call	  Send_Working	
		movlw	  "e"
	    call	  Send_Working
		movlw	  "r"
	    call	  Send_Working
		movlw	  "n"
	    call	  Send_Working
		movlw	  "s"
	    call	  Send_Working
		movlw	  " "
	    call	  Send_Working	
		movlw	  "S"
	    call	  Send_Working
		movlw	  "e"
	    call	  Send_Working
		movlw	  "l"
	    call	  Send_Working
		movlw	  "e"
	    call	  Send_Working
		movlw	  "c"
	    call	  Send_Working	
		movlw	  "t"
	    call	  Send_Working
		movlw	  "e"
	    call	  Send_Working	
		movlw	  "d"
	    call	  Send_Working
		movlw	  ":"
	    call	  Send_Working	
		movlw	  " "
	    call	  Send_Working
		goto	  Actual_Patterns


Pattern_Convert					;convert the keypad numbers, into numbers for hyperterminal
		movwf		Memory_Temp
		btfsc	 	Memory_Temp,3
		goto		convert8
		btfss		Memory_Temp,2
		goto		add1
Pattern_Good
		movfw		Memory_Temp
		return
		
convert8
		clrf		Memory_Temp
		goto		Pattern_Good
add1
		incf		Memory_Temp,F
		goto		Pattern_Good


Actual_Patterns
		movfw	  Pattern_Number_1
		call      Pattern_Convert
		call	  Send_Working_Number
		movlw	  ","
		call	  Send_Working
		movfw	  Pattern_Number_2
		call      Pattern_Convert
		call	  Send_Working_Number
		movlw	  ","
		call	  Send_Working
		movfw	  Pattern_Number_3
		call      Pattern_Convert
		call	  Send_Working_Number
		movlw	  " "
		call	  Send_Working
		clrf	  offset

WhiteLeft
		movf      offset,w       ; Load TAB[offset] into W
		movlw	  "W"
	    call	  Send_Working	
		movlw	  "h"
	    call	  Send_Working
		movlw	  "i"
	    call	  Send_Working	
		movlw	  "t"
	    call	  Send_Working
		movlw	  "e"
	    call	  Send_Working	
		movlw	  " "
	    call	  Send_Working
		movlw	  "L"
	    call	  Send_Working	
		movlw	  "e"
	    call	  Send_Working
		movlw	  "f"
	    call	  Send_Working	
		movlw	  "t"
	    call	  Send_Working
		movlw	  ":"
	    call	  Send_Working	
		movlw	  " "
	    call	  Send_Working
		goto	  Actual_White

SendTheBalls
		movfw		Memory_Temp
		sublw		d'9'				;check if over nine
		btfss		STATUS,C			;will be set if negative
		goto		OverNineSend
		goto		UnderNineSend
		
OverNineSend		
		movlw		"1"
		call		Send_Working
		movlw		d'10'
		subwf		Memory_Temp,F

UnderNineSend
		movfw		Memory_Temp	
		call		Send_Working_Number
		return

Actual_White 
		movfw	  WhiteBalls
		movwf	  Memory_Temp
		call	  SendTheBalls			;account for 10+
;		movfw	  WhiteBalls
;		call	  Send_Working_Number
		movlw	  d'13'					;go to new line (only one for single space on notepad++)
		call	  Send_Working
		clrf	  offset

OrangeLeft
		movf      offset,w       ; Load TAB[offset] into W
		movlw	  "O"
	    call	  Send_Working	
		movlw	  "r"
	    call	  Send_Working
		movlw	  "a"
	    call	  Send_Working	
		movlw	  "n"
	    call	  Send_Working
		movlw	  "g"
	    call	  Send_Working
		movlw	  "e"
	    call	  Send_Working	
		movlw	  " "
	    call	  Send_Working
		movlw	  "L"
	    call	  Send_Working	
		movlw	  "e"
	    call	  Send_Working
		movlw	  "f"
	    call	  Send_Working	
		movlw	  "t"
	    call	  Send_Working
		movlw	  ":"
	    call	  Send_Working	
		movlw	  " "
	    call	  Send_Working

Actual_Orange
		movfw	  OrangeBalls
		movwf	  Memory_Temp
		call	  SendTheBalls			;account for 10+
;		movfw	  OrangeBalls
;		call	  Send_Working_Number
		movlw	  " "
		call	  Send_Working
		clrf	  offset

Time_Elapsed
		movf      offset,w       ; Load TAB[offset] into W
		movlw	  "T"
	    call	  Send_Working
		movlw	  "i"
	    call	  Send_Working		
		movlw	  "m"
	    call	  Send_Working
		movlw	  "e"
	    call	  Send_Working
		movlw	  " "
	    call	  Send_Working		
		movlw	  "E"
	    call	  Send_Working
		movlw	  "l"
	    call	  Send_Working
		movlw	  "a"
	    call	  Send_Working		
		movlw	  "p"
	    call	  Send_Working
		movlw	  "s"
	    call	  Send_Working
		movlw	  "e"
	    call	  Send_Working		
		movlw	  "d"
	    call	  Send_Working
		movlw	  ":"
	    call	  Send_Working
		movlw	  " "
	    call	  Send_Working		

Actual_Time
		movfw	  TSE
		call	  Send_Working_Number
		movfw	  OSE
		call	  Send_Working_Number
		movlw	  "s"
		call	  Send_Working

		;movlw		0x11				;Go to next log (add 17)
		;addwf		Read_Address_Counter,F
		movfw		Read_Address_Counter				
		sublw		0xFF							;check if at end of logs
		btfsc		STATUS,Z						;zero bit to see if 0
		goto		FinishDownloadingLogs

		bcf			PCLATH,4			;go back to Page 0
		bcf 		PCLATH,3 		
		call		Retrieveinfo		
		bcf			PCLATH,4			;go back to Page 1
		bsf 		PCLATH,3
		goto        Date_of_End 

FinishDownloadingLogs
		bcf       STATUS,RP0     ; select bank 0
		bcf       RCSTA,SPEN     ; Asynchronous serial port disable 
		bcf			PCLATH,4			;go back to Page 0
		bcf 		PCLATH,3 		
		call		ClrLCD	
		movlw		"D"
		call		WR_DATA	
		movlw		"o"
		call		WR_DATA	
		movlw		"n"
		call		WR_DATA	
		movlw		"e"
		call		WR_DATA	
		movlw		" "
		call		WR_DATA	
		movlw		"D"
		call		WR_DATA	
		movlw		"o"
		call		WR_DATA	
		movlw		"w"
		call		WR_DATA	
		movlw		"n"
		call		WR_DATA	
		movlw		"l"
		call		WR_DATA	
		movlw		"o"
		call		WR_DATA	
		movlw		"a"
		call		WR_DATA	
		movlw		"d"
		call		WR_DATA	
		movlw		"i"
		call		WR_DATA	
		movlw		"n"
		call		WR_DATA	
		movlw		"g"
		call		WR_DATA	
		call		Switch_Lines
		movlw		"*"
		call		WR_DATA	
		movlw		" "
		call		WR_DATA	
		movlw		"t"
		call		WR_DATA	
		movlw		"o"
		call		WR_DATA	
		movlw		" "
		call		WR_DATA	
		movlw		"r"
		call		WR_DATA	
		movlw		"e"
		call		WR_DATA	
		movlw		"t"
		call		WR_DATA	
		movlw		"u"
		call		WR_DATA	
		movlw		"r"
		call		WR_DATA	
		movlw		"n"
		call		WR_DATA	
		call		StarPressed
		call 		ButtonReleased
		bcf			PCLATH,4			;go back to Page 1
		bsf 		PCLATH,3
		goto		FourthPageEndMenu			
	
DisplayTimeTaken
		bcf			PCLATH,4
		bcf 		PCLATH,3    		;calls functions in Page 0 
		call		ClrLCD
		call		ButtonReleased
		movlw		"T"
		call		WR_DATA
		movlw		"i"
		call		WR_DATA
		movlw		"m"
		call		WR_DATA
		movlw		"e"
		call		WR_DATA
		movlw		" "
		call		WR_DATA
		movlw		"t"
		call		WR_DATA
		movlw		"a"
		call		WR_DATA
		movlw		"k"
		call		WR_DATA
		movlw		"e"
		call		WR_DATA
		movlw		"n"
		call		WR_DATA
		movlw		" "
		call		WR_DATA
		movfw		TSE
		call		writenumber
		movfw		OSE
		call		writenumber
		movlw		"s"
		call		WR_DATA
		call		Switch_Lines
		movlw		"*"
		call		WR_DATA
		movlw		" "
		call		WR_DATA
		movlw		"t"
		call		WR_DATA
		movlw		"o"
		call		WR_DATA
		movlw		" "
		call		WR_DATA
		movlw		"r"
		call		WR_DATA
		movlw		"e"
		call		WR_DATA
		movlw		"t"
		call		WR_DATA
		movlw		"u"
		call		WR_DATA
		movlw		"r"
		call		WR_DATA
		movlw		"n"
		call		WR_DATA
		call		StarPressed			;Check if star is pressed
		call 		ButtonReleased
		bcf			PCLATH,4			;go back to Page 1
		bsf 		PCLATH,3 
		goto		SecondPageEndMenu

BallsMenu
		bcf			PCLATH,4			;go back to Page 0
		bcf 		PCLATH,3 
		call		ClrLCD
		call		ButtonReleased
		movlw		"1"
		call		WR_DATA
		movlw		"-"
		call		WR_DATA
		movlw		"W"
		call		WR_DATA
		movlw		"h"
		call		WR_DATA
		movlw		"i"
		call		WR_DATA
		movlw		"t"
		call		WR_DATA
		movlw		"e"
		call		WR_DATA
		call 		Switch_Lines
		movlw		"2"
		call		WR_DATA
		movlw		"-"
		call		WR_DATA
		movlw		"O"
		call		WR_DATA
		movlw		"r"
		call		WR_DATA
		movlw		"a"
		call		WR_DATA
		movlw		"n"
		call		WR_DATA
		movlw		"g"
		call		WR_DATA
		movlw		"e"
		call		WR_DATA
		bcf			PCLATH,4			;go back to Page 1
		bsf 		PCLATH,3 
	    btfss		PORTB,1    		 	;Wait until data is available from the keypad
        goto		$-1 
		btfss		PORTB,4				;Check if W or O
		goto		White
		goto		Orange

White
		bcf			PCLATH,4			;go back to Page 0
		bcf 		PCLATH,3 
		call		ClrLCD
		bcf			PCLATH,4			;go back to Page 1
		bsf 		PCLATH,3 
		movfw		WhiteBalls
		movwf		Memory_Temp			;put balls in temp
		movfw		Memory_Temp
		sublw		d'9'				;check if over nine
		btfss		STATUS,C			;will be set if negative
		goto		OverNine
		goto		DoneChecking
Orange
		bcf			PCLATH,4			;go back to Page 0
		bcf 		PCLATH,3 
		call		ClrLCD
		bcf			PCLATH,4			;go back to Page 1
		bsf 		PCLATH,3 
		movfw		OrangeBalls
		movwf		Memory_Temp			;put balls in temp
		movfw		Memory_Temp
		sublw		d'9'				;check if over nine
		btfss		STATUS,C			;will be set if negative
		goto		OverNine
		goto		DoneChecking
		
OverNine
		bcf			PCLATH,4			;go back to Page 0
		bcf 		PCLATH,3 
		movlw		d'1'
		call		OneToNine
		call		WR_DATA
		movlw		d'10'
		subwf		Memory_Temp,F

DoneChecking
		movfw		Memory_Temp	
		bcf			PCLATH,4			;go back to Page 0
		bcf 		PCLATH,3 
		andlw		0x0F
		call		OneToNine
		call		WR_DATA				;Write remaining balls
		call		ButtonReleased
		bcf			PCLATH,4			;go back to Page 1
		bsf 		PCLATH,3 
		goto		GoBack

PatternsChosen
		bcf			PCLATH,4			;go back to Page 0
		bcf 		PCLATH,3 	
        call 		ClrLCD
		movlw		"P"
		call		WR_DATA
		movlw		"a"
		call		WR_DATA
		movlw		"t"
		call		WR_DATA
		movlw		"t"
		call		WR_DATA
		movlw		"e"
		call		WR_DATA
		movlw		"r"
		call		WR_DATA
		movlw		"n"
		call		WR_DATA
		movlw		"s"
		call		WR_DATA
		movlw		":"
		call		WR_DATA
		movlw		" "
		call		WR_DATA
		movf		Pattern_Number_1,W
		call    	KPHexToChar 				;Display up to nine.
		call		WR_DATA
		movlw		","
		call		WR_DATA						;insert comma
		movf		Pattern_Number_2,W
		call    	KPHexToChar 				;Display up to nine.
		call		WR_DATA
		movlw		","
		call		WR_DATA						;insert comma
		movf		Pattern_Number_3,W
		call    	KPHexToChar					;Display up to nine.
		call		WR_DATA
		call		ButtonReleased
		bcf			PCLATH,4			;go back to Page 1
		bsf 		PCLATH,3 
		goto		GoBack	

GoBack
		bcf			PCLATH,4			;go back to Page 0
		bcf 		PCLATH,3 
		call		Switch_Lines
		movlw		"*"
		call		WR_DATA
		movlw		" "
		call		WR_DATA
		movlw		"t"
		call		WR_DATA
		movlw		"o"
		call		WR_DATA
		movlw		" "
		call		WR_DATA
		movlw		"r"
		call		WR_DATA
		movlw		"e"
		call		WR_DATA
		movlw		"t"
		call		WR_DATA
		movlw		"u"
		call		WR_DATA
		movlw		"r"
		call		WR_DATA
		movlw		"n"
		call		WR_DATA
		call		StarPressed			;Check if star is pressed
		call 		ButtonReleased
		bcf			PCLATH,4			;go back to Page 1
		bsf 		PCLATH,3 
		goto		EndMenu


Switch_To_Page0
		bcf			PCLATH,4			;go back to Page 0
		bcf 		PCLATH,3 
		bsf			PORTB,3
		return

	END
