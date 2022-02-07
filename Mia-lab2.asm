; ISR_example.asm: a) Increments/decrements a BCD variable every half second using
; an ISR for timer 2; b) Generates a 2kHz square wave at pin P1.1 using
; an ISR for timer 0; and c) in the 'main' loop it displays the variable
; incremented/decremented using the ISR for timer 2 on the LCD.  Also resets it to 
; zero if the 'BOOT' pushbutton connected to P4.5 is pressed.
$NOLIST
$MODLP51
$LIST

; There is a couple of typos in MODLP51 in the definition of the timer 0/1 reload
; special function registers (SFRs), so:

CLK           EQU 22118400 ; Microcontroller system crystal frequency in Hz
TIMER0_RATE   EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD EQU ((65536-(CLK/TIMER0_RATE)))
TIMER2_RATE   EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/TIMER2_RATE)))

BOOT_BUTTON   equ P4.5
SOUND_OUT     equ P1.1
UPDOWN        equ P0.0

Csecond equ P0.2
Cminute equ P0.4
Chour equ   P0.6
alarm equ P2.3; 0 for off, 1 for on 
counter equ P2.5
; Reset vector
org 0x0000
    ljmp main

; External interrupt 0 vector (not used in this code)
org 0x0003
	reti

; Timer/Counter 0 overflow interrupt vector
org 0x000B
	ljmp Timer0_ISR

; External interrupt 1 vector (not used in this code)
org 0x0013
	reti

; Timer/Counter 1 overflow interrupt vector (not used in this code)
org 0x001B
	reti

; Serial port receive/transmit interrupt vector (not used in this code)
org 0x0023 
	reti
	
; Timer/Counter 2 overflow interrupt vector
org 0x002B
	ljmp Timer2_ISR

; In the 8051 we can define direct access variables starting at location 0x30 up to location 0x7F
dseg at 0x30
Count1ms:     ds 2 ; Used to determine when one second has passed
second:		  ds 1
minute:			ds 1
hour:			ds 1
ahour: ds 1
aminute: ds 1
asecond: ds 1
; In the 8051 we have variables that are 1-bit in size.  We can use the setb, clr, jb, and jnb
; instructions with these variables.  This is how you define a 1-bit variable:
bseg
half_second_flag: dbit 1 ; Set to one in the ISR every time 500 ms had passed
one_second_flag: dbit 1
cseg
; These 'equ' must match the hardware wiring
LCD_RS equ P3.2
;LCD_RW equ PX.X ; Not used in this code, connect the pin to GND
LCD_E  equ P3.3
LCD_D4 equ P3.4
LCD_D5 equ P3.5
LCD_D6 equ P3.6
LCD_D7 equ P3.7

$NOLIST
$include(LCD_4bit.inc) ; A library of LCD related functions and utility macros
$LIST

;                     1234567890123456    <- This helps determine the location of the counter
Initial_Message:  db '12:00:00P',0
Sec_Message: db 'ALARM 00:00P Off',0
c_Message: db 'COUNT',0
a_Message: db 'ALARM',0

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 0                     ;
;---------------------------------;
Timer0_Init:
	mov a, TMOD
	anl a, #0xf0 ; Clear the bits for timer 0
	orl a, #0x01 ; Configure timer 0 as 16-timer
	mov TMOD, a
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	; Set autoreload value
	mov RH0, #high(TIMER0_RELOAD)
	mov RL0, #low(TIMER0_RELOAD)
	; Enable the timer and interrupts
    setb TR0  ; Start timer 0
    setb ET0  ; Enable timer 0 interrupt
	ret

;---------------------------------;
; ISR for timer 0.  Set to execute;
; every 1/4096Hz to generate a    ;
; 2048 Hz square wave at pin P1.1 ;
;---------------------------------;
Timer0_ISR:
	push acc
	push ar5
	;clr TF0  ; According to the data sheet this is done for us already.
	cjne r6, #'N', skip_sound ; no sound if off
	cjne r7, #'A',skip_sound ; check is current time is equal if in alarm mode  
	mov a, ahour
	mov r5, hour
	subb a, r5
	cjne a, #0, skip_sound 
	mov a, aminute
	mov r5, minute 
	subb a, r5
	cjne a, #0, skip_sound 
	cpl SOUND_OUT ; Connect speaker to P1.1!
skip_sound:
	pop ar5
	pop acc
	reti
check_counter:
	mov a, asecond 
	cjne a, #0, skip_sound
	mov a, aminute
	cjne a, #0, skip_sound
	cpl SOUND_OUT ; Connect speaker to P1.1!
	ljmp skip_sound
;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 2                     ;
;---------------------------------;
Timer2_Init:
	mov T2CON, #0 ; Stop timer/counter.  Autoreload mode.
	mov TH2, #high(TIMER2_RELOAD)
	mov TL2, #low(TIMER2_RELOAD)
	; Set the reload value
	mov RCAP2H, #high(TIMER2_RELOAD)
	mov RCAP2L, #low(TIMER2_RELOAD)
	; Init One millisecond interrupt counter.  It is a 16-bit variable made with two 8-bit parts
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Enable the timer and interrupts
    setb TR2  ; Enable timer 2
    setb ET2  ; Enable timer 0 interrupt
	ret

;---------------------------------;
; ISR for timer 2                 ;
;---------------------------------;
Timer2_ISR:
	clr TF2  ; Timer 2 doesn't clear TF2 automatically. Do it in ISR
	cpl P1.0 ; To check the interrupt rate with oscilloscope. It must be precisely a 1 ms pulse.
	
	; The two registers used in the ISR must be saved in the stack
	push acc
	push psw
	
;check for increment button is pushed or not
	; Increment the 16-bit one mili second counter

	inc Count1ms+0    ; Increment the low 8-bits first
	mov a, Count1ms+0 ; If the low 8-bits overflow, then increment high 8-bits
	jnz Inc_Done
	inc Count1ms+1
Inc_Done:
	; Check if half second has passed
	mov a, Count1ms+0
	cjne a, #low(1000), Timer2_ISR_done ; Warning: this instruction changes the carry flag!
	mov a, Count1ms+1
	cjne a, #high(1000), Timer2_ISR_done
	
	; 500 milliseconds have passed.  Set a flag so the main program knows
	setb one_second_flag ; Let the main program know half second had passed
	setb half_second_flag
	cpl TR0 ; Enable/disable timer/counter 0. This line creates a beep-silence-beep-silence sound.
	; Reset to zero the milli-seconds counter, it is a 16-bit variable
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Increment the BCD counter
	CJNE R4, #'C', SKIP_COUNTER
	CJNE R6, #'N',SKIP_COUNTER ; If in counter mode and 'on', start decrementing 
	lcall amin_dec ; decremetn and display in LCD
SKIP_COUNTER:
	mov a, second
	jnb UPDOWN, Timer2_ISR_decrement
	add a, #0x01
	sjmp Timer2_ISR_da
	
Timer2_ISR_decrement:
	add a, #0x99 ; Adding the 10-complement of -1 is like subtracting 1.
Timer2_ISR_da:
	da a ; Decimal adjust instruction.  Check datasheet for more details!
	mov second, a
	
Timer2_ISR_done:
	pop psw
	pop acc
	reti
	

amin_dec:
	jnb half_second_flag, SKIP_COUNTER
	push acc
	push ar0
	clr half_second_flag
	mov a, asecond 
	cjne a, #0, skip_resett
	mov @r0, aminute 
	cjne @r0, #0, minute_decs
	mov a, #0
	da a
	mov a, #0
	da a
	mov asecond, a
	mov aminute, a
	cpl SOUND_OUT ; Connect speaker to P1.1!
	ljmp display_counter
skip_resett:
	add a, #0x99
	da a
	mov asecond,a
display_counter:
	Set_Cursor(2,7)
	Display_BCD(aminute)
	Set_Cursor(2,10)
	Display_BCD(asecond)
	pop ar0
	pop acc
	ret
minute_decs:
	lcall dec_fun
	mov a, #0x60
	ljmp skip_resett
dec_fun:
	push acc
	mov a, @r0
	add a, #0x99
	da a
	mov aminute,a
	pop acc
	ret
;---------------------------------;
; increment second/minute/hour    ;
;---------------------------------;

sec_inc: 
	push acc
	mov b, #0x60
	mov a, second 
	div ab
	mov a, b
	da a
	mov second, a
	pop acc
	ret
min_inc:
	push acc
	push b
	mov b, #0x60
	mov a, minute 
	div ab
	mov a, b
	da a
	mov minute,a
	pop acc
	pop b
	ret
hour_inc:
	push acc
	mov b, #0x12
	mov a, hour 
	div ab
	mov a, b
	da a
	mov hour, a
	cjne a, #0x11, skip3
	mov @r0, #1 
skip3:
	cjne a, #0, notequal2
	mov a, #0x12
	da a
	mov hour, a
	mov a, @r0
	cjne a, #1, mid1
	ljmp change_to_am
mid1:
	mov a, #0
skipp:
	pop acc
	ret

notequal2:
	cjne a, #1, mid1
	mov a, minute
	cjne a, #0,mid1
	
change_to_am:
	mov @r0, #0
	cjne r2, #'A', change_to_pm
	mov r2, #'P'
	ljmp mid1
change_to_pm:
	Set_cursor(1,12)
	Display_char(#'P')
	mov r2, #'A'
	ljmp mid1

sec_incto_min: ; every time seconds goes back to reset, minute increment 1
	push acc
	mov a, second 
	cjne a, #0, skip1
	mov a, minute 
	inc a
	da a
	mov minute, a
	lcall min_incto_hour
skip1:
	pop acc
	ret
	
min_incto_hour: ; every time seconds & minute goes back to reset, hour increment 1
	push acc
	mov a, minute 
	cjne a, #0x60, skip2
	mov a, hour 
	inc a
	da a
	cjne a, #0x11, skip4
	mov @r0, #1
skip4:
	mov hour, a
skip2:
	pop acc
	ret

hour_inc_slot:
	push acc
	mov a, hour
	cjne a, #0x12, skip1
	mov a, #1
	da a
	mov hour, a 
	ljmp change_to_am
	pop acc
	ret 
;---------------------------------;
; Main program. Includes hardware ;
; initialization and 'forever'    ;
; loop.                           ;
;---------------------------------;
main:

	; Initialization
    mov SP, #0x7F
    lcall Timer0_Init
    lcall Timer2_Init
    ; In case you decide to use the pins of P0, configure the port in bidirectional mode:
    mov P0M0, #0
    mov P0M1, #0
    mov @r0, #0
    mov @r1, #0
    mov r2, #'P'
    mov r3, #'H'
    mov r4, #'A' ; counter/alarm mode
    mov r6, #'F'
    mov r7, #'A'
    set_Cursor(1,15)
	Display_char(r3)
    setb EA   ; Enable Global interrupts
    lcall LCD_4BIT
    ; For convenience a few handy macros are included in 'LCD_4bit.inc':
	Set_cursor(2, 1)
    Send_Constant_String(#Sec_Message)
	Set_Cursor(1, 4)
    Send_Constant_String(#Initial_Message)
    setb one_second_flag
	mov second, #0x00
	mov minute, #0x00
	mov hour, #0x12
	mov aminute, #0x00
	mov ahour, #0x12
loop:
	ljmp loop1
	ljmp loop2
	ljmp loop3 
	
	; After initialization the program stays in this 'forever' loop
loop2:
	jb BOOT_BUTTON, loop_a  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb BOOT_BUTTON, loop_a              ; if the 'BOOT' button is not pressed skip
	jnb BOOT_BUTTON, $		; Wait for button release.  The '$' means: jump to same instruction.
	; A valid press of the 'BOOT' button has been detected, reset the BCD counter.
	; But first stop timer 2 and reset the milli-seconds counter, to resync everything.
	clr TR2                 ; Stop timer 2
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Now clear the BCD counter
	mov second, a
	setb TR2                ; Start timer 2
	sjmp loop_b             ; Display the new value
loop_a:
	jnb one_second_flag, loop1
loop_b:
    clr one_second_flag ; We clear this flag in the main loop, but it is set in the ISR for timer 2
	Set_Cursor(1, 10)     ; the place in the LCD where we want the BCD counter value
	lcall sec_inc
	Display_BCD(second) ; This macro is also in 'LCD_4bit.inc'
	Set_Cursor(1,7)
	lcall sec_incto_min
	lcall min_inc
	Display_BCD(minute)
	lcall hour_inc
	Set_Cursor(1,4)
	Display_BCD(hour)
	Set_cursor(1,12)
	Display_char(r2)
    ljmp loop

loop1:
	jb csecond, loop_wait  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb csecond, loop_wait  ; if the 'BOOT' button is not pressed skip
	jnb csecond, $		; Wait for button release.  The '$' means: jump to same instruction.
	; A valid press of the 'BOOT' button has been detected, reset the BCD counter.
	; But first stop timer 2 and reset the milli-seconds counter, to resync everything.
	clr TR2                 ; Stop timer 2
	cjne r3, #'H',inc_min ; increment hour in time 
	mov r3, #'M'
write:
	set_Cursor(1,15)
	Display_char(r3)
	setb TR2 ; Start timer 2
	ljmp loop_b 
inc_min:
	cjne r3, #'M', inc_sec ; increment minute in time 
	mov r3, #'S'
	ljmp write
inc_sec:
	mov r3, #'H';increment second in time 
	ljmp write
loop_wait:
	jnb one_second_flag, loop3
	ljmp loop_b; if not pressed go to loop_b
	
loop3:
	jb cminute, loop_wait1  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb cminute, loop_wait1  ; if the 'BOOT' button is not pressed skip
	jnb cminute, $		; Wait for button release.  The '$' means: jump to same instruction.
	; A valid press of the 'BOOT' button has been detected, reset the BCD counter.
	; But first stop timer 2 and reset the milli-seconds counter, to resync everything.
	clr TR2                 ; Stop timer 2
	cjne r3, #'H',inc_mins
	inc hour
	setb TR2                ; Start timer 2
	ljmp loop_d 
loop_wait1:
	jnb one_second_flag, loop4
	ljmp loop_b ; if not pressed go to loop_b
inc_mins:
	cjne r3, #'M', inc_secs
	inc minute
	setb TR2                ; Start timer 2
	ljmp loop_d ; if not pressed go to loop_b
inc_secs:
	inc second 
	setb TR2                ; Start timer 2
	ljmp loop_d ; if not pressed go to loop_b
	
	
loop_d:
	clr one_second_flag ; We clear this flag in the main loop, but it is set in the ISR for timer 2
	Set_Cursor(1, 10)     ; the place in the LCD where we want the BCD counter value
	lcall sec_inc
	Display_BCD(second) ; This macro is also in 'LCD_4bit.inc'
	Set_Cursor(1,7)
	lcall min_inc
	Display_BCD(minute)
	lcall hour_inc
	Set_Cursor(1,4)
	Display_BCD(hour)
	Set_cursor(1,12)
	Display_char(r2)
    ljmp loop
 
loop4:
	jb chour, loop_wait2  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb chour, loop_wait2  ; if the 'BOOT' button is not pressed skip
	jnb chour, $		; Wait for button release.  The '$' means: jump to same instruction.
	; A valid press of the 'BOOT' button has been detected, reset the BCD counter.
	; But first stop timer 2 and reset the milli-seconds counter, to resync everything.
	clr TR2                 ; Stop timer 2
	cjne r3, #'H',inc_amins
	cjne r4, #'A',loop_wait2 
	inc ahour
	setb TR2                ; Start timer 2
	lcall ahour_inc 
inc_amins:
	cjne r3, #'M', loop_wait2
	cjne r4, #'A', aminute_dec
	inc aminute
	setb TR2                ; Start timer 2
	lcall amin_inc ; if not pressed go to loop_b
	set_cursor(2,10)
	Display_BCD(aminute)
loop_wait2:
	jnb one_second_flag, loop5
	ljmp loop_b ; if not pressed go to loop_b

aminute_dec:
	inc aminute 
	lcall amin_inc
	set_cursor(2,7)
	Display_BCD(aminute)
	mov a, #0x0
	da a
	mov asecond, a
	set_cursor(2,10)
	Display_BCD(asecond)
	ljmp loop_wait2
amin_inc:
	mov b, #0x60
	mov a, aminute 
	div ab
	mov a, b
	da a
	mov aminute,a
	ret
ahour_inc:
	mov b, #0x12
	mov a, ahour 
	div ab
	mov a, b
	cjne a, #0, ship
	mov a, #0x12
ship:
	da a
	mov ahour, a
	set_cursor(2,7)
	Display_BCD(ahour)
	ret

loop5:
	jb alarm, loop_wait3  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb alarm, loop_wait3  ; if the 'BOOT' button is not pressed skip
	jnb alarm, $		; Wait for button release.  The '$' means: jump to same instruction.
	; A valid press of the 'BOOT' button has been detected, reset the BCD counter.
	; But first stop timer 2 and reset the milli-seconds counter, to resync everything.
	clr TR2                 ; Stop timer 2
	cjne r6, #'F',inc_off
	mov r6, #'N'
	Set_Cursor(2,15)
	Display_char(r6)
	Display_char(#' ')
	setb TR2                ; Start timer 2
	ljmp loop_wait3
inc_off:
	mov r6, #'F'
	Set_Cursor(2,15)
	Display_char(r6)
	Display_char(r6)
	setb TR2 
	ljmp loop_wait3
loop_wait3:
	jnb one_second_flag, loop6
	ljmp loop_b ; if not pressed go to loop_b
	
mid:
 ljmp loop2
loop6:
	jb counter, loop_wait4  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb counter, loop_wait4  ; if the 'BOOT' button is not pressed skip
	jnb counter, $		; Wait for button release.  The '$' means: jump to same instruction.
	; A valid press of the 'BOOT' button has been detected, reset the BCD counter.
	; But first stop timer 2 and reset the milli-seconds counter, to resync everything.
	clr TR2                 ; Stop timer 2
	cjne r4, #'C',inc_offf
	mov r4, #'A'
	Set_Cursor(2,1)
	Send_Constant_String(#a_Message)
	setb TR2                ; Start timer 2
	ljmp loop_wait4
inc_offf:
	mov r4, #'C'
	Set_Cursor(2,1)
	Send_Constant_String(#c_Message)
	setb TR2 
	ljmp loop_wait4
loop_wait4:
	jnb one_second_flag, mid
	ljmp loop_b ; if not pressed go to loop_b
END
