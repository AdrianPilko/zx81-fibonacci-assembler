#include "zx81defs.asm"
;EQUs for ROM routines
#include "zx81rom.asm"
;ZX81 char codes/how to survive without ASCII
#include "charcodes.asm"
;system variables
#include "zx81sys.asm"

;the standard REM statement that will contain our 'hex' code
#include "line1.asm"


	jp start
	
s1_mem
	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
s2_mem
	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
sumof_mem
	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
to_print_mem
	DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
mainLoopCount_mem
	DEFB 0
mainCLSCountDown_mem
	DEFB 0
;s1 		.equ 30000 ;use in sum128
;s2 		.equ 30016 ;use in sum128
;sumof 	.equ 30032 ;use in sum128
;to_print .equ 30048 ;use hprint128
;mainLoopCount .equ 30064

mainLoopCount .equ mainLoopCount_mem
s1 		.equ s1_mem ;use in sum128
s2 		.equ s2_mem ;use in sum128
sumof 	.equ sumof_mem ;use in sum128
to_print .equ to_print_mem ;use hprint128


hprint 		;;http://swensont.epizy.com/ZX81Assembly.pdf?i=1
	PUSH AF ;store the original value of A for later
	AND $F0 ; isolate the first digit
	RRA
	RRA
	RRA
	RRA
	ADD A,$1C ; add 28 to the character code
	CALL PRINT ;
	POP AF ; retrieve original value of A
	AND $0F ; isolate the second digit
	ADD A,$1C ; add 28 to the character code
	CALL PRINT
	LD A,_NL
	CALL PRINT ; print a space character
	RET
	
	
hprint128  ; print one 16byte number stored in location $to_print
	;ld hl,$to_print
	ld hl,$to_print+$0f	
	ld b,16	
hprint128_loop	
	ld a, (hl)
	push af ;store the original value of a for later
	and $f0 ; isolate the first digit
	rra
	rra
	rra
	rra
	add a,$1c ; add 28 to the character code
	call PRINT ;
	pop af ; retrieve original value of a
	and $0f ; isolate the second digit
	add a,$1c ; add 28 to the character code
	call PRINT
	ld a, 00;_NL ;print new line ; 00 is space
	;call PRINT ; print a space character
	
	dec hl
	djnz hprint128_loop
	; restore registers
	ld a, _NL
	call PRINT
	ret
	
hprint256  ; print one 32byte number stored in location $to_print
	;ld hl,$to_print
	ld hl,$to_print+31	
	ld b,32	
hprint256_loop	
	ld a, (hl)
	push af ;store the original value of a for later
	and $f0 ; isolate the first digit
	rra
	rra
	rra
	rra
	add a,$1c ; add 28 to the character code
	call PRINT ;
	pop af ; retrieve original value of a
	and $0f ; isolate the second digit
	add a,$1c ; add 28 to the character code
	call PRINT
	ld a, 00;_NL ;print new line ; 00 is space
	;call PRINT ; print a space character
	
	dec hl
	djnz hprint256_loop
	; restore registers
	ld a, _NL
	call PRINT
	
	ret	
	
sum128 ;; add two 128bit values from memory locations s1,s2 , then store in sumof
		;; all three locations MUST be consecutive in memory 16bytes (128bits!) appart
    ld hl,(s1) \ ld de,(s2) \ add hl,de \ ld (sumof),hl
    ld hl,(s1+2) \ ld de,(s2+2) \ adc hl,de \ ld (sumof+2),hl
    ld hl,(s1+4) \ ld de,(s2+4) \ adc hl,de \ ld (sumof+4),hl
    ld hl,(s1+6) \ ld de,(s2+6) \ adc hl,de \ ld (sumof+6),hl
    ld hl,(s1+8) \ ld de,(s2+8) \ adc hl,de \ ld (sumof+8),hl
	ld hl,(s1+10) \ ld de,(s2+10) \ adc hl,de \ ld (sumof+10),hl
	ld hl,(s1+12) \ ld de,(s2+12) \ adc hl,de \ ld (sumof+12),hl
	ld hl,(s1+14) \ ld de,(s2+14) \ adc hl,de \ ld (sumof+14),hl
	ret

sum256 ;; add two 256bit values from memory locations s1, s2, then store in sumof
		;; all three locations MUST be consecutive in memory 16bytes (128bits!) appart
    ld hl,(s1) \ ld de,(s2) \ add hl,de \ ld (sumof),hl
    ld hl,(s1+2) \ ld de,(s2+2) \ adc hl,de \ ld (sumof+2),hl
    ld hl,(s1+4) \ ld de,(s2+4) \ adc hl,de \ ld (sumof+4),hl
    ld hl,(s1+6) \ ld de,(s2+6) \ adc hl,de \ ld (sumof+6),hl
    ld hl,(s1+8) \ ld de,(s2+8) \ adc hl,de \ ld (sumof+8),hl
	ld hl,(s1+10) \ ld de,(s2+10) \ adc hl,de \ ld (sumof+10),hl
	ld hl,(s1+12) \ ld de,(s2+12) \ adc hl,de \ ld (sumof+12),hl
	ld hl,(s1+14) \ ld de,(s2+14) \ adc hl,de \ ld (sumof+14),hl
	
	ld hl,(s1+16) \ ld de,(s2+16) \ adc hl,de \ ld (sumof+16),hl
	ld hl,(s1+18) \ ld de,(s2+18) \ adc hl,de \ ld (sumof+18),hl
	ld hl,(s1+20) \ ld de,(s2+20) \ adc hl,de \ ld (sumof+20),hl
	ld hl,(s1+22) \ ld de,(s2+22) \ adc hl,de \ ld (sumof+22),hl
	ld hl,(s1+24) \ ld de,(s2+24) \ adc hl,de \ ld (sumof+24),hl
	ld hl,(s1+26) \ ld de,(s2+26) \ adc hl,de \ ld (sumof+26),hl
	ld hl,(s1+28) \ ld de,(s2+28) \ adc hl,de \ ld (sumof+28),hl
	ld hl,(s1+30) \ ld de,(s2+30) \ adc hl,de \ ld (sumof+30),hl	
	ret	
	
zero128				; zero 128bits starting from hl,
					; b and hl clobbered by this
	ld b,$10			; set loop control variable for next loop "$" shows its hex
zero128_loop
	ld (hl),0			; set the thing hl points to to a constant
	inc hl	
	djnz zero128_loop		; jump if loop control variable not zero
	ret

zero256				; zero 128bits starting from hl,
					; b and hl clobbered by this
	ld b,$10			; set loop control variable for next loop "$" shows its hex
zero256_loop
	ld (hl),0			; set the thing hl points to to a constant
	inc hl	
	djnz zero256_loop		; jump if loop control variable not zero
	ret	
	
start

	call CLS	

	ld hl, s1		;;; initialise s1 s2 and sumof all to zero
	call zero256	
	ld hl, s2	
	call zero256		
	ld hl, sumof		
	call zero256
					;;; set s2 to the second in fibonacci sequence ie "1", s1 is already zero	
	ld hl, s2		
	ld (hl),1	

	ld hl, s1		; copy s1 to print buffer 
	ld de, to_print		;
	ld bc, 32
	ldir				; copy 16 bytes from hl to de	
	call hprint256 		; print a 128 bit (16byte numnber as hex to screen)

	ld hl, s2		; copy s2 to print buffer 
	ld de, to_print		;
	ld bc, 32
	ldir				; copy 16 bytes from hl to de	
	call hprint256 		; print a 128 bit (16byte numnber as hex to screen)
	
	ld hl,mainLoopCount
	ld a,8
	ld (hl),a

	ld hl,mainCLSCountDown_mem  ;; limit the number of screen clears and the 23 * 8 fib numbers
	ld a,36
	ld (hl),a
		
	
	
mainloopFib
	; s1 contains first number to add
	; s2 contains second number to add
	; sumof is result	
	call sum256

	ld hl, sumof		; copy sumof to print buffer 
	ld de, to_print		;
	ld bc, 32
	ldir				; move 16 bytes from hl of de

	call hprint256 		; print a 128 bit (16byte numnber as hex to screen)
	
	ld hl, s2			; copy s2 to s1 
	ld de, s1			;
	ld bc, 32
	ldir				; move 16 bytes from hl of de
	
	ld hl, sumof		; copy sumof to s2
	ld de, s2			;
	ld bc, 32
	ldir				; move 16 bytes from hl of de

	ld hl,mainLoopCount
	ld a,(hl)	
	dec a
	jp nz, skip
	
	ld (hl),a

	ld hl,mainLoopCount
	ld a,10
	ld (hl),a	

	ld hl,mainCLSCountDown_mem
	ld a, (hl)
	dec a
	jp z, endPROG
	ld (hl),a
	call hprint 	; print out how many iterations (of screen clears)
	call CLS	
	
	jp mainloopFib
	
skip
	ld hl,mainLoopCount
	ld (hl),a			; save a back to (hl) (mainLoopCount)
	
	jp mainloopFib	
	
endPROG
	ret





;include our variables
#include "vars.asm"

; ===========================================================
; code ends
; ===========================================================
;end the REM line and put in the RAND USR line to call our 'hex code'
#include "line2.asm"

;display file defintion
#include "screen.asm"               

;close out the basic program
#include "endbasic.asm"
