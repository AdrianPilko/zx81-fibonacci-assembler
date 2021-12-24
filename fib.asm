#include "zx81defs.asm"
;EQUs for ROM routines
#include "zx81rom.asm"
;ZX81 char codes/how to survive without ASCII
#include "charcodes.asm"
;system variables
#include "zx81sys.asm"

;the standard REM statement that will contain our 'hex' code
#include "line1.asm"

current	.equ $5000
last 	.equ $5010
s1 		.equ $5020 ;use in sum128
s2 		.equ $5030 ;use in sum128
sumof 	.equ $5040 ;use in sum128
to_print .equ $5050 ;use hprint128
	
	
	jp start
	
hprint 
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
	ld a, 0;_NL ;print new line ; 00 is space
	call PRINT ; print a space character
	ret

hprint128  ; print one 16byte number stored in location $to_print
	ld hl,$to_print
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
	
	inc hl
	djnz hprint128_loop
	; restore registers
	ld a, 00
	
	ret
	
sum128 ;; add two 128bit values from memory locations s1 , then store in sumof
		;; all three locations MUST be consecutive in memory $10 appart
		;; a b and ix are clobbered
	ld ix,$s1
	ld b,16
	or a
sum128_loop
	ld a,(ix)
	adc a,(ix+$10)
	ld (ix+$20),a
	inc ix
	djnz sum128_loop
	ret
	
copy128	; copy 16byte value from start locations adressed by de to hl	
	ld b,$10			; set loop counter
copy128_loop
	ld a,(de)	
	ld (hl),a
	inc de
	inc hl
	djnz copy128_loop	; single instruction decrements b compares not zero else jumps
	ret
	
;start
	call CLS
	ld a,0  		; set a to first in fibonacci sequenceie zero	
	ld b,1 			; set b to 2nd in fibonacci sequence	
	ld hl,(s1) 		; store a in s1
	ld (hl),a		; 
	ld hl,(s2) 		; store b in s2
	ld (hl),b		; 
	;call sum128		; requires s1 s2 to have 128bit locations with the two numbers to add
	;setup fo copy128 from ix to hl
	ld ix,sumof
	ld hl,to_print
	call copy128
	call hprint128  ; print the sum which is stored in to_print
start	
	ld e,$10			; set loop control variable for next loop "$" shows its hex
	ld hl, to_print		; set hl to point to "to_print" print buffer
	
loopzero 	
	ld (hl),0			; set the thing hl points to to a constant
	inc hl	
	dec e				; update loop control variable, decrement 
	cp e				; compare loop control
	jp nz, loopzero		; jump if loop control variable not zero
	
	ld e,$05			; set loop control variable for next loop
	ld hl, to_print		; reset hl back to to_print print buffer		

loop1 	
	ld (hl),e
	inc hl	
	
	push hl				; need to save hl as clobbered by hprint128
	call hprint128 		; print a 128 bit (16byte numnber as hex to screen)
	pop hl				; restore hl after hprint128 
	push hl				; push it again because copy128 clobbers it
	push de				; push de because copy128 clobbers it
	ld de, to_print		; copy from print buffer to s1 for the sum
	ld hl, s1			;
	call copy128		; clobbers de and hl
	;pop de				; restore de (comment out not yet)
	;pop hl				; restore hl (commented out not yet)
	
	; now print s1 to confirm it has same value as to_print had, but zero to_print before the copy
	ld hl, to_print		; set hl to point to "to_print" print buffer
;	ld e,$10			; set loop control variable for next loop "$" shows its hex
;loopzero1
	;ld (hl),0			; set the thing hl points to to a constant
	;inc hl	
	;dec e				; update loop control variable, decrement 
	;cp e				; compare loop control
	;jp nz, loopzero1		; jump if loop control variable not zero

	ld de, to_print		; copy from print buffer to s2 for the sum
	ld hl, s2			;
	call copy128		; clobbers de and hl	
	; s1 contains first number to add
	; s1 contains second number to add
	; sumof is result	
	push af
	push bc
	push ix
	call sum128
	pop ix
	pop bc
	pop af
	ld de, sumof		; copy sumof to print buffer 
	ld hl, to_print		;
	call copy128		; clobbers de and hl	
	
	call hprint128 		; print a 128 bit (16byte numnber as hex to screen)
	;;;;;
	pop de				; restore de (comment out not yet)
	pop hl				; restore hl (commented out not yet)	
	
	dec e				; update loop control variable, decrement 
	cp e				; compare loop control
	jp nz, loop1		; jump if loop control variable not zero
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
						
