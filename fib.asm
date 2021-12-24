#include "zx81defs.asm"
;EQUs for ROM routines
#include "zx81rom.asm"
;ZX81 char codes/how to survive without ASCII
#include "charcodes.asm"
;system variables
#include "zx81sys.asm"

;the standard REM statement that will contain our 'hex' code
#include "line1.asm"

current	.equ $8f00
last 	.equ $8f10
s1 		.equ $8f20 ;use in sum128
s2 		.equ $8f30 ;use in sum128
sumof 	.equ $8f40 ;use in sum128
to_print .equ $8f50 ;use hprint128
mainLoopCount .equ $8f60	
	
	jp start
	
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
	LD A,$00
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
	
zero128				; zero 128bits starting from hl,
					; b and hl clobbered by this
	ld b,$10			; set loop control variable for next loop "$" shows its hex
zero128_loop
	ld (hl),0			; set the thing hl points to to a constant
	inc hl	
	dec b				; update loop control variable, decrement 
	cp b				; compare loop control
	jp nz, zero128_loop		; jump if loop control variable not zero
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
	
start	
	call CLS	
	ld hl, s1		;;; initialise s1 s2 and sumof all to zero
	call zero128	
	ld hl, s2		
	call zero128		
	ld hl, sumof		
	call zero128
					;;; set s2 to the second in fibonacci sequence ie "1", s1 is already zero
	push ix
	ld ix, s2		
	ld (ix),1	
	pop ix
	
	ld de, s1		; copy sumof to print buffer 
	ld hl, to_print		;
	call copy128		; clobbers de and hl	
	call hprint128 		; print a 128 bit (16byte numnber as hex to screen)

	ld de, s2		; copy sumof to print buffer 
	ld hl, to_print		;
	call copy128		; clobbers de and hl	
	call hprint128 		; print a 128 bit (16byte numnber as hex to screen)
	
	ld hl,mainLoopCount
	ld a,$10
	ld (hl),a
	
loop1 			
	
	; s1 contains first number to add
	; s2 contains second number to add
	; sumof is result	
	push ix
	call sum128
	pop ix

	ld de, sumof		; copy sumof to print buffer 
	ld hl, to_print		;
	
	call copy128		; clobbers b, de and hl		
	call hprint128 		; print a 128 bit (16byte numnber as hex to screen)
	ld de, s2			; copy from s1 to s2 for the next sum
	ld hl, s1			;	
	call copy128		; clobbers b, de and hl			
	ld de, sumof		; copy from sumof to s2 for the next sum
	ld hl, s2			;	
	call copy128		; clobbers b, de and hl	
	

	ld hl,mainLoopCount
	ld a,(hl)	
	dec a
	ld (hl),a
	cp a
	jp nz, skip
	
	ld hl,mainLoopCount
	ld a,$10
	ld (hl),a
	
	;cp a
	;jp nz, skip
	;call PAUSE
	;call SCROLL
	;call CLS
	;ld a,$0f			; set loop control variable for next loop	
	jp loop1
skip	
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
