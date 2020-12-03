;
; main-ppc.s
;
; A simple calculator using the Mac OS X assembler targeting the ppc architecture.
;
; Relavent documentation:
; Mac OS X Assembler Reference
; Mac OS X ABI Function Call Guide
;
	.cstring
startupMessage: .asciz "ppc Calculator Started!\n= 0"
outputFormatString: .asciz "= %d\n"

	.lcomm inputBuffer, 20
	
	.text
	
	.globl _main
_main:
	mflr	r0		; setup the stack frame
	li		r13,0	; initialize Calculator's accumulator
	
	; Since all instructions are 32-bits long, there is no way to specify a complete 32-bit address
	; in a single instruction. Therefore, two instructions must be used to load the address. See
	; "PowerPC Addressing Modes and Assembler Instructions" in the "Mac OS X Assembler Reference"
	; for more details.
	
	; Copy the address of startupMessage into r3, the first parameter register.
	addis	r2,0,hi16(startupMessage) ; Load the high order bits of startupMessage into the high order bits of r2
	ori		r3,r2,lo16(startupMessage); Bitwise OR the low order bits of startupMessage with r2 into r3
	
	bl _puts ; Call puts
		
getInput:
	; Get the next string of input
	addis	r2,0,hi16(inputBuffer)
	ori		r3,r2,lo16(inputBuffer)
	bl _gets
	
	; Check the return value of _gets.
	cmpwi r3,0			; Is r3 0?
	beq exit			; If r3 equals 0 then branch to exit.
	
	; Convert the string to an integer
	; Since the pointer is already in r3, don't need to do anything else.
	bl _atoi
	
	; Add the result to Calculator's accumulator
	add		r13,r13,r3
	
	; Print the accumulated result
	addis	r2,0,hi16(outputFormatString)	; Set the first parameter to outputFormatString
	ori		r3,r2,lo16(outputFormatString)
	mr		r4,r13							; Set the second parameter to equal to the Calculator's accumulator.
	bl _printf

	b getInput
	
exit:
	li r3,0
	b _exit
	