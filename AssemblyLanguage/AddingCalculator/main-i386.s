#
# main-i386.s
#
# A simple calculator using the Mac OS X assembler targeting the IA-32 architecture.
#
# Relavent documentation:
# Mac OS X Assembler Reference
# Mac OS X ABI Function Call Guide
#
	.cstring
startupMessage: .asciz "i386 Calculator Started!\n= 0"
outputFormatString: .asciz "= %d\n"

	.lcomm inputBuffer, 20
	
	.text
	
	.globl _main
_main:

	# Make sure stack is 16-byte aligned. Once it is 16-byte aligned don't
	# do anything to it that will screw up the alignment. That way I don't
	# have to check that it's 16-byte aligned before each call to an Mac OS X ABI
	# compliant function.
	andl $0xfffffff0, %esp
	
	# Initialize the Calculator's accumulator
	# %ebx is available for general use in non-position independent code
	# and is preserved between function calls so it doesn't need to be saved
	# to the stack.
	movl $0, %ebx
	
	# Print out the Calculated Started! message.
	# Call puts. It takes one pointer argument.
	subl $16, %esp				# allocate space for arguments and maintain 16-byte alignment
	leal startupMessage, %eax	# copy the address of the startup message to eax. Equivalent to "movl $startupMessage, %eax"
	movl %eax, (%esp)			# copy eax to the position of the first argument to puts
	call _puts
	
	# Normally, deallocate space from the stack using
	# addl $16,%esp
	# However, the rest of the methods in this program make ABI calls that
	# require less than 16 bytes of arguments, so just leave it how it is. No need
	# to change the stack pointer anymore.
	
getInput:

	# Get a line of input. If over 20 bytes it will overflow.
	# Call gets. It takes one poiner argument.
	leal inputBuffer, %eax		# Copy the address of the input buffer to eax
	movl %eax, (%esp)			# Copy eax to the position of the first argument to gets
	call _gets					# Get the string
	
	or	%eax, %eax			# Sets ZF (the zero flag) if both are zero.
	jz exit
	
	# This time, doin't deallocate the space for the gets arguments because
	# the next function to call, atoi, takes the same parameter, a single pointer.
	call _atoi					# Call atoi, ABI says return value for integers place in eax
	
	# Continue to put off deallocation of the state since we're going to make another call
	# to printf, whose arguments can fit into the 16 bytes we've allocated.

	# Add the result of atoi to Calculator's accumulator, ebx.
	addl %eax, %ebx
	
	# Print Calculator's accumulator using the format string.
	movl %ebx, 4(%esp)
	leal outputFormatString, %eax
	movl %eax, (%esp)
	call _printf
	
	jmp getInput
	
exit:
	movl	$0, (%esp)
	call _exit
	