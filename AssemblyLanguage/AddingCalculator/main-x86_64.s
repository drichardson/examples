#
# main-x86_64.s
#
# A simple calculator using the Mac OS X assembler targeting the x86-64 architecture.
#
# Relavent documentation:
# Mac OS X Assembler Reference
# Mac OS X ABI Function Call Guide
# System V Application Binary Interface AMD64 Architecture
#
# Note: INTEGER argument (see in comments below) refer to the INTEGER class of argumnets
# from the System V Application Binary Interface AMD64 Architecture document. It represents
# fix point types, including pointers.
#
	.cstring
startupMessage: .asciz "x86-64 Calculator Started!\n= 0"
outputFormatString: .asciz "= %ld\n"

	.lcomm inputBuffer, 20, 4
	
	.text
	
	.globl _main
_main:	

	# Make sure stack is 16-byte aligned. Once it is 16-byte aligned don't
	# do anything to it that will screw up the alignment. That way I don't
	# have to check that it's 16-byte aligned before each call to an Mac OS X ABI
	# compliant function.
	andq $0xfffffffffffffff0, %rsp
	
	# Initialize the Calculator's accumulator
	# %rbx is available for general use in non-position independent code
	# and is preserved between function calls so it doesn't need to be saved
	# to the stack.
	movq $0, %rbx
	
	# Print out the Calculated Started! message.
	# Call puts. It takes one pointer argument.
	leaq startupMessage(%rip), %rdi	# copy the address of the startup message to rdi, the first INTEGER class argument.
	call _puts
	
getInput:

	# Get a line of input. If over 20 bytes it will overflow.
	# Call gets. It takes one poiner argument.
	leaq inputBuffer(%rip), %rdi	# Copy the address of the input buffer to rdi, the first INTEGER class argument.
	call _gets						# Get the string
	
	or		%rax, %rax				# See if _gets returned 0. The zero flag (ZF) will be set if it is 0.
	jz		exit
	
	leaq inputBuffer(%rip), %rdi	# Copy the address of the input buffer to rdi, the first INTEGER class argument.
	call _atoi					# Call atoi, ABI says return value for integers place in rax
	
	# Continue to put off deallocation of the state since we're going to make another call
	# to printf, whose arguments can fit into the 16 bytes we've allocated.

	# Add the result of atoi to Calculator's accumulator, rbx.
	addq %rax, %rbx
	
	# Print Calculator's accumulator using the format string.
	leaq outputFormatString(%rip), %rdi # rdi is the first INTEGER class argument register
	movq %rbx, %rsi						# rsi is the second INTEGER class argument register.
	movq $0, %rax						# Since printf takes variable arguments, %rax says how many vector args are used.
	call _printf
	
	jmp getInput
	
exit:
	movq $0, %rdi
	call _exit