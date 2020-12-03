#include <stdio.h>
#include <stdint.h>

static void asmTest1()
{
	// The code below could also have been put into a single
	// asm(...) block which each string separated by a newline.
	// However, individual asm statements written like this allow
	// stepping throught by the Xcode debugger.
	asm("push %eax");
	asm("#This is an assembly comment");
	asm("mov $0,%eax");
	asm("myLoopLabel:");
	asm("inc %eax");
	asm("cmp $10,%eax");
	asm("jl myLoopLabel");
	asm("pop %eax");
}

extern void myASMCallableFunction()
{
	printf("This is a ASM callable function.\n");
}

static void asmTest2(char* myVar)
{
	asm(
		"# Beginning of asmTest2 asssembly code\n"
		"call _myASMCallableFunction\n"
		);
}

static void asmTest_inputOutputVariables(void)
{
	uint32_t a=10,b;
	asm(
		"movl	%1, %%eax;\n\t"
		"movl	%%eax, %0;\n\t"
		: "=r"(b) // b is a write only output operand. r means use any register for storing operands.
		: "r"(a) // a is a read only input operand
		: "%eax" // clobbered register
	);
	
	printf("a = %u, b = %u\n", a, b);
}

int main (int argc, const char * argv[]) {

	asmTest1();
	asmTest2("This is a test string\n");
	printf("This is a test string\n");
	
	asmTest_inputOutputVariables();
	
    return 0;
}
