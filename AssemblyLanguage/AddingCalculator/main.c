// main.c - Calculator
// This program is a reference for the other targets written in assembly.

#include <stdio.h>
#include <stdlib.h>

int main (int argc, const char * argv[]) {
	
	long int currentValue = 0;
	static char buf[20];
	
	puts("C Calculator Started!\n= 0");
	
	while (gets(buf))
	{
		currentValue += atoi(buf);
		printf("= %ld\n", currentValue);
	}
	
    return 0;
}
