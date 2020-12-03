// NOTE: This code is for Leopard and later.
// This test only produces interesting output when built in Debug mode since the functions seems to put
// placed inline in Release mode. Make sure in your real app the symbols are available in your release build.

#include <stdio.h>
#include <string.h>
#include <execinfo.h>

void func_one(void);
void func_two(void);
void dump_frames(void);
void PrintIsItInCallstack(const char* function);

int main(int argc, char *argv[]) {
	PrintIsItInCallstack("main");
	PrintIsItInCallstack("func_one");
	PrintIsItInCallstack("func_two");
	func_one();
	return 0;
}

void func_one(void) {
	PrintIsItInCallstack("main");
	PrintIsItInCallstack("func_one");
	PrintIsItInCallstack("func_two");
	
	func_two();
}

void func_two(void) {
	PrintIsItInCallstack("main");
	PrintIsItInCallstack("func_one");
	PrintIsItInCallstack("func_two");
	
	dump_frames();
}

void dump_frames(void) {
	void *backtraceFrames[128];
	int frameCount = backtrace(&backtraceFrames[0], 128);
	char **frameStrings = backtrace_symbols(&backtraceFrames[0], frameCount);
	
	const char* funcname = "func_two";
	
	if(frameStrings != NULL) {
		int x = 0;
		for(x = 0; x < frameCount; x++) {
			if(frameStrings[x] == NULL) { break; }
			printf("%s\n", frameStrings[x]);
			
			if(strstr(frameStrings[x], funcname) != 0)
				printf("  Found %s\n", funcname);
		}
		free(frameStrings);
	}
	
	printf("SEARCHING FOR ADDRESS OF func_one (%p)\n", func_one);
	int i;
	for(i = 0; i < frameCount; ++i)
	{
		printf("  Looking at %p\n", backtraceFrames[i]);
		if(backtraceFrames[i] == func_one)
			printf("    Found func_one: %p\n", func_one);
	}
}

int IsFunctionInCallstack(const char* function)
{
	int result = 0;
	void *backtraceFrames[128];
	int frameCount = backtrace(&backtraceFrames[0], 128);
	char **frameStrings = backtrace_symbols(&backtraceFrames[0], frameCount);
	
	if(frameStrings != NULL) {
		int x = 0;
		for(x = 0; x < frameCount; x++)
		{
			if(frameStrings[x] == NULL)
				break;
			
			if(strstr(frameStrings[x], function) != 0)
			{
				result = 1;
				break;
			}
		}
		free(frameStrings);
	}
	
	return result;
}

void PrintIsItInCallstack(const char* function)
{
	printf("Is %s in callstack? %s\n", function, IsFunctionInCallstack(function) ? "Yes." : "No.");
}
