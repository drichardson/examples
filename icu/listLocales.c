#import <stdio.h>
#import <unicode/uloc.h>

int main(int argc, char** argv)
{
	int32_t count = uloc_countAvailable();

	for(int i = 0; i < count; ++i)
	{
		printf("Got local %d: %s\n", i, uloc_getAvailable(i));
	}

	return 0;
}