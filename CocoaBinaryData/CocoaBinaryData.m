#import <Foundation/Foundation.h>

static void dumpData(NSData *data)
{
	printf("Data Length: %d\nBytes: ", [data length]);
	int i;
	int length = [data length];
	uint8_t *bytes = (uint8_t*)malloc(length);
	if(bytes) {
		[data getBytes:bytes];
		for(i = 0; i < length; ++i) {
			uint8_t bh = (bytes[i]>>4) & 0xF;
			uint8_t bl = bytes[i] & 0xF;
			if(bh > 9)
				putchar('A' + bh - 0xa);
			else
				putchar('0' + bh);
			
			if(bl > 9)
				putchar('A' + bl - 0xa);
			else
				putchar('0' + bl);
		}
		
		putchar('\n');
		free(bytes);
		bytes = 0;
	}
}

int main (int argc, const char * argv[]) {
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];

	const uint8_t b1[] = { 0, 1, 2, 10, 13, 15, 16, 255 };
	
	NSData *d1 = [NSData dataWithBytes:b1 length:sizeof(b1)];
	dumpData(d1);
	
	NSMutableData * md = [NSMutableData data];
	dumpData(md);
	
	int i;
	for(i = 0; i < 5; ++i) {
		[md appendData:d1];
		dumpData(md);
	}
	
	[md appendData:md];
	dumpData(md);
	
	[md increaseLengthBy:10];
	dumpData(md);
	
    [pool release];
    return 0;
}
