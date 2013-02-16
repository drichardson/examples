#import <Foundation/Foundation.h>
#import "BackgroundThread.h"

int main (int argc, const char * argv[]) {
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];

	NSNotificationCenter *nCenter = [NSNotificationCenter defaultCenter];
    NSDistributedNotificationCenter *dnCenter = [NSDistributedNotificationCenter defaultCenter];
	
	// Start a thread from a class method.
	[NSThread detachNewThreadSelector:@selector(MyClassThreadMethod:) toTarget:[BackgroundThread class] withObject:nil];
	
	// Start a thread from an instance method.
	BackgroundThread *bt = [[BackgroundThread alloc] init];
	[NSThread detachNewThreadSelector:@selector(MyInstanceThreadMethod:) toTarget:bt withObject:nil];
	
	sleep(10);
	
    [pool release];
    return 0;
}
