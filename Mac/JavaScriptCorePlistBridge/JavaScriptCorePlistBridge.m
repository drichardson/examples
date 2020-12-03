#import <Foundation/Foundation.h>
#import "JavaScriptRunner.h"

int main (int argc, const char * argv[]) {
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];

	JavaScriptRunner* runner = [JavaScriptRunner new];
	
	NSStringEncoding encoding = 0;
	NSError* error = nil;
	NSString* example1 = [NSString stringWithContentsOfFile:@"example1.js" usedEncoding:&encoding error:&error];
	
	if ( example1 )
	{
		[runner runJavaScript:example1 withSecurityContext:nil];
	}
	else
	{
		NSLog(@"Error loading example1.js. %@", error);
	}

	[runner release];
	
    [pool drain];
    return 0;
}
