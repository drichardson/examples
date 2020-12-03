#import <Foundation/Foundation.h>

@interface MyClass : NSObject
{
}
@end

@implementation MyClass

- (void)myHandler:(NSNotification*)notification
{
	printf("Got notification: %s with userInfo: %s\n", [[notification description] UTF8String], [[[notification userInfo] description] UTF8String]);
	exit(0);
}

@end


int main (int argc, const char * argv[]) {
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];

	MyClass *obj = [MyClass new];
	
	if (argc > 1)
	{
		puts("Looking for notification\n");
		[[NSDistributedNotificationCenter defaultCenter] addObserver:obj selector:@selector(myHandler:) name:@"DougsDistributedNotification" object:nil];
		[[NSRunLoop currentRunLoop] runUntilDate:[NSDate dateWithTimeIntervalSinceNow:10]];
	}
	else
	{
		puts("Sending notification");
		NSDictionary *userInfo = [NSDictionary dictionaryWithObjectsAndKeys:@"Value1", @"Key1", @"Value2", @"Key2", nil];
		[[NSDistributedNotificationCenter defaultCenter] postNotificationName:@"DougsDistributedNotification" object:@"MyTest" userInfo:userInfo];
	}


    [pool drain];
    return 0;
}
