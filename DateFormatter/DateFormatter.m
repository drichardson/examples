#import <Foundation/Foundation.h>

int main (int argc, const char * argv[]) {
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];
	
	NSDateFormatter *mDateStampFormatter = [[NSDateFormatter alloc] init];
	[mDateStampFormatter setFormatterBehavior:NSDateFormatterBehavior10_4];
	[mDateStampFormatter setDateStyle:NSDateFormatterNoStyle];
	[mDateStampFormatter setDateFormat:@"yyyy-MM-dd'T'HH:mm:ss'Z'"];
	[mDateStampFormatter setTimeZone:[NSTimeZone timeZoneForSecondsFromGMT:0]];
		
	NSString *str = [mDateStampFormatter stringFromDate:[NSDate date]];
	NSLog(@"Time is now %@ GMT", str);
	
    [pool drain];
    return 0;
}
