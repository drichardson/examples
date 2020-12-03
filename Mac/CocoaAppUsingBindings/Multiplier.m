#import "Multiplier.h"

@implementation Multiplier

+ (void) initialize {
	// Don't called parent class initializer - the runtime system does that automatically.
	[Multiplier setKeys:
		[NSArray arrayWithObjects:@"value1", @"value2", nil]
		triggerChangeNotificationsForDependentKey:@"result"];
}

- (double) result
{
	return value11 * value2;
}

- (void) setValue1:(double)newValue
{
	value11 = newValue;
}

- (double) value1
{
	return value11;
}

@end
