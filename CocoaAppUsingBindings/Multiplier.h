/* Multiplier */

#import <Cocoa/Cocoa.h>
#import <CoreFoundation/CoreFoundation.h>

@interface Multiplier : NSObject
{
	double value11;
	double value2;
}

- (double) result;

// Cocoa binding can use either the variable directly or accessor methods
// like so. In this case, Cocoa binding can access value11 as value1.
// Note: it doesn't appear that a change to the variable directly will
// update GUIs connected to it. It appears that you need to use accessor methods.
- (void) setValue1:(double)newValue;
- (double) value1;

@end
