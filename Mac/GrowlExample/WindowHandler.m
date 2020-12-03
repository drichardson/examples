#import "WindowHandler.h"
#import "Notifier.h"
#import "Growl/Growl.h"

@implementation WindowHandler

- (IBAction)postGrowlNotification:(id)sender
{
	NSLog(@"Field is %@", [mName stringValue]);
	[[Notifier defaultNotifier] buttonPushed:[mName stringValue]];
}

- (void)awakeFromNib
{
	NSLog(@"awakeFromNib called");
	[GrowlApplicationBridge setGrowlDelegate:[Notifier defaultNotifier]];
}

@end
