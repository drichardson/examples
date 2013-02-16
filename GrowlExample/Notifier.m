//
//  Notifier.m
//  GrowlExample
//
//  Created by Douglas Richardson on 6/21/07.
//  Copyright 2007 __MyCompanyName__. All rights reserved.
//

#import "Notifier.h"

static NSString* const kButtonPush = @"Button Push";

@implementation Notifier

+(Notifier*)defaultNotifier
{
	static Notifier* gd = nil;
	if(gd == nil) {
		// synchronized is expensive, so only call it if we have to.
		@synchronized(self) {
			// now that we have the lock, make sure gd is still nil.
			if(gd == nil) {
				gd = [[Notifier alloc] init];
			}
		}
	}
	return gd;
}

- (NSDictionary *) registrationDictionaryForGrowl
{
	NSArray *notifications = [NSArray arrayWithObjects:kButtonPush, nil];
	return [NSDictionary dictionaryWithObjectsAndKeys:notifications, GROWL_NOTIFICATIONS_ALL,
		notifications, GROWL_NOTIFICATIONS_DEFAULT, nil];
	
}

-(void)growlIsReady
{
	NSLog(@"growl is ready");
}

-(void) growlNotificationWasClicked:(id)clickContext
{
	NSLog(@"growl notification was clicked: %@", clickContext);
}

-(void)buttonPushed:(NSString*)message
{
	[GrowlApplicationBridge notifyWithTitle:NSLocalizedString(@"Button Pushed", @"Button Push Comment")
								description:message
						   notificationName:kButtonPush
								   iconData:nil
								   priority:0
								   isSticky:NO
							   clickContext:nil];
}

@end
