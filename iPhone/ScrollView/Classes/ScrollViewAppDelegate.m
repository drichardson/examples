//
//  ScrollViewAppDelegate.m
//  ScrollView
//
//  Created by Doug on 8/1/08.
//

#import "ScrollViewAppDelegate.h"
#import "ScrollViewViewController.h"

@implementation ScrollViewAppDelegate

@synthesize window;
@synthesize viewController;


- (void)applicationDidFinishLaunching:(UIApplication *)application {	
	
	// Override point for customization after app launch	
    [window addSubview:viewController.view];
	[window makeKeyAndVisible];
}


- (void)dealloc {
    [viewController release];
	[window release];
	[super dealloc];
}


@end
