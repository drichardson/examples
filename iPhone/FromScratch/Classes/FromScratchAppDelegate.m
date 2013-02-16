//
//  FromScratchAppDelegate.m
//  FromScratch
//
//  Created by Doug on 3/11/08.
//  Copyright __MyCompanyName__ 2008. All rights reserved.
//

#import "FromScratchAppDelegate.h"
#import "MyView.h"
#import "BlueViewController.h"
#import "RedViewController.h"

@implementation FromScratchAppDelegate

@synthesize window;

- (void)applicationDidFinishLaunching:(UIApplication *)application {	
	// Create window
	self.window = [[[UIWindow alloc] initWithFrame:[[UIScreen mainScreen] bounds]] autorelease];
    
#if 0
    // Set up content view
	self.contentView = [[[MyView alloc] initWithFrame:[[UIScreen mainScreen] applicationFrame]] autorelease];
	[window addSubview:contentView];
#endif
	
	toolbarController = [[UIToolbarController alloc] init];
	
	
	BlueViewController *blueVC = [[[BlueViewController alloc] init] autorelease];
	RedViewController *redVC = [[[RedViewController alloc] init] autorelease];
	UIViewController *noSubclassVC = [[[UIViewController alloc] init] autorelease];
	noSubclassVC.view = [[[UIView alloc] init] autorelease];
	noSubclassVC.view.backgroundColor = [UIColor greenColor];
	noSubclassVC.title = @"Green";
	toolbarController.viewControllers = [NSArray arrayWithObjects:blueVC, redVC, noSubclassVC, nil];
	
	
	[window addSubview:toolbarController.view];
    
	// Show window
	[window makeKeyAndVisible];
}

- (void)dealloc {
	[window release];
	[toolbarController release];
	[super dealloc];
}

@end
