//
//  TuningForkAppDelegate.m
//  TuningFork
//
//  Created by Doug on 3/11/08.
//  Copyright Douglas Richardson 2008. All rights reserved.
//

#import "TuningForkAppDelegate.h"

#import "TuningViewController.h"
#import "MetronomeViewController.h"
#import "ToneGeneratorViewController.h"

@implementation TuningForkAppDelegate

@synthesize window;
@synthesize toolbarController;

- (void)applicationDidFinishLaunching:(UIApplication *)application {
    // Create window
    self.window = [[[UIWindow alloc] initWithFrame:[[UIScreen mainScreen] bounds]] autorelease];
    
	 // Create a toolbar controller and an array to contain the view controllers
	toolbarController = [[UIToolbarController alloc] init];
	NSMutableArray *viewControllers = [[NSMutableArray alloc] initWithCapacity:3];
	
	TuningViewController *tuningForkViewController = [[[TuningViewController alloc] init] autorelease];
	[viewControllers addObject:tuningForkViewController];
	
	MetronomeViewController *tempoViewController = [[[MetronomeViewController alloc] init] autorelease];
	[viewControllers addObject:tempoViewController];
	
	ToneGeneratorViewController *toneGeneratorViewController = [[[ToneGeneratorViewController alloc] init] autorelease];
	[viewControllers addObject:toneGeneratorViewController];
	
	// Add the view controllers to the toolbar controller
	toolbarController.viewControllers = viewControllers;
	[viewControllers release];
	
	// Add the toolbar controller's current view as a subview of the window, then display the window
	[window addSubview:toolbarController.view];
    [window makeKeyAndVisible];
}

- (void)dealloc {
    [toolbarController release];
    [window release];
	[super dealloc];
}

@end

