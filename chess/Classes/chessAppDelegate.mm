//
//  chessAppDelegate.m
//  chess
//
//  Created by Doug on 11/16/08.
//  Copyright Douglas Richardson 2008. All rights reserved.
//

#import "chessAppDelegate.h"
#import "ChessView.h"

@implementation chessAppDelegate

@synthesize window;
@synthesize gameController;

- (void)applicationDidFinishLaunching:(UIApplication *)application {
	[gameController runGame];
}


- (void)applicationWillResignActive:(UIApplication *)application {	
	// TODO: Probably should stop process at this point since the user is doing something else. Or, maybe finish the current calculation first.
}


- (void)applicationDidBecomeActive:(UIApplication *)application {
	// TODO: If processing stopped in applicationWillResignActive, then start it up again here.
}


- (void)dealloc {
	[gameController release];
	[window release];
	[super dealloc];
}

@end
