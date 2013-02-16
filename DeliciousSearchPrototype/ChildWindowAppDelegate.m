//
//  ChildWindowAppDelegate.m
//  ChildWindow
//
//  Created by Doug on 9/16/09.
//  Copyright 2009 Douglas Richardson. All rights reserved.
//

#import "ChildWindowAppDelegate.h"
#import "DXSearchController.h"

@implementation ChildWindowAppDelegate

@synthesize window;

- (void)applicationDidFinishLaunching:(NSNotification *)aNotification {
	// Insert code here to initialize your application 
	[[[DXSearchController alloc] init] showWindow:self];
}

@end
