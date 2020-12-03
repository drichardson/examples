//
//  CustomOpenGLViewAppDelegate.m
//  CustomOpenGLView
//
//  Created by Doug Richardson on 12/29/10.
//  Copyright 2010 Doug Richardson. All rights reserved.
//

#import "CustomOpenGLViewAppDelegate.h"
#import "CustomOpenGLView.h"

@implementation CustomOpenGLViewAppDelegate

@synthesize window;

- (void)applicationDidFinishLaunching:(NSNotification *)aNotification {
	// Insert code here to initialize your application
	NSView* contentView = [window contentView];
	CustomOpenGLView* openGLView = [[CustomOpenGLView alloc] initWithFrame:contentView.bounds];
	[openGLView setAutoresizingMask:NSViewWidthSizable | NSViewHeightSizable];
	[contentView addSubview:openGLView];
    
    [self setGlView:openGLView];
}

@end
