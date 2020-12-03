//
//  FullScreenAppDelegate.m
//  FullScreen
//
//  Created by Doug Richardson on 12/29/10.
//  Copyright 2010 Doug Richardson. All rights reserved.
//

#import "FullScreenAppDelegate.h"
#import "CustomOpenGLView.h"

@implementation FullScreenAppDelegate

- (void)applicationDidFinishLaunching:(NSNotification *)aNotification
{
	NSRect mainDisplayRect = [[NSScreen mainScreen] frame];
	NSWindow* window = [[NSWindow alloc] initWithContentRect:mainDisplayRect styleMask:NSBorderlessWindowMask backing:NSBackingStoreBuffered defer:YES];
	[window setLevel:NSMainMenuWindowLevel+1]; // In front of the main menu
	[window setOpaque:YES];
	[window setHidesOnDeactivate:YES]; // Don't want this shown when it shouldn't be frontmost (i.e. when you command+tab away)
	
	
	NSOpenGLPixelFormatAttribute attributes[] = {
		NSOpenGLPFADoubleBuffer,
		0
	};
	NSOpenGLPixelFormat* pixelFormat = [[NSOpenGLPixelFormat alloc] initWithAttributes:attributes];
	
	CustomOpenGLView* openGLView = [[CustomOpenGLView alloc] initWithFrame:NSMakeRect( 0, 0, mainDisplayRect.size.width, mainDisplayRect.size.height ) pixelFormat:pixelFormat];
	[window setContentView:openGLView];
	
	[window makeKeyAndOrderFront:self];
	
	[pixelFormat release];
	[openGLView release];
}

@end
