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
	
	NSOpenGLPixelFormatAttribute attributes[] = {
		
		// setup for anti-aliasing
		NSOpenGLPFASampleBuffers, 1,
		NSOpenGLPFASamples, 4,
		
		// 0 terminated
		0
	};
	
	NSOpenGLPixelFormat* pixelFormat = [[NSOpenGLPixelFormat alloc] initWithAttributes:attributes];
	
	NSView* contentView = [window contentView];
	CustomOpenGLView* openGLView = [[CustomOpenGLView alloc] initWithFrame:contentView.bounds pixelFormat:pixelFormat];
	[openGLView setAutoresizingMask:NSViewWidthSizable | NSViewHeightSizable];
	[contentView addSubview:openGLView];
	
	[pixelFormat release];
	[openGLView release];
}

@end
