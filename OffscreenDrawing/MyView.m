//
//  MyView.m
//  OffscreenDrawing
//
//  Created by Douglas Richardson on 3/29/07.
//  Copyright 2007 __MyCompanyName__. All rights reserved.
//

#import "MyView.h"


@implementation MyView

const int WIDTH = 800;
const int HEIGHT = 600;

- (id)initWithFrame:(NSRect)frameRect
{
	self = [super initWithFrame:frameRect];
	NSLog(@"in INIT");
	if(self) {
		mRep =
			[[NSBitmapImageRep alloc] initWithBitmapDataPlanes:nil
													pixelsWide:WIDTH
													pixelsHigh:HEIGHT
												 bitsPerSample:8
											   samplesPerPixel:4
													  hasAlpha:YES
													  isPlanar:NO
												colorSpaceName:NSCalibratedRGBColorSpace
												   bytesPerRow:0
												  bitsPerPixel:32];
		uint32_t *p = (uint32_t*)[mRep bitmapData];
		mNewColor = 0x000000ff;
		
		// Make it red.
		int i;
		for(i = 0; i < WIDTH * HEIGHT; ++i) {
			p[i] = 0xff000000 | mNewColor;
		}
	}
	
	return self;
}

-(void) dealloc {
	[mRep release];
	mRep = nil;
	
	[super dealloc];
}

-(void) drawRect:(NSRect)rect
{
	NSLog(@"drawRect");
	
	// TODO: can I cache the NSImage or do I have to create
	// one after every change the the underlying bitmap?
	NSImage *image = [[NSImage alloc] init];
	[image addRepresentation:mRep];
	[image drawInRect:rect // TODO: what if the rect is only part of the window? This is wrong.
			 fromRect:NSZeroRect
			operation:NSCompositeSourceOver
			 fraction:1.0];
	
	[image release];
	
}

- (IBAction) copyNewRect:(id) sender
{
	NSGraphicsContext *gcontex = [NSGraphicsContext graphicsContextWithBitmapImageRep:mRep];
	[NSGraphicsContext saveGraphicsState];
	[NSGraphicsContext setCurrentContext:gcontex];
	
	
	// Cast away const qualifier. It doesn't matter if the pixel data in the update
	// message is changed, it won't be used again.
	NSBitmapImageRep *rep =
		[[NSBitmapImageRep alloc] initWithBitmapDataPlanes:nil
												pixelsWide:200
												pixelsHigh:400
											 bitsPerSample:8
										   samplesPerPixel:3
												  hasAlpha:NO
												  isPlanar:NO
											colorSpaceName:NSCalibratedRGBColorSpace
											   bytesPerRow:0
											  bitsPerPixel:32];
	
	// Make a green rectangle.
	uint32_t *p = (uint32_t*)[rep bitmapData];
	int i;
	for(i = 0; i < 200 * 400; ++i) {
		p[i] = mNewColor;
	}
	
	NSImage *image = [[NSImage alloc] init];
	[image addRepresentation:rep];
	[image drawInRect:NSMakeRect(0, 0, 200, 400)
			 fromRect:NSZeroRect
			operation:NSCompositeSourceOver
			 fraction:1.0];		

	[image release];
	[rep release];
	
	
	[NSGraphicsContext restoreGraphicsState];
	[self setNeedsDisplay:YES];
}

- (IBAction) makeNewRectColor:(id) sender
{
	if(mNewColor == 0x0000ff)
		mNewColor = 0x00ff00;
	else if(mNewColor == 0x00ff00)
		mNewColor = 0xff0000;
	else
		mNewColor = 0x000ff;
}

@end
