//
//  MyTimerView.m
//  CocoaTimer
//
//  Created by Doug on 5/8/08.
//  Copyright 2008 Douglas Richardson. All rights reserved.
//

#import "MyTimerView.h"


@implementation MyTimerView

- (id)initWithFrame:(NSRect)frame {
    self = [super initWithFrame:frame];
    if (self) {
        // Initialization code here.
		mFrameColor = [[NSColor blueColor] retain];
		mFillColor = [[NSColor redColor] retain];
		[NSTimer scheduledTimerWithTimeInterval:2 target:self selector:@selector(timerFired:) userInfo:nil repeats:YES];
    }
    return self;
}

-(void)dealloc
{
	[mFrameColor release];
	[mFillColor release];
	[super dealloc];
}

- (void)drawRect:(NSRect)rect {
    // Drawing code here.
	[mFillColor setFill];
	[mFrameColor setStroke];
	//NSRectFill(rect);
	NSBezierPath *path = [NSBezierPath bezierPathWithRect:rect];
	[path setLineWidth:40.0];
	[path fill];
	[path stroke];
}

-(void)timerFired:(NSTimer*)timer
{	
	NSColor *tmp = mFillColor;
	mFillColor = mFrameColor;
	mFrameColor = tmp;
		
	[self setNeedsDisplay:YES];
}

@end
