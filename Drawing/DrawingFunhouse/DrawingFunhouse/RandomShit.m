//
//  RandomShit.m
//  DrawingFunhouse
//
//  Created by Douglas Richardson on 3/7/12.
//  Copyright (c) 2012 Doug Richardson. All rights reserved.
//

#import "RandomShit.h"

@implementation RandomShit

- (id)initWithFrame:(NSRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
        // Initialization code here.
    }
    
    return self;
}

- (void)drawRect:(NSRect)dirtyRect
{    
    [[NSColor whiteColor] setFill];
    NSRectFill(dirtyRect);
    
    // Draw a fake button
    NSRect buttonRect = NSMakeRect(0, 0, 100, 40);
    NSDrawButton(buttonRect, buttonRect);
    [@"Hello" drawInRect:NSMakeRect(10, 10, 100, 20) withAttributes:nil];
}

@end
