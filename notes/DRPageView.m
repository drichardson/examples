//
//  DRPageView.m
//  notes
//
//  Created by Doug on 2/18/08.
//  Copyright 2008 Douglas Richardson. All rights reserved.
//

#import "DRPageView.h"


@implementation DRPageView

- (id)initWithFrame:(NSRect)frame {
    self = [super initWithFrame:frame];
    if (self) {
        // Initialization code here.
    }
    return self;
}

- (void)drawRect:(NSRect)rect {
	[[NSColor yellowColor] setFill];
	NSRectFill(rect);
	
	
}

@synthesize contentItems;

@end
