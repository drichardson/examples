//
//  MyView.m
//  TuningFork
//
//  Created by Doug on 3/11/08.
//  Copyright Douglas Richardson 2008. All rights reserved.
//

#import "TuningView.h"

@implementation TuningView

@synthesize color;

- (void)drawRect:(CGRect) rect {
	// Draw a box with my color inset by 40 points
	[self.color set];
	UIEdgeInsets inset = UIEdgeInsetsMake(40, 40, 40, 40);
	CGRect box = UIEdgeInsetsInsetRect(self.bounds, inset);
	UIRectFill(box);
}

- (void)dealloc {
    [color release];
	[super dealloc];
}

@end
