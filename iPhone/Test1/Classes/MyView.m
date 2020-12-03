//
//  MyView.m
//  Test1
//
//  Created by Doug on 3/6/08.
//  Copyright __MyCompanyName__ 2008. All rights reserved.
//

#import "MyView.h"

@implementation MyView

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
