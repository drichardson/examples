//
//  AppController.m
//  ControlLayoutView
//
//  Created by Doug on 1/20/08.
//  Copyright 2008 __MyCompanyName__. All rights reserved.
//

#import "AppController.h"


@implementation AppController

-(void)awakeFromNib
{
	[flowLayoutView setHorizontalPadding:2];
	[flowLayoutView setVerticalPadding:2];
}

-(IBAction)addPressed:(id)sender
{
	static int i = 0;
	++i;
	NSButton *button = [[[NSButton alloc] initWithFrame:NSMakeRect(0, 0, 1, 1)] autorelease];
	NSLog(@"Button frame before sizeToFit: %@", NSStringFromRect([button frame]));
	[button setTitle:[NSString stringWithFormat:@"Button %d", i]];
	//[button setBezelStyle:NSRecessedBezelStyle];
	[button setBezelStyle:NSRoundRectBezelStyle];
	[button sizeToFit];
	NSLog(@"             after sizeToFit: %@", NSStringFromRect([button frame]));
	[flowLayoutView addSubview:button];
}

-(IBAction)removeAllPressed:(id)sender
{
	[flowLayoutView removeAllSubviews];
}

@end
