//
//  AppController.m
//  Layout
//
//  Created by Doug on 1/20/08.
//  Copyright 2008 __MyCompanyName__. All rights reserved.
//

#import "AppController.h"


@implementation AppController

-(void)awakeFromNib
{
	NSLog(@"awake");
	//[flowLayout setSizing:ZMakeFlowLayoutSizing(NSMakeSize(0, 0), 0, ZSpringRight, NO)];
	[flowLayout setSizing:ZMakeFlowLayoutSizing(NSMakeSize(100, 20), 2, ZSpringRight, NO)];
	[flowLayout setBackgroundColor:[NSColor redColor]];
}

-(IBAction)addButtonPressed:(id)sender
{
	static int i = 0;
	++i;
	NSButton *button = [[NSButton alloc] initWithFrame:[flowLayout frame]];
	[button setBezelStyle:NSRoundedBezelStyle];
	[button setTitle:[NSString stringWithFormat:@"Button %d", i]];
	[button sizeToFit];
	[flowLayout addSubview:button];
}

@end
