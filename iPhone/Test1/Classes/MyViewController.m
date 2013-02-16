//
//  MyViewController.m
//  Test1
//
//  Created by Doug on 3/6/08.
//  Copyright __MyCompanyName__ 2008. All rights reserved.
//

#import "MyViewController.h"
#import "MyView.h"

@implementation MyViewController

@synthesize color;

- (void)loadView {
	// Create and initialize the view the controller will manage
	MyView *myView = [[MyView alloc] initWithFrame:[[UIScreen mainScreen] bounds]];
	myView.color = self.color;
	self.view = myView;
	[myView release];
}

- (void)dealloc {
    [color release];
	[super dealloc];
}

@end
