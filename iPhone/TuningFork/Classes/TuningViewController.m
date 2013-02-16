//
//  MyViewController.m
//  TuningFork
//
//  Created by Doug on 3/11/08.
//  Copyright Douglas Richardson 2008. All rights reserved.
//

#import "TuningViewController.h"
#import "TuningView.h"

@implementation TuningViewController

- (id)init
{
	if(self = [super init])
	{
		self.toolbarItem.image = [UIImage imageNamed:@"Tuning-Fork-Off.png"];
		self.toolbarItem.selectedImage = [UIImage imageNamed:@"Tuning-Fork-On.png"];
	}
	
	return self;
}

- (void)loadView {
	// Create and initialize the view the controller will manage
	TuningView *myView = [[TuningView alloc] initWithFrame:[[UIScreen mainScreen] bounds]];
	self.view = myView;
	[myView release];
}

- (void)dealloc {
	[super dealloc];
}

@end
