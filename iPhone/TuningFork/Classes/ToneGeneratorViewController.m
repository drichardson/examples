//
//  ToneGeneratorViewController.m
//  TuningFork
//
//  Created by Doug on 3/12/08.
//  Copyright 2008 Douglas Richardson. All rights reserved.
//

#import "ToneGeneratorViewController.h"


@implementation ToneGeneratorViewController

- (id)init
{
	if (self = [super init]) {
		self.toolbarItem.image = [UIImage imageNamed:@"Tone-Generator-Off.png"];
		self.toolbarItem.selectedImage = [UIImage imageNamed:@"Tone-Generator-On.png"];
	}
	return self;
}


- (void)loadView
{
	// Create a custom view hierarchy.
	UIView *view = [[UIView alloc] initWithFrame:[UIScreen mainScreen].applicationFrame];
	view.autoresizingMask = UIViewAutoresizingFlexibleHeight|UIViewAutoresizingFlexibleWidth;
	self.view = view;
	[view release];
}

- (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)interfaceOrientation
{
	// Return YES for supported orientations.
	return (interfaceOrientation == UIInterfaceOrientationPortrait);
}

- (void)didReceiveMemoryWarning
{
	[super didReceiveMemoryWarning]; // Releases the view if it doesn't have a superview.
	// Release anything that's not essential, such as cached data.
}

- (void)dealloc
{
	[super dealloc];
}


@end
