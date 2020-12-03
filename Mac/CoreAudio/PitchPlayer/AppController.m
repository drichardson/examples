//
//  AppController.m
//  PitchPlayer
//
//  Created by Doug on 3/18/08.
//  Copyright 2008 Douglas Richardson. All rights reserved.
//

#import "AppController.h"


@implementation AppController

-(void)awakeFromNib
{
	NSLog(@"Awake from nib");
	
	pitchGenerator = [[PitchGenerator alloc] init];
}

-(void)dealloc
{
	[pitchGenerator release];
	[super dealloc];
}

-(IBAction)playPressed:(id)sender
{
	NSLog(@"Play pressed, delegate is %@", pitchFrequency.delegate);
	pitchGenerator.frequencyInHz = pitchFrequency.objectValue;
	[pitchGenerator startPlaying];
}

-(IBAction)stopPressed:(id)sender
{
	NSLog(@"Stop pressed");
	[pitchGenerator stopPlaying];
}


#pragma mark Text Field Delegates

- (void)controlTextDidEndEditing:(NSNotification *)aNotification
{
	if([aNotification object] == pitchFrequency)
	{
		NSLog(@"Setting frequency to %@", pitchFrequency.objectValue);
		pitchGenerator.frequencyInHz = pitchFrequency.objectValue;
	}
}

@end
