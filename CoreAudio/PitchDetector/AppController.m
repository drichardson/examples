//
//  AppController.m
//  PitchDetector
//
//  Created by Doug on 3/23/08.
//  Copyright 2008 Douglas Richardson. All rights reserved.
//

#import "AppController.h"


@implementation AppController

-(void)awakeFromNib
{	
	frequencyDetector = [[DominantFrequencyDetector alloc] init];
	//[frequencyDetector performSelectorInBackground:@selector(startDetecting) withObject:nil];
	
	[[NSNotificationCenter defaultCenter] addObserver:self
											 selector:@selector(frequencyChangedNotification:)
												 name:kDominantFrequencyDetector_FrequencyChangedNotification
											   object:frequencyDetector];
	
	[frequencyDetector startDetecting];
}

-(void)dealloc
{
	[[NSNotificationCenter defaultCenter] removeObject:self];
	[frequencyDetector release];
	[super dealloc];
}

-(void)frequencyChangedNotification:(NSNotification*)notification
{
	DominantFrequencyDetector *detector = [notification object];
	[detector.dominantFrequencyInHz descriptionWithLocale:[NSLocale currentLocale]];
}

-(IBAction)toggleStartStop:(id)sender
{
	NSButton *b = sender;
	
	if(b.state == NSOnState)
		[frequencyDetector startDetecting];
	else if(b.state == NSOffState)
		[frequencyDetector stopDetecting];
}

@end
