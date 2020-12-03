//
//  Speaker.m
//  Speech
//
//  Created by Doug on 2/19/08.
//  Copyright 2008 Douglas Richardson. All rights reserved.
//

#import "Speaker.h"


@implementation Speaker

- (id)init {
    self = [super init];
    if (self) {
		synth = [[NSSpeechSynthesizer alloc] init]; //start with default voice
		[synth setDelegate:self];
    }
    return self;
}

-(void)dealloc
{
	[synth setDelegate:nil];
	[synth release];
	[super dealloc];
}

- (void)speak:(NSString*)phrase
{
    [synth startSpeakingString:phrase];
}

@end
