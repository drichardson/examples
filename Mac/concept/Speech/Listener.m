//
//  Listener.m
//  Speech
//
//  Created by Doug on 2/19/08.
//  Copyright 2008 Douglas Richardson. All rights reserved.
//

#import "Listener.h"


@implementation Listener

static const NSString * kHi= @"Hi";
static const NSString * kWhatsYourName = @"What's your name";
static const NSString * kThankYou = @"Thank you";

- (id)init
{
    self = [super init];
    if (self) {
		speaker = [[Speaker alloc] init];
        NSArray *cmds = [NSArray arrayWithObjects:kHi, kWhatsYourName, kThankYou, nil];
        recognizer = [[NSSpeechRecognizer alloc] init];
        [recognizer setCommands:cmds];
        [recognizer setDelegate:self];
		[recognizer startListening];
    }
    return self;
}

-(void)dealloc
{
	[speaker release];
	
	[recognizer setDelegate:nil];
	[recognizer release];
	
	[super dealloc];
}

- (void)speechRecognizer:(NSSpeechRecognizer *)sender didRecognizeCommand:(id)aCmd
{	
	NSLog(@"Something recognized");
	
    if ([kHi isEqualToString:aCmd])
	{
		NSLog(@"Hi");
		[speaker speak:@"Hello."];
    }
	
    if ([kWhatsYourName isEqualToString:aCmd])
	{
		NSLog(@"Whats your name?");
		[speaker speak:@"One bad mother fucker."];
    }
	
    if ([kThankYou isEqualToString:aCmd])
	{
		NSLog(@"Thank you");
		[speaker speak:@"You're welcome."];
    }
}

@end
