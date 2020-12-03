//
//  AppController.m
//  Internationalization
//
//  Created by Doug on 2/19/08.
//  Copyright 2008 Douglas Richardson. All rights reserved.
//

#import "AppController.h"


@implementation AppController

-(IBAction)buttonPressed:(id)sender
{
	[label setStringValue:inputField.stringValue];
}

-(IBAction)loadLocalizedText:(id)sender
{
	[label setStringValue:NSLocalizedString(@"Hello, World!", @"Some sample text that should be localized.")];
}

@end
