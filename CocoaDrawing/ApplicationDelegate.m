//
//  ApplicationDelegate.m
//  CocoaDrawing1
//
//  Created by Douglas Richardson on 3/13/07.
//  Copyright 2007 __MyCompanyName__. All rights reserved.
//

#import "ApplicationDelegate.h"
#import "MultiplicationValueTransformer.h"

@implementation ApplicationDelegate

+ (void) initialize
{
	puts("initialize called - registering value transformers");
	MultiplicationValueTransformer *myXFormer;
	myXFormer = [[[MultiplicationValueTransformer alloc] initWithFactor:100] autorelease];
	[NSValueTransformer setValueTransformer:myXFormer forName:@"FractionToPercentageTransformer"];
}

- (void)applicationDidHide:(NSNotification *)aNotification
{
	printf("ApplicationDelegate: applicationDidHide\n");
}

@end
