//
//  MyController.m
//  TokenField
//
//  Created by Douglas Richardson on 12/1/07.
//  Copyright 2007 __MyCompanyName__. All rights reserved.
//

#import "MyController.h"

static NSString *possibilities[] = {
	@"One", @"Two", @"Three", @"Four", @"Five", @"Six", @"Seven",
	@"Eight", @"Nine", @"Ten", @"Twenty One", @"Zero", @"Fourty"
};

@implementation MyController

-(void)awakeFromNib
{
	NSLog(@"awaking");
	
	
	[myTokenField setTokenizingCharacterSet:[NSCharacterSet characterSetWithCharactersInString:@".|"]];
	[myTokenField setDelegate:self];
	//[tokenField setTokenStyle:NSPlainTextTokenStyle];
}

-(NSArray *)tokenField:(NSTokenField *)tokenField completionsForSubstring:(NSString *)substring
		   indexOfToken:(int)tokenIndex
	indexOfSelectedItem:(int *)selectedIndex
{
	NSLog(@"tokenField: %@, completionsFor: %@, indexOfToken: %d, indexOfSelectedItem: %p",
		  tokenField, substring, tokenIndex, selectedIndex);
	
#if 0
	*selectedIndex = 2;
	return [NSArray arrayWithObjects:@"One", @"Two", @"Three", nil];
#else
	NSArray *a = [NSArray arrayWithObjects:possibilities count:sizeof(possibilities)/sizeof(possibilities[0])];
	
	//NSPredicate *predicate = [NSPredicate
//							  predicateWithFormat:@"(lastName like[cd] %@) AND (birthday > %@)",
//							  lastNameSearchString, birthdaySearchDate];
	NSPredicate *predicate = [NSPredicate predicateWithFormat:@"SELF beginswith[cd] %@", substring];
	NSLog(@"Predicate: %@", predicate);
	NSLog(@"a1 = %@", a);
	a = [a filteredArrayUsingPredicate:predicate];
	NSLog(@"a2 = %@", a);
	
	return a;
#endif
}

@end
