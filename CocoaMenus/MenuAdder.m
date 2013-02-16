//
//  MenuAdder.m
//  CocoaMenus
//
//  Created by Douglas Richardson on 7/29/07.
//  Copyright 2007 __MyCompanyName__. All rights reserved.
//

#import "MenuAdder.h"


@implementation MenuAdder

- (void) myMethod
{
	NSLog(@"myMethod called");
}

-(IBAction)addMenu:(id)sender
{
	NSLog(@"Add Menu");
	
	NSMenu *mm = [NSApp mainMenu];
	
	NSLog(@"Main menu has %d items", [mm numberOfItems]);
	
	// Programatically Create a Menu
	id <NSMenuItem> mi = [mm addItemWithTitle:@"My Menu" action:nil keyEquivalent:@""];
	
	NSMenu *subMenu = [[NSMenu alloc] initWithTitle:@"My Menu"];
	
	[mi setSubmenu:subMenu];
	
	mi = [subMenu addItemWithTitle:@"Item 1" action:@selector(myMethod) keyEquivalent:@""];
	[mi setTarget:self];
	
	// Create a menu that exists in a NIB.
	NSNib *nib = [[NSNib alloc] initWithNibNamed:@"ExtraMenu" bundle:nil];
	NSArray *topLevelObjects = nil;
	if([nib instantiateNibWithOwner:self topLevelObjects:&topLevelObjects])
	{
		NSLog(@"NIB was loaded");
		
		NSLog(@"Count: %u, Array: %@", [topLevelObjects count], topLevelObjects);
		
		id obj = [topLevelObjects objectAtIndex:0];
		if([obj isKindOfClass:[NSMenu class]])
		{
			NSMenu *extraMenu = (NSMenu*)obj;
			NSMenuItem *menuItem = [mm addItemWithTitle:[extraMenu title]
												 action:nil
										  keyEquivalent:@""];
			[menuItem setSubmenu:extraMenu];
			[menuItem setTarget:self];
		}
		
		[nib release];
	}
	else
	{
		NSLog(@"Error loading Nib for self.");
	}
	
	NSLog(@"Main menu has %d items", [mm numberOfItems]);
	
	[mm setTitle:@"howdy"];
	
	NSEnumerator *itemEnum = [[mm itemArray] objectEnumerator];
	id obj;
	while(obj = [itemEnum nextObject])
	{
		NSLog(@"Item: %@", obj);
	}
}

// Actions
-(IBAction)menuItem1:(id)sender
{
	NSLog(@"menuItem1 called");
}

-(IBAction)menuItem2:(id)sender
{
	NSLog(@"menuItem2 called");
}

-(IBAction)subMenuItem1:(id)sender
{
	NSLog(@"subMenuItem1 called");
}

-(IBAction)subMenuItem2:(id)sender
{
	NSLog(@"subMenuItem1 called");
}

@end
