//
//  InventoryEntryWindowController.m
//  Inventory1
//
//  Created by Doug on 7/23/09.
//  Copyright 2009 Douglas Richardson. All rights reserved.
//

#import "InventoryEntryWindowController.h"
#import "InventoryDocument.h"

@interface InventoryEntryWindowController ()
-(void)reset;
@end

@implementation InventoryEntryWindowController

@synthesize title, description, price, purchaseDate;

- (id)init
{
	return [super initWithWindowNibName:@"InventoryEntryWindow"];
}

- (void)dealloc
{
	[super dealloc];
}

- (void)windowWillLoad
{
	[self reset];
}

- (void)reset
{
	self.title = @"";
	self.description = @"";
	self.price = [NSNumber numberWithInt:0];
	self.purchaseDate = [NSDate date];
}

-(IBAction)newRecordPressed:(id)sender
{
	[self reset];
}

-(IBAction)storeRecordPressed:(id)sender
{
	NSLog(@"%@, %@, %@, %@", title, description, price, purchaseDate);
	
	InventoryRecord *record = [InventoryRecord recordWithTitle:title
											   withDescription:description
													 withPrice:price
											  withPurchaseDate:purchaseDate];
	
	[(InventoryDocument*)[self document] addRecord:record];
}

@end
