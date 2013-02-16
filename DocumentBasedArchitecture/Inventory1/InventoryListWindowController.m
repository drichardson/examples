//
//  InventoryListWindowController.m
//  Inventory1
//
//  Created by Doug on 7/23/09.
//  Copyright 2009 Douglas Richardson. All rights reserved.
//

#import "InventoryListWindowController.h"
#import "InventoryDocument.h"

@implementation InventoryListWindowController

- (id)init
{
	return [super initWithWindowNibName:@"InventoryListWindow"];
}

#pragma mark NSTableDataSource

- (NSInteger)numberOfRowsInTableView:(NSTableView *)aTableView
{
	if(aTableView == tableView)
		return [(InventoryDocument*)[self document] recordCount];
	
	return 0;
}

- (id)tableView:(NSTableView *)aTableView objectValueForTableColumn:(NSTableColumn *)aTableColumn row:(NSInteger)rowIndex
{
	id result;
	
	if(aTableView == tableView)
	{
		InventoryDocument *doc = [self document];
		InventoryRecord *record = [doc recordAtIndex:rowIndex];
		
		result = [record valueForKey:[aTableColumn identifier]];
	}
	else
		result = nil;
	
	return result;
}

-(void)update
{
	[tableView reloadData];
}

@end
