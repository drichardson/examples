//
//  InventoryDocument.h
//  Inventory1
//
//  Created by Doug on 7/23/09.
//  Copyright 2009 Douglas Richardson. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "InventoryRecord.h"

@interface InventoryDocument : NSDocument
{
	NSMutableArray *records;
	NSWindowController *listController;
}

-(void)addRecord:(InventoryRecord*)record;
-(void)replaceRecord:(InventoryRecord*)record atIndex:(NSUInteger)index;
-(NSUInteger)recordCount;
-(InventoryRecord*)recordAtIndex:(NSUInteger)index;

@end
