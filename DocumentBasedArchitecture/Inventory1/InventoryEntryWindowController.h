//
//  InventoryEntryWindowController.h
//  Inventory1
//
//  Created by Doug on 7/23/09.
//  Copyright 2009 Douglas Richardson. All rights reserved.
//

#import <Cocoa/Cocoa.h>


@interface InventoryEntryWindowController : NSWindowController {
	NSString *title;
	NSString *description;
	NSNumber *price;
	NSDate *purchaseDate;
}

@property (retain) NSString* title;
@property (retain) NSString* description;
@property (retain) NSNumber* price;
@property (retain) NSDate* purchaseDate;

-(IBAction)newRecordPressed:(id)sender;
-(IBAction)storeRecordPressed:(id)sender;

@end
