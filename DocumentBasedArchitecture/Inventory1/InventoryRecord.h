//
//  InventoryRecord.h
//  Inventory1
//
//  Created by Doug on 7/23/09.
//  Copyright 2009 Douglas Richardson. All rights reserved.
//

#import <Cocoa/Cocoa.h>


@interface InventoryRecord : NSObject {
	NSString *title;
	NSString *description;
	NSNumber *price;
	NSDate *purchaseDate;
}

+(InventoryRecord*)recordWithTitle:(NSString*)title withDescription:(NSString*)description withPrice:(NSNumber*)price withPurchaseDate:(NSDate*)datePurchased;

+(InventoryRecord*)recordWithDescriptor:(NSDictionary*)descriptor;
- (NSDictionary*)descriptor;

@property (retain) NSString* title;
@property (retain) NSString* description;
@property (retain) NSNumber* price;
@property (retain) NSDate* purchaseDate;

@end
