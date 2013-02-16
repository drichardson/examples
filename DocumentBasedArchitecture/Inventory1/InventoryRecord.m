//
//  InventoryRecord.m
//  Inventory1
//
//  Created by Doug on 7/23/09.
//  Copyright 2009 Douglas Richardson. All rights reserved.
//

#import "InventoryRecord.h"


@implementation InventoryRecord

@synthesize title, description, price, purchaseDate;

+(InventoryRecord*)recordWithTitle:(NSString*)title withDescription:(NSString*)description withPrice:(NSNumber*)price withPurchaseDate:(NSDate*)datePurchased
{
	InventoryRecord *result = [[[InventoryRecord alloc] init] autorelease];
	
	result.title = title;
	result.description = description;
	result.price = price;
	result.purchaseDate = datePurchased;
	
	return result;
}

+(InventoryRecord*)recordWithDescriptor:(NSDictionary*)descriptor
{
	InventoryRecord *result = [self recordWithTitle:[descriptor objectForKey:@"title"]
									withDescription:[descriptor objectForKey:@"description"]
										  withPrice:[descriptor objectForKey:@"price"]
								   withPurchaseDate:[descriptor objectForKey:@"purchaseDate"]];
	
	return result;
}

- (NSDictionary*)descriptor
{
	return [NSDictionary dictionaryWithObjectsAndKeys:title, @"title", description, @"description", price, @"price", purchaseDate, @"purchaseDate", nil];
}

@end
