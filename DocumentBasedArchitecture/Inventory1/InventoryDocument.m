//
//  InventoryDocument.m
//  Inventory1
//
//  Created by Doug on 7/23/09.
//  Copyright 2009 Douglas Richardson. All rights reserved.
//

#import "InventoryDocument.h"
#import "InventoryEntryWindowController.h"
#import "InventoryListWindowController.h"

@implementation InventoryDocument

- (id)init
{
	self = [super init];
	
	if(self)
	{
		records = [[NSMutableArray alloc] init];
	}
	
	return self;
}

- (void)dealloc
{
	[records release];
	[super dealloc];
}

- (void)makeWindowControllers
{	
	[self addWindowController:[[[InventoryEntryWindowController alloc] init] autorelease]];
	
	listController = [[[InventoryListWindowController alloc] init] autorelease];
	[self addWindowController:listController];
}

- (NSData *)dataOfType:(NSString *)typeName error:(NSError **)outError
{
    // Insert code here to write your document to data of the specified type. If the given outError != NULL, ensure that you set *outError when returning nil.
	
	NSString *errorString = nil;
	NSDictionary *plist = [NSDictionary dictionaryWithObject:records forKey:@"records"];
	NSData *result = [NSPropertyListSerialization dataFromPropertyList:plist format:NSPropertyListXMLFormat_v1_0 errorDescription:&errorString];
	
	if(result == nil && outError)
	{
		NSDictionary *userInfo = errorString == nil ? nil : [NSDictionary dictionaryWithObject:errorString forKey:NSUnderlyingErrorKey];
		*outError = [NSError errorWithDomain:@"InventoryDocument" code:1 userInfo:userInfo];
	}
	
	if(errorString)
		[errorString release];
	
	return result;
}

- (BOOL)readFromData:(NSData *)data ofType:(NSString *)typeName error:(NSError **)outError
{
    // Insert code here to read your document from the given data of the specified type.  If the given outError != NULL, ensure that you set *outError when returning NO.

	NSString *errorString = nil;
	NSDictionary *plist = [NSPropertyListSerialization propertyListFromData:data mutabilityOption:NSPropertyListMutableContainersAndLeaves format:nil errorDescription:&errorString];
	
	[records release];
	
	if(plist && [plist isKindOfClass:[NSDictionary class]])
		records = [[plist objectForKey:@"records"] retain];
	else
		records = nil;
	
	BOOL result = records != nil ? YES : NO;
	
	if(!result && outError)
	{
		NSDictionary *userInfo = errorString == nil ? nil : [NSDictionary dictionaryWithObject:errorString forKey:NSUnderlyingErrorKey];
		*outError = [NSError errorWithDomain:@"InventoryDocument" code:1 userInfo:userInfo];
	}
	
	if(errorString)
		[errorString release];
    
    return result;
}

-(void)addRecord:(InventoryRecord*)record
{
	[records addObject:[record descriptor]];
	[self updateChangeCount:NSChangeDone];
	[(InventoryListWindowController*)listController update];
}

-(void)replaceRecord:(InventoryRecord*)record atIndex:(NSUInteger)index
{
	if(index < [records count])
	{
		[records replaceObjectAtIndex:index withObject:[record descriptor]];
		[self updateChangeCount:NSChangeDone];
		[(InventoryListWindowController*)listController update];
	}
}

-(NSUInteger)recordCount
{
	return [records count];
}

-(InventoryRecord*)recordAtIndex:(NSUInteger)index
{
	InventoryRecord *result;
	
	if(index < [records count])
		result = [InventoryRecord recordWithDescriptor:[records objectAtIndex:index]];
	else
		result = nil;
	
	return result;
}

@end
