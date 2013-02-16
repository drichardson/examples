//
//  Run.m
//  CDCLI
//
//  Created by Doug on 7/20/09.
//  Copyright 2009 Douglas Richardson. All rights reserved.
//

#import "Run.h"


@implementation Run

@dynamic date, primitiveDate;

- (NSInteger)processID
{
	[self willAccessValueForKey:@"processID"];
	NSInteger pid = processID;
	[self didAccessValueForKey:@"processID"];
	
	return pid;
}

- (void)setProcessID:(NSInteger)newProcessID
{
	[self willChangeValueForKey:@"processID"];
	processID = newProcessID;
	[self didChangeValueForKey:@"processID"];
}

- (void)setNilValueForKey:(NSString*)key
{
	if([key isEqualToString:@"processID"])
		self.processID = 0;
	else
		[super setNilValueForKey:key];
}

- (void)awakeFromInsert
{
	[super awakeFromInsert];	
	self.primitiveDate = [NSDate date];
}

@end
