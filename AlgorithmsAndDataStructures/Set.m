//
//  Set.m
//  AlgorithmsAndDataStructures
//
//  Created by Douglas Richardson on 3/17/07.
//  Copyright 2007 __MyCompanyName__. All rights reserved.
//

#import "Set.h"

@implementation Set

-(void) dealloc
{
	[list release];
	[super dealloc];
}

-(void)add:(NSObject*)value
{
	if(![self contains:value]) {
		LinkedList *tmp = [[LinkedList alloc] initWithNext:list value:value];
		[list release];
		list = tmp;
	}
}

-(void)remove:(NSObject*)value
{
	LinkedList *p, *prev = nil;
	for(p = list; p != nil; p = [p next]) {
		if([[p value] isEqual:value]) {
			if(prev == nil) {
				list = [[list next] retain];
				[p release];
				break;
			} else {
				[prev setNext:[p next]];
				[p release];
				break;
			}
		}
		prev = p;
	}
}

-(BOOL)contains:(NSObject*)value
{
	LinkedList *p;
	for(p = list; p != nil; p = [p next]) {
		if([[p value] isEqual:value]) return YES;
	}
	
	return NO;
}

-(NSNumber*)cardinality
{
	unsigned long long count;
	LinkedList *p;
	for(p = list, count = 0; p != nil; p = [p next], ++count);
	return [NSNumber numberWithUnsignedLongLong:count];
}

-(BOOL) isSubSetOf:(Set*)set
{
	LinkedList *p;
	for(p = list; p != nil; p = [p next]) {
		if(![set contains:[p value]]) return NO;
	}
	return YES;
}

-(BOOL) isEqual:(id)other
{
	if(other == self)
		return YES;
	
	if(!other || ![other isKindOfClass:[self class]])
		return NO;
	
	return [self isSubSetOf:other] && [other isSubSetOf:self];
}

@end
