//
//  LinkedList.m
//  AlgorithmsAndDataStructures
//
//  Created by Douglas Richardson on 3/17/07.
//  Copyright 2007 __MyCompanyName__. All rights reserved.
//

#import "LinkedList.h"


@implementation LinkedList

-(id) initWithNext:(LinkedList*)newNext value:(id)newValue
{
	self = [super init];
	if(self) {
		[self setNext:newNext];
		[self setValue:newValue];
	}
	return self;
}

-(void) dealloc
{
	[self setValue:nil];
	[self setNext:nil];
	[super dealloc];
}

-(void) setNext:(LinkedList*)newNext
{
	if(mNext != newNext) {
		[mNext release];
		mNext = [newNext retain];
	}
}

-(LinkedList*) next { return [[mNext retain] autorelease]; }

-(void) setValue:(id)newValue
{
	if(mValue != newValue) {
		[mValue release];
		mValue = [newValue copy];
	}
}

-(id) value { return [[mValue retain] autorelease]; }

-(id)copyWithZone:(NSZone*)zone
{
	LinkedList *copy = [[[self class] alloc] initWithNext:nil value:[self value]];
	LinkedList *p = copy;
	
	LinkedList *pMe;
	for(pMe = [self next]; pMe; pMe = [pMe next]) {
		[p setNext:[[[self class] alloc] initWithNext:nil value:[[pMe value] copy]]];
		p = [p next];
	}
		
	return copy;
}

@end
