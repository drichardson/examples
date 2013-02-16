//
//  Queue.m
//  AlgorithmsAndDataStructures
//
//  Created by Douglas Richardson on 3/19/07.
//  Copyright 2007 __MyCompanyName__. All rights reserved.
//

#import "Queue.h"


@implementation Queue

-(void)dealloc
{
	[mTail release];
	mHead = mTail = 0;
	[super dealloc];
}

-(void)enqueue:(id)object
{
	LinkedList *newHead = [[LinkedList alloc] initWithNext:nil value:object];
	
	if(mHead) {
		[mHead setNext:newHead];
		mHead = newHead;
		[newHead release]; // The previous head retains this item.
	} else {
		mHead = newHead;
		mTail = mHead;
	}
}

-(id)dequeue
{
	id object = nil;
	
	if(mTail) {
		object = [mTail value];
		LinkedList *oldTail = mTail;
		mTail = [mTail next];
		[mTail retain];
		[oldTail release];
		if(mTail == 0) mHead = 0;
	}
	
	return object;
}

@end

