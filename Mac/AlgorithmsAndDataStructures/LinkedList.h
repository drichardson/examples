//
//  LinkedList.h
//  AlgorithmsAndDataStructures
//
//  Created by Douglas Richardson on 3/17/07.
//  Copyright 2007 __MyCompanyName__. All rights reserved.
//

#import <Foundation/NSObject.h>

@interface LinkedList : NSObject <NSCopying> {
	LinkedList *mNext;
	id mValue;
}

-(id) initWithNext:(LinkedList*)newNext value:(id)value;
-(void) setNext:(LinkedList*)newNext;
-(LinkedList*) next;

-(void) setValue:(id)newValue;
-(id) value;
@end
