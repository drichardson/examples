//
//  Queue.h
//  AlgorithmsAndDataStructures
//
//  Created by Douglas Richardson on 3/19/07.
//  Copyright 2007 __MyCompanyName__. All rights reserved.
//

#import <Foundation/NSObject.h>
#import "LinkedList.h"

@interface Queue : NSObject {
	LinkedList *mHead, *mTail;
}
-(void)enqueue:(id)object;
-(id)dequeue;
@end
