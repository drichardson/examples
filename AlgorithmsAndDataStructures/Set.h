//
//  Set.h
//  AlgorithmsAndDataStructures
//
//  Created by Douglas Richardson on 3/17/07.
//  Copyright 2007 __MyCompanyName__. All rights reserved.
//

#import <Foundation/NSObject.h>
#import <Foundation/NSValue.h>
#import "LinkedList.h"
#import "Comparable.h"

@interface Set : NSObject
{
	LinkedList *list;
}

-(void)add:(NSObject*)value;
-(void)remove:(NSObject*)value;
-(BOOL)contains:(NSObject*)value;
-(NSNumber*)cardinality;
-(BOOL) isSubSetOf:(Set*)set;
-(BOOL) isEqual:(id)other;
@end
