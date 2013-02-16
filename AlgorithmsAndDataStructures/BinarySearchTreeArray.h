#import <Foundation/NSObject.h>
#import "Comparable.h"

@interface BSTItem : NSObject
{
	id <OrderedComparable> key;
	id value;
}
-(id) initWithKey:(id <OrderedComparable, NSObject>)key value:(id)value;
-(id <OrderedComparable>)key;
-(id)value;
@end

@interface BinarySearchTreeArray : NSObject
{
	BSTItem* *mArray;
	unsigned mSize;
	unsigned mGrowBy;
}

-(id) initWithSize:(unsigned)size growBy:(unsigned)growBy;
-(void) insertItem:(id <OrderedComparable, NSObject>)key value:(id)value;
-(void) deleteItem:(id <OrderedComparable>)key;
-(id) search:(id <OrderedComparable>)key;
@end
