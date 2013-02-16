#import "BinarySearchTreeArray.h"
#import <stdlib.h>
#import <string.h>
#import <Foundation/NSException.h>

@implementation BSTItem : NSObject
-(id) initWithKey:(id <OrderedComparable, NSObject>)k value:(id)v
{
	self = [super init];
	if(self) {
		key = [k retain];
		value = [v retain];
	}
	return self;
}

-(id <OrderedComparable>)key { return key; }
-(id)value { return value; }
@end

static unsigned rightChild(unsigned i) { return (2*i)+2; }
static unsigned leftChild(unsigned i) { return (2*i)+1; }

@implementation BinarySearchTreeArray
-(id) initWithSize:(unsigned)size growBy:(unsigned)growBy
{
	self = [super init];
	if(self) {
		mSize = size;
		mGrowBy = growBy;
		mArray = (BSTItem**)calloc(mSize, sizeof(BSTItem*));
	}
	return self;
}

-(void)dealloc
{
	unsigned i;
	for(i = 0; i < mSize; ++i) {
		if(mArray[i]) {
			[mArray[i] release];
			mArray[i] = 0;
		}
	}
	free(mArray);
	[super dealloc];
}

-(int)findIndex:(id <OrderedComparable>)key
{
	unsigned i = 0;
	while(i < mSize) {
		BSTItem *item = mArray[i];
		if(item == NULL) return i;
		int compare = [key compare:[item key]];
		if(compare == 0)
			return i;
		else if(compare < 0)
			i = leftChild(i);
		else
			i = rightChild(i);
	}
	return -1;
}

-(void) insertItem:(id <OrderedComparable, NSObject>)key value:(id)value
{
	int i = [self findIndex:key];
	if(i < 0) {
		unsigned newSize = mSize + mGrowBy;
		
		BSTItem* *tmp = (BSTItem**)realloc(mArray, newSize * sizeof(BSTItem*));
		if(tmp) {
			memset(&tmp[mSize], 0, mGrowBy * sizeof(BSTItem*));
			mArray = tmp;
			mSize = newSize;
		} else {
			@throw [NSException exceptionWithName:@"Memory Allocation Failure"
										   reason:@"Couldn't grow internal array for binary search tree."
										 userInfo:nil];
		}
		
		i = [self findIndex:key];
	}
	if(mArray[i]) [mArray[i] release];
	mArray[i] = [[BSTItem alloc] initWithKey:key value:value];
}

-(void) deleteItem:(id <OrderedComparable>)key
{
	unsigned i = [self findIndex:key];
	if(i >= 0 && mArray[i]) {
		[mArray[i] release];
		mArray[i] = 0;
	}
}

-(id) search:(id <OrderedComparable>)key
{	
	unsigned i = [self findIndex:key];
	if(i >= 0 && mArray[i]) {
		return [mArray[i] value];
	}
	return nil;
}

@end
