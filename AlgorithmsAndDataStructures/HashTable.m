#import "HashTable.h"
#import <stdlib.h>

// Pick a prime number not too close to a power of 2.
static const size_t DEFAULT_HASHTABLE_SIZE = 307;

@interface LinkValue : NSObject <NSCopying>
{
	id <NSObject> key;
	id <NSObject> value;
}
-(id) initWithKey:(id)newKey value:(id)newValue;
-(id <NSObject>) key;
-(id <NSObject>) value;
@end

@implementation LinkValue
-(id) initWithKey:(id)newKey value:(id)newValue
{
	self = [super init];
	if(self) {
		key = [newKey retain];
		value = [newValue retain];
	}
	return self;
}

-(id <NSObject>) key { return key; }
-(id <NSObject>) value { return value; }

-(id)copyWithZone:(NSZone*)zone
{
	LinkValue *copy = [[[self class] allocWithZone:zone] initWithKey:key value:value];
	return copy;
}

-(void)dealloc
{
	[key release];
	[value release];
	[super dealloc];
}
@end

@implementation HashTable

static unsigned mapHash(unsigned hashValue, size_t tableSize)
{
	return hashValue % tableSize;
}

-(id)initFromSize:(size_t)size
{
	self = [super init];
	if(self) {
		tableSize = size;
		table = (LinkedList**)calloc(tableSize, sizeof(LinkedList*));
	}
	return self;
}

-(id) init
{
	return [self initFromSize:DEFAULT_HASHTABLE_SIZE];
}

-(void) dealloc
{
	if(table) {
		size_t i;
		for(i = 0; i < tableSize; ++i) {
			if(table[i]) [table[i] release];
		}
		free(table);
		table = 0;
	}
	[super dealloc];
}

-(id <NSObject>)search:(id <NSObject>)key
{
	LinkedList *p;
	for(p = table[mapHash([key hash], tableSize)]; p != nil; p = [p next]) {
		LinkValue *lv = [p value];
		if([key isEqual:[lv key]]) return [lv value];
	}
	return nil;
}

-(void)insertItem:(id <NSObject>)key value:(id <NSObject>)value
{
	unsigned i = mapHash([key hash], tableSize);
	LinkValue *lv = [[LinkValue alloc] initWithKey:key value:value];
	LinkedList *l = [[LinkedList alloc] initWithNext:table[i] value:lv];
	table[i] = l;
	[lv release];
}

-(void)deleteItem:(id <NSObject>)key
{
	unsigned i = mapHash([key hash], tableSize);
	LinkedList *p = table[i];
	LinkedList *prev = nil;
	for(p = table[i]; p != nil; p = [p next]) {
		LinkValue *lv = [p value];
		if([key isEqual:[lv key]]) {
			LinkedList *next = [p next];
			[p release];
			if(prev) {
				[prev setNext:next];
			} else {
				table[i] = next;
			}
			break;
		}
		prev = p;
	}
}

@end
