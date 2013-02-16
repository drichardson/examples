#import <Foundation/Foundation.h>

typedef struct chainNode
{
	id object;
	NSString *key;
	struct chainNode *next;
} Chain;

@interface HashTable : NSObject
{
	Chain **mTable;
	unsigned mTableSize;
}
-(void)addObject:(id)object withKey:(NSString*)key;
-(id)objectForKey:(NSString*)key;
@end

@implementation HashTable

-(id)initWithCapacity:(unsigned)capacity
{
	if([super init]) {
		mTableSize = capacity;
		mTable = (Chain**)calloc(mTableSize, sizeof(Chain*));
	}
	return self;
}

-(void)dealloc
{
	unsigned i;
	for(i = 0; i < mTableSize; ++i) {
		Chain *p;
		for(p = mTable[i]; p;) {
			Chain *tmp = p;
			[tmp->key release];
			[tmp->object release];
			p = p->next;
			free(tmp);
		}			
	}
	free(mTable);
	[super dealloc];
}

-(unsigned)hashKey:(NSString*)key
{
	unsigned size = [key length];
	unsigned i;
	unsigned result = 0;
	for(i = 0; i < size; ++i) {
		unichar c = [key characterAtIndex:i];
		result = (result + c) % mTableSize;
	}
	return result;
}

-(void)addObject:(id)object withKey:(NSString*)key
{
	unsigned i = [self hashKey:key];
	Chain *p;
	for(p = mTable[i]; p; p = p->next) {
		if([p->key isEqual:key])
			break;
	}

	if(p) {
		if(p->object != object) {
			[p->object release];
			p->object = [object retain];
		}
	} else {
		Chain *chain = (Chain*)malloc(sizeof(Chain));
		chain->object = [object retain];
		chain->key = [key retain];
		chain->next = mTable[i];
		mTable[i] = chain;
	}
}

-(id)objectForKey:(NSString*)key
{
	unsigned i = [self hashKey:key];
	Chain *p;
	for(p = mTable[i]; p; p = p->next) {
		if([p->key isEqual:key])
			break;
	}
	
	return p == NULL ? NULL : p->object;
}

@end

int main (int argc, const char * argv[]) {
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];

	HashTable *ht = [[[HashTable alloc] initWithCapacity:100] autorelease];
	
	[ht addObject:@"Douglas" withKey:@"First Name"];
	[ht addObject:@"Richardson" withKey:@"Last Name"];
	[ht addObject:@"Male" withKey:@"Sex"];
	[ht addObject:@"1979" withKey:@"DOB"];
	[ht addObject:@"California" withKey:@"State of Residence"];
	
	NSLog(@"First Name: %@", [ht objectForKey:@"First Name"]);
	NSLog(@"Last Name: %@", [ht objectForKey:@"Last Name"]);
	NSLog(@"Sex: %@", [ht objectForKey:@"Sex"]);
	NSLog(@"DOB: %@", [ht objectForKey:@"DOB"]);
	NSLog(@"State of Residence: %@", [ht objectForKey:@"State of Residence"]);
	
    [pool release];
    return 0;
}
