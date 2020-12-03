#import <Foundation/Foundation.h>
#include <stdlib.h>

@interface Person : NSObject
{
	NSString *name;
	NSNumber *age;
}

@property(retain, readwrite) NSString* name;
@property(retain, readwrite) NSNumber* age;
@end

@implementation Person

-(id)initWithName:(NSString*)newName withAge:(NSNumber*)newAge
{
	if([super init])
	{
		name = newName;
		age = newAge;
	}
	return self;
}

@synthesize name;
@synthesize age;
@end


@interface Class1 : NSObject
{
	NSNumber *readOnlyNumber;
	NSNumber *readWriteNumber;
	NSNumber *randomValue;
	Person *person;
}

@property(retain, readonly) NSNumber* readOnlyNumber;
@property(retain, readwrite) NSNumber* readWriteNumber;
@property(retain, readonly) NSNumber* randomValue;
@property(retain, readonly) Person* person;
@end

@implementation Class1 : NSObject

-(id) init
{
	if([super init])
	{
		readOnlyNumber = [NSNumber numberWithInt:12345];
		person = [[[Person alloc] initWithName:@"Douglas Richardson" withAge:[NSNumber numberWithInt:28]] autorelease];
		srandomdev();
	}
	
	return self;
}

@synthesize readOnlyNumber;
@synthesize readWriteNumber;
@synthesize person;
@dynamic randomValue;

-(NSNumber*)randomValue
{
	return [NSNumber numberWithLong:random()];
}

@end



static void property_test()
{
	Class1 *c1 = [[Class1 alloc] init];
	
	NSLog(@"c1.readOnlyNumber = %@, %d", c1.readOnlyNumber, [c1.readOnlyNumber intValue]);
	
	NSLog(@"c1.readWriteNumber = %@", c1.readWriteNumber);
	c1.readWriteNumber = [NSNumber numberWithFloat:1.523];
	NSLog(@"c1.readWriteNumber = %@", c1.readWriteNumber);
	
	NSLog(@"c1.person.age = %@, name = %@", c1.person.age, c1.person.name);
	
	NSLog(@"c1.randomValue = %@, %@, %@", c1.randomValue, c1.randomValue, c1.randomValue);
}

static void fast_enumeration_test()
{
	NSArray *array = [NSArray arrayWithObjects:@"My String Object", [NSNumber numberWithInt:412], [NSDate date], nil];
	for(id obj in array)
	{
		NSLog(@"Object is %@", obj);
	}
	
	NSLog(@"Dictionary Values:");
	NSDictionary *dict = [NSDictionary dictionaryWithObjectsAndKeys:@"Value 1", @"Key 1", @"Value 2", @"Key 2", @"Value 3", @"Key 3", nil];
	for(NSString *key in [dict.allKeys sortedArrayUsingSelector:@selector(localizedCaseInsensitiveCompare:)])
	{
		NSLog(@"\t%@=%@", key, [dict objectForKey:key]);
	}
	
}

int main () {
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];

	property_test();
	fast_enumeration_test();
		
    [pool drain];
    return 0;
}
