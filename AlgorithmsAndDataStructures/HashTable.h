#import "LinkedList.h"
#import <Foundation/NSObject.h>
#import <sys/types.h>

@interface HashTable : NSObject
{
	size_t tableSize;
	LinkedList* *table;
}
-(id)initFromSize:(size_t)size;
-(id <NSObject>)search:(id <NSObject>)key;
-(void)insertItem:(id <NSObject>)key value:(id <NSObject>)value;
-(void)deleteItem:(id <NSObject>)key;
@end
