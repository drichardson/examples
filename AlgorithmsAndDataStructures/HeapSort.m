#import "HeapSort.h"

typedef int (*CompareFunction)(void*,void*);

static unsigned parent(unsigned i) { return (i-1)/2; }
static unsigned left(unsigned i) { return 2*i + 1; }
static unsigned right(unsigned i) { return 2*i + 2; }

@interface Heap : NSObject
{
	size_t length;
	size_t heapSize;
	id *array;
	CompareFunction compare;
}
-(id)initWithArray:(id*) a length:(unsigned)len compare:(CompareFunction)comp;
-(void)heapify:(unsigned)i;
-(void)build;
-(void)sort;
@end

void heapSort(id *array, unsigned length, int (*compare)(void*,void*))
{
	Heap *heap = [[Heap alloc] initWithArray:array length:length compare:compare];
	[heap sort];
	[heap release];
}

@implementation Heap

-(id)initWithArray:(id*) a length:(unsigned)len compare:(CompareFunction)comp
{
	self = [super init];
	if(self) {
		array = a;
		length = len;
		compare = comp;
	}
	return self;
}

-(void)heapify:(unsigned)i
{
	unsigned l = left(i);
	unsigned r = right(i);
	unsigned largest;

	if(l < heapSize && compare(array[l], array[i]) > 0)
		largest = l;
	else
		largest = i;
	
	if(r < heapSize && compare(array[r], array[largest]) > 0)
		largest = r;
	
	if(largest != i) {
		id tmp = array[i];
		array[i] = array[largest];
		array[largest] = tmp;
		[self heapify:largest];
	}
}

-(void)build
{
	int i;
	heapSize = length;
	for(i = length / 2 - 1; i >= 0; --i)
		[self heapify:i];
}

-(void)sort
{
	[self build];
	
	unsigned i;
	heapSize = length;
	for(i = length - 1; i >= 1; --i) {
		//printf("\nBefore: Array[0] = %d, Array[%d] = %d\n", [array[0] intValue], i, [array[i] intValue]);
		id tmp = array[0];
		array[0] = array[i];
		array[i] = tmp;
		//printf("After Swap: Array[0] = %d, Array[%d] = %d\n", [array[0] intValue], i, [array[i] intValue]);
		--heapSize;
		[self heapify:0];
		//printf("After Heapify: Array[0] = %d, Array[%d] = %d\n", [array[0] intValue], i, [array[i] intValue]);
	}
}

@end
