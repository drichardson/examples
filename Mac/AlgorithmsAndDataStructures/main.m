#import <stdio.h>
#import <stdlib.h>
#import <sys/time.h>
#import "HeapSort.h" 
#import "Set.h"
#import "HashTable.h"
#import "BinarySearchTreeArray.h"
#import "Queue.h"
#import "Graph.h"

static BOOL isArrayInAscendingOrder(NSNumber **array, size_t count)
{
	size_t i;
	NSNumber *last = count > 0 ? array[0] : nil;
	for(i = 1; i < count; ++i) {
		if([last isGreaterThan:array[i]]) return NO;
		last = array[i];
	}
	
	return YES;
}

static void printArray(NSNumber **a, unsigned l)
{
	int i;
	for(i = 0; i < l; ++i)
		printf("%2d: %12d\n", i, [a[i] longValue]);
	
	printf("Is Array In Sorted Order: %s\n",
		   isArrayInAscendingOrder(a,l) ? "YES" : "NO");
}

static int compareNSNumber(void* n1, void* n2)
{
	int i = 0;
	id n1Id = (id)n1;
	
	switch([n1Id compare:n2]) {
		case NSOrderedAscending:
			i = -1;
			break;
		case NSOrderedSame:
			i = 0;
			break;
		case NSOrderedDescending:
			i = 1;
			break;
	}
	
	return i;
}

static Set* initFromArray(id* array, size_t count)
{
	Set *set = [[Set alloc] init];
	printf("Building set from array"); fflush(stdout);
	int numberOfUpdateDots = 5;
	int updateDotsEveryXLoops = count / numberOfUpdateDots;
	int i;
	for(i = 0; i < count; ++i) {
		if(i % updateDotsEveryXLoops == 0) {
			printf(" %2.0f%%", (float)i * 100.0/(float)count);
			fflush(stdout);
		}
		[set add:array[i]];
	}
	puts(" 100%");
	return set;
}

static void doHashTableLookup(HashTable *ht, id <NSObject> key)
{
	id value = [ht search:key];
	printf("Search Results for '%s': Value is %p", [[key description] UTF8String], value);
	if(value) printf(": %s", [[value description] UTF8String]);
	putchar('\n');
}

static void doBSTLookup(BinarySearchTreeArray *bst, id <OrderedComparable, NSObject> key)
{
	id value = [bst search:key];
	printf("Search Results for '%s': Value is %p", [[key description] UTF8String], value);
	if(value) printf(": %s", [[value description] UTF8String]);
	putchar('\n');
}

@interface FriendVisitor : NSObject
{
}
-(void)visit:(Vertex*)vertex;
@end

static const char* colorToString(enum VertexColor color) {
	switch(color) {
		case VCWhite:
			return "White";
		case VCBlack:
			return "Black";
		case VCGray:
			return "Gray";
	}
	
	return "INVALID COLOR VALUE";
}

@implementation FriendVisitor
-(void)visit:(Vertex*)v;
{
	printf("key: %s, value: %s, distance: %d, color: %s\n",
		   [[[v key] description] UTF8String],
		   [[[v value] description] UTF8String],
		   [[v searchData] depth], colorToString([[v searchData] color]));
}
@end

#define SORTING_ON
#define HASHTABLE_ON
#define BINARYSEARCHTREEARRAY_ON
#define QUEUE_ON
#define GRAPH_ON

int main(int argc, char** argv)
{
	NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];

#ifdef SORTING_ON
	const size_t count = 2000;
	NSNumber* *array = (NSNumber**)malloc(count * sizeof(NSNumber*));
	long long i;
	unsigned numberOfUpdates = 10;
	unsigned updateEveryXCounts = count / numberOfUpdates;
	
	srandom(time(NULL));
	
	printf("Building random value array"); fflush(stdout);
	for(i = 0; i < count; ++i) {
		long r = random();
		array[i] = [NSNumber numberWithLongLong:r];
		if(i % updateEveryXCounts == 0) { putchar('.'); fflush(stdout); }
	}
	putchar('\n');
	
	Set *preSortSet = initFromArray(array, count);
	printf("SORTING ------------------------------\n");
	
	//puts("Before Sort");
	//printArray(array, count);
	struct timeval tv1, tv2;
	gettimeofday(&tv1, 0);
	quickSort(array, count, compareNSNumber);
	//heapSort(array, count, compareNSNumber);
	//insertionSort(array, count, compareNSNumber);
	gettimeofday(&tv2, 0);
	double a1 = tv1.tv_sec + tv1.tv_usec / 1000000.0;
	double a2 = tv2.tv_sec + tv2.tv_usec / 1000000.0;
	printf("Start: %f, End: %f, Diff: %f\n", a1, a2, a2 - a1);
	printf("Array is in decending order? "); fflush(stdout);
	printf("%s\n", isArrayInAscendingOrder(array, count) ? "YES" : "NO");
	Set *postSortSet = initFromArray(array, count);
	//puts("After Sort");
	//printArray(array, count);
	printf("pre = post? %s\n", [preSortSet isEqual:postSortSet] ? "YES" : "NO");
	[preSortSet release];
	[postSortSet release];
	preSortSet = postSortSet = 0;
	free(array);
	array = 0;
#endif

#ifdef HASHTABLE_ON
	printf("HASHING ------------------------------\n");
	HashTable *ht = [[HashTable alloc] initFromSize:7];
	[ht insertItem:@"Doug" value:[NSNumber numberWithInt:1979]];
	[ht insertItem:@"Rebecca" value:[NSNumber numberWithInt:1979]];
	[ht insertItem:@"Katelyn" value:[NSNumber numberWithInt:2003]];
	[ht insertItem:@"Mae" value:[NSNumber numberWithInt:2005]];
	[ht insertItem:@"Mom" value:[NSNumber numberWithInt:1954]];
	[ht insertItem:@"Dad" value:[NSNumber numberWithInt:1957]];
	[ht insertItem:@"Grandpa" value:[NSNumber numberWithInt:1931]];
	[ht insertItem:@"Grandma" value:[NSNumber numberWithInt:1933]];
	
	doHashTableLookup(ht, @"lalahead");
	doHashTableLookup(ht, @"Doug");
	doHashTableLookup(ht, @"Rebecca");
	doHashTableLookup(ht, @"Grandpa");
	doHashTableLookup(ht, @"Mae");
	doHashTableLookup(ht, @"Mom");
	[ht release];
#endif
	
#ifdef BINARYSEARCHTREEARRAY_ON
	BinarySearchTreeArray *bst = [[BinarySearchTreeArray alloc] initWithSize:1 growBy:1];
	[bst insertItem:@"Long Beach" value:@"California"];
	[bst insertItem:@"Irvine" value:@"California"];
	[bst insertItem:@"Phoenix" value:@"Arizona"];
	[bst insertItem:@"Chicago" value:@"Illinois"];
	doBSTLookup(bst, @"Houston");
	doBSTLookup(bst, @"Long Beach");
	doBSTLookup(bst, @"Irvine");
	doBSTLookup(bst, @"Phoenix");
	doBSTLookup(bst, @"Chicago");
	[bst deleteItem:@"Irvine"];
	doBSTLookup(bst, @"Irvine");
#endif
	
#ifdef QUEUE_ON
	printf("QUEUES --------------------------------------\n");
	Queue *q = [[Queue alloc] init];
	[q enqueue:@"First Queue Item"];
	[q enqueue:@"Second Queue Item"];
	[q enqueue:@"Third Queue Item"];
	[q enqueue:@"Fouth Queue Item"];
	id queueObj;
	while(queueObj = [q dequeue]) {
		printf("Queue Object: %s\n", [[queueObj description] UTF8String]);
	}
	
	[q release];
#endif
	
#ifdef GRAPH_ON
	NSAutoreleasePool *graphPool = [[NSAutoreleasePool alloc] init];
	printf("GRAPHS --------------------------------------\n");
	// Relationship graph - models who knows who.
	Graph *graph = [[Graph alloc] init];
	Vertex *mrPopular = [graph createVertex:@"mrpop" value:@"Mr. Popular"];
	Vertex *doug = [graph createVertex:@"doug" value:@"Douglas Richardson"];
	Vertex *rebecca = [graph createVertex:@"becca" value:@"Rebecca Richardson"];
	Vertex *george = [graph createVertex:@"jorge" value:@"George Handle"];
	Vertex *ricky = [graph createVertex:@"musicMan" value:@"Ricky Martin"];
	
	[mrPopular addEdgeTo:doug];
	
	[doug addEdgeTo:rebecca];
	[doug addEdgeTo:mrPopular];
	
	[rebecca addEdgeTo:doug];
	[rebecca addEdgeTo:mrPopular];
	
	[george addEdgeTo:mrPopular];
	[george addEdgeTo:ricky];
	
	[ricky addEdgeTo:mrPopular];
	[ricky addEdgeTo:rebecca];
	

	FriendVisitor *visitor = [[FriendVisitor alloc] init];
	puts("mrPopular's connections");
	[graph breadthFirstSearch:mrPopular visitor:nil method:@selector(visit:)];

	puts("george's connections");
	[graph breadthFirstSearch:george visitor:visitor method:@selector(visit:)];
	[graph breadthFirstSearch:george visitor:visitor method:@selector(visit:)];
	
	[graph release];
	[graphPool release];
	
#endif
	
	[pool release];
	
	return 0;
}
