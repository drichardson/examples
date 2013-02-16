#import "QuickSort.h"
#import <Foundation/Foundation.h>

static unsigned
partition(id *array, unsigned p, unsigned r, int (*compare)(void*, void*))
{
	id x = array[p];
	int i = p - 1;
	int j = r + 1;
	
	while(YES) {
		do {
			--j;
		} while(compare(array[j], x) > 0);
		do {
			++i;
		} while(compare(array[i], x) < 0);
		
		if(i < j) {
			id tmp = array[i];
			array[i] = array[j];
			array[j] = tmp;
		} else {
			return j;
		}
	}
	
	return 0;
}

static void
internalQuickSort(id *array, int p, int r, int (*compare)(void*, void*))
{
	if(p < r) {
		unsigned q = partition(array, p, r, compare);
		internalQuickSort(array, p, q, compare);
		internalQuickSort(array, q + 1, r, compare);
	}
}

void quickSort(id *array, unsigned length, int (*compare)(void*, void*))
{
	int l = length;
	internalQuickSort(array, 0, l - 1, compare);
}
