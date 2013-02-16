#import "InsertionSort.h"

void insertionSort(id *array, unsigned length, int (*compare)(void*,void*))
{
	int j;
	for(j = 1; j < length; ++j) {
		id key = array[j];
		// Insert array[j] into the sorted sequence array[0..j-1]
		int i;
		for(i = j - 1; i >= 0 && compare(array[i], key) > 0; --i) {
			array[i + 1] = array[i];
		}
		array[i+1] = key;
	}
}