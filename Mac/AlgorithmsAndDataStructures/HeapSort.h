/*
 *  HeapSort.h
 *  AlgorithmsAndDataStructures
 *
 *  Created by Douglas Richardson on 3/17/07.
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 */

#import <Foundation/Foundation.h>

void heapSort(id *array, unsigned length, int (*compare)(void*, void*));