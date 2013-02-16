//
//  heap.h
//  EmbeddedHeapExperiment
//
//  Created by Doug Richardson on 1/18/13.
//  Copyright (c) 2013 Doug Richardson. All rights reserved.
//

#ifndef EmbeddedHeapExperiment_heap_h
#define EmbeddedHeapExperiment_heap_h

void heap_init(void* start, void* end);
void* heap_allocate(size_t bytes);
void heap_free(void*);
void heap_print(const char* label);

#endif
