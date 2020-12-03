//
//  heap.c
//  EmbeddedHeapExperiment
//
//  Created by Doug Richardson on 1/18/13.
//  Copyright (c) 2013 Doug Richardson. All rights reserved.
//

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#define SMALLEST_FREE_CHUNK (sizeof(struct FreeChunk) + 16)
#define ALIGNMENT 4

struct FreeChunk
{
    size_t size; // Size of data that can be accomodated if this FreeChunk is replaced with a UsedChunk
	struct FreeChunk* next;
	struct FreeChunk* previous;
};

struct UsedChunk
{
    size_t size; // Size of data
	unsigned char data[0];
};

static void* sHeapStart;
static void* sHeapEnd;
static struct FreeChunk* sFreeChunks;

void heap_init(void* start, void* end)
{
	sHeapStart = start;
	sHeapEnd = end;
    
	sFreeChunks = (struct FreeChunk*)sHeapStart;
	sFreeChunks->previous = NULL;
	sFreeChunks->next = NULL;
	sFreeChunks->size = end - start - sizeof(struct UsedChunk);
}

void* heap_allocate(size_t bytes)
{
    bytes = (bytes + (ALIGNMENT - 1)) & ~(ALIGNMENT - 1); // allocate multiples of ALIGNMENT FreeChunk/UsedChunk pointers following the data section are aligned properly.
    
    void* result = NULL;
    
    for(struct FreeChunk* chunk = sFreeChunks; chunk; chunk = chunk->next)
    {
        if ( chunk->size >= bytes )
        {
            struct FreeChunk* previous = chunk->previous;
            struct FreeChunk* next = chunk->next;
            
            struct UsedChunk* used = (struct UsedChunk*)chunk;
            result = used->data;
            
            size_t remaining = chunk->size - bytes;
            if ( remaining > SMALLEST_FREE_CHUNK )
            {
                // Split the FreeChunk into the UsedChunk followed by a new FreeChunk.
                used->size = bytes;
                struct FreeChunk* newFreeChunk = (struct FreeChunk*)(used->data + bytes);
                assert((((size_t)newFreeChunk) & (ALIGNMENT - 1)) == 0); // Make sure aligned properly.
                
                newFreeChunk->size = remaining;
                newFreeChunk->previous = previous;
                newFreeChunk->next = next;
                
                if ( previous )
                    previous->next = newFreeChunk;
                if ( next )
                    next->previous = newFreeChunk;
                if ( chunk == sFreeChunks )
                    sFreeChunks = newFreeChunk;
            }
            else
            {
                // Not enough space to split, so give the entire area to the UsedChunk and remove it from
                // the free list.
                if ( previous )
                    previous->next = next;
                if ( next )
                    next->previous = previous;
                if ( chunk == sFreeChunks )
                    sFreeChunks = sFreeChunks->next;
            }
            
            assert(result);
            break;
        }
    }
    
    return result;
}

void heap_free(void* p)
{
    struct UsedChunk* used = (struct UsedChunk*)(((unsigned char*)p) - sizeof(size_t));
    struct FreeChunk* newFree = (struct FreeChunk*)used;
    //printf("Freeing used chunk %p for address %p of size %zu\n", used, p, used->size);
    
    if ( sFreeChunks == NULL )
    {
        // No free chunks. Now there is!
        newFree->next = NULL;
        newFree->previous = NULL;
        sFreeChunks = newFree;
    }
    else if ( newFree < sFreeChunks )
    {
        newFree->next = sFreeChunks;
        newFree->previous = NULL;
        sFreeChunks->previous = newFree;
        sFreeChunks = newFree;
    }
    else
    {
        // Scan the free list to find the right place to insert.
        struct FreeChunk* chunk;
        for(chunk = sFreeChunks; chunk; chunk = chunk->next)
        {
            if ( newFree > chunk && (chunk->next == NULL || newFree < chunk->next) )
            {
                // w00t, found a place to insert.
                newFree->next = chunk->next;
                newFree->previous = chunk;
                chunk->next = newFree;
                
                if ( newFree->next )
                    newFree->next->previous = newFree;
                
                break;
                
                
                // TODO: coalesce?
            }
        }
        
        assert(chunk); // One of the chunks had to match or the heap is corrupted.
    }
}

void heap_print(const char* label)
{
    printf("heap %s\n", label);
    
    printf("Free list: \n");
    for(struct FreeChunk* chunk = sFreeChunks; chunk; chunk = chunk->next )
    {
        printf("\tFree Chunk %p\n\t\tsize: %zu\n", chunk, chunk->size);
    }
}
