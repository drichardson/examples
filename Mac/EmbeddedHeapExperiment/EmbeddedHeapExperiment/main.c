//
//  main.c
//  EmbeddedHeapExperiment
//
//  Created by Doug Richardson on 1/18/13.
//  Copyright (c) 2013 Doug Richardson. All rights reserved.
//

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "heap.h"

int main(int argc, const char * argv[])
{
//    //unsigned char x;
//    //unsigned char* p = &x;
//    size_t pl = 0;
//    for(int i = 0; i < 20; ++i)
//    {
//        printf("%zX is %zX\n", pl, (pl+0x7) & ~0x7);
//        pl += 1;
//    }
//    
//    exit(1);
    
    unsigned char* start = malloc(8000);
    unsigned char* end = start + 8000;
    heap_init(start, end);
    heap_print("init");
    
    char* p1 = heap_allocate(1000);
    heap_print("p1");
    
    char* p2 = heap_allocate(2000);
    heap_print("p2");
    
    char* p3 = heap_allocate(8);
    heap_print("p3");
    
    char* p4 = heap_allocate(5);
    heap_print("p4");
    
    heap_free(p1);
    heap_print("p1 free");
    
    heap_free(p3);
    heap_print("p3 free");
    
    heap_free(p2);
    heap_print("p2 free");
    
    heap_free(p4);
    heap_print("p4 free");
    
    
    return 0;
}

