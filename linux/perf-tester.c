// perf-tester.c is just a playground for messing with Linux's perf
// tool. This code on it's own isn't meant to demonstrate anything in
// particular.
#include <assert.h>
#include <search.h>
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>

typedef struct {
    unsigned int value;
    unsigned int counter;
} uint_count;

int uint_count_compare(const void* p1, const void* p2) {
    uint_count const* uc1 = (uint_count const*)p1;
    uint_count const* uc2 = (uint_count const*)p2;
    return uc2->value - uc1->value;
}

static unsigned int g_total_counted;

static void uint_count_print_action(const void *nodep, const VISIT which, const int depth) {
    //printf("nodep: %p, which: %d, depth: %d\n", nodep, which, depth);
    uint_count const * const * countp = nodep;
    uint_count const* count = *countp;

#if 0
    switch(which) {
    case postorder: // postorder is more commonly known as inorder, but this API is weird
    case leaf:
        g_total_counted += count->counter;
        break;
    case preorder:
    case endorder: // more commonly called postorder, but postorder in this API means inorder.
        break;
    }
#else
    if (which == leaf || which == postorder) {
        g_total_counted += count->counter;
    }
#endif
}

static void uint_count_free(void* nodep) {
    free(nodep);
}

//#error do your own allocator that 
//static void init_zone() {
//}



static uint_count* uint_count_malloc(void) {
    uint_count* r = (uint_count*)malloc(sizeof(uint_count));
    r->counter = 1;
    return r;
}

static void sig_handler(int signum) {
    printf("Got signal %d. g_total_counted is %u\n", signum, g_total_counted);
    exit(0);
}

int main(int argc, char* const* argv) {

    unsigned int data_set[] = {
        5, 2, 9, 8, 3, 2, 2, 1, 7, 9, 1, 0,
        8, 7, 2, 3, 4, 5, 4, 1, 7, 8, 4, 0
    };

    signal(SIGTERM, sig_handler);

    while(1) {
        // count frequency of values in data_set
        void* root = NULL;
        uint_count* new_count = uint_count_malloc();
        for(int i = 0; i < sizeof(data_set)/sizeof(data_set[0]); ++i) {
            new_count->value = data_set[i];
            uint_count** count = tsearch(new_count, &root, uint_count_compare);
            if (new_count == *count) {
                new_count = uint_count_malloc();
            } else {
                ++(*count)->counter;
            }
        }
        uint_count_free(new_count);

        twalk(root, uint_count_print_action);
        tdestroy(root, uint_count_free);
    }

    return 0;
}

