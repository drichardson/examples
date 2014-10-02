#include <stdio.h>
#include <pthread.h>
#include <sys/types.h>
#include <unistd.h>

static _Thread_local int threadlocal_variable;
static int global_variable;

static void* thread_routine(void* arg) {
    for(int i = 0; i < 4; ++i) {
        printf("thread %zd: thread local value: %d, global value: %d\n",
                (ssize_t)arg,
                ++threadlocal_variable,
                __sync_fetch_and_add(&global_variable, 1));
        sleep(1);
    }
    return NULL;
}


int main(int argc, char const* const* argv) {

    pthread_t t1, t2;

    puts("Starting threads");

    pthread_create(&t1, NULL, thread_routine, (void*)1);
    pthread_create(&t2, NULL, thread_routine, (void*)2);

    pthread_join(t1, NULL);
    pthread_join(t2, NULL);

    puts("Done");

    return 0;
}

