#include <array>
#include <functional>
#include <future>
#include <sched.h>
#include <stdio.h>
#include <unistd.h>

#define USE_SHAREDPTR 0
#define USE_SET_AFFINITY 0

inline uint64_t start_rdtsc(void)
{
    uint32_t high, low;
    __asm__ __volatile__(
        "cpuid\n\t"
        "rdtsc\n\t"
        "mov %%edx, %0\n\t"
        "mov %%eax, %1\n\t"
        : "=r"(high), "=r"(low)::"%rax", "%rbx", "%rcx", "%rdx");
    return (uint64_t)high << 32 | low;
}

inline uint64_t end_rdtsc(void)
{
    uint32_t high, low;
    __asm__ __volatile__(
        "rdtscp\n\t"
        "mov %%edx, %0\n\t"
        "mov %%eax, %1\n\t"
        "cpuid\n\t"
        : "=r"(high), "=r"(low)::"%rax", "%rbx", "%rcx", "%rdx");
    return (uint64_t)high << 32 | low;
}

void foo() {
    puts("hi");
}

void doSomething(std::function<void()> f)
{
    f();
}

std::function<void()> mything(int x)
{
    return [x] { printf("x is %d\n", x); };
}

#if USE_SHAREDPTR
std::shared_ptr<std::function<void()>> global_f;
#else
std::function<void()> global_f;
#endif

void doGlobalF()
{
#if USE_SHAREDPTR
    (*global_f.get())();
#else
    global_f();
#endif
}



int main(int argc, char* argv[])
{
#if USE_SET_AFFINITY
    printf("setting affinity\n");
    cpu_set_t mask;
    CPU_ZERO(&mask);
    CPU_SET(0, &mask);
    int rc = sched_setaffinity(getpid(), sizeof(mask), &mask);
    if (rc == -1) {
        printf("Couldn't set affinity: %d\n", errno);
        exit(1);
    }
#endif

    std::function<void()> f;
#if 0
    doSomething(foo);
    f = []{ puts("lambda"); }; 
    doSomething(f);
    f = mything(123);
    doSomething(f);
    for(int i = 0; i < 5; ++i) {
        uint64_t before = start_rdtsc();
        global_f = f; // -O0 ~600, -O3 ~300
        // i += 2; // -O0 ~50, -O3 32
        uint64_t after = end_rdtsc();
        printf("count: %lu\n", after - before);
    }

    doGlobalF();
#endif

    f = mything(argc);

#if USE_SHAREDPTR
    std::array<std::shared_ptr<std::function<void()>>, 198> fs;
#else
    std::array<std::function<void()>, 198> fs;
#endif

    for(std::size_t i = 0; i < fs.size(); ++i) {
#if USE_SHAREDPTR
        fs[i].reset(new std::function<void()>(mything(i)));
#else
        fs[i] = mything(i);
#endif
    }

#if USE_SHAREDPTR
    global_f.reset(new std::function<void()>(f));
#else
    global_f = f;
#endif
    std::thread t(doGlobalF);
    t.join();
    fprintf(stdout, "Hi\n");
    fflush(stdout);

    sleep(2);

    printf("size of f: %zu\n", sizeof(f));
    
    uint64_t sum = 0;
    int todo = 100000 + argc;

    for(int i = 0; i < todo; ++i) {
        //printf("before\n");    

#if USE_SHAREDPTR
        std::shared_ptr<std::function<void()>> f_ref = fs[i%fs.size()];
        uint64_t before = start_rdtsc();
        // global_f = f; // -O3 ~220, not printing before and after
        global_f = f_ref;
        uint64_t after = end_rdtsc();
#else
        std::function<void()> f = fs[i%fs.size()];
        uint64_t before = start_rdtsc();
        global_f = f; // -O3 ~220, not printing before and after
        uint64_t after = end_rdtsc();
#endif
        //printf("count: %lu\n", after - before);
        sum += (after - before);
    }

    printf("Average is %f\n", static_cast<double>(sum)/static_cast<double>(todo));

    doGlobalF();
}


