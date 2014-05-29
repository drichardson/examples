#include <functional>
#include <future>
#include <stdio.h>
#include <unistd.h>
#include <sched.h>

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

std::shared_ptr<std::function<void()>> global_f;

void doGlobalF()
{
    (*global_f.get())();
}



int main(int argc, char** argv)
{
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

    std::array<std::shared_ptr<std::function<void()>>, 198> fs;
    for(std::size_t i = 0; i < fs.size(); ++i) {
        fs[i].reset(new std::function<void()>(mything(i)));
    }

    global_f.reset(new std::function<void()>(f));
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
        std::shared_ptr<std::function<void()>> f_ref = fs[i%fs.size()];
        uint64_t before = start_rdtsc();
        // global_f = f; // -O3 ~220, not printing before and after
        global_f = f_ref;
        uint64_t after = end_rdtsc();
        //printf("count: %lu\n", after - before);
        sum += (after - before);
    }

    printf("Average is %f\n", static_cast<double>(sum)/static_cast<double>(todo));

    doGlobalF();

    return 0;
}


