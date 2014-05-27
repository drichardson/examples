#include <sys/syscall.h>
#include <sys/types.h>
#include <thread>
#include <stdio.h>
#include <unistd.h>

pid_t gettid()
{
    pid_t r = syscall(SYS_gettid);
    return r;
} 

void dump()
{
    printf("tid is %d, thread id is %u\n",
            gettid(), std::this_thread::get_id());
}

int main()
{
    printf("mypid is %d\n", getpid());
    dump();
    std::thread t1([]{ dump(); });
    std::thread t2([]{ dump(); });
    t1.join();
    t2.join();
    dump();
    return 0;
}

