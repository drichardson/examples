#include <sys/syscall.h>
#include <sys/types.h>
#include <thread>
#include <stdio.h>
#include <unistd.h>
#include <sstream>

pid_t gettid()
{
    pid_t r = syscall(SYS_gettid);
    return r;
} 

void dump()
{
    std::ostringstream oss;
    oss << std::this_thread::get_id();
    printf("tid is %d, thread id is %s\n",
            gettid(), oss.str().c_str());
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

