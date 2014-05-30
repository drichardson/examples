#include <iostream>
#include <condition_variable>
#include <thread>
#include <mutex>
#include <chrono>

std::mutex g_mutex;
std::condition_variable g_cond;
int val = 0;

void waiter()
{
    std::unique_lock<std::mutex> lock(g_mutex);

    auto now = std::chrono::steady_clock::now();
    auto end_time = now + std::chrono::seconds(2);

    std::this_thread::sleep_for(std::chrono::milliseconds(200));

    for(;;) {
        std::cv_status status = g_cond.wait_until(lock, end_time);
        if (status == std::cv_status::timeout) {
            std::cout << "waiter: timeout" << std::endl;
            return;
        } else {
            std::cout << "waiter: signalled val=" << val << std::endl;
        }
    }
}

void signaller()
{
    // see if this notify is lost. waiter should be sleeping at this point.
    // val == 0
    g_cond.notify_one();

    std::this_thread::sleep_for(std::chrono::milliseconds(500));
    g_mutex.lock();
    val = 1;
    g_mutex.unlock();
    g_cond.notify_one();

    // demonstrate 2 things:
    // 1. g_cond doesn't wake up while I have mutex
    // 2. because we take the lock immediately after notifying, it's possible
    // this thread acquires g_mutex lock before the waiter thread, thus
    // the waiter thread may never see val == 1.
    g_mutex.lock();
    std::this_thread::sleep_for(std::chrono::milliseconds(500));
    val = 2;
    g_mutex.unlock();

    std::this_thread::sleep_for(std::chrono::milliseconds(500));
    g_cond.notify_one();

}

int main()
{
    std::thread waiter_t(waiter);
    std::thread signaller_t(signaller);

    waiter_t.join();
    signaller_t.join();

    return 0;
}

