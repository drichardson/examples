#include <unordered_map>
#include <thread>
#include <iostream>

std::unordered_map<int, int> g_map;

void writer()
{
    int i = 0;
    while(true) {
        g_map[i] = i;
        i++;
        if (i > 0 && i % 1000) std::cout << "writer " << i << std::endl;
    }
}

void reader()
{
    while(true) {
        g_map.find(0);
    }
}

int main()
{
    std::thread t1(writer);
    std::thread t2(reader);

    t1.join();
    t2.join();

    return 0;
}

