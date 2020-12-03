#include "vector.hpp"
#include <iostream>

int main()
{
    containers::vector<unsigned> v(0,0);
#if 0
    // if you don't reserve, it will be a LOT slower.
    v.reserve(80000);
#endif

    for(size_t len = 1; len <= 80000; ++len) {
        v.append(len);
#if 1
        if (len % 1000 == 0) std::cout << '.' << std::flush;
        v.resize(len, len);
#endif
    }

    std::cout << "done\n";
}
