#include "stack.hpp"
#include <iostream>

int main()
{
    typedef containers::stack<unsigned> ustack;
    using std::cout;

    ustack s;

    for(unsigned i = 1; i <= 100000; ++i) {
        s.push(i);
    }

    while(s.size()) {
        if (s.size() <= 10 || s.size() % 1000 == 0)
        {
            cout << s.top() << ' ';
        }
        s.pop();
    }
    cout << '\n';
}

