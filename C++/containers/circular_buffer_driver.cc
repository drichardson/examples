#include "circular_buffer.hpp"
#include <iostream>

class A
{
    char *buf;
    A & operator=(A const & rhs);
public:
    int x;
    static int ctor_calls;
    static int dtor_calls;
    A() { buf = new char[10]; x = -1; ++ctor_calls; }
    A(int x) { buf = new char[10]; this->x = x; ++ctor_calls; }
    A(A const & rhs) { buf = new char[10]; x = rhs.x; ++ctor_calls; }
    ~A() { ++dtor_calls; delete[] buf;}
};

int A::ctor_calls = 0;
int A::dtor_calls = 0;

int main()
{
    using std::cout;
    containers::circularBuffer<A> cb(5);

    for(unsigned i = 0; i < 10; ++i) {
        cb.append(A(i));
    }

    while(cb.size()) {
        cout << cb.oldest().x << ' ';
        cb.remove_oldest();
    }
    cout << '\n';
}
