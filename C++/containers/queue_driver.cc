#include "queue.hpp"
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

int main() {
    typedef containers::queue<A> aqueue;
    using std::cout;

    aqueue q;
    for(unsigned count = 0; count < 132; ++count) q.append(A(count));
    for(unsigned count = 0; count < 132; ++count) q.remove_front();

#if 0
    unsigned total_count = 0;
    for(unsigned count = 0; count < 3; ++count) {
        for(unsigned i = 0; i < 1000; ++i) {
            q.append(A(total_count++));
            cout << "size: " << q.size() << ": front=" << q.front().x << '\n';
        }
        q.remove_front();
        q.remove_front();
        q.remove_front();
        q.remove_front();
        q.remove_front();
        cout << "size: " << q.size() << ": front=" << q.front().x << '\n';
    }

    while(q.size()) {
        //cout << "size: " << q.size() << ": front=" << q.front() << '\n';
        q.remove_front();
    }
#endif

    cout << "ctor_calls is " << A::ctor_calls<< "\n";
    cout << "dtor_calls is " << A::dtor_calls<< "\n";
}

