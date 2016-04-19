#include "circular_buffer.hpp"

#include <cstdlib>
#include <iostream>

class A
{
    A & operator=(A const & rhs) = delete;
public:
    int x;
    static int ctor_calls;
    static int dtor_calls;
    // A() { x = -1; ++ctor_calls; }
    A(int x) { this->x = x; ++ctor_calls; }
    A(A const & rhs) { x = rhs.x; ++ctor_calls; }
    ~A() { ++dtor_calls; }
};

int A::ctor_calls = 0;
int A::dtor_calls = 0;

template<class T, class U> void
assert_eq(const T& a, const U& b)
{
    if (!(a == b))
    {
        std::cerr << a << " != " << b << std::endl;
        std::abort();
    }
}

template <template <class A> class CircularBuffer>
void circularBufferTest()
{
    using std::cout;

    //containers::circularBuffer<A> cb(5);
    CircularBuffer<A> cb(5);

    for(unsigned i = 1; i <= 5; ++i) {
        cb.append(A(i));
        assert_eq(cb.front().x, 1);
    }

    size_t last_size = cb.size();
    assert_eq(last_size, 5);

    for(unsigned i = 1; i <=5; ++i)
    {
        assert_eq(cb.front().x, i);
        cb.remove_front();
        assert_eq(cb.size(), last_size - 1);
        last_size = cb.size();
    }

    assert_eq(cb.size(), 0);
}

int main()
{
    //circularBufferTest<containers::circularBuffer>();
    circularBufferTest<containers::CircularBufferSizeImplementation>();
    std::cout << "ctor_calls: " << A::ctor_calls << ", dtor_calls: " << A::dtor_calls << "\n";
}
