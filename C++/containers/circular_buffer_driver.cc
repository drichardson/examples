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

    A(A&& rhs) { this->x = rhs.x; ++ctor_calls; }
    A(int x) { this->x = x; ++ctor_calls; }
    A(A const & rhs) { x = rhs.x; ++ctor_calls; }
    ~A() { ++dtor_calls; }
};

int A::ctor_calls = 0;
int A::dtor_calls = 0;

template<class T, class U> void
assert_equal(const T& a, const U& b, const char* filename, int lineno, char const* a_expression, const char* b_expression)
{
    if (!(a == b))
    {
        std::cerr << filename << ":" << lineno
            << " assert_eq failed "
            << a_expression << " != " << b_expression
            << " (" << a << " != " << b << ")"
            << std::endl;
        std::abort();
    }
}

#define assert_eq(a,b) assert_equal(a, b, __FILE__, __LINE__, #a, #b)

template <template <class A> class CircularBuffer>
void circularBufferTest()
{
    using std::cout;

    CircularBuffer<A> cb(5);

    //
    // Empty case
    //
    assert_eq(cb.capacity(), 5);
    assert_eq(cb.size(), 0);

    //
    // Partially full to full cases
    //
    for(unsigned i = 1; i <= cb.capacity(); ++i) {
        cb.append(A(i));
        assert_eq(cb.front().x, 1);
    }

    size_t last_size = cb.size();
    assert_eq(last_size, cb.capacity());

    for(unsigned i = 1; i <= cb.capacity(); ++i)
    {
        assert_eq(cb.front().x, i);
        cb.remove_front();
        assert_eq(cb.size(), last_size - 1);
        last_size = cb.size();
    }

    assert_eq(cb.size(), 0);

    //
    // Overwriting case
    //
    for(unsigned i = 1; i <= cb.capacity(); ++i) {
        cb.append(A(i));
        assert_eq(cb.front().x, 1);
    }
    auto next = cb.capacity() + 1;
    cb.append(A(next));
    assert_eq(cb.size(), cb.capacity());
    assert_eq(cb.front().x, 2); // because 1 was overwritten.
    ++next;
    cb.append(next);
    assert_eq(cb.size(), cb.capacity());
    assert_eq(cb.front().x, 3); // because 2 was overwritten.
}

int main()
{
    circularBufferTest<containers::CircularBufferHeadTailImplementation>();
    assert_eq(A::ctor_calls, A::dtor_calls);

    circularBufferTest<containers::CircularBufferSizeImplementation>();
    assert_eq(A::ctor_calls, A::dtor_calls);

    std::cout << "TESTS PASSED\n";
}
