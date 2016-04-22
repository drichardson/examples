#include <cstdint>
#include <cstdlib>
#include <iostream>

// compute a raised to the power n
// both a and n must be positive
template <class T>
T power_simple(T a, T n)
{
    T r = 1;

    for(int i = 0; i < n; ++i)
    {
        r *= a;
    }

    return r;
}

// compute a raised to the power n
// both a and n must be positive
// from The Algorithm Design Manual 2nd Edition pg 48.
template <class T>
T power_fast(T a, T n)
{

    if(n == 0)
    {
        return 1;
    }

    static_assert(std::is_integral<T>::value, "Integer value required.");
    T floor_half = n >> 1;
    T x = power_fast(a, floor_half);
    bool n_is_even = (n & 1) == 0;
    if(n_is_even)
    {
        return x * x;
    }

    return a * x * x;
}

// Create "op" structs to the functions above can be passed as template parameters.
// Maybe it's possible to pass template functions as template parameters, but I wasn't
// able to figure out how.
template <class T>
struct power_simple_op {
    T operator() (T a, T b) {
        return power_simple(a, b);
    }
};

template <class T>
struct power_fast_op {
    T operator() (T a, T b) {
        return power_fast(a, b);
    }
};

template <template <class> class POWER_OP, class T>
void test(T x, T y, T expected)
{
    T r = POWER_OP<T>()(x,y);
    std::cout << x << "^" << y << "=" << r << "\n";
    if(r != expected)
    {
        std::cerr << "wrong value, expected " << expected << "\n";
        std::abort();
    }
}

template <template <class> class OP>
void test_battery()
{
    test<OP>(3,0,1);
    test<OP>(3,1,3);
    test<OP>(3,2,9);
    test<OP>(3,3,27);
    test<OP>(3,8,6561);
    test<OP>(3L,30L,205891132094649);
    test<OP>(2,1,2);
    test<OP>(2,20,1<<20);
    test<OP>(2,30,1<<30);
    test<OP, int64_t>(2,40,1L<<40);
    test<OP, long>(2,50,1L<<50);
    test<OP, uint64_t>(2,62,1L<<62);
    test<OP>(2UL,63ul,1uL<<63);
}

int main()
{
    test_battery<power_simple_op>();
    test_battery<power_fast_op>();

    test<power_simple_op>(3.0, 2.0, 9.0); // OK
    // test<power_fast_op>(3.0, 2.0, 9.0); // FAILS. power_fast requires integers.

    return 0;
}

