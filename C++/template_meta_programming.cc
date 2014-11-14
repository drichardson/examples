#include <iostream>

//
// Demonstrate a few ways to calculate factorials at compile time.
//

// pre C++11 way using enums
template<int n>
struct factorial_pre_cpp11 {
    enum { value = n * factorial_pre_cpp11<n-1>::value };
};

template <>
struct factorial_pre_cpp11<0> {
    enum { value = 1 };
};

// C++11 way using constexpr in a struct instead of enum
template <int n>
struct factorial_cpp11 {
    constexpr static int value = n * factorial_cpp11<n-1>::value;
};

template <>
struct factorial_cpp11<0> {
    constexpr static int value = 1;
};


// C++11 way using constexpr and no struct
constexpr int factorial_constexpr(int n) {
    return n > 1 ?  n * factorial_constexpr(n-1) : 1;
}

// notice results of all factorial calculations can be stored
// in constexprs, which means they're all computed at compile time.
void demo_compile_time_factorial_calculations() {
    using namespace std;

    // using template meta programming technique with enum
    constexpr auto f5 = factorial_pre_cpp11<5>::value;
    constexpr auto f10 = factorial_pre_cpp11<10>::value;
    cout << "f5 = " << f5 << endl;
    cout << "f10 = " << f10 << endl;

    // using template meta programming with constexpr
    constexpr auto f5_v2 = factorial_cpp11<10>::value;
    cout << "f5_v2 = " << f5_v2 << endl;

    // using constexpr
    constexpr auto f5_v3 = factorial_constexpr(5);
    cout << "f5_v3 = " << f5_v3 << endl;
}


//
// Demonstrate SFINAE (Substitution Failure Is Not An Error) technique to detect
// members of objects at compile time.
// http://en.wikipedia.org/wiki/Substitution_failure_is_not_an_error
//
struct S1 {
    typedef int mytype;
};

struct S2 {
    typedef int yourtype;
};

struct S3 {
    int mytype; // mytype is a member, not a type here
};

template <typename T>
struct has_typedef_mytype {
    typedef char yes[1];
    typedef char no[2];
    template <typename C> static yes& test(typename C::mytype*);
    template <typename> static no& test(...);
    constexpr static bool value = sizeof(test<T>(0)) == sizeof(yes);
};

void demo_SFINAE() {
    using namespace std;
    cout << "S1 has mytype? " << has_typedef_mytype<S1>::value << endl;
    cout << "S2 has mytype? " << has_typedef_mytype<S2>::value << endl;
    cout << "S3 has mytype? " << has_typedef_mytype<S3>::value << endl;
    cout << "int has mytype? " << has_typedef_mytype<int>::value << endl;
}

int main() {
    demo_compile_time_factorial_calculations();    
    demo_SFINAE();
}

