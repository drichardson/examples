#include <iostream>
using namespace std;

template <typename T>
class Base
{
public:
    T val;
    void doSomething() {
        cout << "doSomething: " << val << '\n';
    }
};

template <typename T>
class Derived : Base<T>
{
public:
    Derived() = delete;
    Derived(T&& v) { this->val = v; }
    void foo() {
        this->doSomething();
    }
};

template <class T>
T mymax(T a, T b)
{
    return a > b ? a : b;
}

template <class T>
T mymin(T a, T b)
{
    return a < b ? a : b;
}

template <class T, T f(T,T) = mymin>
void print_result(T a, T b)
{
    std::cout << "print_result: a=" << a << ", b=" << b << ", result=" << f(a,b) << "\n";
}

int main()
{
    Derived<int> d{12};
    d.foo();
    decltype(d) d2{11};
    d2.foo();

    std::cout << "defaulting to mymin\n";
    print_result(1,2);
    print_result(3,2);
    std::cout << "explicit mymax\n";
    print_result<int, mymax>(1,2);
    print_result<int, mymax>(3,2);
}

