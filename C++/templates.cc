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

int main()
{
    Derived<int> d(12);
    d.foo();
    decltype(d) d2(11);
    d2.foo();
    return 0;
}

