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
struct mymax
{
    T operator()(T a, T b)
    {
        return a > b ? a : b;
    }
};

template <class T>
struct mymin
{
    T operator()(T a, T b)
    {
        return a < b ? a : b;
    }
};

template <class T, template <class> class OP=mymin>
void print_result(T a, T b)
{
    std::cout << "print_result: a=" << a << ", b=" << b << ", result=" << OP<T>()(a,b) << "\n";
}

template <class T, template <class> class OP=mymin>
void print_results_fixed_T()
{
    print_result<T,OP>(1, 2);
    print_result<T,OP>(2, 3);
}

template <template<typename> class OP>
void print_results_variable_T()
{
    print_result<int, OP>(1,2);
    print_result<int, OP>(2,3);

    print_result<float, OP>(1.1,2.2);
    print_result<float, OP>(2.2,3.3);
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

    std::cout << "print_results_fixed_T mymax\n";
    print_results_fixed_T<int, mymax>();
    std::cout << "print_results_fixed_T mymin\n";
    print_results_fixed_T<int>(); // use default value, mymin

    std::cout << "print_results_variable_T mymax\n";
    print_results_variable_T<mymax>();
}

