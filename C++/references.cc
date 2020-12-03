#include <iostream>
#include <utility>

#if 0
void foo(int x) {
    std::cout << "int" << std::endl;
}
#endif

#if 0
void foo(int&& x) {
    std::cout << "int&&" << std::endl;
}
#endif

void foo(const int& x) {
    std::cout << "const int& " << x << std::endl;
}

void foo(int& x) {
    std::cout << "int& " << x << std::endl;
}

void foo(int&& x) {
    std::cout << "int&& " << x << std::endl;
}

int main() {
    int y = 123;

    // int&&
    foo(y);

    int& yref = y;
    foo(yref);

    // int&&
    foo(123);
    foo(std::move(y));
    foo(static_cast<int&&>(y));

    // const int&
    foo(const_cast<const int&>(y));
    const int& ycref = y;
    foo(ycref);

    return 0;
}

