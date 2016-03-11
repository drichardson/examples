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

template <typename T>
void foo(T&& x) {
    std::cout << "universal " << x << std::endl;
}

int main() {
    int y = 123;

    foo(y);
    foo(123);
    foo(std::move(y));

    const int& ycref = y;
    foo(ycref);

    int& yref = y;
    foo(yref);

    return 0;
}

