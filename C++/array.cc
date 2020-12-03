#include <iostream>
#include <array>

int main() {
    int ibefore;
    std::array<int, 5> a;
    int iafter;

    std::cout << "a: " << a.size() << std::endl;
    std::cout << "a[0] (garbage data): " << a[0] << std::endl;
    std::cout << "a[4] (garbage data): " << a[4] << std::endl;
    std::cout << "a[5] (out of bounds): " << a[5] << std::endl;
    a[0] = 123;
    std::cout << "a[0] (should be 123): " << a[0] << std::endl;
    a.fill(5);
    std::cout << "a[0] (should be 5): " << a[0] << std::endl;
    std::cout << "a[3] (should be 5): " << a[3] << std::endl;
    std::cout << "a[5] (out of bounds): " << a[5] << std::endl;

    // Data appears to be on the stack (at least when I'm running my test).
    std::cout << "a.data() " << a.data() << std::endl;
    std::cout << "&ibefore " << &ibefore << std::endl;
    std::cout << "&iafter " << &iafter << std::endl;

    // Bad things are going to happen. Probably set fault.
    std::cout << "a[50000000] (way out of bounds): " << a[500000000] << std::endl;
}
