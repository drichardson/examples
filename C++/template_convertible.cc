#include <iostream>
#include <vector>
#include <type_traits>

// Conversion class from Modern C++ Design, Chapter 2: Techniques.
// For C++ 11 and later, you can use std::is_convertible instead.
template <class T, class U>
class Conversion
{
        typedef char Small; // Used to indicate conversion from T to U is allowed.
        class Big { char dummy[2]; }; // Used to indicate conversion from T to U is not allowed.
        static Small Test(const U&);
        static Big Test(...); // ... matches only if conversion from const U& not available.
        static T MakeT(); // Because default constructor T() might not exist.

public:
        enum { exists = sizeof(Test(MakeT())) == sizeof(Small) };
        enum { sameType = false };
};

// Specialize 

int main() {
        std::cout
                << Conversion<double, int>::exists << ' '
                << Conversion<char, char*>::exists << ' '
                << Conversion<size_t, std::vector<int>>::exists << '\n';

        // Should agree with C++ 11 std::is_convertible.
        std::cout
                << std::is_convertible<double, int>::value << ' '
                << std::is_convertible<char, char*>::value << ' '
                << std::is_convertible<size_t, std::vector<int>>::value << '\n';

        return 0;
}

