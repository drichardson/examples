#include <iostream>
#include <vector>

// Conversion class from Modern C++ Design, Chapter 2: Techniques.
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
};

int main() {
        std::cout
                << Conversion<double, int>::exists << ' '
                << Conversion<char, char*>::exists << ' '
                << Conversion<size_t, std::vector<int>>::exists << '\n';

        return 0;
}

