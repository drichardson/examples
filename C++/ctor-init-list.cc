#include <iostream>
#include <string>

using std::cout;

class Trace
{
    std::string str;
public:
    Trace(const char* str) : str(str) { cout << str << "\n"; }
    ~Trace() { cout << "~" << str << "\n"; }
};

class A
{
    // declaration order defines initialization order
    Trace v1;
    Trace v2;
    Trace v3;

public:

    // Use -Wreorder and -Werror=reorder to generate warnings and errors respectively if
    // constructor initialization lists are in a different order than the declared member variables.

    A()
        // since this order matches the declaration order, there will be no warnings when -Werror=reorder is on
        : v1("v1_default")
        , v2("v2_default")
        , v3("v3_default")
    {
    }

    A(int)
        // will produce a warning since v3 is before v2. Note that the actually initialization will be
        // v1, v2, v3 since that is the declared order.
        : v1("v1_int")
        , v3("v3_int")
        , v2("v2_int")
    {
    }

    A(double)
        // will produce a warning since v3 is before v2. Note that the actually initialization will be
        // v1, v2, v3 since that is the declared order.
        : v3("v3_double")
        , v2("v2_double")
        , v1("v1_double")
    {
    }

};

int main()
{
    A a;
    A a1(123);
    A a2(123.456);

    return 0;
}

