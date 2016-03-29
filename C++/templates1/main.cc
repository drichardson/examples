#include "templates.h"
#include <iostream>

int main()
{
    using std::cout;

    // Template that takes a type and a constant.
    using FA3 = templates::FixedArray<double, 3>;
    //templates::FixedArray<double, 3> doubleArray3;
    FA3 doubleArray3;
    doubleArray3.buffer[0] = 0.0;
    doubleArray3.buffer[1] = 1.0;
    doubleArray3.buffer[2] = 2.0;
    // doubleArray3.buffer[3] = 3.0; // would result in compile time error
    cout << "doubleArray3: size = " << decltype(doubleArray3)::bufferSize << "\n";
    cout << "doubleArray3: size = " << doubleArray3.bufferSize << "\n";
    cout << "values: "
        << doubleArray3.buffer[0] << ','
        << doubleArray3.buffer[1] << ','
        << doubleArray3.buffer[2] << "\n";

    templates::Convert c;
    c.set(1);
    cout << "c(1 int) = " << c.i << "\n";
    c.set(2.0);
    cout << "c(2.0 double) = " << c.i << "\n";
    c.set('h');
    cout << "c(h char) = " << c.i << "\n";
    c.set<char>('c');
    cout << "c(c char) = " << c.i << "\n";
    // c.set(doubleArray3); // error: assignment to int from incompatible type

    // 
    templates::DoSomething<double>(1.234);
    templates::DoSomething(1);
    templates::DoSomething('h');

    return 0;
}

