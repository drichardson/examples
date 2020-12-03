#include "templates.h"
#include <iostream>

namespace templates {

// Template definition. An alternative definition is in templates2.cc. Any instatiation
// in this file will use this definition.
template <class T>
void DoSomething(const T& v)
{
    std::cout << "DoSomething " << __FILE__ << ": " << v << "\n";
}

// Not create symbols so it will link. You have to do this for each different type
// you want to use with DoSomething.
template void DoSomething<double>(const double&);
template void DoSomething<int>(const int&);

}

