#include <iostream>

namespace templates {

// An alternative generic definition of DoSomething (the other one is in templates.cc).
// Any instantiations in this file will use this definition.
template <class T>
void DoSomething(const T& v)
{
    std::cout << "DoSomething " << __FILE__ << ": " << v << "\n";
}


template void DoSomething<char>(const char&);
}


