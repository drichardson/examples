#include "templates.h"

#include <iostream>
#include <vector>

class Derived : public templates::WithVirtual<int> {
public:

    void printT(const int & i) final {
        std::cout << "printT(derived): " << i << "\n";;
    }
};


// Like any template declaration, an alias template can only be declared at class scope or namespace scope.
template<typename T>
using My_vec = std::vector<T, std::allocator<T>>; // Use standard allocator.

// template<int> int templates::WithStaticMember::staticMember = 1;
//template int templates::WithStaticMember::staticMember;
//template <typename T> T templates::WithStaticMember<T>::staticMember = 1;

int main()
{
    using std::cout;

    //
    // Template that takes a type and a constant.
    //
    using FA3 = templates::FixedArray<double, 3>;
    FA3 doubleArray3;
    doubleArray3.buffer[0] = 0.0;
    doubleArray3.buffer[1] = 1.0;
    doubleArray3.buffer[2] = 2.0;
    // doubleArray3.buffer[3] = 3.0; // would result in compile time error
    cout << "decltype(doubleArray3)::bufferSize= " << decltype(doubleArray3)::bufferSize << "\n";
    cout << "FA3::bufferSize = " << FA3::bufferSize << "\n";
    cout << "doubleArray3.bufferSize = " << doubleArray3.bufferSize << "\n";
    cout << "values: "
        << doubleArray3.buffer[0] << ','
        << doubleArray3.buffer[1] << ','
        << doubleArray3.buffer[2] << "\n";

    //
    // Non-template class with a template member function.
    //
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
    // Template function with definitions and instantiations in other files (templates.cc and templates2.cc).
    //
    templates::DoSomething<double>(1.234);
    templates::DoSomething(1);
    templates::DoSomething('h');

    //
    // template with virtual member function
    //
    templates::WithVirtual<double> wv;
    wv.printT(1.231);
    Derived d;
    templates::WithVirtual<int> *ip = &d;
    ip->printT(243);


    //
    // template class with non-const static member
    //
    templates::WithStaticMember<int>::staticMember = 2;
    cout << "staticMember<int> = " << templates::WithStaticMember<int>::staticMember << "\n";
    cout << "staticMember<double> = " << templates::WithStaticMember<double>::staticMember << "\n";

    //
    // template template parameter
    //

    templates::WithTemplateTemplateParameter<int, My_vec> ttp;
    ttp.buf.push_back(10);
    cout << "ttp: " << ttp.buf[0] << "\n";

    return 0;
}



