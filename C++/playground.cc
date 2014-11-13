#include <iostream>
#include <typeinfo>

using namespace std;

int main() {
    typedef int myi;
    using myi2 = int;
    myi i1 = 123;
    myi2 i2 = 456;
    cout << "IDs: ";
    cout << "int=" << typeid(int).name();
    cout << ", i1=" << typeid(i1).name();
    cout << ", i2=" << typeid(i2).name();
    cout << ", myi2=" << typeid(myi2).name();
    cout << endl;
}
