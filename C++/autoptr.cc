//
// auto_ptr example for pre-C++ 11 code. This has been deprecated in
// by unique_ptr in C++11.
//

#include <iostream>
#include <memory>

using namespace std;

class B {
public:
    ~B() {
        cout << "destructing B" << endl;
    }
};

class A {
    auto_ptr<B> _s;
public:
    A() : _s(new B) {
        // or, instead of in the member initialization list:
        // _s.reset(new B);
    }
};

int main() {
    A a1;
    A *a2 = new A;
    delete a2;
    cout << "ok" << endl;
}

