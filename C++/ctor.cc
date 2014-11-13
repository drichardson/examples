#include <iostream>
#include <typeinfo>
#include <cstring>
#include <cstdlib>

using namespace std;

int foo(int&& i) {
    return i;
}

template <typename T>
void bar(T&& x) {
    cout << "x is " << x << ", type is " << typeid(T).name() << endl;
}


class A {
    char* _s;
public:
    A() {
        cout << "default" << endl;
        _s = nullptr;
    };

    A(A const & rhs) {
        cout << "copy" << endl;
        if (rhs._s != nullptr) {
            _s = strdup(rhs._s);
        } else {
            _s = nullptr;
        }
    }

    A & operator=(A const & rhs) {
        cout << "assignment" << endl;
        return *this;
    }

    explicit A(char const* s) {
        cout << "ctor called" << endl;
        if (s == nullptr) {
            _s = nullptr;
        } else {
            int len = strlen(s);
            _s = new char[len];
            for(int i = 0; i < len; ++i) {
                _s[i] = s[i];
            }
        }
    }

    // NOTE: This never gets called, at least not with clang++ 3.5 or gcc 4.9.1
    // with -O0 optimizations... probably due to return value optimization
    // which is at liberty to remove intermediate copy construction calls,
    // even if those calls have side effects.
    A(A&& rhs) : _s(rhs._s) {
        cout << "move called" << endl;
        rhs._s = NULL;
    }

    ~A() {
        cout << "dtor called" << endl;
        delete[] _s;
    }

    char const* str() const { return _s; }
};

A makeA(char const* t) {
    cout << "makeA called" << endl;
    A a{"a one"};
    cout << "about to do b" << endl;
    A b{"b one"};
    cout << "about to return" << endl;
    return random() % 2 == 0 ? a : b;
}

int main(int const argc, char const* const* argv) {

    A a{"this is a test"};
//    cout << "a is " << a.str() << endl;

    A b{a}; // copy ctor
    A c{"test"};
    A d{};
    A e;
    A&& fr = A{"auto"};
    A f{fr};
    f = a;
    A g = makeA("testing");
    cout << "after makeA" << endl;
//    cout << "b is " << b.str() << endl;

    return 0;
}
