#include <iostream>

using namespace std;

class A {
    int _i;
    char _c;
public:
    A(int i, char c) : _i(i), _c(c) {
        cout << this << "=A(" << _i << "," << _c << ")\n";
    }

    ~A() {
        cout << this << "=~A(" << _i << "," << _c << ")\n";
    }
};

int main(int const argc, char const* const* argv) {
    A a{1, 'a'};
    unsigned char* blob[10000];
    int next {0};
    A* p = new(blob+next) A{2, 'b'};
    next += sizeof(A);
    A* p2 = new(blob+next) A{3, 'c'};
    next += sizeof(A);
    p->~A();
    p2->~A();
    return 0;
}

