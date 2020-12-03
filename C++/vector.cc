#include <vector>
#include <iostream>
#include <algorithm>
#include <cstring>

using namespace std;

class A
{
    int x;
    char* buf;

   A & operator=(A const & rhs);

public:
    A(char const* ss) {
        x = strlen(ss);
        buf = new char[x+1];
        std::memcpy(buf,ss,x+1);
    }

    // Objects inserted into std::vector must implement copy ctor.
    A(A const & rhs) : A(rhs.str()) {
        cout << "A copy ctor\n";
    }

    ~A() {
        cout << "~A for this=" << this << ", deleting buf=" << static_cast<void*>(buf) << '\n';
        delete[] buf;
    }

    char const* str() const { return buf; }
};

class B : public A
{
    char *buf;
public:
    B(char const* ss) : A(ss) {
        buf = new char[strlen(ss)+1];
        buf[0] = 'a';
        buf[1] = 0;
        cout << "B buf is " << static_cast<void*>(buf) << '\n';
    }

    ~B() {
        cout << "~B for this=" << this << ", buf=" << static_cast<void*>(buf) << '\n';
        delete[] buf;
    }
};

int main() {
    vector<int> a = {1, 3, 4};
    vector<int> b = {4, 1, 1};
    vector<int> c = a;

    cout << "a > b: " << (a > b) << endl;
    cout << "a < b: " << (a > b) << endl;
    cout << "a == a: " << (a == a) << endl;
    cout << "a == b: " << (a == b) << endl;
    cout << "a == c: " << (a == c) << endl;
    cout << "max(a,b) == a: " << (max(a,b) == a) << endl;
    cout << "max(a,b) == b: " << (max(a,b) == b) << endl;

    cout << "Class test ==================\n";
    vector<A> va;
    va.push_back(A("one"));
    va.push_back(A("two"));
    va.push_back(B("B.three")); // note: ~B won't be called
    for(A const & e : va) {
        cout << e.str() << '\n';
    }
}
