#include <algorithm>
#include <cstring>
#include <iostream>
#include <memory>
#include <vector>

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

    virtual ~A()
    {
	    cout << "~A for this=" << this
		 << ", deleting buf=" << static_cast<void *>(buf) << '\n';
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

    ~B()
    {
	    cout << "~B for this=" << this
		 << ", buf=" << static_cast<void *>(buf) << '\n';
	    delete[] buf;
    }
};

int main() {
	{
		cout << "Int test ==================\n";
		vector<int> a = {1, 3, 4};
		vector<int> b = {4, 1, 1};
		vector<int> c = a;

		cout << "a > b: " << (a > b) << endl;
		cout << "a < b: " << (a > b) << endl;
		cout << "a == a: " << (a == a) << endl;
		cout << "a == b: " << (a == b) << endl;
		cout << "a == c: " << (a == c) << endl;
		cout << "max(a,b) == a: " << (max(a, b) == a) << endl;
		cout << "max(a,b) == b: " << (max(a, b) == b) << endl;
	}

    {
	    cout << "Class test ==================\n";
	    vector<A> va;
	    va.push_back(A("one"));
	    va.push_back(A("two"));
	    va.push_back(
		B("B.three")); // note: ~B won't be called. A not a pointer.
	    for (A const &e : va)
	    {
		    cout << e.str() << '\n';
	    }
    }

    {
	    cout << "Class pointer test ===============\n";
	    vector<unique_ptr<A>> va;
	    va.push_back(make_unique<A>("one"));
	    va.push_back(make_unique<A>("two"));
	    va.push_back(make_unique<B>("B.three")); // note: ~B will be called
    }

    {
	    cout << "Vector of strings test ============\n";
	    vector<string> vs;
	    vs.emplace_back("hello");
	    vs.emplace_back("world");

	    for (auto const &s : vs)
	    {
		    cout << s << '\n';
	    }

	    vs[0][1] = '3';
	    vs[0][4] = '0';
	    vs[1][4] = 'D';

	    for (auto const &s : vs)
	    {
		    cout << s << '\n';
	    }
    }

    return 0;
}
