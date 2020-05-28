//
// https://en.wikipedia.org/wiki/Curiously_recurring_template_pattern
//

#include <iostream>
using namespace std;

template <typename T>
class Base
{
public:
	void foo3Times()
	{
		T* t = static_cast<T*>(this);
		t->foo();
		t->foo();
		t->foo();
	}
};

class DerivedA : public Base<DerivedA>
{
public:
    void foo() {
        cout << "DerivedA::foo\n";
    }
};

class DerivedB : public Base<DerivedB>
{
public:
    void foo() {
        cout << "DerivedB::foo\n";
    }
};

int main()
{
	DerivedA DA;
	DA.foo();
	DA.foo3Times();

	DerivedB DB;
	DB.foo();
	DB.foo3Times();

	return 0;
}

