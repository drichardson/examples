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
		foo();
		foo();
		foo();
	}

    void foo() {
        cout << "Base::foo\n";
    }
};

class DerivedA : Base<DerivedA>
{
public:
    void foo() {
        cout << "DerivedA::foo\n";
    }
};


class DerivedB : Base<DerivedB>
{
public:
    void foo() {
        cout << "DerivedB::foo\n";
    }
};

int main()
{

	Base<void> B;
	B.foo();

	DerivedA DA;
	DA.foo();

	DerivedB DB;
	DB.foo();

	return 0;
}

