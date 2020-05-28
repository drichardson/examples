#include <iostream>

class Base
{
public:
	void foo3Times()
	{
		foo();
		foo();
		foo();
	}

	void foo()
	{
		std::cout << "Base::foo\n";
	}
};

class Derived : public Base
{
public:
	void foo()
	{
		std::cout << "Derived::foo\n";
	}
};

int main()
{
	Base B;
	B.foo();
	B.foo3Times();

	Derived D;
	D.foo();
	D.foo3Times();
	return 0;
}

