#include <iostream>

template <class Class>
class A
{
public:
	A() { p = new Class(); }
	Class *p;
};

class B {
public:
	// forward declaration okay because only used as pointer in Template
	A<class C> a_c_forward;
};

class C
{
public:
	C() : i(123) {}
	int i;
};

int main()
{
	B b;
	std::cout << "i is " << b.a_c_forward.p->i << std::endl;
	return 0;
}
