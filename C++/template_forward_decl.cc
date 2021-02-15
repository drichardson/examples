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
	A<struct S> a_s_forward;
	A<union U> a_u_forward;
};

class C
{
public:
	C() : i(123) {}
	int i;
};

struct S
{
	S() : i(555) {}
	int i;
};

union U
{
	U() : i(3) {}
	int i;
	int j;
};

int main()
{
	B b;
	std::cout << "c i is " << b.a_c_forward.p->i << std::endl;
	std::cout << "s i is " << b.a_s_forward.p->i << std::endl;
	std::cout << "u i is " << b.a_u_forward.p->i << std::endl;
	std::cout << "u j is " << b.a_u_forward.p->j << std::endl;
	return 0;
}
