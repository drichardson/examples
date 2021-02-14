#include <iostream>

template <class C>
class Template
{
public:
	Template() { p = new C(); }

	C *p;
};

class TemplateUser {
public:
	// forward declaration okay because only used as pointer in Template
	Template<class Class> value;
};

class Class
{
public:
	Class() : i(123) {}
	int i;
};

int main()
{
	TemplateUser tu;
	std::cout << "i is " << tu.value.p->i << std::endl;
	return 0;
}
