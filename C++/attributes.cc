#include <iostream>

[[deprecated]] void myfunc() {}

[[deprecated("Use myfunc3 instead")]] void myfunc2() {}

void myfunc3() {}

int main()
{
	std::cout << "hello\n";

	myfunc(); // warning: ‘int myfunc()’ is deprecated

	myfunc2(); // warning: ‘void myfunc2()’ is deprecated: Use myfunc3
		   // instead

	myfunc3();

	return 0;
}
