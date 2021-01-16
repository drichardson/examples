#include <iostream>

namespace A {
using std::cout;
namespace {
void internal()
{
	cout << "internal 1" << std::endl;
}
} // namespace

void foo()
{
	cout << "foo" << std::endl;
}
} // namespace A

namespace A {
void bar()
{
	// this is okay, because the 1st A namespace did using std::cout.
	cout << "bar" << std::endl;
}

namespace {
#if 0
// error: internal previously defined
void internal()
{
	cout << "internal 1" << std::endl;
}
#endif
} // namespace

} // namespace A

namespace B {
namespace {
void internal()
{
	// error: cout was not declared in this scope
	// cout << "internal 3" << std::endl;

	std::cout << "internal 3" << std::endl;
}
} // namespace
} // namespace B

int main(int argc, char const **argv)
{
	A::foo();
	A::bar();
	A::internal(); // can call from this translation unit, but not another.
	B::internal(); // can call from this translation unit, but not another.
	return 0;
}
