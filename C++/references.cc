#include <iostream>
#include <utility>

using std::cout;
using std::endl;

#if 0
// error: if defined, get call of overloaded is ambiguous
void foo(int x) {
	std::cout << "int" << std::endl;
}
#endif

void foo(const int &x)
{
	cout << "const int& " << x << endl;
}

void foo(int &x)
{
	cout << "int& " << x << endl;
}

void foo(int &&x)
{
	cout << "int&& " << x << endl;
}

class X {
	char const *s;

public:
	X(char const *str) : s(str) { cout << "X(): " << s << endl; }

	// Rule of five for testing
	// https://en.cppreference.com/w/cpp/language/rule_of_three

	// destructor
	~X() { cout << "~X(): " << s << endl; }

	// copy constructor
	X(X const &other) : s(other.s) { cout << "X(X const&): " << s << endl; }

	// move constructor
	X(X &&other) : s(std::exchange(other.s, nullptr))
	{
		cout << "X(X&&): " << s << endl;
	}

	// copy assignment
	X &operator=(X const &other)
	{
		*this = X(other);
		cout << "operator=(X const&): " << s << endl;
		return *this;
	}

	// move assignment
	X &operator=(X &&other)
	{
		std::swap(s, other.s);
		cout << "operator=(X&&): " << s << endl;
		return *this;
	}
};

X xfoo()
{
	X x("xfoo");
	return x;
}

int main()
{
	int y = 123;

	// int&&
	foo(y);

	int &yref = y;
	foo(yref);

	// int&&
	foo(123);
	foo(std::move(y));
	foo(static_cast<int &&>(y));

	// const int&
	foo(const_cast<const int &>(y));
	const int &ycref = y;
	foo(ycref);

	X x("1");
	x = xfoo();
	X x2("2");
	x2 = x;

	return 0;
}
