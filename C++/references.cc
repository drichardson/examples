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
	X(char const *str) : s(str)
	{
		cout << "X(char const* str=" << s << ")" << endl;
	}

	// Rule of five for testing
	// https://en.cppreference.com/w/cpp/language/rule_of_three

	// destructor
	~X() { cout << "~X(): s=" << s << endl; }

	// copy constructor
	X(X const &other) : s(other.s)
	{
		cout << "X(X const& other=" << other.s << "): s=" << s << endl;
	}

	// move constructor
	X(X &&other)
	{
		// s(std::exchange(other.s, nullptr))
		cout << "X(X&& other=" << other.s;
		s = other.s;
		other.s = "MOVED by move constructor X(X&&)";
		char *buf = new (char[100]);
		std::sprintf(buf, "MOVED by X(X&&) Previous value=%s", s);
		other.s = buf;
		cout << "): s=" << s << endl;
	}

	// copy assignment
	X &operator=(X const &other)
	{
		s = other.s;
		cout << "operator=(X const& other=" << other.s << "): s=" << s
		     << endl;
		return *this;
	}

	// move assignment
	X &operator=(X &&other)
	{
		cout << "operator=(X&& other=" << other.s;
		s = other.s;
		char *buf = new (char[100]);
		std::sprintf(
		    buf, "MOVED by X &operator=(X&&). Previous value=%s", s);
		other.s = buf;
		cout << "): s=" << s << endl;
		return *this;
	}
};

X xfoo()
{
	X x("xfoo");
	return x;
}

void print_section_header(char const *header)
{
	cout << "\n*** " << header << " ***\n";
}

int main()
{
	int y = 123;

	print_section_header("int&");
	foo(y);

	int &yref = y;
	foo(yref);

	print_section_header("int&&");
	foo(123);
	foo(std::move(y));
	foo(static_cast<int &&>(y));

	print_section_header("const int&");
	foo(const_cast<const int &>(y));
	const int &ycref = y;
	foo(ycref);

	print_section_header("class X");
	X x1("1");
	x1 = xfoo();
	X x2("2");
	x2 = x1;
	X x3("3");
	X x4 = x3;	      // copy constructor used
	X x5 = std::move(x3); // move constructor used, x3 now invalid.
	X x6 = std::move(x3); // Move again. Relies on x3 being in valid
			      // state after previous move.

	return 0;
}
