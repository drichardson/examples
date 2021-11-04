#include <iostream>

template <typename T>
concept EqualityComparable = requires(T a, T b)
{
	// clang-format off
	{ a == b } ->std::same_as<bool>;
	{ a != b } ->std::same_as<bool>;
	// clang-format on
};

// Constrained function template declaration
void foo(const EqualityComparable auto &e)
{
	bool is_default = e == decltype(e){};
	std::cout << (is_default ? "true" : "false") << std::endl;
}

template <EqualityComparable T>
void foo2(const T &e)
{
	bool is_default = e == decltype(e){};
	std::cout << (is_default ? "true" : "false") << std::endl;
}

struct Custom
{
	int x = 0;
};

struct CustomOK
{
	int x = 0;

	bool operator==(CustomOK const &rhs) const { return x == rhs.x; }
};

int main()
{
	foo(0);
	foo(10);
	foo2(0);
	foo2(10);

	// Custom c;
	// foo(c); // causes error below
	/*
	concepts.cc:13:6: note: candidate template ignored:
	      constraints not satisfied [with e:auto = Custom]
	void foo(const EqualityComparable auto &e)
	     ^
	concepts.cc:13:16: note: because 'Custom' does not
	      satisfy 'EqualityComparable'
	void foo(const EqualityComparable auto &e)
		       ^
	concepts.cc:7:6: note: because 'a == b' would be
	      invalid: invalid operands to binary expression ('Custom' and
	'Custom') { a == b } ->std::same_as<bool>;
		*/

	std::cout << "CUSTOM\n";

	CustomOK c_ok;
	foo(c_ok);
	foo2(c_ok);
	c_ok.x = 42;
	foo(c_ok);
	foo2(c_ok);

	return 0;
}
