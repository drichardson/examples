/*
From: https://en.cppreference.com/w/cpp/language/initialization

For non-array type, the single object is constructed in the acquired memory
area.

- If initializer is absent, the object is default-initialized.

- If initializer is a parenthesized list of arguments, the object is
direct-initialized.

- If initializer is a brace-enclosed list of arguments, the object is
list-initialized.


From: https://en.cppreference.com/w/cpp/language/new
For placement new, the syntax is:
::(optional) new (placement_params)(optional) type initializer(optional)
*/

#include <algorithm>
#include <array>
#include <iostream>
#include <sstream>

using namespace std;

template <typename T>
string describe(T const &a)
{
	ostringstream oss;
	oss << '{';
	oss << hex;

	constexpr size_t elements = sizeof(a) / sizeof(a[0]);

	if (elements > 0)
	{
		oss << a[0];
	}

	for (size_t i = 1; i < elements; ++i)
	{
		oss << ',' << a[i];
	}

	oss << '}';

	return oss.str();
}

int main()
{
	cout << hex;

	unsigned int i;
	unsigned a[4];
	auto a_begin = &a[0];
	auto a_end = &a[sizeof(a) / sizeof(a[0])];
	auto reset = [&i, a_begin, a_end]() {
		i = 0xdeadbeef;
		fill(a_begin, a_end, 0xdeadbeef);
		// cout << "i reset to " << i << endl;
	};

	// empty initializier list. Using placement new
	// so that I know what the memory is set to.
	reset();
	cout << "i is: " << i << endl;
	cout << "a is " << describe(a) << endl;

	// https://en.cppreference.com/w/cpp/language/default_initialization
	reset();
	new (&i) int;
	cout << "default initialization: " << i << endl;
	assert(i == 0xdeadbeef);

	// https://en.cppreference.com/w/cpp/language/default_initialization
	// If initializer is absent, each element is default-initialized (for
	// non-class objects, left indeterminate).
	reset();
	new (a) int[4];
	cout << "default initialization of array with empty parens: "
	     << describe(a) << endl;
	assert(all_of(a_begin, a_end, [](auto v) { return v == 0xdeadbeef; }));

	// https://en.cppreference.com/w/cpp/language/direct_initialization
	reset();
	new (&i) int{1};
	cout << "direct initialization non-class type with single brace "
		"encloused initializer: "
	     << i << endl;
	assert(i == 1);

	// https://en.cppreference.com/w/cpp/language/direct_initialization
	reset();
	new (&i) int(2);
	cout << "direct initialization: " << i << endl;
	assert(i == 2);

	//
	// Value Initialization
	// This is the initialization performed when an object is constructed
	// with an empty initializer.
	//
	// https://en.cppreference.com/w/cpp/language/value_initialization
	//

	reset();
	new (&i) int();
	cout << "value initialization (empty parens): " << i << endl;
	assert(i == 0);

	reset();
	new (&i) int{};
	cout << "value initialization (empty braces): " << i << endl;
	assert(i == 0);

	// If initializer is an empty pair of parentheses, each element is
	// value-initialized.
	new (a) int[4]();
	cout << "value initialization of array (empty parens): " << describe(a)
	     << endl;
	assert(all_of(a_begin, a_end, [](auto v) { return v == 0; }));

	// Value initialized by rule 5:
	// T{} 	(5) 	(since C++11)
	reset();
	new (a) int[4]{};
	cout << "value initialization of array (empty braces): " << describe(a)
	     << endl;
	assert(all_of(a_begin, a_end, [](auto v) { return v == 0; }));

	// https://en.cppreference.com/w/cpp/language/list_initialization
	// initialization of an object with dynamic storage duration with a
	// new-expression, where the initializer is a brace-init-list
	//
	// https://en.cppreference.com/w/cpp/language/aggregate_initialization
	// Otherwise, if T is an aggregate type, aggregate initialization is
	// performed.
	reset();
	new (a) int[4]{1};
	cout << "aggregate initialization of array (braces with single item): "
	     << describe(a) << endl;
	assert(a[0] == 1);
	assert(a[1] == 0);
	assert(a[2] == 0);
	assert(a[3] == 0);

	reset();
	new (a) int[4]{1, 2};
	cout << "aggregate initialization of array (braces with fewer items "
		"than array):  "
	     << describe(a) << endl;
	assert(a[0] == 1);
	assert(a[1] == 2);
	assert(a[2] == 0);
	assert(a[3] == 0);

	reset();
	new (a) int[3]{1, 2, 3};
	cout << "aggregate initialization of array (braces same items as "
		"array): "
	     << describe(a) << endl;
	assert(a[0] == 1);
	assert(a[1] == 2);
	assert(a[2] == 3);
	assert(a[3] == 0xdeadbeef);
}
