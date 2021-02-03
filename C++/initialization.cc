#include <iostream>

using namespace std;

int main()
{
	cout << hex;

	unsigned int i;
	auto reset = [&i]() {
		i = 0xdeadbeef;
		cout << "reset i to " << i << endl;
	};

	// empty initializier list. Using placement new
	// so that I know what the memory is set to.
	reset();
	cout << "i is: " << i << endl;
	new (&i) int{};
	cout << "after empty initialization list: " << i << endl;

	reset();
	new (&i) int{1};
	cout << "after non-empty initialization list: " << i << endl;

	reset();
	new (&i) int();
	cout << "after default initialization: " << i << endl;

	reset();
	new (&i) int(2);
	cout << "after explicit initizliation: " << i << endl;
}
