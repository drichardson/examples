#include <iostream>
#include <limits>

template <typename T>
void dump()
{
	using l = std::numeric_limits<T>;
	std::cout << "min,max=(" << l::min() << ',' << l::max() << ")\n";
}

int main()
{
	std::cout << "int min,max: " << std::numeric_limits<int>::min() << ','
		  << std::numeric_limits<int>::max() << std::endl;

	using il = std::numeric_limits<int>;

	std::cout << "using int min,max: " << il::min() << ',' << il::max()
		  << std::endl;

	dump<unsigned>();
	dump<int>();
	dump<long>();
	dump<long long>();
	dump<float>();

	return 0;
}
