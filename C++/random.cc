#include <algorithm>
#include <array>
#include <iomanip>
#include <iostream>
#include <numeric>
#include <random>

using namespace std;

int main()
{

	constexpr unsigned samples = 20000;
	constexpr unsigned min_val = 1;
	constexpr unsigned max_val = 10; // inclusive range
	static_assert(max_val > min_val);
	constexpr unsigned N = max_val - min_val + 1;

	random_device rd;

	cout << "Read 10 values from random_device:\n";
	for (unsigned i = 0; i < 10; ++i)
	{
		cout << rd() << ' ';
	}
	cout << '\n';

	cout
	    << "Generate histogram of " << samples
	    << " samples from a uniform distribution from the random device:\n";

	array<int, N> frequency{};
	uniform_int_distribution<unsigned> dist(min_val, max_val);

	for (unsigned i = 0; i < samples; ++i)
	{
		++frequency[dist(rd) - min_val];
	}

	auto total = accumulate(frequency.begin(), frequency.end(), 0);
	assert(total == samples);

	array<float, N> percents;
	transform(
	    frequency.begin(),
	    frequency.end(),
	    percents.begin(),
	    [](unsigned count) { return static_cast<float>(count) / samples; });

	unsigned max_val_str_width = to_string(max_val).size();

	constexpr unsigned max_stars = 72;

	for (unsigned i = 0; i < N; ++i)
	{
		unsigned const value = min_val + i;
		unsigned const count = frequency[i];
		unsigned const stars = max_stars * percents[i];

		cout << '[' << setw(max_val_str_width) << value << "]: ";

		for (unsigned j = 0; j < stars; ++j)
		{
			cout << '*';
		}

		cout << ' ' << count << ' ' << percents[i] * 100.0f << "\n";
	}

	return 0;
}
