#include <algorithm>
#include <array>
#include <cassert>
#include <cmath>
#include <functional>
#include <iomanip>
#include <iostream>
#include <numeric>
#include <random>

using namespace std;

constexpr unsigned samples = 20000;
constexpr unsigned min_val = 1;
constexpr unsigned max_val = 10; // inclusive range
static_assert(max_val > min_val);
constexpr unsigned N = max_val - min_val + 1;

using frequency_table = array<unsigned, N>;

// Returns count of items too small and too big to fit in table.
template <typename Rnd>
tuple<unsigned, unsigned> sample(Rnd &rnd, frequency_table &freq)
{
	unsigned too_small = 0;
	unsigned too_big = 0;

	for (unsigned i = 0; i < samples; ++i)
	{
		int const val = round(rnd());

		if (val < static_cast<int>(min_val))
		{
			++too_small;
		}
		else if (val > static_cast<int>(max_val))
		{
			++too_big;
		}
		else
		{
			++freq[val - min_val];
		}
	}

	return make_tuple(too_small, too_big);
}

void print_histogram(array<unsigned, N> const &frequency)
{
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
}

int main()
{

	// WARNING: using random_device directly for random numbers can cause
	// performance issues when the entropy pool is exhausted. For practical
	// applications, you should only use the random device to seed a PRNG
	// like mt19937.

	random_device rd;

	{
		cout << "Raw values from random_device:\n";
		for (unsigned i = 0; i < 5; ++i)
		{
			cout << rd() << ' ';
		}
		cout << '\n';
	}

	{
		cout << "Uniform distribution from the random device:\n";

		array<unsigned, N> frequency{};
		uniform_int_distribution<unsigned> dist(min_val, max_val);
		auto rnd = bind(dist, ref(rd));

		auto r = sample(rnd, frequency);

		// make sure no samples out of range
		assert(get<0>(r) == 0 && get<1>(r) == 0);

		print_histogram(frequency);
	}

	{
		cout << "Generate histogram of " << samples
		     << " samples from a uniform distribution from mt19937:\n";

		mt19937 mt(rd());

		array<unsigned, N> frequency{};
		uniform_int_distribution<unsigned> dist(min_val, max_val);
		auto rnd = bind(dist, ref(rd));

		auto r = sample(rnd, frequency);

		// make sure no items out of range
		assert(get<0>(r) == 0 && get<1>(r) == 0);

		print_histogram(frequency);
	}

	{
		cout << "Generate histogram of " << samples
		     << " samples from a normal distribution from mt19937:\n";

		mt19937 mt(rd());

		array<unsigned, N> frequency{};
		constexpr float mean = (max_val + min_val) / 2.f;
		constexpr float stddev = (max_val - min_val) / 4.f;
		normal_distribution<float> dist(mean, stddev);
		auto rnd = bind(dist, ref(mt));

		unsigned too_small, too_big;
		tie(too_small, too_big) = sample(rnd, frequency);
		print_histogram(frequency);

		cout << "too big: " << too_big << " too_small: " << too_small
		     << endl;
	}

	return 0;
}
