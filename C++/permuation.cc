#include <algorithm>
#include <iostream>
#include <vector>

using namespace std;

template <typename T> ostream &operator<<(ostream &out, vector<T> const &vec)
{
	for (T const &v : vec)
	{
		out << v << ' ';
	}

	return out;
}

template <typename C> void print_permuations(C &&c)
{
	cout << "PERM " << c << "----------------\n";

	sort(c.begin(), c.end());

	do
	{
		cout << c << '\n';
	} while (next_permutation(c.begin(), c.end()));
}

int main()
{
	print_permuations(vector<int>{1, 2, 3});
	print_permuations(vector<float>{1.2, 2.4, 10.341});
	print_permuations(string{"doug"});

	return 0;
}
