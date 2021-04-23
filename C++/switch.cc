#include <iostream>
#include <vector>

int main()
{
	using std::cin;
	using std::cout;
	using std::flush;

	int i = 0, j = 0;
	std::vector<char const *> trace;

	cout << "enter interger i: " << flush;
	cin >> i;

	cout << "enter integer j: " << flush;
	cin >> j;

	switch (i)
	{
	case 0:
		trace.push_back("i0");

		switch (j)
		{
		case 0:
			trace.push_back("j0");
			break;

		case 1:
			trace.push_back("j1");
			[[fallthrough]];

		default:
			trace.push_back("jdefault");
			[[fallthrough]];

		case 2:
			trace.push_back("j2");
		}

		trace.push_back("i0end");

		break;
	default:
		trace.push_back("idefault");
		break;
	}

	auto it = trace.begin();

	if (it != trace.end())
	{
		cout << *it;
		++it;
	}

	for (; it != trace.end(); ++it)
	{
		cout << "> " << *it;
	}

	cout << "\n";

	return 0;
}
