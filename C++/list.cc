#include <iostream>
#include <list>

using namespace std;

ostream &operator<<(ostream &out, list<int> const &l)
{
	auto it = l.begin();
	auto const end = l.end();

	if (it != end)
	{
		cout << *it;
		++it;
	}

	for (; it != end; ++it)
	{
		cout << ',' << *it;
	}

	return out;
}

int main()
{
	{
		list<int> l;
		l.push_front(1);
		l.push_back(2);
		l.push_front(0);
		cout << "l: " << l << endl;
		l.reverse();
		cout << "reverse: " << l << endl;
		l.sort();
		cout << "sort: " << l << endl;
	}

	{
		list<int> l1, l2;

		l1.push_front(1);
		l1.push_front(5);
		l1.push_front(3);

		l2.push_front(2);
		l2.push_front(6);
		l2.push_front(4);

		cout << "l1: " << l1 << endl;
		cout << "l2: " << l2 << endl;

		l1.sort();
		l2.sort();
		cout << "SORT\n";
		cout << "l1: " << l1 << endl;
		cout << "l2: " << l2 << endl;

		l1.merge(l2);

		cout << "MERGE\n";
		cout << "l1: " << l1 << endl;
		cout << "l2: " << l2 << endl;
	}

	{
		list<int> l1, l2;

		l1.push_back(1);
		l1.push_back(100);

		l2.push_back(10);
		l2.push_back(8);
		l2.push_back(11);

		cout << "l1: " << l1 << endl;
		cout << "l2: " << l2 << endl;

		auto itr = l1.begin();
		++itr;
		l1.splice(itr, l2);

		cout << "SPLICE\n";
		cout << "l1: " << l1 << endl;
		cout << "l2: " << l2 << endl;

		l2.push_back(50);
		l2.push_back(48);
		l2.push_back(30);

		cout << "l2: " << l2 << endl;

		auto l2_start = l2.begin();
		l2_start++;
		l1.splice(itr, l2, l2_start, l2.end());
		cout << "SPLICE partial\n";
		cout << "l1: " << l1 << endl;
		cout << "l2: " << l2 << endl;
	}

	return 0;
}
