#include <iostream>
#include <map>
#include <string>
#include <unordered_map>

using namespace std;

int main()
{
	unordered_map<string, float> height;

	height["doug"] = 1.86f;
	height["cassidy"] = 1.78f;
	height["milo"] = 2.01;

	for (auto p : height)
	{
		cout << p.first << ": " << p.second << endl;
	}

	cout << "doug: " << height["doug"] << endl;
	cout << "does not exist: " << height["does not exist"] << endl;

	return 0;
}
