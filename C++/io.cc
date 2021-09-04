#include <iostream>

using namespace std;

int main()
{
	string s1, s2, s3;
	getline(cin, s1, '\n');
	getline(cin, s2);
	getline(cin, s3);

	cout << "|" << s1 + s2 + s3 << "|" << endl;

	cout << "s1: " << s1 << "\n";
	cout << "s2: " << s2 << "\n";
	cout << "s3: " << s3 << "\n";

	return 0;
}
