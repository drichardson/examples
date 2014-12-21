#include <vector>
#include <iostream>
#include <algorithm>

using namespace std;

int main() {
    vector<int> a = {1, 3, 4};
    vector<int> b = {4, 1, 1};
    vector<int> c = a;

    cout << "a > b: " << (a > b) << endl;
    cout << "a < b: " << (a > b) << endl;
    cout << "a == a: " << (a == a) << endl;
    cout << "a == b: " << (a == b) << endl;
    cout << "a == c: " << (a == c) << endl;
    cout << "max(a,b) == a: " << (max(a,b) == a) << endl;
    cout << "max(a,b) == b: " << (max(a,b) == b) << endl;
}
