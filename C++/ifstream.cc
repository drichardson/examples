#include <iostream>
#include <fstream>
#include <cstring>

int main() {
    using namespace std;
    char const* file = "/dev/urandom";
    cout << "Opening " << file << endl;
    fstream in(file);
    cout << "fstream constructed. good? " << in.good() << endl;
    char c = 0;
    short s = 0;
    int i = 0;

    cout << "reading c..." << endl;
    in >> c;

    cout << "reading s..." << endl;
    in.read(reinterpret_cast<char*>(&s), sizeof(s));

    cout << "reading i..." << endl;
    in.read(reinterpret_cast<char*>(&i), sizeof(i));

    cout << "Got c=" << static_cast<short>(c) << ", s=" << s << ", i=" << i << endl;
}

