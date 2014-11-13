#include <iostream>

using namespace std;

int main(int const argc, char const* const* argv) {
    unsigned int i = 0xdeadbeef;
    cout << "i is " << hex << i << endl;

    // empty initializier list. Using placement new
    // so that I know what the memory is set to.
    decltype(i)* pi = new(&i) decltype(i){};
    cout << "pi is " << *pi << endl;
    return 0;
}

