#include <iostream>
#include <fstream>

int main() {
    std::fstream o{"a.txt", std::ios::app};
    if (o.good()) {
        o << "Hi";
    }
}
