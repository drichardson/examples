#include <iostream>
#include <functional>
#include <stdint.h>
#include <vector>
#include <string>

void stdlib_hashes() {
    using std::hash;
    using std::cout;

    std::hash<bool> h0;
    std::hash<std::string> h1;
    std::hash<int> h2;

    cout << "bool: false=" << h0(false) << ", true=" << h0(true) <<'\n';
    cout << "string: hi=" << h1("hi") << ", hj: " << h1("hj") << '\n';
    cout << "int: 1=" << h2(1) << ", 2=" << h2(2) << ", 3=" << h2(3) << '\n';
}

// FNV-1 hash function
uint32_t fnv1_32(void const* datav, size_t data_len ) {
    uint8_t const *data = static_cast<uint8_t const*>(datav);
    constexpr uint32_t offset_basis = 2166136261;
    uint32_t hash = offset_basis;
    uint32_t constexpr magic_fnv1_prime32 = 16777619;
    uint8_t const * const end = data+data_len;
    for(; data < end; ++data) {
        hash *= magic_fnv1_prime32;
        hash ^= *data;
    }
    return hash;
}

void myhashes() {
    std::vector<std::string> v = {
        {"hello, world"},
        {"hello, worlD"},
        {"hello,world"},
        {"hi"},
        {"hi0"}
    };

    for(auto s : v) {
        std::cout << "hash(" << s << ")=" << fnv1_32(s.c_str(), s.size()) << '\n';
    }
}

int main() {
    stdlib_hashes();
    myhashes();
}

