#include <iostream>
#include <vector>

template <class Iter>
void print_all(Iter start, Iter end)
{
    std::cout << "PRINT ALL:\n";
    for(Iter i = start; i < end; ++i) {
        std::cout << "\t" << *i << "\n";
    }
}

int main()
{
    using std::cout;

    // Use an STL iterator
    std::vector<int> iv{{1,2,3}};
    print_all(iv.begin(), iv.end());

    // Use a const pointer as an iterator with print_all
    float fa[] = {4.1, 5.2, 6.3, 7.4};
    float* fa_end = fa+(sizeof(fa)/sizeof(fa[0]));
    print_all((const float*)fa, (const float*)fa_end);

    // Use a pointer as an iterator with std::copy and print_all.
    float fa2[8];
    constexpr int fa2_count = sizeof(fa2)/sizeof(fa2[0]);
    float* fa2_end = fa2+fa2_count;
    for(auto i = fa2; i != fa2_end; ++i) {
        *i = 100;
    }
    std::copy(fa, fa_end, fa2);
    print_all(fa2, fa2_end);

    return 0;
}

