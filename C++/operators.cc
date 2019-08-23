#include <cstddef>
#include <iostream>
#include <typeinfo>

#define print_expr(expr) print_expr_internal(#expr, expr)

template <typename T>
void print_expr_internal(const char* msg, T&& value) {
    std::cout << msg << " = " << value
        << " of type " << typeid(T).name()
        << std::endl;
}

constexpr long double operator "" _K(long double v) {
    return v * 1e3;
}

const long double operator "" _M(long double v) {
    return v * 1e6;
}

struct unaligned {
    char c1;
    int i1;
};

struct alignedInt {
    char c1;
    alignas(sizeof(int)) int i1;
};

struct alignedIntx2 {
    char c1;
    alignas(sizeof(int)*2) int i1;
};

struct alignedIntx4 {
    char c1;
    alignas(sizeof(int)*4) int i1;
};

int main() {
    // newer boolean and bitwise operator names.
    print_expr(false and 1);
    print_expr(false && 1);

    print_expr(false or 1);
    print_expr(false || 1);

    print_expr(1 bitor 2);
    print_expr(1 | 2);

    print_expr(1 bitand 2);
    print_expr(1 & 2);

    // alignof and alignas
    print_expr(alignof(unaligned));
    print_expr(alignof(alignedInt));
    print_expr(alignof(alignedIntx2));
    print_expr(alignof(alignedIntx4));
    print_expr(offsetof(unaligned, i1));
    print_expr(offsetof(alignedIntx4, i1));
    print_expr((size_t)&((alignedIntx4*)0)->i1); // my own offsetof for shits and giggles

    // user defined literals
    print_expr(2.0);
    print_expr(2.0_K);
    print_expr(2.0_M);
}

