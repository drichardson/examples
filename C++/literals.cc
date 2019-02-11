#include <iostream>
#include <typeinfo>

#define print_expr(expr) print_expr_internal(#expr, expr)

template <typename T>
void print_expr_internal(const char* msg, T&& value) {
    std::cout << msg << " = " << value
        << " of type " << typeid(T).name()
        << std::endl;
}

int main() {
    using namespace std;
    cout << "===== Various Bases for Ints ========" << endl;
    print_expr(10);
    print_expr(010);
    print_expr(0x10);
    print_expr(0X10);
    print_expr(0b10);

    cout << "====== ways of writing floats/doubles ======" << endl;
    print_expr(10.0);
    print_expr(1e1);

    cout << "======= literal suffix to specify type =====" << endl;
    print_expr(10);
    print_expr(10UL);
    print_expr(10Ul);
    print_expr(10ul);
    print_expr(10L);
    print_expr(10l);
    print_expr(0.0); // no f suffix means double
    print_expr(0.0f);
}

