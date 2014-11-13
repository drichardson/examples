#include <array>
#include <boost/type_index.hpp>
#include <iostream>

#define print_type(x) print_type_internal<decltype(x)>(#x)

template <typename T>
void print_type_internal(char const* msg)
{
    using boost::typeindex::type_id_with_cvr;
    std::cout << msg << "=" << type_id_with_cvr<T>().pretty_name() << std::endl;
}

// demonstrate user of auto for return type. Auto here has nothing to do with
// type deduction, it's simply letting you postpone specifying the return type
// until after the function parameters, which lets us use decltype on one of
// the parameters.
template <typename Container, typename Index>
auto returnElementAtIndex(Container& c, Index i) -> decltype(c[i]) {
    return c[i];
}

int main() {
    // see how auto/decltype handle braced initialization (std::initializer_list),
    // parens (like assignment), and assignment (like parens).
    // Also see how decltype handles expressions like x (as you'd expect) vs
    // expressions like (x) (which are references to x).
    auto x{1}; print_type(x); print_type( (x) );
    auto y(1); print_type(y); print_type( (y) );
    auto z = 1; print_type(z); print_type( (z) );
    const auto & zr = z; print_type(zr);

    std::array<std::string, 3> a{{"string1", "string2", "string3"}}; print_type(a);
    auto e1 = returnElementAtIndex(a, 2); print_type(e1);
    auto && e2 = returnElementAtIndex(a, 2); print_type(e2); // returnElementAtIndex returns l-value, so e2 is &
    auto && e3 = []{ return std::string("hello, ") + std::string("world"); }(); print_type(e3); // lambda returns r value, so e3 is &&
    auto && e4 = []{ return std::string("world"); }(); print_type(e4); // ditto e4 is &&
    auto && e5 = std::string("world"); print_type(e5); // ditto e5 is &&
    auto s1 = std::string("world"); print_type(s1);
    auto && e6 = s1; print_type(e6);
    //auto & e7 = std::string("world"); // illegal, because can't bind & to rvalue.

    auto f = returnElementAtIndex<std::array<int, 3>, unsigned>; print_type(f);
    print_type_internal<int()>("void()");
    int f2(void); print_type(f2);
    print_type_internal<decltype(f2())>("decltype(f2())");
}

