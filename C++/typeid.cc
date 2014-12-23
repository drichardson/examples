#include <iostream>
#include <typeinfo>
#include <boost/type_index.hpp>
#include <boost/current_function.hpp>

using namespace std;

#define print_type(x) print_type_internal<decltype(x)>(#x)

template <typename T>
void print_type_internal(char const* msg)
{
    using std::cout;
    using std::endl;
    using boost::typeindex::type_id_with_cvr;

    cout << msg
        << ": typeid.name()=" << typeid(T).name()
        << ", boost pretty_name()=" << type_id_with_cvr<T>().pretty_name()
        << endl;
}

class SomeClass {
public:
    int x;
};

// My simple version that dumps the typename info, using the current
// function macro provided by the compiler. Boost does a more work to get
// const, ref, volatileness out.
template <typename T>
void my_typename(char const* msg, T) {
    cout << "my_typename_with_crv: " << BOOST_CURRENT_FUNCTION << endl;

    // equivalently, on clang++/g++
    //cout << "my_typename_with_crv: " << __PRETTY_FUNCTION__ << endl;
}

int main() {
    typedef int myi;
    using myi2 = int;
    myi i1 = 123;
    myi2 i2 = 456;
    SomeClass swc;
    SomeClass const & swc_ref = swc;
    SomeClass swc_array[3];
    auto const & swc_array_ref = swc_array;
    auto const* swc_array_ptr = swc_array;
    (void)swc_array_ptr; // silience scan-build unused value warning.

    cout << "IDs: ";
    print_type(i1);
    print_type(i2);
    print_type(swc);
    print_type(swc_ref);
    print_type(swc_array);
    print_type(swc_array_ref);
    print_type(swc_array_ptr);

    cout << "something:" << endl;
    my_typename("i1", i1);
    my_typename("swc_ref", swc_ref);
    my_typename("swc_array_ref", swc_array_ref);
}
