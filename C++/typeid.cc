#include <iostream>
#include <typeinfo>
#include <boost/type_index.hpp>

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

int main(int const argc, char const* const* argv) {
    typedef int myi;
    using myi2 = int;
    myi i1 = 123;
    myi2 i2 = 456;
    SomeClass swc;
    SomeClass const & swc_ref = swc;
    SomeClass swc_array[3];
    auto const & swc_array_ref = swc_array;
    auto const* swc_array_ptr = swc_array;

    cout << "IDs: ";
    print_type(i1);
    print_type(i2);
    print_type(swc);
    print_type(swc_ref);
    print_type(swc_array);
    print_type(swc_array_ref);
    print_type(swc_array_ptr);

    return 0;
}
