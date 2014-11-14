#include "sort.h"
#include <boost/type_index.hpp>
#include <functional>
#include <iostream>
#include <vector>

template<typename T>
std::ostream& operator<<(std::ostream& out, std::vector<T> const & v) {
    char const* space = "";
    for(auto const & e : v) {
        out << space << e;
        space = " ";
    }
    return out;
}

template <typename Container, typename LessThan>
struct sorting_algorithms
{
    using C = Container;
    using L = LessThan;
    template <typename E> using sorter_function = std::function<void(C&, L)>;

    template <typename E> struct named_algorithm {
        const char* name;
        sorter_function<E> function;
    };

    std::vector<named_algorithm<typename Container::value_type>> values = {{
        { "stupid", &sort::stupid_sort<C, L> },
        { "bubble", &sort::bubble_sort<C, L> }
    }};
};

template <typename Container, typename LessThan>
void run_sort_tests(Container const& a, LessThan lt) {

    int comparisons = 0;
    using E = typename Container::value_type;
    auto cmp = [&comparisons, &lt](E const & a, E const & b) {
        comparisons++;
        return lt(a, b);
    };

    using std::cout;
    using std::endl;

    cout << "=== Sort "
        << boost::typeindex::type_id_with_cvr<typename Container::value_type>().pretty_name()
        << " Test ===\n";
    cout << "container type: "
        << boost::typeindex::type_id_with_cvr<Container>().pretty_name()
        << "\n";
    cout << "input size: " << a.size() << "\n";
    cout << "input: {" << a << "}\n";

    sorting_algorithms<Container, decltype(cmp)> algorithms;
    for(auto const & algo : algorithms.values) {
        Container a_copy = a;
        comparisons = 0; // reset comparisons (incremented in cmp lambda above).
        algo.function(a_copy, cmp);
        cout << " * " << algo.name << "=>{" << a_copy << "} in "
            << comparisons << " comparisons.\n";
    }
}

int main() {
    std::vector<char> ac = { 'D', 'A', 'Z', 'U', 'N', 'M', 'B', 'A', 'C', 'E' };
    run_sort_tests(ac, [](char a, char b) { return a < b; });
    std::vector<int> ai = { 1, 5, 10, -100, 2, 5, 0, 1, 5 };
    run_sort_tests(ai, [](int a, int b) { return a < b; });
}
