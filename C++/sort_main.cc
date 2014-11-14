#include "sort.h"
#include <boost/type_index.hpp>
#include <functional>
#include <iostream>
#include <vector>
#include <array>
 
template<typename T>
void print_elements(std::ostream& out, T const & v) {
    char const* space = "";
    for(auto const & e : v) {
        out << space << e;
        space = " ";
    }
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
    cout << "input: {"; print_elements(cout, a); cout << "}\n";

    sorting_algorithms<Container, decltype(cmp)> algorithms;
    for(auto const & algo : algorithms.values) {
        Container a_copy = a;
        comparisons = 0; // reset comparisons (incremented in cmp lambda above).
        algo.function(a_copy, cmp);
        cout << " * " << algo.name << "=>{"; print_elements(cout, a_copy);
        cout << "} in " << comparisons << " comparisons.\n";
    }
}

int main() {
    std::array<char, 10> ac = { 'D', 'A', 'Z', 'U', 'N', 'M', 'B', 'A', 'C', 'E' };
    run_sort_tests(ac, [](auto a, auto b) { return a < b; });
    std::vector<int> ai = { 1, 5, 10, -100, 2, 5, 0, 1, 5 };
    run_sort_tests(ai, [](auto a, auto b) { return a < b; });
    std::vector<std::string> as = {{ "hello", "computer", "space", "zero", "about", "fortunate" }};
    run_sort_tests(as, [](auto & a, auto & b) { return a < b; });
}
