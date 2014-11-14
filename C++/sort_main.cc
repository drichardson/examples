#include "sort.h"
#include <array>
#include <boost/type_index.hpp>
#include <functional>
#include <iostream>
#include <vector>
 
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
        { "bubble", &sort::bubble_sort<C, L> },
        { "quicksort", &sort::quicksort<C,L> }
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

    std::ostream & out = std::cout;

    out << "=== Sort "
        << boost::typeindex::type_id_with_cvr<typename Container::value_type>().pretty_name()
        << " Test ===\n";
    out << "container type: "
        << boost::typeindex::type_id_with_cvr<Container>().pretty_name()
        << "\n";
    out << "input size: " << a.size() << "\n";
    out << "input: {"; print_elements(out, a); out << "}\n";

    sorting_algorithms<Container, decltype(cmp)> algorithms;
    for(auto const & algo : algorithms.values) {
        Container a_copy = a;
        comparisons = 0; // reset comparisons (incremented in cmp lambda above).

        // run sort
        algo.function(a_copy, cmp);

        // verify sort is correct
        bool sorted = true;
        for(typename Container::size_type i = 1; i < a_copy.size(); ++i) {
            if (lt(a_copy[i], a_copy[i-1])) {
                sorted = false;
                break;
            }
        }

        // display status
        out << " * " << algo.name << "=>{"; print_elements(out, a_copy);
        out << "} in " << comparisons << " comparisons. "
            << (sorted ? "sorted" : "!!!NOT SORTED!!!")
            << "\n";

        if (!sorted) abort();
    }
}

int main() {
#if 0
    std::vector<int> ad{{ 1, 3, 2 }};
    run_sort_tests(ad, [](auto a, auto b) { return a < b; });
#endif
#if 1
    std::array<char, 10> ac{{ 'D', 'A', 'Z', 'U', 'N', 'M', 'B', 'A', 'C', 'E' }};
    run_sort_tests(ac, [](auto a, auto b) { return a < b; });

    std::vector<int> ai{{ 1, 5, 10, -100, 2, 5, 0, 1, 5 }};
    run_sort_tests(ai, [](auto a, auto b) { return a < b; });

    std::vector<std::string> as{{ "hello", "computer", "space", "zero", "about", "fortunate" }};
    run_sort_tests(as, [](auto & a, auto & b) { return a < b; });
#endif
}
