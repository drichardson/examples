#include "sort.h"
#include <array>
#include <boost/type_index.hpp>
#include <functional>
#include <iostream>
#include <vector>
#include <deque>
#include <queue>

template <typename T>
constexpr std::string typestring() {
    return boost::typeindex::type_id_with_cvr<T>().pretty_name();
}

template <typename T>
constexpr std::string typestring(T t) {
    return typestring<T>();
}
 
template<typename T>
void print_elements(std::ostream& out, T const & v) {
    char const* space = "";
    auto len = v.size();
    for(decltype(len) i = 0; i < len; ++i) {
        auto e = v[i];
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
void sort_and_report_for_each_algorithm(std::ostream & out, Container a, LessThan lt) {

    int comparisons = 0;
    using E = typename Container::value_type;
    auto cmp = [&comparisons, &lt](E const & a, E const & b) {
        comparisons++;
        return lt(a, b);
    };

    out << "input={"; print_elements(out, a); out << "}\n";

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

        if (!sorted) {
            out << "ERROR: Algorithm failed to sort." << std::endl;
            abort();
        }
    }
}

void print_title(std::ostream & out, std::string const & title) {
    std::string border(title.size(), '=');
    out << border << '\n' << title << '\n' << border << '\n';
}

template <typename Container>
constexpr std::string test_battery_title() {
    return "Test Battery for Container: " + typestring<Container>();
}

template <typename Container, typename Compare>
void run_int_sort_test_battery(std::ostream & out, Compare compare) {

    print_title(out, test_battery_title<Container>());

    auto test = [&out, compare](Container v) {
        sort_and_report_for_each_algorithm(out, v, compare);
    };

    // empty
    test(Container());

    // single element
    test(Container{{1}}); 

    // 2 elements
    test(Container{{1, 2}}); // arragenement 1 of 2
    test(Container{{2, 1}}); // arragenement 2 of 2

    // 3 elements
    test(Container{{1, 2, 3}}); // arrangement 1 of 6
    test(Container{{1, 3, 2}}); // arrangement 2 of 6
    test(Container{{2, 1, 3}}); // arrangement 3 of 6
    test(Container{{2, 3, 1}}); // arrangement 4 of 6
    test(Container{{3, 1, 2}}); // arrangement 5 of 6
    test(Container{{3, 2, 1}}); // arrangement 6 of 6

    // duplicates
    test(Container{{1, 1}}); 
    test(Container{{1, 1, 1}}); 
}

bool int_less_than(int const & a, int const & b) {
    return a < b;
}

bool int_greater_than(int const & a, int const & b) {
    return a > b;
}

template <typename T>
class MinimalContainer {
public:
    using value_type = T;
    using size_type = size_t;

    MinimalContainer(T* a, size_type s) : _array(a), _size(s) {}

    value_type & operator[](size_type index) { return _array[index]; }
    value_type const & operator[] (size_type index) const { return _array[index]; }
    size_type size() const { return _size; }

private:
    value_type* _array = nullptr;
    size_type _size = 0;
};


int main() {
    auto & out = std::cout;
    run_int_sort_test_battery<std::vector<int>>(out, int_less_than);
    run_int_sort_test_battery<std::vector<int>>(out, int_greater_than);
    run_int_sort_test_battery<std::deque<int>>(out, int_less_than); // non-vector, random access container

    print_title(out, "std::array Test");
    sort_and_report_for_each_algorithm(out, std::array<int, 3>{{1, 3, 2}}, int_less_than); // array test

    // test minimal container
    print_title(out, "MinimalContainer Test");
    int data[] = { 3, 1, 2 };
    MinimalContainer<int> min_container{static_cast<int*>(data), static_cast<size_t>(3)};
    sort_and_report_for_each_algorithm(out, min_container, int_less_than); // array test

    // test something besides ints
    print_title(out, "string Test");
    std::vector<std::string> as{{ "hello", "computer", "space", "zero", "about", "fortunate" }};
    sort_and_report_for_each_algorithm(out, as, [](auto const & a, auto const & b) { return a < b; });
}

