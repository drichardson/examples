#include "sort.h"
#include <array>
#include <boost/type_index.hpp>
#include <functional>
#include <iostream>
#include <climits>
#include <vector>
#include <deque>
#include <queue>

template <typename T>
std::string typestring() {
    return boost::typeindex::type_id_with_cvr<T>().pretty_name();
}

template <typename T>
std::string typestring(T t) {
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
    using value_type = decltype(((Container*)0)[0]);

    template <typename E> struct named_algorithm {
        const char* name;
        sorter_function<E> function;
    };

    std::vector<named_algorithm<value_type>> values = {{
        { "lucky", &sort::lucky_sort<C, L> },
        { "stupid", &sort::stupid_sort<C, L> },
        { "bubble", &sort::bubble_sort<C, L> },
        { "quicksort", &sort::quick_sort<C,L> },
        { "insertion", &sort::insertion_sort<C,L> },
        { "selection", &sort::selection_sort<C,L> },
        { "merge", &sort::merge_sort<C,L> },
        { "heap", &sort::heap_sort<C,L> },
    }};
};

template <typename Container>
void sort_and_report_for_radix(std::ostream & out, Container const & a) {
    // sorting routines that don't take a comparison function
    Container a_copy(a);
    sort::radix_sort(a_copy);
    out << " * radix ={"; print_elements(out, a_copy); out << "}\n";
    for(decltype(a_copy.size()) i = 1; i < a_copy.size(); ++i) {
        if (a_copy[i-1] > a_copy[i]) {
            out << "ERROR: Radix sort failed at index " << i << std::endl;
            abort();
        }
    }
}

// can't sort containers of std::string using my implementation of radix sort, so use
// SFINAE to skip this sort for strings.
template <>
void sort_and_report_for_radix(std::ostream & out, std::vector<std::string> const & a) {
    out << " * radix skipped for vector<string>" << std::endl;
}

template <typename Container, typename LessThan>
void sort_and_report_for_each_algorithm(std::ostream & out, Container const & a, LessThan lt) {

    int comparisons = 0;
    using E = decltype(a[0]);
    auto cmp = [&comparisons, &lt](E const & a, E const & b) {
        comparisons++;
        return lt(a, b);
    };

    out << "input={"; print_elements(out, a); out << "}\n";

    sorting_algorithms<Container, decltype(cmp)> algorithms;
    for(auto const & algo : algorithms.values) {
        Container a_copy(a);
        comparisons = 0; // reset comparisons (incremented in cmp lambda above).

        // run sort
        algo.function(a_copy, cmp);

        // verify sort is correct
        bool sorted = true;
        using index = decltype(a_copy.size());
        for(index i = 1; i < a_copy.size(); ++i) {
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

    sort_and_report_for_radix(out, a);
}

void print_title(std::ostream & out, std::string const & title) {
    std::string border(title.size(), '=');
    out << border << '\n' << title << '\n' << border << '\n';
}

template <typename Container>
std::string test_battery_title() {
    return "Test Battery for Container: " + typestring<Container>();
}

template <typename Container, typename Compare>
void run_int_sort_test_battery(std::ostream & out, Compare compare) {

    print_title(out, test_battery_title<Container>());

    auto test = [&out, compare](Container v) {
        sort_and_report_for_each_algorithm(out, v, compare);
    };

    // empty (0! = 1 permuation)
    test(Container{});

    // single object (1! = 1 permuation)
    test(Container{1}); 

    // 2 objects (2! = 2 permuation)
    test(Container{{1,2}});
    test(Container{{2,1}});

    // 3 objects (3! = 6 permuation)
    test(Container{{1,2,3}});
    test(Container{{1,3,2}});
    test(Container{{2,1,3}});
    test(Container{{2,3,1}});
    test(Container{{3,1,2}});
    test(Container{{3,2,1}});

    // 4 objects (4! = 24 permuation)
    test(Container{{1,2,3,4}});
    test(Container{{1,2,4,3}});
    test(Container{{1,3,2,4}});
    test(Container{{1,3,4,2}});
    test(Container{{1,4,2,3}});
    test(Container{{1,4,3,2}});
    test(Container{{2,1,4,3}});
    test(Container{{2,1,3,4}});
    test(Container{{2,3,4,1}});
    test(Container{{2,3,1,4}});
    test(Container{{2,4,3,1}});
    test(Container{{2,4,1,3}});
    test(Container{{3,1,2,4}});
    test(Container{{3,1,4,2}});
    test(Container{{3,2,1,4}});
    test(Container{{3,2,4,1}});
    test(Container{{3,4,1,2}});
    test(Container{{3,4,2,1}});
    test(Container{{4,1,3,2}});
    test(Container{{4,1,2,3}});
    test(Container{{4,2,3,1}});
    test(Container{{4,2,1,3}});
    test(Container{{4,3,2,1}});
    test(Container{{4,3,1,2}});

    // duplicate objects
    test(Container{{1,1}}); 
    test(Container{{1,2,1,3,2}}); 

    // negative values
    test(Container{{1,-1}}); 
    test(Container{{-1,1}}); 

    // edge values
    test(Container{{INT_MAX,INT_MIN}});
    test(Container{{INT_MIN,INT_MAX}});
    test(Container{{INT_MIN,-1,0,1,INT_MAX}}); 
    test(Container{{INT_MAX,1,0,-1,INT_MIN}});
}

bool int_less_than(int const & a, int const & b) {
    return a < b;
}

bool int_greater_than(int const & a, int const & b) {
    return a > b;
}

template <typename T>
class MinimalContainer {
private:
    using size_type = size_t;
    using value_type = T;
    value_type* _array = nullptr;
    size_type _size = 0;
    bool _owns_array = false;

public:
    // The following two functions are the required container interface for
    // all algorithms in the sort namespace.
    // 1. non-const container subscript operator
    value_type & operator[](size_type index) { return _array[index]; }
    // 2. container size operator
    size_type size() const { return _size; }


    // Other public functions are not required by algorithms in the sort
    // namespace, but are used to construct and display objects in this file.
public:
    // Weak, unsafe reference to underlying array. Don't do this in real life.
    explicit MinimalContainer(T* a, size_type s) : _array(a), _size(s) {}
    // copy constructor used by test runner, to copy input array so it can sort
    // multiple times.
    explicit MinimalContainer(MinimalContainer const& rhs) :
        _array(nullptr), _size(rhs._size), _owns_array(true)
    {
        _array = new T[_size];
        for(size_type i = 0; i < _size; ++i) {
            _array[i] = rhs._array[i];
        }
    }

    explicit MinimalContainer(size_type size) :
        _array(nullptr), _size(size), _owns_array(true) {
       _array = new T[_size];     
    }

    ~MinimalContainer() {
        if (_owns_array) {
            delete[] _array;
        }
    }

    MinimalContainer() = delete;
    MinimalContainer(MinimalContainer&&) = delete;
    MinimalContainer& operator=(MinimalContainer const&) = delete;

    // required by print_elements
    value_type const & operator[] (size_type index) const { return _array[index]; }
};


int main() {
    auto & out = std::cout;
    run_int_sort_test_battery<std::vector<int>>(out, int_less_than);
    run_int_sort_test_battery<std::vector<int>>(out, int_greater_than);
    run_int_sort_test_battery<std::deque<int>>(out, int_less_than); // non-vector, random access container

    // TODO: merge_sort broke the array test. Fix.
#if 0
    print_title(out, "std::array Test");
    sort_and_report_for_each_algorithm(out, std::array<int, 3>{{1, 3, 2}}, int_less_than); // array test
#endif

    // test minimal container
    print_title(out, "MinimalContainer Test");
    int data[] = { 3, 1, 2 };
    MinimalContainer<int> min_container{static_cast<int*>(data), static_cast<size_t>(3)};
    sort_and_report_for_each_algorithm(out, min_container, int_less_than); // array test

    // TODO: radix sort breaks this. Fix.
    // test something besides ints
    print_title(out, "string Test");
    std::vector<std::string> as{{ "hello", "computer", "space", "zero", "about", "fortunate" }};
    sort_and_report_for_each_algorithm(out, as, [](auto const & a, auto const & b) { return a < b; });
}

