// a big sort, but still the size that can fit in memory.
#include "sort.h"
#include <iostream>
#include <fstream>
#include <cmath>
#include <functional>
#include <iterator>
#include <vector>
#include <chrono>

using namespace std;

template<typename T>
void print_elements(T const & v) {
    char const* space = "";
    auto len = v.size();
    for(decltype(len) i = 0; i < len; ++i) {
        auto e = v[i];
        cout << space << e;
        space = " ";
    }
}

template <typename Container, typename LessThan>
void radix_sort_facade(Container & c, LessThan lt) {
    sort::radix_sort(c);
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
        { "parallel_merge", &sort::parallel_merge_sort<C,L> },
        { "quicksort", &sort::quick_sort<C,L> },
        { "merge", &sort::merge_sort<C,L> },
        { "heap", &sort::heap_sort<C,L> },
        { "radix", &radix_sort_facade<C,L> },
        { "insertion", &sort::insertion_sort<C,L> },
        { "selection", &sort::selection_sort<C,L> },
        { "bubble", &sort::bubble_sort<C, L> },
        { "lucky", &sort::lucky_sort<C, L> },
        { "stupid", &sort::stupid_sort<C, L> },
    }};
};

// comparisons disabled by default because it significantly slows down the parallel 
// merge sort. Probably because caches are being invalidated.
#define USE_COMPARISON_TRACKING 0

template <typename Container, typename LessThan>
void sort_and_report_for_each_algorithm(Container const & a, LessThan lt, ostream & out=cout) {

#if USE_COMPARISON_TRACKING
    int comparisons = 0;
    using E = decltype(a[0]);
    auto cmp = [&comparisons, &lt](E const & a, E const & b) {
        comparisons++;
        return lt(a, b);
    };
#else
    auto cmp = lt;
#endif

    sorting_algorithms<Container, decltype(cmp)> algorithms;
    for(auto const & algo : algorithms.values) {
        Container a_copy(a);
#if USE_COMPARISON_TRACKING
        comparisons = 0; // reset comparisons (incremented in cmp lambda above).
#endif

        out << " * " << algo.name << "..." << flush;

        auto start_time = chrono::high_resolution_clock::now();
        // run sort
        algo.function(a_copy, cmp);
        auto end_time = chrono::high_resolution_clock::now();
        
        out << "complete.";
#if USE_COMPARISON_TRACKING
        out << " comparisons=" << static_cast<double>(comparisons);
#endif
        out << " duration="
            << chrono::duration_cast<chrono::milliseconds>(end_time-start_time).count()
            << "ms" << flush;

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

        if (!sorted) {
            out << "ERROR: Algorithm failed to sort." << std::endl;
            abort();
        }

        out << endl;
    }
}

int lessthan(int a, int b) { return a < b; }

int main(int argc, char* argv[]) {

    double n = 1e3;

    if (argc == 2) {
        n = stod(argv[1]);
    }

    ifstream in("/dev/urandom");
    vector<int> data(n);
    cout << "data constructed\n";
    for(auto & e : data) {
        in.read(reinterpret_cast<char*>(&e), sizeof(e));
    }
    cout << "data read\n";
    cout << "n=" << n 
        << ", log2(n)=" << log2(n)
        << ", n*log2(n)=" << n*log2(n)
        << ", n^2=" << n*n
        << "\n";
    sort_and_report_for_each_algorithm(data, lessthan);
}
