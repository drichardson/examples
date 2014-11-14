#pragma once
#include <functional>
#include <iostream>

namespace sort {


// All sort algorithms sort their input in acending order, assuming
// the LessThan comparison function provided is actually less than.
// If LessThan comparison function is instead defined to be greater
// than, the order will be reversed.

template<typename T>
void swap(T &a, T &b) {
    T tmp = a;
    a = b;
    b = tmp;
}

// Number of iterations through loop is count^2.
// Time complexity is O(n^2)
// Space complexity is O(1) auxillary space.
template<typename Container, typename LessThan>
void stupid_sort(Container & items, LessThan lt)
{
    auto count = items.size();
    for(decltype(count) i = 0; i < count; ++i) {
        for(decltype(count) j = 0; j < count; ++j) {
            if(lt(items[i], items[j])) {
                swap(items[i], items[j]);
            }
        }
    }
}


// Number of iterations through loop is dependent on the order of the data.
// Time complexity is O(n^2), because in the worst case the largest
// item is at the beginning, which means the outer loop has to run
// n times, and for each of those, the inner loop runs n-1 times.
// Space complexity is O(1) auxillary space.
template<typename Container, typename LessThan>
void bubble_sort(Container & items, LessThan lt) {
    auto n = items.size();
    bool swapped;
    do {
        swapped = false;
        for(decltype(n) i = 1; i < n; ++i) {
            auto & left = items[i-1];
            auto & right = items[i];
            if (lt(right, left)) {
                swap(right, left);
                swapped = true;
            }
        }
    } while(swapped);
}

}
