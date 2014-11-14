#pragma once
#include <functional>
#include <iostream>
#include <type_traits>

namespace sort {

template<typename T>
void swap(T &a, T &b) {
    T tmp = a;
    a = b;
    b = tmp;
}

// Number of iterations through loop is count^2.
// Time complexity is O(n^2)
// Space complexity is O(1)
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


template<typename Container, typename LessThan>
void bubble_sort(Container & items, LessThan lt) {
}

}
