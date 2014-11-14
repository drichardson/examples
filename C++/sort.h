#pragma once

namespace sort {

// All sort algorithms sort their input in acending order, assuming
// the LessThan comparison function provided is actually less than.
// If LessThan comparison function is instead defined to be greater
// than, the order will be reversed.

// probably should use std:swap and it's many specializations, but trying to keep
// all the magic in this file for clarity
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


// Bubble sort inspired by psudo code on http://en.wikipedia.org/wiki/Bubble_sort
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

// Quicksort inspired by pseudo code on http://en.wikipedia.org/wiki/Quicksort

template <typename Container, typename LessThan> 
struct quicksort_internal {
    using IndexType = typename Container::size_type;

    static IndexType choosePivot(Container & items,
            IndexType left, IndexType right) {
        return right;
    }

    static IndexType partition(Container & items, LessThan lt,
            IndexType left, IndexType right) {
        auto pivotIndex = choosePivot(items, left, right);
        auto pivotValue = items[pivotIndex];
        swap(items[pivotIndex], items[right]);
        auto storeIndex = left;
        for(auto i = left; i < right; ++i) {
            if(lt(items[i], pivotValue)) {
                swap(items[i], items[storeIndex]);
                ++storeIndex;
            }
        }
        swap(items[storeIndex], items[right]);
        return storeIndex;
    }

    static void quicksort(Container & items, LessThan lt,
            IndexType i, IndexType k) {
        if (i < k) {
            auto p = partition(items, lt, i, k);
            if (p > 0) quicksort(items, lt, i, p-1);
            quicksort(items, lt, p+1, k);
        }
    }
};

template <typename Container, typename LessThan> 
void quicksort(Container & items, LessThan lt) {
    auto size = items.size();
    if (size == 0) return;
    quicksort_internal<Container,LessThan>::quicksort(items, lt, 0, size - 1);
}

}
