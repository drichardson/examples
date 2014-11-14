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

// TODO:DOUG Analyze this. Explain how it works.
// Number of iterations through loop is count^2.
// Time complexity is O(n^2)
// Space complexity is O(1) auxillary space.
template<typename Container, typename LessThan>
void stupid_sort(Container & items, LessThan lt)
{
    using index = decltype(items.size());
    auto count = items.size();
    for(index i = 0; i < count; ++i) {
        for(index j = 0; j < count; ++j) {
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
    using index = decltype(items.size());
    auto n = items.size();
    bool swapped;
    do {
        swapped = false;
        for(index i = 1; i < n; ++i) {
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
// Time complexity O(n^2)
// Average case O(n log n)
// Space complexity O(n) auxillary space since the partition could
// in the worst case have one recursive call for each item in the list.

template <typename Container, typename LessThan> 
struct quicksort_internal {
    using index = decltype(((Container*)0)->size());

    static index choosePivot(Container & items,
            index left, index right) {
        return right;
    }

    static index partition(Container & items, LessThan lt, index left, index right) {
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

    static void quicksort(Container & items, LessThan lt, index i, index k) {
        if (i < k) {
            auto p = partition(items, lt, i, k);
            if (p > 0) quicksort(items, lt, i, p-1);
            quicksort(items, lt, p+1, k);
        }
    }
};

template <typename Container, typename LessThan> 
void quick_sort(Container & items, LessThan lt) {
    auto size = items.size();
    // make sure size != 0, otherwise size-1 will underflow if size is an unsigned type.
    if (size == 0) return;
    quicksort_internal<Container,LessThan>::quicksort(items, lt, 0, size - 1);
}

// Insertion sort inspired by psuedo code on http://en.wikipedia.org/wiki/Insertion_sort
// Worst case array is reverse sorted, so the inner
// loop has to perform i compares and swaps for each run of the outer loop. That's
// (1+2+3+4+...+N) = N(N+1)/2 = (N^2+N)/2 compares and swaps, which means quadratic worst
// case behavior.
// Time complexity: O(n^2)
// Space complexity: O(1)
template <typename Container, typename LessThan> 
void insertion_sort(Container & items, LessThan lt) {
    using index = decltype(items.size());
    auto len = items.size();
    for(index i = 1; i < len; ++i) {
        for(auto j = i; j > 0 and lt(items[j], items[j-1]); --j) {
            swap(items[j], items[j-1]);
        }
    }
}

// Divide list into sorted and unsorted portions. Initially
// sorted contains no items and unsorted contains all items.
// Select the minimum value in the unsorted portion and swap it
// with the first value in the unsorted portion. Now, the sorted has
// increased in length by 1 and unsorted has decreased in length by one.
// Continue doing this until the unsorted portion is empty.
// To select the minimum value in the unsorted portion requires
// |unsorted| comparisons. This must be done N times, since only one
// value is moved from the unsorted to sorted at a time. Therefore,
// the number of comparisons is N+(N-1)+...+1=N(N+1)/2=(N^2+N)/2.
// Time complexity: O(n^2)
// Space complexity: O(1)
template <typename Container, typename LessThan> 
void selection_sort(Container & items, LessThan lt) {
    using index = decltype(items.size());
    auto len = items.size();
    index sorted = 0;
    for(index i = 0; i < len; ++i) {
        // find minimum value in unsorted portion.
        int min = i;
        for(index j = i+1; j < len; ++j) {
            if (lt(items[j], items[min])) {
                min = j;
            }
        }
        swap(items[min], items[i]);
    }
}

}
