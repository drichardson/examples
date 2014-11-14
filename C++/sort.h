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

// TODO: Figure out how lucky_sort works.
// Background:
// lucky sort is so called, because it works, in spite of me not originally understanding
// why it works and with it's seemlying opposite of what you'd think is correct use of
// the lt comparison.
//
// Description:
// For each position I in the array, lucky passes over every item in the array.
// During this inner loop pass, if the item at I is less than the other item,
// item I is swapped with the other item. After the first pass through the
// outer loop, position 0 will have been compared against every other position
// in the array, and if it was found to be less than any of the other elements
// it would have been swapped, therefore it contains the max element in the array.
// So, we now have:
// [MAX | ?]
// In the second pass through the outer loop, position 1 is compared against
// every other item in the array. When it ends, it will be
// [? MAX | ?]

//
// A more descriptive name for lucky sort would be greedy sort or max sort, since
// each outer loop is taking the max element in the array for itself.
//
// ? even vs odd imporant for analysis?
// 
// Analysis:
// lucky sort's outer loop passes through each item A in the array
// in order. For each item A, it passes through the entire array
// again, swapping A with another value if A is less than that value.
// Time complexity is O(n^2)
// Space complexity is O(1).
template<typename Container, typename LessThan>
void lucky_sort(Container & items, LessThan lt)
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

// TODO: Figure out why this works.
// Description:
// stupid sort is a variation of lucky sort, but instead of the inner loop
// going from 0 to n-1, it goes from i (outer loop index) to n-1. Also, the
// lt comparison is reversed.
//
// As with lucky sort, I don't know why this works yet.
//
// Analysis:
// The outer loop runs N times. For each run through the outer loop, the inner
// performs N-i comparisons. Therefore, the number of comparisons, given
// by runs through the outer loop are: (N-1)+(N-2)+...1
// which equals N^2/2 comparisons.
// Time complexity is O(n^2)
// Space complexity is O(1).
template<typename Container, typename LessThan>
void stupid_sort(Container & items, LessThan lt)
{
    using index = decltype(items.size());
    auto count = items.size();
    for(index i = 0; i < count; ++i) {
        for(index j = i+1; j < count; ++j) {
            if(lt(items[j], items[i])) {
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
