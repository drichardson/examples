#pragma once

namespace heap {

// probably should use std:swap and it's many specializations, but trying to keep
// all the magic in this file for clarity
template<typename T>
void swap(T &a, T &b) {
    T tmp = a;
    a = b;
    b = tmp;
}

// Heap implemented as binary tree in an array.
//    indexes of tree         nodes at level    level
//            0                     1             0
//      1           2               2             1
//   3    4      5     6            4             2
//  7 8  9 10  11 12 13 14          8             3
//
// Relationships:
//   2^level = nodes at level
//   left_child(node) = 2*node+1 = right_child(node)-1
//   right_child(node) = 2*node+2 = left_child(node)+1


// heapify - build a heap out of a node and 2 heap children.
// Inspired by psudo code on http://en.wikipedia.org/wiki/Binary_heap
template <typename Container, typename Ordering, typename SizeType>
void heapify(Container & items, SizeType const node, SizeType const size, Ordering lessthan)
{
    auto left = 2*node+1;
    auto right = left+1;
    auto least = node;
    if (left < size and lessthan(items[left], items[least]))
        least = left;
    if (right < size and lessthan(items[right], items[least]))
        least = right;
    if (least != node) {
        swap(items[least], items[node]);
        heapify(items, least, size, lessthan);
    }
}

// build_heap turns an array into a heap. It starts at the bottom nodes of the
// heap and works its way to the top, building sub-heaps and combining them
// into larger heaps until the entire input array is a heap.
template <typename Container, typename Ordering, typename SizeType>
void build_heap(Container & items, SizeType const size, Ordering lessthan)
{
    // The bottom layer of the heap is, by definition, made up of 1 or 0 node trees.
    // Also by definition, a 1 or 0 node tree is a heap. So, the bottom layer is skipped,
    // which explains starting at size / 2 instead of size.
    for(auto i = size / 2; i > 0;) {
        --i;
        heapify(items, i, size, lessthan);
    }
}

}

