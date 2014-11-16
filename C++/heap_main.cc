#include "heap.h"
#include <array>
#include <iostream>
#include <vector>

bool lessthan(int a, int b) { return a < b; }

template<typename Container>
void print_elements(std::ostream& out, Container const & v) {
    char const* space = "";
    auto len = v.size();
    for(decltype(len) i = 0; i < len; ++i) {
        auto e = v[i];
        out << space << e;
        space = " ";
    }
}

template <typename Container, typename Compare>
bool is_heap(Container const & v, Compare lessthan, typename Container::size_type node=0) {
    if (node >= v.size()) return true;
    auto left = node * 2 + 1;
    auto right = node * 2 + 2;
    if (left < v.size() and lessthan(v[left], v[node])) return false;
    if (right < v.size() and lessthan(v[right], v[node])) return false;
    return is_heap(v, lessthan, left) and is_heap(v, lessthan, right);
}

template <typename Container, typename Compare>
void heap_test(std::ostream & out, Container && v, Compare compare) {
    out << "input: {"; print_elements(out, v); out << "}  is_heap? " << is_heap(v, compare) << "\n";
    heap::build_heap(v, v.size(), compare);
    auto ok = is_heap(v, compare);
    out << "  heapified: {"; print_elements(out, v); out << "}  is_heap? " << ok << "\n";
    if (!ok) {
        out << "heapify didn't make it a heap.\n";
        out.flush();
        abort();
    }
}

int main() {
    std::ostream & out = std::cout;
    using ivec = std::vector<int>;

    auto test = [&out](ivec&& v) {
        heap_test(out, v, lessthan);
    };


    test(ivec{{1,3,4,2}});

    // empty
    test(ivec());

    // 1 object
    test(ivec{1});

    // 2 objects
    test(ivec{{1,2}});
    test(ivec{{2,1}});

    // 3 objects
    test(ivec{{1,2,3}});
    test(ivec{{1,3,2}});
    test(ivec{{2,1,3}});
    test(ivec{{2,3,1}});
    test(ivec{{3,1,2}});
    test(ivec{{3,2,1}});

    // 4 objects
    test(ivec{{1,2,3,4}});
    test(ivec{{1,2,4,3}});
    test(ivec{{1,3,2,4}});
    test(ivec{{1,3,4,2}});
    test(ivec{{1,4,2,3}});
    test(ivec{{1,4,3,2}});
    test(ivec{{2,1,4,3}});
    test(ivec{{2,1,3,4}});
    test(ivec{{2,3,4,1}});
    test(ivec{{2,3,1,4}});
    test(ivec{{2,4,3,1}});
    test(ivec{{2,4,1,3}});
    test(ivec{{3,1,2,4}});
    test(ivec{{3,1,4,2}});
    test(ivec{{3,2,1,4}});
    test(ivec{{3,2,4,1}});
    test(ivec{{3,4,1,2}});
    test(ivec{{3,4,2,1}});
    test(ivec{{4,1,3,2}});
    test(ivec{{4,1,2,3}});
    test(ivec{{4,2,3,1}});
    test(ivec{{4,2,1,3}});
    test(ivec{{4,3,2,1}});
    test(ivec{{4,3,1,2}});

    
    // duplicates
    test(ivec{{1,1}});

    // ad hoc
    test(ivec{{5, 1, 8, 4, 9, 1, 5, 2, 1, 5, 1, 6}});
}

