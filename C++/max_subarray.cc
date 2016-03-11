#include <algorithm>
#include <iostream>
#include <vector>

struct max_subarray_result {
    unsigned start = 0;
    unsigned end = 0;
    int value = 0;
    std::vector<int> maxes;
};

max_subarray_result max_subarray_kadane(std::vector<int> const & v) {
    max_subarray_result r;
    r.maxes.resize(v.size());
    if (v.size() == 0) return r;

    int max_ending_here = v[0];
    int max_so_far = v[0];
    unsigned start = 0;
    unsigned max_start = start;
    unsigned end = 0;
    for(unsigned i = 1; i < v.size(); ++i) {
        auto e = v[i];

        if (e > max_ending_here+e) {
            max_ending_here = e;
            start = i;
        } else {
            max_ending_here += e;
        }

        max_so_far = std::max(max_so_far, max_ending_here);
        if (max_so_far > max_ending_here) {
            // same
        } else {
            max_so_far = max_ending_here;
            max_start = start;
            end = i;
        }
    }
    r.value = max_so_far;
    r.start = max_start;
    r.end = end;
    return r;
}

void print_max_subarray(std::vector<int> const & v) {
    using std::vector;
    using std::cout;
    for(auto e : v) {
        cout << e << ' ';
    }
    cout << '\n';
    auto r = max_subarray_kadane(v);
    cout << "max range=[ " << r.start << "," << r.end
        << "] value: " << r.value
        << "\n";
}

int main() {
    print_max_subarray({1,2,3,4,5});
    print_max_subarray({1,2,-3,4,5});
    print_max_subarray({1,2,-4,4,5});
    print_max_subarray({-10, -40, -100});
    print_max_subarray({0,0,1,0,0,-1,2,-1,0});
}

