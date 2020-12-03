// Figure out the number of ways objects can be arranged relative to each other
// on a 1 dimentional line, assuming they cannot occupy the same location.

#include <set>
#include <iostream>

using namespace std;

template<typename E> set<E>
minus_element(set<E> const & s, E e) {
    set<E> r;
    auto pred = [e](E x) { return e != x; };
    copy_if(s.begin(), s.end(), inserter(r, r.begin()), pred);
    return r;
}

// First, figure out how many ways a fixed number of objects (4) can be arranged.
int
ways_4_objects_can_be_arranged() {
    int counter = 0;

    set<int> s1234{1,2,3,4};
    for(auto p1 : s1234) {
        auto s234 = minus_element(s1234, p1);
        for(auto p2 : s234) {
            auto s34 = minus_element(s234, p2);
            for(auto p3 : s34) {
                auto s4 = minus_element(s34, p3);
                for(auto p4 : s4) {
                    (void)p4;
                    ++counter;
                }
            }
        }
    }

    return counter;
}

// Now, figure out how many ways N objects can be arranged.
int ways_n_objects_can_be_arranged(int n) {
    int counter = 0;

    if(n == 1) {
        // there is only one way to arrange 1 object.
        return 1;
    }

    // for each object I select from N, there are n-1 objects left. Figure
    // out the number of ways to arrange n-1 objects given this initial selection.
    int nMinusOneArrangements = ways_n_objects_can_be_arranged(n-1);
    for(int i = 1; i <= n; ++i) {
        counter += nMinusOneArrangements;
    }

    return counter;
}

// Now, use multiplication instead of looping addition to show how many ways N objects can be arranged.
// This is the definition of a factorial.
int ways_n_objects_can_be_arranged_with_multiplication(int n) {
    int counter = 0;
    if (n == 1) {
        // there is only one way to arrange 1 object.
        return 1;
    }

    // for each object I select from N, there are n-1 objects left. Figure
    // out the number of ways to arrange n-1 objects given this initial selection.
    int nMinusOneArrangements = ways_n_objects_can_be_arranged_with_multiplication(n-1);

    // The loop in ways_n_objects_can_be_arranged adds nMinusOneArrangements n times, so use
    // multiplication instead.
    counter = nMinusOneArrangements * n;

    return counter;
}

int
main(int const argc, char* const* argv) {

    // how to parameterize number of loops?

    cout << "4 objects can be arranged " << ways_4_objects_can_be_arranged() <<
        " ways." << endl;

    cout << "4 objects can be arranged " << ways_n_objects_can_be_arranged(4) << " ways." << endl;
    cout << "5 objects can be arranged " << ways_n_objects_can_be_arranged(5) << " ways." << endl;
    cout << "6 objects can be arranged " << ways_n_objects_can_be_arranged(6) << " ways." << endl;

    cout << "6 objects can be arranged " << ways_n_objects_can_be_arranged_with_multiplication(6) << " ways." << endl;

    return 0;
}

