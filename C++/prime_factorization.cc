#include <iostream>
#include <vector>
#include <cassert>
#include <stdlib.h>
#include <limits.h>

static void
prime_factor(long long N, std::vector<long long> & factors)
{
    while(N % 2 == 0) {
        factors.push_back(2);
        N /= 2;
    }
    for(long long f = 3; N > 1; f += 2) {
        while(N % f == 0) {
            factors.push_back(f);
            N /= f;
        }
    }
}

int main(int argc, char** argv) {
    using std::cerr;
    using std::cout;
    using std::vector;
    long long N = 12345;
    if (argc == 2) {
        char *end = nullptr;
        N = std::strtoll(argv[1], &end, 10);
        if (N == 0 && *end != 0) {
            cerr << "Invalid number starting at " << end << '\n';
            return 1;
        }
        if (N < 1) {
            cerr << "Cannot factor number less than 1.\n";
            return 1;
        }
        if (N == LLONG_MAX) {
            cerr << "Overlow while parsing input.\n";
            return 1;
        }
    }

    vector<long long> prime_factors;
    prime_factor(N, prime_factors);

    for(auto e : prime_factors) {
        cout << e << ' ';
    }
    cout << '\n';
}

