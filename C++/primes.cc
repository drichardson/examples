#include <iostream>
#include <algorithm>
#include <vector>

// http://en.wikipedia.org/wiki/Sieve_of_Eratosthenes
std::vector<unsigned> sieve_of_eratosthenes(unsigned upto) {
    // is_prime[i] is true if the number i is prime and
    // false otherwise.
    std::vector<bool> is_prime(upto+1, true);
    is_prime[0] = false; // 0 is not prime
    is_prime[1] = false; // 1 is not prime
    for(unsigned i = 2; i <= upto; ++i) {
        for(unsigned j = i+i; j <= upto; j += i) {
            is_prime[j] = false;
        }
    }

    std::vector<unsigned> result;
    for(unsigned i = 1; i <= upto; ++i) {
        if (is_prime[i])
            result.push_back(i);
    }

    return result;
}

int main(int argc, char const* argv[]) {
    unsigned upto = 1000000;
    if (argc == 2) {
        char *end = nullptr;
        auto ll = ::strtoll(argv[1], &end, 0);
        if (ll < 0) {
            std::cerr << "Invalid negative number\n";
            return 1;
        }
        constexpr decltype(ll) max = std::numeric_limits<unsigned>::max();
        if (ll > max) {
            std::cerr << ll << " is larger than max allowed " << max << '\n';
            return 1;
        }
        upto = ll;
    }
    auto primes = sieve_of_eratosthenes(upto);
    for(auto p : primes) {
        std::cout << p << ' ';
    }
    std::cout << '\n';
    std::cout << "Found " << primes.size() << '\n';
}
