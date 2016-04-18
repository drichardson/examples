// Test the performance of some different ways of doing ring buffer
// type arithmetic, where you increment and wrap. Can do it with:
// - division: x=(x+1) mod n
// - bitwise and: x=(x+1) & (n-1), if n is a power of 2.
// - increment and compare: x = x + 1; if x == n then x = 0.

#include <chrono>
#include <cstdlib>
#include <iomanip>
#include <iostream>

constexpr bool is_power_of_two(int x)
{
    // From http://www.exploringbinary.com/ten-ways-to-check-if-an-integer-is-a-power-of-two-in-c/
    return ((x > 0) && !(x & (x - 1)));
}

int main(int argc, char** argv)
{
    int i, j=0, k=0, l=0;
    constexpr int end = 1000000000;
    //constexpr int divisor = 128;
    if (argc != 2) {
        std::cerr << "Missing required divisor argument. Divisor must be power of 2.\n";
        std::exit(1);
    }
    const int divisor = std::atoi(argv[1]);
    if (!is_power_of_two(divisor)) {
        std::cerr << "divisor " << divisor << " is not a power of 2.\n";
        std::exit(1);
    }

    auto s1 = std::chrono::high_resolution_clock::now();

    // Use % operator. On intel, this uses idivl.
    for(i = end; i != 0; --i) {
        j = (j+1) % divisor;
    }

    auto s2 = std::chrono::high_resolution_clock::now();

    // Use & operator. We can do this since we're modding by a power of 2.
    //static_assert(is_power_of_two(divisor), "divisor is not a power of 2");
    const int and_divisor = divisor-1;
    for(i = end; i != 0; --i) {
        k = (k+1) & and_divisor;
    }

    auto s3 = std::chrono::high_resolution_clock::now();

    for(i = end; i != 0; --i) {
        if (++l == divisor) {
            l = 0;
        }
    }

    auto s4 = std::chrono::high_resolution_clock::now();

    if (j != k || k != l) {
        std::cerr << "j != k or k != l. j=" << j << ", k=" << j << ", l=" << l << "\n";
        std::exit(1);
    }

    auto mod_time = std::chrono::duration_cast<std::chrono::microseconds>(s2 - s1).count();
    auto and_time = std::chrono::duration_cast<std::chrono::microseconds>(s3 - s2).count();
    auto increment_and_compare_time = std::chrono::duration_cast<std::chrono::microseconds>(s4 - s3).count();

    constexpr int w = 8;
    std::cout
        << "mod: " << std::setw(w) << mod_time << "us\n"
        << "and: " << std::setw(w) << and_time << "us\n"
        << "cmp: " << std::setw(w) << increment_and_compare_time << "us\n"; 

    return 0;
}
