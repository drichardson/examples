// from introduction to algorithms 3rd edition 15.1

#define USE_DYNAMIC_PROGRAMMING 0

#include <iostream>
#include <vector>
#include <cassert>

using std::vector;

struct LengthPrice { 
    unsigned length;
    unsigned price;
};

static unsigned num_max_prices_calculated;

static unsigned cut_rod_internal(
        std::vector<LengthPrice> const & prices,
        unsigned RodLength,
        std::vector<unsigned> & max_prices
        )
{
#if USE_DYNAMIC_PROGRAMMING
    if (max_prices[RodLength] > 0) return max_prices[RodLength];
#endif

    unsigned max_price = 0;
    for(auto price : prices) {
        if (price.length < RodLength) {
            unsigned p = price.price + cut_rod_internal(prices, RodLength-price.length, max_prices);
            if (p > max_price) {
                max_price = p;
            }
        } else if (price.length == RodLength) {
            max_price = price.price;
        }
    }

    ++num_max_prices_calculated;
#if USE_DYNAMIC_PROGRAMMING
    max_prices[RodLength] = max_price;
#endif
    return max_price;
}

static unsigned cut_rod(
        std::vector<LengthPrice> const & prices,
        unsigned RodLength)
{
    std::vector<unsigned> max_prices;
    max_prices.resize(RodLength+1);
    return cut_rod_internal(prices, RodLength, max_prices);
}

int main() { 
    using std::cerr;
    using std::cout;

    vector<LengthPrice> prices{
        {1,1}, {2,5}, {3,8}, {4,9}, {5,10}, {6,17}, {7,17}, {8,20}, {9,24}, {10,30}
    };

    for(unsigned length = 1; length < 30; ++length) {
        unsigned value = cut_rod(prices, length);
        cout << "Length=" << length << " Value=" << value << '\n';
    }

    cout << "USE_DYNAMIC_PROGRAMMING: " << USE_DYNAMIC_PROGRAMMING << '\n';
    cout << "Number of times max price calculated: " << num_max_prices_calculated << '\n';
}

