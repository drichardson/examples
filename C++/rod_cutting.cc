// from introduction to algorithms 3rd edition 15.1

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
        std::vector<unsigned> & max_prices,
        bool use_dynamic_programming
        )
{
    if (use_dynamic_programming && max_prices[RodLength] > 0) return max_prices[RodLength];

    unsigned max_price = 0;
    for(auto price : prices) {
        if (price.length < RodLength) {
            unsigned p = price.price +
                cut_rod_internal(prices,
                        RodLength-price.length,
                        max_prices,
                        use_dynamic_programming);

            if (p > max_price) {
                max_price = p;
            }
        } else if (price.length == RodLength) {
            max_price = price.price;
        }
    }

    ++num_max_prices_calculated;
    max_prices[RodLength] = max_price;
    return max_price;
}

static unsigned cut_rod(
        std::vector<LengthPrice> const & prices,
        unsigned RodLength,
        bool use_dynamic_programming)
{
    std::vector<unsigned> max_prices;
    max_prices.resize(RodLength+1);
    return cut_rod_internal(prices, RodLength, max_prices, use_dynamic_programming);
}

int main(int argc, char const* argv[]) { 
    using std::cerr;
    using std::cout;

    if (argc < 2) {
        cerr << "Usage: rod_cutting <use_dynamic>\n"
            << "  0 - don't use dynamic programming\n"
            << "  1 - use dynamic programming lookup table\n";
        return 1;
    }

    bool use_dynamic_programming = false;
    switch(argv[1][0]) {
    case '0':
        use_dynamic_programming = false;
        break;
    case '1':
        use_dynamic_programming = true;
        break;
    default:
        cerr << "Expected 0 or 1 but got " << argv[1][0] << '\n';
        return 1;
    }

    vector<LengthPrice> prices{
        {1,1}, {2,5}, {3,8}, {4,9}, {5,10}, {6,17}, {7,17}, {8,20}, {9,24}, {10,30}
    };

    for(unsigned length = 1; length < 30; ++length) {
        unsigned value = cut_rod(prices, length, use_dynamic_programming);
        cout << "Length=" << length << " Value=" << value << '\n';
    }

    cout << "Number of times max price calculated: " << num_max_prices_calculated << '\n';
}

