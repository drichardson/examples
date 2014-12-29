#include "hash_table.hpp"
#include <iostream>
#include <string>

template <typename Container>
static void
print(Container & container, std::string const & key)
{
    using std::cout;
    typename Container::iterator itr = container.find(key);
    if (itr.end()) {
        cout << "No result found for key=" << key << "\n";
        return;
    }
    cout << "Found for key=" << key << ": ";
    for(; !itr.end(); itr.moveNext()) {
        cout << itr.get() << ' ';
    }
    cout << '\n';
}

int main()
{
    using std::cout;
    using std::string;
    containers::hashtable<std::string, unsigned> ht;

    ht.put("jose", 150);
    ht.put("george", 23);
    ht.put("alexandra", 50);
    ht.put("karen", 1);

    print(ht, "nobody");
    print(ht, "jose");
    print(ht, "george1");
    print(ht, "george");
    print(ht, "georgE");
    print(ht, "alexandra");
    print(ht, "karen");
}

