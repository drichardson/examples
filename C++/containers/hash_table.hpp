#ifndef CONTAINERS_HASH_TABLE_HPP
#define CONTAINERS_HASH_TABLE_HPP 1

#include "vector.hpp"
#include "list.hpp"
#include <stdint.h>
#include <cstring>

namespace containers
{

namespace {

uint32_t fnv1_32(void const* datav, size_t data_len ) {
    uint8_t const *data = static_cast<uint8_t const*>(datav);
    uint32_t hash = 0;
    uint8_t const * const end = data+data_len;
    uint32_t const magic_fnv1_prime32 = 0x01000193;
    for(; data < end; ++data) {
        hash *= magic_fnv1_prime32;
        hash ^= *data;
    }
    return hash;
}
}

// Key must support access to a contiguous array of bytes.
// key.size(), to return the size of the key in bytes
// &key[0] to return a pointer to the first byte of key.
// The copy constructor.
//
// Element must support a copy constructor.
template <typename Key, typename Element>
class hashtable
{
    struct hashtableEntry
    {
        Key key;
        Element element;
        hashtableEntry(Key const & k, Element const & e)
            : key(k), element(e) {}
    };

    typedef singly_linked_list<hashtableEntry> htlist;

    vector<htlist> mTable;

    uint32_t index(Key const & key)
    {
        uint32_t hv = fnv1_32(&key[0], key.size());
        return hv % mTable.size();
    }

public:
    hashtable(size_t initial_size = 64)
        : mTable(64)
    {
    }

    ~hashtable()
    {
    }

    void put(Key const & key, Element const & element)
    {
        htlist & l = mTable.get(index(key));
        l.insert_front(hashtableEntry(key,element));
    }

    class iterator
    {
        typename htlist::iterator mIterator;
        Key mKey;
        friend hashtable;

        bool keyMatches(Key const & key)
        {
            return key.size() == mKey.size() &&
                std::memcmp(&key[0], &mKey[0], mKey.size()) == 0;
        }

        iterator(typename htlist::iterator itr, Key const & key)
            : mIterator(itr),
            mKey(key)
        {
            if (!mIterator.end() && !keyMatches(mIterator.get().key))
                moveNext();
        }
    public:
        bool end() const { return mIterator.end(); }

        void moveNext() {
            for(mIterator.moveNext(); !mIterator.end(); mIterator.moveNext()) {
                if (keyMatches(mIterator.get().key)) {
                    break;
                }
            }
        }

        Element & get() {
            Element & e = mIterator.get().element;
            return e;
        }
    };

    iterator find(Key const & key)
    {
        htlist & l = mTable.get(index(key));
        return iterator(l.get_iterator(), key);
    }
};

}

#endif // CONTAINERS_HASH_TABLE_HPP
