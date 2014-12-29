#ifndef CONTAINERS_VECTOR_HPP
#define CONTAINERS_VECTOR_HPP 1

// include cstddef for NULL
#include <cstddef>

#include <cstdlib>
#include <cstring>
#include <iostream>

namespace containers {

template <typename Element>
class vector
{
    Element* mArray;
    size_t mArraySize; // size in elements
    size_t mArrayCapacity; // capacity in elements
    unsigned char* mArrayBytes;

    vector(vector const & rhs);
    vector & operator=(vector const & rhs);
public:
    vector(size_t size, Element const & value = Element())
    {
        mArraySize = size;
        mArrayCapacity = size;
        mArrayBytes = new unsigned char[size * sizeof(Element)];
        mArray = reinterpret_cast<Element*>(mArrayBytes);
        for(size_t i = 0; i < mArraySize; ++i) {
            new(mArray + i)Element(value);
        }
    }

    ~vector()
    {
        for(size_t i = 0; i < mArraySize; ++i) {
            mArray[i].~Element();
        }
        delete[] mArrayBytes;
    }

    //
    // Element access
    //
    size_t size() const { return mArraySize; }
    Element & get(size_t index) { return mArray[index]; }
    Element const & get(size_t index) const { return mArray[index]; }

    void set(size_t index, Element const & element)
    {
#if 1
        mArray[index].~Element();
        new(mArray+index)Element(element);
#else
        mArray[index] = element;
#endif
    }

    //
    // Iterators
    //
    class iterator
    {
        size_t mCurrent;
        vector<Element> & mVector;
        friend vector;
        iterator(vector<Element> & v) : mCurrent(0), mVector(v) {}
    public:
        bool end() const { return mCurrent >= mVector.size(); }
        void moveNext() { ++mCurrent; }
        Element & get() const { return mVector.get(mCurrent); }
    };

    iterator get_iterator() { return iterator(*this); }

    //
    // Resizing
    //
    size_t capacity() const { return mArrayCapacity; }

    // reserve space for newCapacity elements. If newCapacity
    // is <= mArrayCapacity, this method does nothing.
    void reserve(size_t newCapacity)
    {
        if (newCapacity <= mArrayCapacity) return;

        // allocate space
        unsigned char* newArray = new unsigned char[newCapacity * sizeof(Element)];
        Element* newElements = reinterpret_cast<Element*>(newArray);

        // copy construct existing elements to new array...
        for(size_t i = 0; i < mArraySize; ++i) {
            new(newElements+i)Element(mArray[i]);
            mArray[i].~Element(); // destruct previous element
        }

        // ... and leave the remaining elements uninitialized.

        delete[] mArrayBytes;
        mArrayBytes = newArray;
        mArray = newElements;
        mArrayCapacity = newCapacity;
    }

    void resize(size_t newSize, Element const & value = Element())
    {
        // allocate space, if necessary
        reserve(newSize);

        // construct new elements at the end using default value in case
        // the array is growing.
        for(size_t i = mArraySize; i < newSize; ++i) {
            new (mArray+i)Element(value);
        }

        // destruct extra elements at the end of the current array in case
        // the array is shrinking.
        for(size_t i = newSize; i < mArraySize; ++i) {
            mArray[i].~Element();
        }

        mArraySize = newSize;
    }

    void append(Element const & value)
    {
        if (mArraySize == mArrayCapacity) {
            if (mArrayCapacity == 0) {
                reserve(16);
            } else if (mArraySize == mArrayCapacity) {
                // amortize allocations by allocating in multiples of the current capacity.
                reserve(mArrayCapacity * 2);
            }
        }

        new(mArray+mArraySize)Element(value);
        mArraySize++;
    }
};

}

#endif // CONTAINERS_VECTOR_HPP
