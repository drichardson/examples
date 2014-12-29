#ifndef CONTAINERS_CIRCULAR_BUFFER_HPP
#define CONTAINERS_CIRCULAR_BUFFER_HPP 1

#include <cstddef>
#include <cassert>

namespace containers {

#error Reimplement using one of the techniques described on wikipedia

template <typename Element>
class circularBuffer
{
    Element* mBuffer;
    size_t mHead; // next place to write into
    size_t mCapacity;
    size_t mSize;
public:
    circularBuffer(size_t capacity)
    {
        assert(capacity > 0);
        mHead = 0;
        mCapacity = capacity+1;
        mSize = 0;
        mBuffer = reinterpret_cast<Element*>(new unsigned char[mCapacity * sizeof(Element)]);
    }

    ~circularBuffer()
    {
        delete[] reinterpret_cast<unsigned char*>(mBuffer);
    }

    size_t capacity() const { return mCapacity; }
    size_t size() const { return mSize; }

    void append(Element const & e)
    {
        Element* p = mBuffer + mHead;
        if (mSize == mCapacity) {
            p->~Element();
        }
        new(p)Element(e);
        mHead = (mHead + 1) % mCapacity;
        ++mSize;
    }

    Element & oldest()
    {
        assert(mSize > 0);
        size_t i = mHead + mSize
        return mBuffer[];
    }

    void remove_oldest()
    {
        assert(mSize > 0);
        mBuffer[mTail].~Element();
        mTail = (mTail + 1) % mCapacity;
        --mSize;
    }
};

}

#endif // CONTAINERS_CIRCULAR_BUFFER_HPP
