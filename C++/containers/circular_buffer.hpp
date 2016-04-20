#ifndef CONTAINERS_CIRCULAR_BUFFER_HPP
#define CONTAINERS_CIRCULAR_BUFFER_HPP 1

#include <cassert>
#include <cstddef>
#include <limits>
#include <utility>

/*
   Circular Buffer supports two operations: get and put.

   - get reads the oldest item in the buffer.

   - put writes an item to the buffer. If the buffer is full, it overwrites the oldest item.
   
   Implementation 1: buffer, capacity, head, and tail
   - head points to the next item to read
   - tail points to the first invalid item, aka the next item to write
   - head = tail means empty
   - head + 1 = tail means full. This definition wastes one slot in the buffer.

   Implementation 2: buffer, capacity, head, and size
   - head points to the next item to read
   - size is how many valid items exist after head


    Comparison of impementations
    ----------------------------

    - Concurrency
    In implementation 1, the producer updates tail and the consumer updates head. This split might
    allow for a more efficient thread safe implementation.

    Implementation 2, however, both producer and consumer update the size member, and both need head,
    the consumer to know where to read and the producer to do the head+size calculation to determine
    where to write.

    - Memory Waste
    Implementation 1 wastes 1 slot in the circular buffer, because if it did not, there would be an
    ambiguity as to weather head == tail means full or empty.

    Implementation 2 wastes no space in the circular buffer.
   */

namespace containers {

// Implementation 1: buffer, capacity, head, and size.
template <class Element>
class CircularBufferSizeImplementation
{
    Element* buffer_;
    size_t const capacity_;
    size_t head_ = 0;
    size_t size_ = 0;

    bool isfull() const { return size_ == capacity_; }

public:
    CircularBufferSizeImplementation(size_t capacity)
        : capacity_(capacity)
    {
        assert(capacity_ > 0);
        buffer_ = reinterpret_cast<Element*>(new unsigned char[capacity_* sizeof(Element)]);
        assert(buffer_ != nullptr);
    }

    CircularBufferSizeImplementation(const CircularBufferSizeImplementation&) = delete;
    CircularBufferSizeImplementation& operator=(const CircularBufferSizeImplementation&) = delete;

    ~CircularBufferSizeImplementation()
    {
        while(size() > 0) {
            remove_front();
        }
        delete[] reinterpret_cast<unsigned char*>(buffer_);
    }

    inline size_t capacity() const { return capacity_; }
    inline size_t size() const { return size_; }

    // T must be an Element, but is a template argument so that type deduction
    // rules can kick in to make T&& a universal reference (and thus handle lvalue refs
    // and cv qualified types).
    template<class T>
    void append(T&& e)
    {
        if(isfull())
        {
            remove_front();
        }

        size_t write = head_ + size_;
        if(write >= capacity_)
        {
            // head_ is <= capacity_-1
            // size_ is <= capacity_
            // therefore
            // head_ + size_ <= capacity_+capacity_-1.
            // so
            // head_ + size_ < capacity_+capacity_.
            // Assume capacity_ <= head_ + size_.
            // capacity_ <= head_ + size_ < capacity_ + capacity_
            // so
            // 0 <= head_ + size_ - capacity < capacity_
            // That means if write >= capacity_, we can subtract
            // capacity_ to do arithmetic modulo capacity_ without
            // using a more expensive mod/div instruction.
            write -= capacity_;
        }
        assert(write < capacity_);

        // Copy construct a new element.
        new(buffer_ + write)Element(std::forward<T>(e));

        ++size_;
        assert(size_ <= capacity_);
    }

    inline Element & front() const
    {
        assert(size() > 0);
        return buffer_[head_];
    }

    inline void remove_front()
    {
        buffer_[head_].~Element();
        ++head_;
        if(head_ >= capacity_)
        {
            head_ = 0;
        }
        --size_;
    }
};

template <typename Element>
class CircularBufferHeadTailImplementation
{
    Element* buffer_;
    size_t const buffer_size_;
    size_t head_ = 0;
    size_t tail_ = 0;

    inline size_t next(size_t head_or_tail) const
    {
        // In my tests on x86-64 -O3, increment,compare,reset
        // is faster than increment and mod.
        // For more information, see:
        // https://dougrichardson.org/2016/04/18/wrapping_counters.html
        size_t r = head_or_tail + 1;
        if (r == buffer_size_) {
            r = 0;
        }
        return r;
    }

    inline bool isfull() const
    {
        return next(tail_) == head_;
    }

public:
    CircularBufferHeadTailImplementation(size_t capacity)
        : buffer_size_(capacity+1)
    {
        assert(capacity > 0);
        buffer_ = reinterpret_cast<Element*>(new unsigned char[buffer_size_* sizeof(Element)]);
        assert(buffer_ != nullptr);
    }

    CircularBufferHeadTailImplementation(const CircularBufferHeadTailImplementation&) = delete;
    CircularBufferHeadTailImplementation& operator=(const CircularBufferHeadTailImplementation&) = delete;

    ~CircularBufferHeadTailImplementation()
    {
        while(size() > 0) {
            remove_front();
        }
        delete[] reinterpret_cast<unsigned char*>(buffer_);
    }

    inline size_t capacity() const
    {
        return buffer_size_ - 1;
    }

    size_t size() const
    {
        if(tail_ >= head_)
        {
            return tail_ - head_;
        }

        // front_ > end_, which means the buffer is non-contiguous, so calculate both sections,
        // 0 to tail_ and head_ to capacity_.
        size_t left_size = tail_;
        size_t right_size = buffer_size_ - head_;
        return left_size + right_size;
    }

    template <class T>
    void append(T&& e)
    {
        if (isfull()) {
            // Array is filled and so the front item will be overwritten.
            // Destroy the current front item and increment the front pointer.
            remove_front();
        }

        // Construct a new element.
        new(buffer_ + tail_)Element(std::forward<T>(e));

        // Increment the back pointer.
        tail_ = next(tail_);
    }

    inline Element & front() const
    {
        assert(size() > 0);
        return buffer_[head_];
    }

    void remove_front()
    {
        front().~Element();
        head_ = next(head_);
    }
};

}

#endif // CONTAINERS_CIRCULAR_BUFFER_HPP
