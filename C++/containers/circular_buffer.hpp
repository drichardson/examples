#ifndef CONTAINERS_CIRCULAR_BUFFER_HPP
#define CONTAINERS_CIRCULAR_BUFFER_HPP 1

#include <cassert>
#include <cstddef>
#include <limits>


// TODO REMOVE
#include <iostream>

/*
   Circular Buffer supports two operations: get and put.

   - get reads the oldest item in the buffer.

   - put writes an item to the buffer. If the buffer is full, it overwrites the oldest item.
   
   Implementation 1: buffer, capacity, head, and tail
   - head points to the next item to read
   - tail points to the next item to write
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

    inline size_t capacity() const { return capacity_; }
    inline size_t size() const { return size_; }

    inline void append(Element const & e)
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
        new(buffer_+write)Element(e);
        ++size_;
        assert(size_ <= capacity_);
    }

    inline Element & front()
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
class circularBuffer
{
    Element* buffer_;
    size_t const capacity_;
    size_t front_ = 0;
    size_t back_ = 0;

public:
    circularBuffer(size_t capacity)
        : capacity_(capacity)
    {
        assert(capacity_ > 0);
        buffer_ = reinterpret_cast<Element*>(new unsigned char[capacity_* sizeof(Element)]);
        assert(buffer_ != nullptr);
    }

    ~circularBuffer()
    {
        while(size() > 0) {
            std::cout << "~circularBuffer::remove_front\n";
            remove_front();
        }
        delete[] reinterpret_cast<unsigned char*>(buffer_);
    }

    inline size_t capacity() const { return capacity_; }

    size_t size() const {
        if (front_ == back_)
        {
            return 0;
        }

        if(back_ > front_)
        {
            return back_ - front_;
        }

        assert(0);
        // front_ > end_, which means the buffer is non-contiguous, so calculate both sections,
        // 0 to end_ and front_ to capacity_.
        size_t left_size = back_;
        size_t right_size = capacity_ - front_;
        return left_size + right_size;
    }

    void append(Element const & e)
    {
        Element* p = buffer_ + back_;

        if (size() == capacity()) {
            // Array is filled and so the front item will be overwritten.
            // Destroy the current front item and increment the front pointer.
            remove_front();
            front_ = (front_ + 1) % capacity_; 
            std::cout << "append. destroyed front because at capacity. front_ = " << front_ << "\n";
        }

        // Copy construct (todo: move?) a new element.
        new(p)Element(e);

        // Increment the back pointer.
        const size_t next_back = (back_ + 1) % capacity_;
        back_ = next_back;
        std::cout << "append. increment back_ to " << back_ << "\n";
    }

    inline Element & front()
    {
        assert(size() > 0);
        return buffer_[front_];
    }

    void remove_front()
    {
        front().~Element();
        front_ = (front_ + 1) % capacity_;
        std::cout << "remove_front: incremented front_ to " << front_ << "\n";
    }
};

}

#endif // CONTAINERS_CIRCULAR_BUFFER_HPP
