#ifndef CONTAINERS_LIST_HPP
#define CONTAINERS_LIST_HPP 1

// include cstddef for NULL
#include <cstddef>

namespace containers {

template <typename Element>
class singly_linked_list
{
    struct list
    {
        Element element;
        list* next;
        list(Element const & e, list* n)
            : element(e), next(n) {}
    };

    list* mListHead;
public:
    singly_linked_list() : mListHead(NULL) {}

    ~singly_linked_list()
    {
        while(mListHead) {
            list* oldHead = mListHead;
            mListHead = mListHead->next;
            delete oldHead;
        }
    }

    //
    // Fundamental singly linked list operations.
    //
    void insert_front(Element const & e)
    {
        mListHead = new list(e, mListHead);
    }

    void pop_front()
    {
        list* oldHead = mListHead;
        mListHead = mListHead->next;
        delete oldHead;
    }

    Element & front() { return mListHead->element; }
    Element const & front() const { return mListHead->element; }
    bool empty() const { return mListHead == NULL; };


    //
    // Iterators
    //
    class iterator
    {
        friend singly_linked_list;
        list* mCurrent;
        iterator(list* start) : mCurrent(start) {}
    public:
        Element & get() { return mCurrent->element; }
        bool end() const { return mCurrent == NULL; }
        void moveNext() { mCurrent = mCurrent->next; }
    };

    class const_iterator
    {
        friend singly_linked_list;
        mutable list* mCurrent;
        const_iterator(list* start) : mCurrent(start) {}
    public:
        Element const & get() const { return mCurrent->element; }
        bool end() const { return mCurrent == NULL; }
        void moveNext() { mCurrent = mCurrent->next; }
    };

    iterator get_iterator() const { return iterator(mListHead); }
    const_iterator get_const_iterator() const { return const_iterator(mListHead); }
};

};

#endif // CONTAINERS_LIST_HPP

