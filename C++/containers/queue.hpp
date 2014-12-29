#ifndef CONTAINERS_QUEUE_HPP
#define CONTAINERS_QUEUE_HPP 1

#include <cstddef>

namespace containers {

template <typename Element>
class queue 
{
    queue(queue const & rhs);
    queue & operator=(queue const & rhs);
public:
    queue()
    {
    }

    void append(Element const & element)
    {
    }

    Element & front()
    {
        return Element();
    }

    void remove_front()
    {
    }

    size_t size() const
    {
        return 0;
    }
};

}

#endif // CONTAINERS_QUEUE_HPP
