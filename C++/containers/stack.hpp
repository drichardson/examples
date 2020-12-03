#ifndef CONTAINERS_STACK_HPP
#define CONTAINERS_STACK_HPP 1

#include "vector.hpp"

namespace containers {

template <typename Element>
class stack
{
    vector<Element> mVector;
public:
    stack() : mVector(0) {}

    void push(Element const & element)
    {
        mVector.append(element);
    }

    void pop()
    {
        mVector.resize(mVector.size()-1);
    }

    size_t size() const { return mVector.size(); }

    Element & top()
    {
        return mVector.get(mVector.size()-1);
    }
};

}

#endif // CONTAINERS_STACK_HPP
