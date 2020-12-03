#ifndef EXAMPLES_TEMPLATES_H
#define EXAMPLES_TEMPLATES_H

#include <iostream>

namespace templates {

template <class Item, int Size>
struct FixedArray
{
    Item buffer[Size];
    static constexpr int bufferSize = Size;
};

struct Convert
{
    int i = 0;

    template <class T> void set(const T& t) {
        i = t;
    }
};

// Definition in templates.cc
template <class T>
void DoSomething(const T& v);

template <class T>
class WithVirtual
{
public:
    virtual ~WithVirtual() {}
    virtual void printT(const T& t) {
        std::cout << "printT: " << t << "\n";
    }

    // error: virtual cannot be specified on member function templates
    // template <class H> virtual void templateVirtual() {}
};

template <class T>
struct WithStaticMember
{
    static T staticMember;
};

template <typename T> T templates::WithStaticMember<T>::staticMember;

template <class T, template<class>class Container>
struct WithTemplateTemplateParameter
{
    Container<T> buf;
};

}

#endif

