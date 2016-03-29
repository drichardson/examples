#ifndef EXAMPLES_TEMPLATES_H
#define EXAMPLES_TEMPLATES_H

namespace templates {

template <class Item, int Size>
struct FixedArray {
    Item buffer[Size];
    static constexpr int bufferSize = Size;
};

struct Convert {
    int i = 0;

    template <class T> void set(const T& t) {
        i = t;
    }
};

// Definition in templates.cc
template <class T>
void DoSomething(const T& v);

}

#endif

