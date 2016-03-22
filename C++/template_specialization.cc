// Modern C++ Design example, section 1.4 example. Demonstrates the limits of template
// specialization.

#include <iostream>

template <class T> class Widget {
public:
        void Fun() {
                std::cout << "Widget::Fun<T>: generic\n";
        }
};

// OK: specialization of a member function of Widget
template <> void Widget<char>::Fun() {
        std::cout << "Widget::Fun<char> specialization\n";
}

template <class T, class U> class Gadget {
public:
        void Fun() {
                std::cout << "Gadget::Fun: generic\n";
        }
};

template <> void Gadget<int, int>::Fun() {
                std::cout << "Gadget::Fun<int, int>: partial specialization\n";
}

#if 0
// Error! Cannot partially specialize a member function of Gadget
template <class U> void Gadget<char, U>::Fun() {
                std::cout << "Gadget::Fun<char, U>: partial specialization\n";
}

#endif

int main() {
        Widget<char> wchar;
        Widget<int> wint;

        wchar.Fun();
        wint.Fun();

        Gadget<int, char> gic;
        gic.Fun();

        Gadget<int, int> gii;
        gii.Fun();

        return 0;
}
