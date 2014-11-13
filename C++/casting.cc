#include <iostream>
#include <typeinfo>

using namespace std;


class A {
public:
    virtual string foo() const { return "A"; }
};

class B : public A {
public:
    string foo() const { return "B"; }
};

class A2 {
public:
    virtual string foo() const { return "A2"; }
};

class C : public A2 {
public:
    string foo() const { return "C"; }
};

class D : public A {
public:
    string foo() const { return "D"; }
    int bar() { return 5; }
};

class E {
};

int test(A* a) {
    D* pd = dynamic_cast<D*>(a);
    cout << "pd is " << pd << endl;
    if (pd) {
        cout << "pd values: " << pd->foo() << ", " << pd->bar() << endl;
    }
    return 1;
}

A* getD() {
    return new D();
}


int main() {
    A a;
    B b;
    C c;
    //E e;
    cout << "a: " << a.foo() << endl;
    cout << "b: " << b.foo() << endl;
    cout << "c: " << c.foo() << endl;

    A* pa = &a;
    cout << "pa(a): " << pa->foo() << endl;
    pa = static_cast<A*>(&b);
    cout << "pa(b): " << pa->foo() << endl;
    //pa = &c; // error: cannot convert C* to A* in assignment
    //pa = static_cast<A*>(&c); // error: invalid static_cast from type C* to type A*
    //pa = dynamic_cast<A*>(&c); // error: dynamic_cast can never succeed
    // pa = dynamic_cast<A*>(&e); // error: source type (E) is not polymorphic
    B* pb= dynamic_cast<B*>(getD());
    cout << "pb(D): " << pb << endl;

    size_t ipa = reinterpret_cast<size_t>(pa);
    cout << "ipa: " << ipa << endl;
    A* pa2 = reinterpret_cast<A*>(ipa);
    cout << "pa2: " << pa2->foo() << endl;

    test(&a);
    test(getD());

    A const* pa_const = &a;
    A* pa_not_const = const_cast<A*>(pa_const);
    cout << "pa_not_const: " << pa_not_const->foo() << endl;
}
