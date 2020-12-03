#include <iostream>
#include <typeinfo>

using namespace std;


class A {
public:
    virtual string foo() const { return "A"; }
};

class B : public A {
public:
    string foo() const override { return "B"; }
    virtual void Bfoo() {}
};

class B2 : public A {
public:
    string foo() const override { return "B2"; }
    virtual void B2foo() {}
};

class A2 {
public:
    virtual string foo() const { return "A2"; }
};

class C : public A2 {
public:
    string foo() const override { return "C"; }
};

class UnrelatedToA
{
public:
    virtual string UnrelatedToAfoo() { return "X"; }
};

class D : public A, public UnrelatedToA {
public:
    string foo() const override { return "D"; }
    int bar() { return 5; }
    string UnrelatedToAfoo() override { return "XD"; }
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
    return static_cast<A*>(new D());
}


int main() {
    A a;
    B b;
    B2 b2;
    C c;
    //E e;
    cout << "a: " << a.foo() << endl;
    cout << "b: " << b.foo() << endl;
    cout << "b2: " << b2.foo() << endl;
    cout << "c: " << c.foo() << endl;

    A* pa = &a;
    cout << "pa(a): " << pa->foo() << endl;
    pa = static_cast<A*>(&b);
    cout << "pa(b): " << pa->foo() << endl;
    pa = static_cast<A*>(&b2);
    cout << "pa(b2): " << pa->foo() << endl;
    //pa = &c; // error: cannot convert C* to A* in assignment
    //pa = static_cast<A*>(&c); // error: invalid static_cast from type C* to type A*
    //pa = dynamic_cast<A*>(&c); // error: dynamic_cast can never succeed
    // pa = dynamic_cast<A*>(&e); // error: source type (E) is not polymorphic
    A* pa_D = getD();
    B* pb_D_dyn = dynamic_cast<B*>(pa_D); // should be nullptr, not related type
    B* pb_D_sta = static_cast<B*>(pa_D); // allowed since B inherits from A, but guess what, this A is not a B so WATCH OUT.
    cout << "pa_D: " << pa_D << ", pb_D_dyn: " << pb_D_dyn << ", pb_D_sta: " << pb_D_sta << endl;
    // UnrelatedToA* un_stat = static_cast<UnrelatedToA*>(pa_D); // error: static_cast not allowed
    // however, if we statically cast to a related type (D) first, and then to UnrelatedToA, it works.
    UnrelatedToA* unrelated_stat = static_cast<UnrelatedToA*>(static_cast<D*>(pa_D)); // error: static_cast not allowed
    // Or just use dynamic_cast and let it figure it out for us.
    UnrelatedToA* unrelated_dyn = dynamic_cast<UnrelatedToA*>(pa_D);
    cout << "unrelated_dyn: " << unrelated_dyn << ", unrelated_stat: " << unrelated_stat << endl;

    size_t ipa = reinterpret_cast<size_t>(pa);
    cout << "ipa: " << ipa << endl;
    A* pa2 = reinterpret_cast<A*>(ipa);
    cout << "pa2: " << pa2->foo() << endl;

    test(&a);
    test(getD());

    A const* pa_const = &a;
    A* pa_not_const = const_cast<A*>(pa_const);
    cout << "pa_not_const: " << pa_not_const->foo() << endl;

    // static downcast and upcast between unrelated types that share a common ancestor.
    pa = &b; // implicit downcast.
    cout << "pa(b): " << pa->foo() << endl;
    B* pb_static_upcast = static_cast<B*>(pa); // upcast back to B
    cout << "pb_static_upcast(b): " << pb_static_upcast->foo() << endl; // works fine since we are pointing to a B.
    B2* pb2_static_upcast = static_cast<B2*>(pa); // upcast back to B2, an unrelated type.
    cout << "pb2_static_upcast(b) pointer: " << pb2_static_upcast << endl; // Invalid cast. Although result not null, calling function through it causes core dump
    // cout << "pb2(b): " << pb2_static_upcast->foo() << endl; // Crashes due to cast between unrelated types.

    // dynamic downcast and upcast between unrelated types that share a common ancestor
    pa = &b; // implicit downcast
    cout << "pa(b): " << pa->foo() << endl;
    B* pb_dyn_upcast = dynamic_cast<B*>(pa); // upcast back to B
    cout << "pb_dyn_upcast(b): " << pb_dyn_upcast->foo() << endl;
    B2* pb2_dyn_upcast = dynamic_cast<B2*>(pa); // upcast back to B2, an unrelated type, which results in null.
    cout << "pb2_dyn_upcast(b) pointer: " << pb2_dyn_upcast << endl; // null since B2 is unrelated to B.
}

