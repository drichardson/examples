#include <iostream>

using std::cout;

class Base1
{
public:
    virtual void foo() = 0;
};

class Base2
{
public:
    virtual void foo() = 0;
    virtual void bar() = 0;
};

class MultipleAmbiguous : public Base1, public Base2
{
public:
    void foo() override { cout << "Multiple::foo\n"; }
    void bar() override { cout << "Multiple::bar\n"; }

    ~Multiple() { cout << "~Multiple\n"; }
};

int main()
{
    Multiple m;
    m.foo();
    m.bar();

    Base1* b1 = &m;
    b1->foo();

    Base2* b2 = &m;
    b2->foo();
    b2->bar();

    return 0;
}
