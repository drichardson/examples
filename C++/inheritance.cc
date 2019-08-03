#include <iostream>
#include <array>

using std::cout;

class A
{
public:

  struct S
  {
    int i;
  };

  void f1(S s) { cout << "A::S.i == " << s.i << '\n'; }
  virtual void f2(S s) { cout << "A::S.i == " << s.i << '\n'; }
};

class B : public A
{
public:

  struct S
  {
    int x;
  };


  void f1(S s) { cout << "B::S.x == " << s.x << '\n'; }
  void f2(A::S s) override { cout << "A::S.i == " << s.i << '\n'; }
};

class C
{
public:
  void pub() { cout << "C::pub()\n"; }
protected:
  void prot() { cout << "C::prot()\n"; }
private:
  void priv() { cout << "C::priv()\n"; }

public:
  void test() {
    pub();
    prot();
    priv();
  }
};

class D : public C
{
public:
  void test() {
    pub();
    prot();
    // priv(); // error private in this context
  }
};


class E : protected C
{
public:
  void test() {
    pub();
    prot();
    // priv(); // error: priv is private in this context
  }
};

class E2 : public E
{
public:
  void test() {
    pub();
    prot();
    // priv(); // error: C::priv is private within this context
  }
};

class F : private C
{
public:
  void test() {
    pub();
    prot();
    // priv(); // error: priv is private in this context
  }
};

class G : C
{
public:
  void test() {
    pub();
    prot();
    // priv(); // error: priv is private in this context
  }
};

class H : public G
{
public:
  void test() {
    // pub(); // error: C::pub() is inaccessible within this context (because C privately inherited in G).
    // prot(); // error: C::prot() is protected within this context
    // priv(); // error: C::priv() is private within this context
  }
};

int main()
{
  cout << "inheritance\n";

  A a;
  A::S as;
  as.i = 1;
  a.f1(as);

  B b;
  B::S bs;
  bs.x = 2;
  b.f1(bs);
  b.f2(as);

  cout << "C\n";
  C c;
  c.test();
  c.pub();
  // c.prot(); // error: protected
  // c.priv(); // error: private

  cout << "D\n";
  D d;
  d.test();
  d.pub();

  cout << "E\n";
  E e;
  e.test();
  // e.pub(); // error: C::pub() inaccessible within this context

  cout << "E2\n";
  E2 e2;
  e2.test();

  cout << "F\n";
  F f;
  f.test();
  // f.pub(); // error: C::pub() inaccessible within this context

  cout << "G\n";
  G g;
  g.test();
  // g.pub(); // error: C::pub() inaccessible within this context

  return 0;
}
