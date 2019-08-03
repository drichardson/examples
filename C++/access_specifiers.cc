#include <iostream>
#include <array>

using std::cout;

class A
{
public:
  virtual void pub() { cout << "A::pub()\n"; }

protected:
  virtual void prot() { cout << "A::prot()\n"; }

private:
  virtual void priv() { cout << "A::priv()\n"; }
};

class B : public A
{
private:

  void pub() override { cout << "B::pub()\n"; }
  void prot() override { cout << "B::prot()\n"; }
  void priv() override { cout << "B::priv()\n"; }
};

// ExposeB demonstrates that B cannot be exposed through
// a sub-class.
class ExposeB : public B
{
public:
  // void pub() override { B::pub(); } // error: B::pub is private in this context
  // void prot() override { B::prot(); } // error: B::prot is private in this context
  // void priv() override { B::priv(); } // error: B::priv is private in this context
};

// ExposeA demonstrates that methods on A can still be called,
// even those they are private in B. This is like having
// an A* to B.
class ExposeA : public B
{
public:
  void pub() override { A::pub(); }
  void prot() override { A::prot(); }
  void priv() override {
     // A::priv(); // error: A::priv is private in this context
    cout << "cannot call A::priv(), it's private\n";
  } 
};

class C : public A
{
public:
  void pub() override { cout << "C::pub()\n"; }
  void prot() override { cout << "C::prot()\n"; }
  void priv() override { cout << "C::priv()\n"; }
};


int main() {
  cout << "access specifier test\n";

  cout << "A:\n";
  A a;
  a.pub();
  // a.prot(); // error A::prot protected in this context
  // a.priv(); // error A::priv private in this context

  cout << "B:\n";
  B b;
  // b.pub(); // error: B::pub private in this context
  // b.prot(); // error: B::prot private in this context
  // b.priv(); // error: B::priv private in this context

  {
    cout << "ExposeB:\n";
    ExposeB eb;
    // eb.pub(); // method cannot be implemented
    // eb.prot();
    // eb.priv();
  }

  {
    cout << "ExposeA:\n";
    ExposeA ea;
    ea.pub();
    ea.prot();
    ea.priv();
  }

  {
    cout << "C:\n";
    C c;
    c.pub();
    c.prot();
    c.priv();
  }

  cout << "A* to &a\n";
  A* pa = &a;
  pa->pub();
  // pa->prot(); // error: A::prot protected in this context
  // pa->priv(); // error: A::priv private in this context

  cout << "A* to &b\n";
  pa = &b;
  pa->pub();
  // pa->prot(); // error: A::prot is protected in this context
  // pa->priv(); // error: A::priv is private in this context

  cout << "B* to &b\n";
  // B* pb = &b; // unused variable due to other lines being commented out.
  // pb->pub(); // error: B::pub is private in this context
  // pb->prot(); // error: B::prot is private in this context
  // pb->priv(); // error: B::priv is private in this context
}
