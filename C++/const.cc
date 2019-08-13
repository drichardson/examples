#include <iostream>

using std::cout;

struct S
{
  int i = 5;
  void NonConstFunc() { i++; }
  void ConstFunc() const {
    // i += 10; // error: assignment of member in read only object.
  }
};

struct S1
{
  S s;

  void NonConstFunc() {
    s.NonConstFunc();
    s.ConstFunc();
  }
  void ConstFunc() const {
    // s.NonConstFunc(); // error: discards const qualifier
    s.ConstFunc();
  }
};

struct SRefs
{
  S& sref;
  S1& s1ref;

  SRefs(S& s, S1& s1)
    : sref(s)
    , s1ref(s1)
  {
  }

  void NonConstFunc() {
    sref.NonConstFunc();
    sref.ConstFunc();
    s1ref.NonConstFunc();
    s1ref.ConstFunc();
  }
  void ConstFunc() const {
    sref.NonConstFunc();
    sref.ConstFunc();
    s1ref.NonConstFunc();
    s1ref.ConstFunc();
  }
};

void TakesConst(const S1& s1)
{
  // s1.NonConstFunc(); // error: discards const qualifier
  s1.ConstFunc();
}

int main()
{
  cout << "***** S ******\n";
  S s;
  cout << "s.i=" << s.i << "\n";
  s.NonConstFunc();
  cout << "s.i=" << s.i << "\n";
  s.ConstFunc();
  cout << "s.i=" << s.i << "\n";

  cout << "***** S1 ******\n";
  S1 s1;
  cout << "s1.s.i=" << s1.s.i << "\n";
  s1.NonConstFunc();
  cout << "s1.s.i=" << s1.s.i << "\n";
  s1.ConstFunc();
  cout << "s1.s.i=" << s1.s.i << "\n";
  TakesConst(s1);
  cout << "s1.s.i=" << s1.s.i << "\n";

  cout << "***** REFS ******\n";
  SRefs sr(s, s1);
  sr.sref.NonConstFunc();
  cout << "s.i=" << s.i << "\n";
  sr.s1ref.NonConstFunc();
  cout << "s1.s.i=" << s.i << "\n";

  sr.ConstFunc();
  cout << "s.i=" << s.i << "\n";
  cout << "s1.s.i=" << s.i << "\n";

  sr.NonConstFunc();
  cout << "s.i=" << s.i << "\n";
  cout << "s1.s.i=" << s.i << "\n";

  SRefs const & srRef = sr;
  srRef.ConstFunc();
  // srRef.NonConstFunc(); // error: discards const qualifier
  cout << "s.i=" << s.i << "\n";
  cout << "s1.s.i=" << s.i << "\n";

  return 0;
}
