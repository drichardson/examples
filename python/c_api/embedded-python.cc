#include "Python.h"

#include <string>
#include <iostream>

using namespace std::string_literals;
using std::cout;
using std::endl;

int main(int argc, char** argv) {
  ::Py_Initialize();

  int rc = ::PyRun_SimpleStringFlags("print([x**2 for x in range(10)])", NULL);
  cout << "Result: " << rc << endl;

  rc = ::PyRun_SimpleStringFlags("BLAH BLAH", NULL);
  cout << "Result: " << rc << endl;

  return 0;
}

