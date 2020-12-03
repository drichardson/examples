#include "Python.h"
#include <string>

using namespace std::string_literals;

int main(int argc, char** argv) {
  wchar_t * v[] = {
    L"this-program",
    L"-h"
  };
  return Py_Main(sizeof(v)/sizeof(v[0]), v);
}

