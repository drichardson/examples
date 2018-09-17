#include "Python.h"
#include <string>

using namespace std::string_literals;

int main(int argc, char** argv) {
  Py_Initialize();

  return 0;
}

