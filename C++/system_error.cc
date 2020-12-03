#include <iostream>
#include <system_error>

void display(char const *prefix, std::exception const &e) {
  std::cout << prefix << ' ' << e.what() << std::endl;
}

int main() {
  display("EPIPE system_category",
          std::system_error(EPIPE, std::system_category()));
  display("EPIPE generic_category",
          std::system_error(EPIPE, std::generic_category()));
}
