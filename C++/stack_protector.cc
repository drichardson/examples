#include <iostream>
#include <cstring>

static void try_it(char const* s) {
    char buf[8];
    std::strcpy(buf, s);
    std::cout << "Original: " << s << "\n"
        "Copy: " << buf << "\n";
}

int main(int argc, char* argv[]) {
    if (argc < 2) {
        std::cerr << "Usage: stack_protector <string>\n"
            "Make string bigger than 8 to try to trigger the -fstack-protector-all functionality.\n";
        return 1;
    }
    try_it(argv[1]);
    std::cout << "OK\n";
}

