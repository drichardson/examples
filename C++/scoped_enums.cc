#include <iostream>

enum class Numbers {
        one,
        two,
        three
};

int main() {

        Numbers n = Numbers::two;

        switch(n) {
        case Numbers::one:
                std::cout << "one\n";
                break;
        case Numbers::two:
                std::cout << "two\n";
                break;
        case Numbers::three:
                std::cout << "three\n";
                break;
        }

        return 0;
}

