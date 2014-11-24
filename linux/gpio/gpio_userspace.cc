#include <iostream>
#include <cstdlib>
#include <fstream>
#include <sstream>
#include <thread>

void usage(char const* msg) {
    using namespace std;
    cout << msg
        << "\nUsage:\n"
        << "  gpio_userspace read <pin>\n"
        << "  gpio_userspace write <pin> <value>\n"
        << "  gpio_userspace wait <pin>\n"
        << endl;
}

using pin_t = int;

enum class pin_direction {
    in,
    out
};

bool export_pin(pin_t pin) {
    std::ofstream f("/sys/class/gpio/export");
    if (!f) {
        std::cerr << "Couldn't open export file." << std::endl;
        return false;
    }
    f << pin << std::flush;
    return f.good();
}

bool unexport_pin(pin_t pin) {
    std::ofstream f("/sys/class/gpio/unexport");
    if (!f) {
        std::cerr << "Couldn't open unexport file." << std::endl;
        return false;
    }
    f << pin << std::flush;
    return f.good();
}

std::string pin_gpio_dir(pin_t pin) {
    std::ostringstream oss;
    oss << "/sys/class/gpio" << pin;
    return oss.str();
}

bool set_pin_direction(pin_t pin, pin_direction direction) {
    auto filename = pin_gpio_dir(pin) + "/direction";
    std::ofstream o(filename);
    if (!o) {
        std::cerr << "Error opening " << filename << std::endl;
        return false;
    }

    switch(direction) {
        case pin_direction::in:
            o << "in" << std::flush;
            break;
        case pin_direction::out:
            o << "out" << std::flush;
            break;
    }

    return o.good();
}

// -1 on error, 0 or 1 on succeess
int read_pin_value(pin_t pin) {
    std::string filename = pin_gpio_dir(pin) + "/value";
    std::ifstream in(filename);
    if (!in) {
        return -1;
    }
    int result;
    in >> result;
    return in.good() ? result : -1;
}

int main(int argc, char *argv[]) {
    using std::cout;
    using std::cerr;
    using namespace std::literals::chrono_literals;

    if (argc < 2) {
        usage("missing command");
        std::exit(1);
    }

    std::string command = argv[1];
    bool ok = true;

    if (command == "read") {
        cout << "TODO\n";
        if (argc < 3) {
            usage("missing pin");
            std::exit(1);
        }
        auto pin = std::atoi(argv[2]);
        if (!export_pin(pin)) {
            cerr << "Error exporting pin " << pin << "\n";
            ok = false;
        }
        std::this_thread::sleep_for(100ms);
        if (!set_pin_direction(pin, pin_direction::in)) {
            cerr << "Error setting pin direction\n";
            ok = false;
        }
        auto value = read_pin_value(pin);
        if (value == -1) {
            cerr << "Error reading pin value\n";
            ok = false;
        }
        cout << "Pin " << pin << " value: " << value << std::endl;
        if (!unexport_pin(pin)) {
            cerr << "Error unexporting pin " << pin << "\n";
            ok = false;
        }
    } else if (command == "write") {
        cout << "TODO\n";
    } else if (command == "wait") {
        cout << "TODO\n";
    }

    if (!ok) {
        cerr << "Error occurred" << std::endl;
    }

    return ok ? 0 : 1;
}
