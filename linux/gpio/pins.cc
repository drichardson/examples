#include "pins.hpp"
#include "file_descriptor.hpp"
#include <sstream>
#include <cstring>
#include <unistd.h>

namespace gpio {

bool export_pin(std::string const & pin) {
    std::string filename = "/sys/class/gpio/export";
    auto file = open_write_only(filename);
    if (!file.ok()) {
        std::perror(("Couldn't open " + filename).c_str());
        return false;
    }
    std::ostringstream oss;
    oss << pin;
    auto const & s = oss.str();
    auto rc = ::write(file.fd(), s.c_str(), s.size());
    if (rc == -1) {
        std::perror(("Error writing to " + filename).c_str());
        return false;
    }
    return true;
}

bool unexport_pin(std::string const & pin) {
    std::string filename = "/sys/class/gpio/unexport";
    FileDescriptor file = open_write_only(filename);
    if (!file.ok()) {
        std::perror(("Error opening " + filename).c_str());
        return false;
    }
    std::ostringstream oss;
    oss << pin;
    auto const & s = oss.str();
    auto rc = ::write(file.fd(), s.c_str(), s.size());
    if (rc == -1) {
        std::perror(("Error writing to " + filename).c_str());
        return false;
    }

    return true;
}

std::string pin_gpio_dir(std::string const & pin) {
    std::ostringstream oss;
    oss << "/sys/class/gpio/gpio" << pin;
    return oss.str();
}

bool set_pin_direction(std::string const & pin, pin_direction direction) {
    auto filename = pin_gpio_dir(pin) + "/direction";
    FileDescriptor file = open_write_only(filename);
    if (!file.ok()) {
        std::perror(("Error opening " + filename).c_str());
        return false;
    }
    char const* str_direction = nullptr;
    switch(direction) {
    case pin_direction::in:
        str_direction = "in";
        break;
    case pin_direction::out:
        str_direction = "out";
        break;
    }
    if (str_direction == nullptr) {
        return false;
    }
    auto rc = ::write(file.fd(), str_direction, std::strlen(str_direction));
    if (rc == -1) {
        std::perror(("Error writing to " + filename).c_str());
        return false;
    }

    return true;
}

bool set_pin_edge_trigger(std::string const & pin, std::string const & trigger) {
    auto filename = pin_gpio_dir(pin) + "/edge";
    FileDescriptor file = open_write_only(filename);
    if (!file.ok()) {
        std::perror(("Error opening " + filename).c_str());
        return false;
    }
    auto rc = ::write(file.fd(), trigger.c_str(), trigger.size());
    if (rc == -1) {
        std::perror(("Error writing trigger to " + filename).c_str());
        return false;
    }
    return true;
}

// -1 on error, 0 or 1 on succeess
int read_pin_value(std::string const & pin) {
    std::string filename = pin_gpio_dir(pin) + "/value";
    FileDescriptor file = open_read_only(filename);
    if (!file.ok()) {
        std::perror(("Error opening " + filename).c_str());
        return -1;
    }
    char c = -1;
    auto rc = ::read(file.fd(), &c, sizeof(c));
    if (rc != 1) {
        std::perror(("Error reading " + filename).c_str());
        return -1;
    }
    switch(c) {
    case '0':
        return 0;
    case '1':
        return 1;
    }
    return -1;
}

bool write_pin_value(std::string const & pin, std::string const & value) {
    std::string filename = pin_gpio_dir(pin) + "/value";
    FileDescriptor file = open_write_only(filename);
    if (!file.ok()) {
        std::perror(("Error opening " + filename).c_str());
        return false;
    }

    auto rc = ::write(file.fd(), value.c_str(), value.size());
    if (rc == -1) {
        std::perror(("Error writing to " + filename).c_str());
        return false;
    }

    return true;
}

}
