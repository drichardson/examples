#include <cstdlib>
#include <cstring>
#include <fcntl.h>
#include <iostream>
#include <sstream>
#include <thread>
#include <unistd.h>

void usage(char const* msg) {
    using namespace std;
    cout << msg
        << "\nUsage:\n"
        << "  gpio_userspace read <pin>\n"
        << "  gpio_userspace write <pin> <value>\n"
        << "  gpio_userspace wait <pin>\n"
        << endl;
}

enum class pin_direction {
    in,
    out
};

class FileDescriptor {
private:
    int _fd = -1;
public:
    FileDescriptor(int fd) : _fd(fd) {
    }

    FileDescriptor(FileDescriptor&& rhs) {
        _fd = rhs._fd;
        rhs._fd = -1;
    }

    ~FileDescriptor() {
        if (_fd >= 0) {
            ::close(_fd);
        }
    }

    int fd() const {
        return _fd;
    }

    bool ok() const {
        return _fd != -1;
    }
};

FileDescriptor open_file_descriptor(std::string const & filename, int flags) {
    return FileDescriptor(::open(filename.c_str(), flags));
}

FileDescriptor open_write_only(std::string const & filename) {
    return open_file_descriptor(filename, O_WRONLY);
}

FileDescriptor open_read_only(std::string const & filename) {
    return open_file_descriptor(filename, O_RDONLY);
}

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
        if (argc < 3) {
            usage("missing pin");
            std::exit(1);
        }
        std::string pin = argv[2];
        if (!export_pin(pin)) {
            std::exit(1);
        }
        if (!set_pin_direction(pin, pin_direction::in)) {
            cerr << "Error setting pin direction\n";
            ok = false;
        }
        auto value = read_pin_value(pin);
        if (value == -1) {
            cerr << "Error reading pin value\n";
            ok = false;
        }
        cout << value << std::endl;
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
