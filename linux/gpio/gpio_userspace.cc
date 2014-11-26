// Access gpio via sysfs interface as described here:
// https://www.kernel.org/doc/Documentation/gpio/sysfs.txt

#include <cstdlib>
#include <cstring>
#include <fcntl.h>
#include <iostream>
#include <poll.h>
#include <sstream>
#include <thread>
#include <unistd.h>

void usage(char const* msg) {
    using namespace std;
    cout << msg
        << "\nUsage:\n"
        << "  gpio_userspace read <pin>\n"
        << "  gpio_userspace write <pin> <value>\n"
        << "  gpio_userspace watch <pin> <rising|falling|both>\n"
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

class PinExporter {
    std::string _pin;
    bool _exported;
public:
    PinExporter(std::string const & pin) : _pin(pin) {
        _exported = export_pin(_pin);
        if (!_exported) {
            std::cerr << "Error exporting pin " << _pin << std::endl;
        }
    }
    PinExporter(PinExporter&& rhs) {
        _pin = rhs._pin;
        _exported = rhs._exported;
        rhs._exported = false;
    }
    ~PinExporter() {
        if (_exported) {
            if (!unexport_pin(_pin)) {
                std::cerr << "Error unexporting pin " << _pin <<
                    " in ~PinExport()" << std::endl;
            }
        }
    }
    bool ok() const { return _exported; }
};

// Returns 0 on success, 1 on failure.
int do_read(std::string const & pin) {
    using std::cerr;
    PinExporter exporter(pin);
    if (!exporter.ok()) {
        return 1;
    }
    if (!set_pin_direction(pin, pin_direction::in)) {
        cerr << "Error setting pin direction\n";
        return 1;
    }
    auto value = read_pin_value(pin);
    if (value == -1) {
        cerr << "Error reading pin value\n";
        return 1;
    }
    std::cout << value << std::endl;
    return 0;
}

int do_write(std::string const & pin, std::string const & value) {
    using std::cout;
    using std::cerr;
    cout << "WARNING: Make sure you do not have pin " << pin <<
        " connected to ground, or you'll have a short. Enter 'ok' to continue: "
        << std::flush;
    std::string answer;
    std::cin >> answer;
    if (answer != "ok") {
        return 1;
    }
    PinExporter exporter(pin);
    if (!exporter.ok()) {
        cerr << "Error exporting pin " << pin << std::endl;
        return 1;
    }

    if (!set_pin_direction(pin, pin_direction::out)) {
        cerr << "Error setting pin direction\n";
        return 1;
    }

    if (!write_pin_value(pin, value)) {
        cerr << "Error writing " << value << " to pin " << pin << std::endl;
        return 1;
    }

    return 0;
}

int do_watch(std::string const & pin, std::string const & edge_trigger) {
    using std::cerr;
    using std::cout;
    using std::endl;
    PinExporter exporter(pin);
    if (!exporter.ok()) {
        cerr << "Error exporting pin " << pin << endl;
        return 1;
    }
    if (!set_pin_direction(pin, pin_direction::in)) {
        cerr << "Error setting pin direction to input" << endl;
        return 1;
    }
    if (!set_pin_edge_trigger(pin, edge_trigger)) {
        cerr << "Error setting pin edge trigger" << endl;
        return 1;
    }

    auto filename = pin_gpio_dir(pin) + "/value";
    FileDescriptor file = open_read_only(filename);
    if (!file.ok()) {
        std::perror(("Error opening pin value file " + filename).c_str());
        return 1;
    }

    struct pollfd pfd;
    pfd.fd = file.fd();
    pfd.events = POLLPRI;
    int constexpr timeout_ms = 5000;
    while(true) {
        pfd.events = POLLPRI;
        pfd.fd = file.fd();
        auto rc = ::poll(&pfd, 1, timeout_ms);
        if (rc > 0) {
            // lseek back to beginning, per gpio sysfs spec
            auto off = ::lseek(pfd.fd, 0, SEEK_SET);
            if (off == -1) {
                std::perror("Error seeking to beginning of file.");
                break;
            }

            if (pfd.revents & POLLPRI) { 
                char buf;
                auto rc = ::read(pfd.fd, &buf, sizeof(buf));
                if (rc == -1) {
                    std::perror("Error reading value after poll signalled it was available.");
                    break;
                }
                cout << buf << endl;
            }
            // Ignore POLLERR. sysfs files always return POLLERR on poll (see fs/sysfs/file.c).
        } else if (rc == 0) {
            // timeout occurred
            cout << "Timeout occurred" << endl;
            break;
        } else if (rc == -1) {
            // other error
            std::perror("poll failed");
            break;
        }
    }

    return 0;
}

int main(int argc, char *argv[]) {
    using std::cout;
    using std::cerr;
    using namespace std::literals::chrono_literals;

    if (argc < 2) {
        usage("missing command");
        return 1;
    }

    std::string command = argv[1];

    if (argc < 3) {
        usage("missing pin");
        return 1;
    }
    std::string pin = argv[2];

    if (command == "read") {
        return do_read(pin);
    } else if (command == "write") {
        if (argc < 4) {
            usage("missing pin value");
            std::exit(1);
        }
        std::string value = argv[3];
        return do_write(pin, value);
    } else if (command == "watch") {
        if (argc < 4) {
            usage("missing edge trigger");
            std::exit(1);
        }
        std::string edge_trigger = argv[3];
        if (edge_trigger != "rising" && edge_trigger != "falling" && edge_trigger != "both") {
            usage("invalid edge trigger");
            std::exit(1);
        }
        return do_watch(pin, edge_trigger);
    }

    usage((std::string("Unknown command " ) + command).c_str());
    return -1;
}
