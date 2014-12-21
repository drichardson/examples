// Access gpio via sysfs interface as described here:
// https://www.kernel.org/doc/Documentation/gpio/sysfs.txt

#include <cstdlib>
#include <cstring>
#include <fcntl.h>
#include <iostream>
#include <poll.h>
#include <thread>
#include <unistd.h>
#include "pins.hpp"
#include "file_descriptor.hpp"

void usage(char const* msg) {
    using namespace std;
    cout << msg
        << "\nUsage:\n"
        << "  gpio_userspace read <pin>\n"
        << "  gpio_userspace write <pin> <value>\n"
        << "  gpio_userspace watch <pin> <rising|falling|both>\n"
        << endl;
}

// Returns 0 on success, 1 on failure.
int do_read(std::string const & pin) {
    using std::cerr;
    gpio::PinExporter exporter(pin);
    if (!exporter.ok()) {
        return 1;
    }
    if (!set_pin_direction(pin, gpio::pin_direction::in)) {
        cerr << "Error setting pin direction\n";
        return 1;
    }
    auto value = gpio::read_pin_value(pin);
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
    gpio::PinExporter exporter(pin);
    if (!exporter.ok()) {
        cerr << "Error exporting pin " << pin << std::endl;
        return 1;
    }

    if (!set_pin_direction(pin, gpio::pin_direction::out)) {
        cerr << "Error setting pin direction\n";
        return 1;
    }

    if (!gpio::write_pin_value(pin, value)) {
        cerr << "Error writing " << value << " to pin " << pin << std::endl;
        return 1;
    }

    return 0;
}

int do_watch(std::string const & pin, std::string const & edge_trigger) {
    using std::cerr;
    using std::cout;
    using std::endl;
    gpio::PinExporter exporter(pin);
    if (!exporter.ok()) {
        cerr << "Error exporting pin " << pin << endl;
        return 1;
    }
    if (!gpio::set_pin_direction(pin, gpio::pin_direction::in)) {
        cerr << "Error setting pin direction to input" << endl;
        return 1;
    }
    if (!gpio::set_pin_edge_trigger(pin, edge_trigger)) {
        cerr << "Error setting pin edge trigger" << endl;
        return 1;
    }

    auto filename = gpio::pin_gpio_dir(pin) + "/value";
    gpio::FileDescriptor file = gpio::open_read_only(filename);
    if (!file.ok()) {
        std::perror(("Error opening pin value file " + filename).c_str());
        return 1;
    }

    struct pollfd pfd;
    pfd.fd = file.fd();
    pfd.events = POLLPRI;
    int constexpr timeout_ms = 5000;
    while(true) {
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
