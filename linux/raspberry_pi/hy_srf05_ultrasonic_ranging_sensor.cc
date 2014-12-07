// Example using HY-SRF05 ultrasonic ranging sensor breakout
// board, which has 5 pins: Vcc (5V), GND, OUT, TRIG, and ECHO.
// OUT is used to put it into a mode that combines TRIG and ECHO
// on a single pin, but is not used in this example.
// TRIG must be held high (low?) long enough to start the output
// pulse. After the echo is receieved (if any) the ECHO line is held
// high in proportion to the delay between the time the TRIG pin was
// signaled and the echo was received.
// 
// On my prototype board, I'm driving the HY-SRF05 with a Raspberry Pi.
// I'm driving the TRIG pin directly with a RPi GPIO pin, since 3.3v
// appears to be able to signal the 5V HY-SRF05 input pin.
// However, when reading the 5V response, I'm level shifting it down
// to 3.3V because RPi pins are not 5V tolerant. I'm using a 7417 hex
// buffer driver with open collector output. The open collector output
// has a pull up resistor wired to RPis 3.3V output pin and the corresponding
// input pin is wired to the output of the HY-SRF05's ECHO pin.
#include "file_descriptor.hpp"
#include "pins.hpp"
#include <cmath>
#include <cstdlib>
#include <cstring>
#include <fcntl.h>
#include <iostream>
#include <poll.h>
#include <thread>
#include <unistd.h>

void usage(char const* msg) {
    using namespace std;
    cout << msg
        << "\nUsage:\n"
        << "  hy-srf05-ultrasonic-ranging-sensor.cc <gpio_trig> <gpio_echo> <air_temperature>\n"
        << "  where air_temperature is in degrees celcius\n"
        << flush;
}

int do_range(std::string const & trigger_pin, std::string const & echo_pin, double temperature_c);

int main(int argc, char *argv[]) {
    using std::cout;
    using std::cerr;
    using namespace std::literals::chrono_literals;

    if (argc < 4) {
        usage("missing arguments");
        return 1;
    }

    std::string trig = argv[1];
    std::string echo = argv[2];
    auto temperature_c = std::atof(argv[3]);

    return do_range(trig, echo, temperature_c);
}

// Get the velocity sound travels through dry air at a given temperature.
// Returns velocity in meters/second.
double velocity_of_sound_in_dry_air(double temperature_c) {
    // Convert to centimeters using formula taken from Sound Speed in Gases
    // at http://hyperphysics.phy-astr.gsu.edu/hbase/sound/souspe3.html#c1
    // which is v = sqrt((yRT)/M), where
    // y = adiabatic constant
    // R = gas constant
    // M = molecular mass of gas
    // T = absolute temperature
    // The average molecular mass of dry air is 28.95 gm/mol.
    // This leads to:
    // v = 20.05*sqrt(T)
    // for dry air, where T is in Kelvin.
    auto temperature_k = temperature_c + 273.15;
    return 20.05*std::sqrt(temperature_k); // in m/s
}

int do_range(std::string const & trigger_pin, std::string const & echo_pin, double temperature_c) {
    using std::cerr;
    using std::cout;
    using std::endl;
    using namespace std::literals::chrono_literals;
    gpio::PinExporter export_trigger(trigger_pin);
    if (!export_trigger.ok()) {
        cerr << "Error exporting trigger pin " << trigger_pin << endl;
        return 1;
    }
    if (!gpio::set_pin_direction(trigger_pin, gpio::pin_direction::out)) {
        cerr << "Error setting trigger pin direction to output" << endl;
        return 1;
    }
 
    gpio::PinExporter export_echo(echo_pin);
    if (!export_echo.ok()) {
        cerr << "Error exporting echo pin " << echo_pin << endl;
        return 1;
    }
    if (!gpio::set_pin_direction(echo_pin, gpio::pin_direction::in)) {
        cerr << "Error setting trigger pin direction to output" << endl;
        return 1;
    }
    if (!gpio::set_pin_edge_trigger(echo_pin, "both")) {
        cerr << "Error setting pin edge trigger" << endl;
        return 1;
    }
    auto echo_filename = gpio::pin_gpio_dir(echo_pin) + "/value";
    gpio::FileDescriptor echo_value_file = gpio::open_read_only(echo_filename);
    if (!echo_value_file.ok()) {
        std::perror(("Error opening echo pin value file " + echo_filename).c_str());
        return 1;
    }

    char c;
    // verify echo pin is already 0.
    auto rc = ::read(echo_value_file.fd(), &c, 1);
    if (rc == -1) {
        std::perror("Error reading echo value to see if it's already 0");
        return 1;
    }
    if (c != '0') {
        cerr << "Expected echo to initially be set to 0 but it is " << c << endl;
        return 1;
    }

    auto trigger_filename = gpio::pin_gpio_dir(trigger_pin) + "/value";
    gpio::FileDescriptor trigger_value_file = gpio::open_write_only(trigger_filename);
    if (!trigger_value_file.ok()) {
        std::perror(("Error opening trigger pin value file " + trigger_filename).c_str());
        return 1;
    }

    // One document on a similar breakout board, the SRF05, said the trigger pulse should
    // be 10uS minimum. So, drive the pin low for 15uS, then pulse high for 15uS, then low again.
    auto srf05_min_pulse = 15us;
    rc = ::write(trigger_value_file.fd(), "0", 1);
    if (rc == -1) {
        std::perror("Error initially setting trigger to 0");
        return 1;
    }
    std::this_thread::sleep_for(srf05_min_pulse);
    rc = ::write(trigger_value_file.fd(), "1", 1);
    if (rc == -1) {
        std::perror("Error setting trigger pulse to 1");
        return 1;
    }
    std::this_thread::sleep_for(srf05_min_pulse);
    rc = ::write(trigger_value_file.fd(), "0", 1);
    if (rc == -1) {
        std::perror("Error setting trigger pulse back to 0");
        return 1;
    }

    struct pollfd pfd;
    pfd.fd = echo_value_file.fd();
    pfd.events = POLLPRI;
    // SRF05 (which I'm basing this code off) doc says it will lower echo after
    // 30ms, even if nothing detected, so timeout some time after that.
    int constexpr timeout_ms = 100;
    std::chrono::high_resolution_clock::time_point echo_pulse_start;
    std::chrono::high_resolution_clock::time_point echo_pulse_end;
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

                if (buf == '1') {
                    // pulse went high
                    echo_pulse_start = std::chrono::high_resolution_clock::now();
                } else if (buf == '0') {
                    echo_pulse_end = std::chrono::high_resolution_clock::now();
                    break;
                } else {
                    cerr << "Unexpected value " << buf << endl;
                    return 1;
                }
            }
            // Ignore POLLERR. sysfs files always return POLLERR on poll (see fs/sysfs/file.c).
        } else if (rc == 0) {
            // timeout occurred. Don't expect this since SRF05 should go low after 30ms
            // regardless if anything was detected.
            cerr << "Timeout occurred" << endl;
            return 1;
        } else if (rc == -1) {
            // other error
            std::perror("poll failed");
            return 1;
        }
    }

    constexpr auto zero = std::chrono::high_resolution_clock::duration::zero();
    if (echo_pulse_start.time_since_epoch() == zero) {
        cerr << "echo pulse started was not set" << endl;
        return 1;
    }

    if (echo_pulse_end.time_since_epoch() == zero) {
        cerr << "echo pulse end was not set" << endl;
        return 1;
    }

    std::chrono::duration<double> pulse_duration_sec = echo_pulse_end - echo_pulse_start;

    // distance = velocity * time
    auto velocity = velocity_of_sound_in_dry_air(temperature_c);
    auto distance = velocity * pulse_duration_sec.count(); // in meters
    // since the sound had to travel to the object and back, divide by 2.
    auto distance_to_object_m = distance / 2.0;
    auto distance_to_object_cm = distance_to_object_m * 100.0;
    cout << "distance: " << distance_to_object_cm << "cm" << endl;

    return 0;
}


