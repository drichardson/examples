#pragma once
#include <string>
#include <iostream>

namespace gpio {

enum class pin_direction {
    in,
    out
};


bool export_pin(std::string const & pin);
bool unexport_pin(std::string const & pin);
std::string pin_gpio_dir(std::string const & pin);
bool set_pin_direction(std::string const & pin, pin_direction direction);
bool set_pin_edge_trigger(std::string const & pin, std::string const & trigger);
int read_pin_value(std::string const & pin);
bool write_pin_value(std::string const & pin, std::string const & value);

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


}

