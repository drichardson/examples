#include "file_descriptor.hpp"

#include <unistd.h>
#include <fcntl.h>

namespace gpio {

FileDescriptor::~FileDescriptor() {
    if (_fd >= 0) {
        ::close(_fd);
    }
}

FileDescriptor open_file_descriptor(std::string const & filename, int flags) {
    return FileDescriptor(::open(filename.c_str(), flags));
}

FileDescriptor open_write_only(std::string const & filename) {
    return open_file_descriptor(filename, O_WRONLY);
}

FileDescriptor open_read_only(std::string const & filename) {
    return open_file_descriptor(filename, O_RDONLY);
}

}

