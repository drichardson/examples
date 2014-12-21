#pragma once

#include <string>

namespace gpio {

// Manages file descriptor, closing when destructed. Will let you
// move ownership of file descriptor but will not let you create
// two FileDescriptor instances owning the same descriptor.
class FileDescriptor {
private:
    int _fd;
public:
    FileDescriptor(int fd) : _fd(fd) {}

    FileDescriptor(FileDescriptor&& rhs) {
        _fd = rhs._fd;
        rhs._fd = -1;
    }

    ~FileDescriptor();

    int fd() const { return _fd; }
    bool ok() const { return _fd != -1; }
};

FileDescriptor open_file_descriptor(std::string const & filename, int flags);
FileDescriptor open_write_only(std::string const & filename);
FileDescriptor open_read_only(std::string const & filename);

}

