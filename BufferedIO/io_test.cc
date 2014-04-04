#include <stdio.h>
#include <iostream>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <stdlib.h>
#include <string.h>

using std::cerr;
using std::cout;
using std::endl;

enum BufferingMode
{
    BufferingMode_None,
    BufferingMode_FILEPTR,
    BufferingMode_IOSTREAM,

    BufferMode_count
};

struct TestParameters
{
    int thread_count;
    int buffer_size;
    int bytes_per_log;
    int test_duration;
    BufferingMode mode;
};

int to_int(char const* str)
{
    char* s = NULL;
    int i = strtol(str, &s, 10); 
    if (i == 0 && s != NULL && *s != 0) {
        cerr << "Error parsing parameter. '" << str << "' is not an integer." << endl;
        exit(1);
    }
}

void read_parameters(int argc, char** argv, TestParameters* p)
{
    if (argc != 6) {
        cerr << "Usage: io_test <NT> <NB> <NL> <T> <M>\n"
            << "  NT - number of logging threads per process\n"
            << "  NB - buffer size in bytes\n"
            << "  NL - number of bytes to emit per log\n"
            << "  T  - test duration in seconds (integer)\n"
            << "  M  - buffering mode. NONE, FILEPTR, or IOSTREAM\n"
            << std::flush;
        return exit(1);
    }

    p->thread_count  = to_int(argv[1]);
    p->buffer_size   = to_int(argv[2]);
    p->bytes_per_log = to_int(argv[3]);
    p->test_duration = to_int(argv[4]);

    char* mode = argv[5];
    if (strcmp(mode, "NONE") == 0) {
        p->mode = BufferingMode_None;
    } else if(strcmp(mode, "FILEPTR") == 0) {
        p->mode = BufferingMode_FILEPTR;
    } else if(strcmp(mode, "IOSTREAM") == 0) {
        p->mode = BufferingMode_IOSTREAM;
    } else {
        cerr << "Invalid buffering mode '" << mode << '\'' << endl;
        exit(1);
    }
}

int main(int argc, char** argv)
{
    TestParameters params;
    read_parameters(argc, argv, &params);

    cout << "Params: "
        << "NT=" << params.thread_count
        << " NB=" << params.buffer_size
        << " NL=" << params.bytes_per_log
        << " T=" << params.test_duration
        << " M=" << params.mode
        << endl;

    return 0;
}

