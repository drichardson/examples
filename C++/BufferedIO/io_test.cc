#include <stdio.h>
#include <iostream>
#include <fstream>
#include <sstream>
#include <streambuf>
#include <string>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <stdlib.h>
#include <string.h>
#include <thread>
#include <mutex>

using std::string;
using std::ostringstream;
using std::cerr;
using std::cout;
using std::endl;
using std::ofstream;
using std::ios_base;

enum BufferingMode
{
    BufferingMode_None,
    BufferingMode_FILEPTR,
    BufferingMode_IOSTREAM,

    BufferMode_count
};

struct TestParameters
{
    // User provided
    int thread_count;
    int buffer_size;
    int min_bytes_per_log;
    int test_duration;
    BufferingMode mode;

    // Other globals
    char const* log_msg_base;
    
    int fd_out;
    FILE* fp_out;
    ofstream stream;

    std::mutex log_mutex;
};

int to_int(char const* str)
{
    char* s = NULL;
    int i = strtol(str, &s, 10); 
    if (i == 0 && s != NULL && *s != 0) {
        cerr << "Error parsing parameter. '" << str << "' is not an integer." << endl;
        abort();
    }
    return i;
}

char* make_log_msg_base(int len)
{
    char* s = new char[len+1];
    int written = snprintf(s, len, "Test log message ");
    int i = 0;
    while(written < len) {
        s[written] = 'a' + (i++ % 26);
        ++written;
    }
    s[len] = 0;
    return s;
}

string output_filename()
{
    ostringstream oss;
    oss << "io_test-" << getpid() << ".log";
    return oss.str();
}

void read_parameters(int argc, char** argv, TestParameters* p)
{
    if (argc != 6) {
        cerr << "Usage: io_test <NT> <NB> <NL> <T> <M>\n"
            << "  NT - number of logging threads per process\n"
            << "  NB - buffer size in bytes\n"
            << "  NL - number of bytes to emit per log, not including thread id and counter\n"
            << "  T  - test duration in seconds (integer)\n"
            << "  M  - buffering mode. NONE, FILEPTR, or IOSTREAM\n"
            << std::flush;
        abort();
    }

    p->thread_count  = to_int(argv[1]);
    p->buffer_size   = to_int(argv[2]);
    p->min_bytes_per_log = to_int(argv[3]);
    p->test_duration = to_int(argv[4]);

    char* mode = argv[5];
    if (strcmp(mode, "NONE") == 0) {
        if (p->buffer_size != 0) {
            cerr << "NONE mode but non-zero buffer size (" << p->buffer_size << ") given. That just doesn't make sense." << endl;
            abort();
        }
        p->mode = BufferingMode_None;
        p->fd_out = open(output_filename().c_str(), O_CREAT | O_EXCL | O_WRONLY, S_IRWXU);
        if (p->fd_out == -1) {
            cerr << "Could not open output file '" << output_filename() << "' Error: " << errno << endl;
            abort();
        }
    } else if(strcmp(mode, "FILEPTR") == 0) {
        if (p->buffer_size <= 0) {
            cerr << "FILEPTR mode but <= zero buffer size (" << p->buffer_size << ") given. That just doesn't make sense." << endl;
            abort();
        }
        p->mode = BufferingMode_FILEPTR;
        p->fp_out = fopen(output_filename().c_str(), "wb");
        if (p->fp_out == NULL) {
            cerr << "Could not open output file '" << output_filename() << endl;
            abort();
        }
        char* buf = new char[p->buffer_size];
        int rc = setvbuf(p->fp_out, buf, _IOFBF, p->buffer_size);
        if (rc != 0) {
            cerr << "Couldn't set fp_out bufsize to " << p->buffer_size << endl;
            abort();
        }
    } else if(strcmp(mode, "IOSTREAM") == 0) {
        if (p->buffer_size <= 0) {
            cerr << "IOSTREAM mode but <= zero buffer size (" << p->buffer_size << ") given. That just doesn't make sense." << endl;
            abort();
        }
        p->mode = BufferingMode_IOSTREAM;
        p->stream.rdbuf()->pubsetbuf(new char[p->buffer_size], p->buffer_size);
        p->stream.open(output_filename(), ios_base::binary|ios_base::out|ios_base::trunc);
    } else {
        cerr << "Invalid buffering mode '" << mode << '\'' << endl;
        abort();
    }

    p->log_msg_base = make_log_msg_base(p->min_bytes_per_log);
}

void run_test(TestParameters * p);

int main(int argc, char** argv)
{
    TestParameters params;
    read_parameters(argc, argv, &params);

    cout << "Params: "
        << "NT=" << params.thread_count
        << " NB=" << params.buffer_size
        << " NL=" << params.min_bytes_per_log
        << " T=" << params.test_duration
        << " M=" << params.mode
        << endl;

    run_test(&params);

    return 0;
}

void log_fd(TestParameters* params, string const & msg)
{
    params->log_mutex.lock();
    int rc = write(params->fd_out, msg.c_str(), msg.size());
    params->log_mutex.unlock();

    if (rc < 0 || static_cast<string::size_type>(rc) != msg.size()) {
        cerr << "log_fd failed. Wrote " << rc
           <<  " bytes but expected " << msg.size()
           << ". Errno: " << errno << ". " << strerror(errno)
           << endl;
        abort();
    }
}

void log_fp(TestParameters* params, string const & msg)
{
    params->log_mutex.lock();
    int rc = fwrite(msg.c_str(), msg.size(), 1, params->fp_out);
    params->log_mutex.unlock();

    if (rc != 1) {
        cerr << "log_fp failed. Expected 1 but got " << rc << endl;
        abort();
    }
}

void log_stream(TestParameters* params, string const & msg)
{
    params->log_mutex.lock();
    params->stream << msg;
    params->stream.write(msg.c_str(), msg.size());
    params->log_mutex.unlock();

    if (!params->stream.good()) {
        cerr << "log_stream got bad stream.";
        abort();
    }
}

void test_routine(TestParameters* params)
{
    void (*f)(TestParameters*, string const&);
    switch(params->mode) {
    case BufferingMode_None:
        f = log_fd;
        break;
    case BufferingMode_FILEPTR:
        f = log_fp;
        break;
    case BufferingMode_IOSTREAM:
        f = log_stream;
        break;
    default:
        cerr << "Unhandled buffering mode." << endl;
        abort();
    }

    int i = 0;

    while(1) {
        ostringstream oss;
        oss << &i << ':' << i << ' ' << params->log_msg_base << '\n';
        ++i;
        f(params, oss.str());
    }
}

void run_test(TestParameters* params)
{
    for(int i = 0; i < params->thread_count; ++i) {
        std::thread t(test_routine, params);
        t.detach();
    }

    sleep(params->test_duration);
    exit(0);
}

