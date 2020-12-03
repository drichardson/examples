#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <iconv.h>
#include <errno.h>

int main(int argc, char** argv) {
    iconv_t cd = iconv_open("UTF-32", "UTF-8");

    if (cd == (iconv_t)-1) fprintf(stderr, "iconv_open failed with %d\n", errno);

    char* in = "this is a test";
    size_t inbufbytes = strlen(in);

    // If I make the outbufbytesleft 1 smaller, I get an Argument List Too Long error. I don't think
    // gives you any sort of idea of a buffer will be big enough, you're just supposed to call it
    // again to keep going when you run out of space. That said, seems like I should be able to determine
    // an upper bound going from UTF-8 (1-3 bytes per character) to UTF-32 (4 bytes per character). Perhaps
    // the +1 is for some null terminator, but I haven't read anything to that effect.
    size_t outbufbytes = (inbufbytes+1) * 4;
    char* out = malloc(outbufbytes);

    size_t inbufbytesleft = inbufbytes;
    size_t outbufbytesleft = outbufbytes;
    char* inbuf = in;
    char* outbuf = out;
    int rc = iconv(cd, &inbuf, &inbufbytesleft, &outbuf, &outbufbytesleft);
    if (rc == -1) fprintf(stderr, "iconv failed with -1. errno is %d: %s\n", errno, strerror(errno));
    printf("rc = %d, inbuf = %s, outbuf = %s, inbufbytesleft = %zu, outbufbytesleft = %zu\n", rc, inbuf, outbuf, inbufbytesleft, outbufbytesleft);


    rc = iconv_close(cd);
    if (rc != 0) fprintf(stderr, "iconv_close failed with %d\n", errno);

    return 0;
}

