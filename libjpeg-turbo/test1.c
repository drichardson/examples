#include <stdio.h>
#include <stdlib.h>
#include <jpeglib.h>

typedef struct {
    int width;
    int height;
    int bytesPerPixel;
    unsigned char *pixels;
} image_t;

int main(int argc, const char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: test1 <jpeg_filename>\n");
        exit(1);
    }

    struct jpeg_decompress_struct cinfo;
    struct jpeg_error_mgr jerr;
    JSAMPARRAY buffer;  
    int row_stride;     
    unsigned char *out;

    FILE* infile = fopen(argv[1],"rb");
    if (infile == 0) {
        fprintf(stderr, "couldn't open input file %s\n", argv[1]);
        exit(1);
    }

    cinfo.err = jpeg_std_error(&jerr);

    jpeg_create_decompress(&cinfo);
    jpeg_stdio_src(&cinfo, (FILE *)infile);
    jpeg_read_header(&cinfo, TRUE);
    jpeg_start_decompress(&cinfo);
    row_stride = cinfo.output_width * cinfo.output_components;
    out = malloc(cinfo.output_width*cinfo.output_height*cinfo.output_components);

    image_t img;
    img.pixels = out;
    img.width = cinfo.output_width;
    img.height = cinfo.output_height;
    img.bytesPerPixel = cinfo.out_color_components;

    while (cinfo.output_scanline < cinfo.output_height) {
        unsigned char *rowp[5];
        rowp[0] = (unsigned char *) out + row_stride * cinfo.output_scanline;
        rowp[1] = (unsigned char *) out + row_stride * cinfo.output_scanline+1;
        rowp[2] = (unsigned char *) out + row_stride * cinfo.output_scanline+2;
        rowp[3] = (unsigned char *) out + row_stride * cinfo.output_scanline+3;
        rowp[4] = (unsigned char *) out + row_stride * cinfo.output_scanline+4;

        int rc = jpeg_read_scanlines(&cinfo, rowp, 5);
        printf("reading line(s). Got %d\n", rc);
    }

    jpeg_finish_decompress(&cinfo);
    jpeg_destroy_decompress(&cinfo);

    fclose(infile);

    return 0;
}

