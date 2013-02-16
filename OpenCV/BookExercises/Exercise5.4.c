//
//  Exercise5.4.c

#include <stdio.h>
#include <opencv2/highgui/highgui_c.h>
#include <opencv2/imgproc/imgproc_c.h>
#include <opencv2/core/core_c.h>

int main (int argc, const char * argv[])
{
	if ( argc != 3 ) {
		fprintf(stderr, "Expected 2 image filenames.\n");
		exit(1);
	}
	
	IplImage* src1 = cvLoadImage(argv[1], CV_LOAD_IMAGE_COLOR);
	
	if ( src1 == NULL ) {
		fprintf(stderr, "Couldn't load file 1 %s\n", argv[1]);
		exit(1);
	}
	
	IplImage* src2 = cvLoadImage(argv[2], CV_LOAD_IMAGE_COLOR);
	
	if ( src2 == NULL ) {
		fprintf(stderr, "Couldn't load file 2 %s\n", argv[2]);
		exit(1);
	}
	
	if ( src1->width != src2->width || src1->height != src2->height ) {
		fprintf(stderr, "2 images should have the same width and height but they don't.\n");
		exit(1);
	}
	
	// part a
	IplImage* diff12 = cvCreateImage( cvSize(src1->width, src1->height), src1->depth, src1->nChannels);
	
	cvAbsDiff(src1, src2, diff12);
	
	cvNamedWindow("diff12", CV_WINDOW_NORMAL);
	cvShowImage("diff12", diff12);
	cvWaitKey(0);
	
	// part b
	IplImage* cleandiff = cvCreateImage( cvSize(src1->width, src1->height), src1->depth, src1->nChannels);
	cvErode(diff12, cleandiff, NULL, 1);
	cvDilate(cleandiff, cleandiff, NULL, 1);
	cvNamedWindow("cleandiff", CV_WINDOW_NORMAL);
	cvShowImage("cleandiff", cleandiff);
	cvWaitKey(0);
	
	// part c
	IplImage* dirtydiff = cvCreateImage( cvSize(src1->width, src1->height), src1->depth, src1->nChannels);
	cvDilate(diff12, dirtydiff, NULL, 1);
	cvErode(dirtydiff, dirtydiff, NULL, 1);
	cvNamedWindow("dirtydiff", CV_WINDOW_NORMAL);
	cvShowImage("dirtydiff", dirtydiff);
	cvWaitKey(0);
	
    return 0;
}

