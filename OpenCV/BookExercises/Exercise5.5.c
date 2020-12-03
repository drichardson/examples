//
//  Exercise5.5.c

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
	
	IplImage* grayscale1 = cvCreateImage(cvSize(src1->width, src1->height), src1->depth, 1);
	IplImage* grayscale2 = cvCreateImage(cvSize(src2->width, src2->height), src2->depth, 1);
	
	cvCvtColor(src1, grayscale1, CV_RGB2GRAY);
	cvCvtColor(src2, grayscale2, CV_RGB2GRAY);
	
	IplImage* diff = cvCreateImage(cvSize(src2->width, src2->height), src2->depth, 1);
	cvAbsDiff(grayscale1, grayscale2, diff);
	
	IplImage* result = cvCreateImage(cvSize(src2->width, src2->height), src2->depth, 1);
	cvThreshold(diff, result, 40, 255, CV_THRESH_BINARY);
	
	IplImage* mopResult = cvCreateImage(cvSize(diff->width, diff->height), diff->depth, 1);
	cvMorphologyEx(result, mopResult, NULL, NULL, CV_MOP_OPEN, 1);
	
	cvNamedWindow("diff", CV_WINDOW_NORMAL);
	cvShowImage("diff", diff);
	cvWaitKey(0);
	cvNamedWindow("result", CV_WINDOW_NORMAL);
	cvShowImage("result", result);
	cvWaitKey(0);
	cvNamedWindow("mopResult", CV_WINDOW_NORMAL);
	cvShowImage("mopResult", mopResult);
	cvWaitKey(0);
	
    return 0;
}

