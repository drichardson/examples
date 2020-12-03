//
//  Exercise5.9.c

#include <stdio.h>
#include <opencv2/highgui/highgui_c.h>
#include <opencv2/imgproc/imgproc_c.h>
#include <opencv2/core/core_c.h>

int main (int argc, const char * argv[])
{
	if ( argc != 2 )
	{
		fprintf(stderr, "Usage: <image>\n");
		exit(1);
	}
	
	IplImage* image = cvLoadImage(argv[1], CV_LOAD_IMAGE_UNCHANGED);
	
	// part a
	IplImage* topHatImage = cvCreateImage(cvSize(image->width, image->height), image->depth, image->nChannels);
	cvMorphologyEx(image, topHatImage, NULL, NULL, CV_MOP_TOPHAT, 2);
	
	cvNamedWindow("topHatImage", CV_WINDOW_NORMAL);
	cvShowImage("topHatImage", topHatImage);
	cvWaitKey(0);
	
	// part b
	IplImage* mask = cvCreateImage(cvSize(image->width, image->height), IPL_DEPTH_8U, 1);
	cvCvtColor(topHatImage, mask, CV_RGB2GRAY);
	
	cvNamedWindow("mask", CV_WINDOW_NORMAL);
	cvShowImage("mask", mask);
	cvWaitKey(0);
	
	// part c
	cvSet(topHatImage, cvScalarAll(80), mask);
	cvNamedWindow("topHatWithMask", CV_WINDOW_NORMAL);
	cvShowImage("topHatWithMask", topHatImage);
	cvWaitKey(0);
	
	
    return 0;
}

