//
//  Exercise5.10.c

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
	
	IplImage* resizedImage = cvCreateImage(cvSize(image->width/2, image->height/2), image->depth, image->nChannels);
	cvResize(image, resizedImage, CV_INTER_LINEAR);
	
	IplImage* down1 = cvCreateImage(cvSize(image->width/2, image->height/2), image->depth, image->nChannels);
	IplImage* down2 = cvCreateImage(cvSize(down1->width/2, down1->height/2), image->depth, image->nChannels);
	IplImage* down3 = cvCreateImage(cvSize(down2->width/2, down2->height/2), image->depth, image->nChannels);
	cvPyrDown(image, down1, CV_GAUSSIAN_5x5);
	cvPyrDown(down1, down2, CV_GAUSSIAN_5x5);
	cvPyrDown(down2, down3, CV_GAUSSIAN_5x5);
	
	cvNamedWindow("image", CV_WINDOW_NORMAL);
	cvShowImage("image", image);
	cvWaitKey(0);
	
	cvNamedWindow("resizedImage", CV_WINDOW_NORMAL);
	cvShowImage("resizedImage", resizedImage);
	cvWaitKey(0);
	
	cvNamedWindow("down3", CV_WINDOW_NORMAL);
	cvShowImage("down3", down3);
	cvWaitKey(0);
	
	
    return 0;
}

