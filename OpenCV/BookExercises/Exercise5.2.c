//
//  Exercise5.2.c

#include <stdio.h>
#include <opencv2/highgui/highgui_c.h>
#include <opencv2/imgproc/imgproc_c.h>
#include <opencv2/core/core_c.h>

int main (int argc, const char * argv[])
{
	IplImage *image = cvCreateImage( cvSize(100, 100), IPL_DEPTH_8U, 1 );
	IplImage *image5x5 = cvCreateImage( cvSize(100, 100), IPL_DEPTH_8U, 1 );
	IplImage *image9x9 = cvCreateImage( cvSize(100, 100), IPL_DEPTH_8U, 1 );
	IplImage *image5x5twice = cvCreateImage( cvSize(100, 100), IPL_DEPTH_8U, 1 );
	
	cvSet(image, cvScalarAll(0), NULL);
	cvSetImageROI(image, cvRect(49, 49, 1, 1));
	cvSet(image, cvScalarAll(255), NULL);
	cvResetImageROI(image);
	
	cvSmooth(image, image5x5, CV_GAUSSIAN, 5, 5, 0, 0);
	cvSmooth(image, image9x9, CV_GAUSSIAN, 9, 9, 0, 0);
	cvSmooth(image5x5, image5x5twice, CV_GAUSSIAN, 5, 5, 0, 0);
	
	cvNamedWindow("Image", CV_WINDOW_AUTOSIZE);
	cvShowImage("Image", image);
	cvNamedWindow("5", CV_WINDOW_AUTOSIZE);
	cvShowImage("5", image5x5);
	cvNamedWindow("9", CV_WINDOW_AUTOSIZE);
	cvShowImage("9", image9x9);
	cvNamedWindow("5,2", CV_WINDOW_AUTOSIZE);
	cvShowImage("5,2", image5x5twice);
	cvWaitKey(0);
	
	cvReleaseImage( &image5x5 );
	cvReleaseImage( &image9x9 );
	cvReleaseImage( &image5x5twice );
	cvReleaseImage( &image );
    return 0;
}

