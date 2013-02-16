//
//  Exercise3.5.c

#include <stdio.h>
#include <opencv2/highgui/highgui_c.h>
#include <opencv2/imgproc/imgproc_c.h>
#include <opencv2/core/core_c.h>

int main (int argc, const char * argv[])
{
	IplImage* image = cvCreateImage( cvSize(210, 210), IPL_DEPTH_8U, 1);
	
	cvSet(image, cvScalarAll(0), NULL);
	int value = 0;
	
	CvRect roi;
	for(value = 0, roi = cvRect(0, 0, 210, 210); value != 200; roi.x = roi.y = roi.x + 5, roi.width = roi.height = roi.width - 10)
	{
		printf("roi: %d, %d, %d, %d\n", roi.x, roi.y, roi.width, roi.height);
		cvSetImageROI(image, roi);
		cvSet(image, cvScalarAll(value), NULL);
		value += 20;
	}
	
	cvResetImageROI(image);
	
	cvNamedWindow("Image", CV_WINDOW_AUTOSIZE);
	cvShowImage("Image", image);
	cvWaitKey(0);
	
	cvReleaseImage(&image);

    return 0;
}

