//
//  Exercise3.4.c

#include <stdio.h>
#include <opencv2/highgui/highgui_c.h>
#include <opencv2/imgproc/imgproc_c.h>
#include <opencv2/core/core_c.h>

int main (int argc, const char * argv[])
{	
	CvMat* mat = cvCreateMat(100, 100, CV_8UC3);
	cvSet(mat, cvScalarAll(0), NULL);
	
	int y, x;
	
	// Draw green rectangle using cvPtr2D.	
	for(y = 5; y < 20; y++)
	{
		uchar* ptr = mat->data.ptr + mat->step * y;
		
		for(x = 20; x < 40; x++)
		{
			ptr[x * 3 + 1] = 0xff;
		}
	}
	
	cvNamedWindow("Circle", CV_WINDOW_AUTOSIZE);
	cvShowImage("Circle", mat);
	cvWaitKey(0);
	
    cvReleaseMat(&mat);

    return 0;
}

