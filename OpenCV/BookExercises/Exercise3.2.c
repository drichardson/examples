//
//  Exercise3.2.c

#include <stdio.h>
#include <opencv2/highgui/highgui_c.h>
#include <opencv2/imgproc/imgproc_c.h>
#include <opencv2/core/core_c.h>

static void PrintMat(const char* msg, const CvMat* mat)
{
	printf("%s\n", msg);
	
	CvSize size = cvGetSize(mat);
	int x, y;
	
	for(x = size.height - 1; x >= 0; x--)
	{
		printf("#%d: ", x);
		const unsigned char* ptr = (const unsigned char*)(mat->data.ptr + x * mat->step);
		for(y = 0; y < size.width; y++)
		{
			printf("%d", ptr[y]);
			
			if ( y < size.width - 1 )
			{
				putchar(',');
			}
		}
		printf("\n");
	}
}

int main (int argc, const char * argv[])
{	
	CvMat* mat = cvCreateMat(100, 100, CV_8UC3);
	PrintMat("Initialized matrix", mat);
	cvSet(mat, cvScalarAll(0), NULL);
	PrintMat("Zeroed matrix", mat);
	
	// Part (a)
	cvCircle(mat, cvPoint(30, 30), 10, cvScalarAll(0xff), 1, CV_AA, 0);
	PrintMat("Circle", mat);
	
	cvNamedWindow("Circle", CV_WINDOW_AUTOSIZE);
	cvShowImage("Circle", mat);
	cvWaitKey(0);
	
    cvReleaseMat(&mat);

    return 0;
}

