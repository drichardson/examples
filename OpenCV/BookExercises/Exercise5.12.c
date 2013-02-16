//
//  Exercise5.12.c

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
	
	IplImage* image = cvLoadImage(argv[1], CV_LOAD_IMAGE_GRAYSCALE);
	
	cvNamedWindow("main", CV_WINDOW_NORMAL);
	cvShowImage("main", image);
	cvWaitKey(0);
	
	IplImage* dst = cvCreateImage(cvSize(image->width, image->height), IPL_DEPTH_8U, 1);
	
	const char* thresholdTypeName[] = { "CV_THRESH_BINARY", "CV_THRESH_BINARY_INV", "CV_THRESH_TRUNC", "CV_THRESH_TOZERO_INV", "CV_THRESH_TOZERO" };
	const int thresholdType[] = { CV_THRESH_BINARY, CV_THRESH_BINARY_INV, CV_THRESH_TRUNC, CV_THRESH_TOZERO_INV, CV_THRESH_TOZERO };
	
	for(int i = 0; i < sizeof(thresholdType)/sizeof(thresholdType[0]); ++i)
	{
		printf("Threshold Type %s\n", thresholdTypeName[i]);
		cvThreshold(image, dst, 128, 255, thresholdType[i]);
		cvShowImage("main", dst);
		cvWaitKey(0);
	}
	
	// part a and b
	const char* adaptiveThresholdTypeName[] = { "CV_THRESH_BINARY", "CV_THRESH_BINARY_INV" };
	const int adaptiveThresholdType[] = { CV_THRESH_BINARY, CV_THRESH_BINARY_INV };
	const int adaptiveMethod[] = { CV_ADAPTIVE_THRESH_MEAN_C, CV_ADAPTIVE_THRESH_GAUSSIAN_C };
	const char* adaptiveMethodName[] = { "CV_ADAPTIVE_THRESH_MEAN_C", "CV_ADAPTIVE_THRESH_GAUSSIAN_C" };
	const int param1Values[]	 = { 5, 0, -5 };
	
	for(int param1ValuesIndex = 0; param1ValuesIndex < sizeof(param1Values)/sizeof(param1Values[0]); param1ValuesIndex++)
	{
		int param1 = param1Values[param1ValuesIndex];
		
		for(int i = 0; i < sizeof(adaptiveThresholdType)/sizeof(adaptiveThresholdType[0]); ++i)
		{
			for(int j = 0; j < sizeof(adaptiveMethod)/sizeof(adaptiveMethod[0]); j++)
			{
				printf("Adaptive threshold type %s, method %s, param1 %d\n", adaptiveThresholdTypeName[i], adaptiveMethodName[j], param1);
				cvAdaptiveThreshold(image, dst, 255, adaptiveMethod[j], adaptiveThresholdType[i], 3, param1);
				cvShowImage("main", dst);
				cvWaitKey(0);
			}
		}
	}
	
    return 0;
}
