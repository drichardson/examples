//
//  Exercise6.6.c

#include <stdio.h>
#include <opencv2/highgui/highgui_c.h>
#include <opencv2/imgproc/imgproc_c.h>
#include <opencv2/core/core_c.h>

static float Max2Values(IplImage* image, CvPoint* max1, CvPoint* max2)
{
	float maxVal = -10000000;
	
	for(int j = 0; j < image->height; ++j)
	{
		float* row = (float*)(image->imageData + image->widthStep * j);
		
		for(int i = 0; i < image->width; ++i)
		{
			float val = row[i];
			
			if ( maxVal < val )
			{
				maxVal = val;
				*max2 = *max1;
				*max1 = cvPoint(i,j);
			}
		}
	}
	
	return maxVal;
}

int main (int argc, const char * argv[])
{
	if ( argc != 2 )
	{
		fprintf(stderr, "Usage: <image>\n");
		exit(1);
	}
	
	IplImage* image = cvLoadImage(argv[1], CV_LOAD_IMAGE_GRAYSCALE);
	
	if ( image == NULL )
	{
		fprintf(stderr, "Couldn't load image.\n");
		exit(1);
	}
	
	cvNamedWindow("main", CV_WINDOW_NORMAL);
	cvShowImage("main", image);
	cvWaitKey(0);
	
	//cvNot(image, image);
	
	cvShowImage("main", image);
	cvWaitKey(0);
	
	IplImage* laplacian = cvCreateImage(cvSize(image->width, image->height), IPL_DEPTH_32F, image->nChannels);
	cvLaplace(image, laplacian, 9);
	
	cvShowImage("main", laplacian);
	cvWaitKey(0);
	
	CvPoint location1 = cvPoint(-1,-1), location2 = cvPoint(-1, -1);
	float max = Max2Values(laplacian, &location1, &location2);
	
	IplImage* thresh = cvCloneImage(laplacian);
	cvThreshold(laplacian, thresh, max - max/5., 1, CV_THRESH_BINARY);
	//cvAdaptiveThreshold(laplacian, thresh, 1, CV_ADAPTIVE_THRESH_MEAN_C, CV_THRESH_BINARY, 3, 5);
	
	cvShowImage("main", thresh);
	cvWaitKey(0);
	
	printf("Max points %d,%d and %d,%d\n", location1.x, location1.y, location2.x, location2.y);
	
    return 0;
}
