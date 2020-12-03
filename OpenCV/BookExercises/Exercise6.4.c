//
//  Exercise6.4.c

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
	
	if ( image == NULL )
	{
		fprintf(stderr, "Couldn't load image.");
		exit(1);
	}
	
	IplImage* dst = cvCreateImage(cvSize(image->width, image->height), IPL_DEPTH_32F, 1);
	
	cvNamedWindow("main", CV_WINDOW_NORMAL);
	cvShowImage("main", image);
	cvWaitKey(0);
	
	const int aperatureSizes[] = { 3, 5, 9, 13 };
	
	for(int i = 0; i < sizeof(aperatureSizes)/sizeof(aperatureSizes[0]); ++i )
	{
		printf("Using aperature size %d\n", aperatureSizes[i]);
		cvSobel(image, dst, 1, 1, aperatureSizes[i]);
		cvShowImage("main", dst);
		cvWaitKey(0);
	}
	
    return 0;
}
