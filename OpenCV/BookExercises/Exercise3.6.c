//
//  Exercise3.6.c

#include <stdio.h>
#include <opencv2/highgui/highgui_c.h>
#include <opencv2/imgproc/imgproc_c.h>
#include <opencv2/core/core_c.h>

int main (int argc, const char * argv[])
{
	if ( argc != 2 )
	{
		fprintf(stderr, "Expected 2 arguments. Usage: program <filename>\n");
		exit(1);
	}
	
	IplImage* image = cvLoadImage(argv[1], CV_LOAD_IMAGE_UNCHANGED);
	
	if ( image == NULL )
	{
		fprintf(stderr, "Couldn't load image.\n");
		exit(1);
	}
	
	IplImage* header1 = cvCreateImageHeader( cvSize(20, 30), image->depth, image->nChannels );
	IplImage* header2 = cvCreateImageHeader( cvSize(20, 30), image->depth, image->nChannels );
	
	// Point to pixel (5,10)
	header1->imageData = image->imageData + (5 * image->nChannels) + (10 * image->widthStep);
	header1->origin = image->origin;
	header1->widthStep = image->widthStep;
	
	// Point to pixel (50,60)
	header2->imageData = image->imageData + (50 * image->nChannels) + (60 * image->widthStep);
	header2->origin = image->origin;
	header2->widthStep = image->widthStep;
	
	cvNot(header1, header1);
	cvNot(header2, header2);
	
	cvNamedWindow("Image", CV_WINDOW_AUTOSIZE);
	cvShowImage("Image", image);
	cvWaitKey(0);
	
	cvReleaseImage(&image);
	cvReleaseImage(&header1);
	cvReleaseImage(&header2);

    return 0;
}

