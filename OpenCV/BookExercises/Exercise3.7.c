//
//  Exercise3.7.c

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
	
	IplImage *red = NULL; //cvCreateImage( cvSize(image->width, image->height), IPL_DEPTH_8U, 1 );
	IplImage *blue = NULL; //cvCreateImage( cvSize(image->width, image->height), IPL_DEPTH_8U, 1 );
	IplImage *green = cvCreateImage( cvSize(image->width, image->height), IPL_DEPTH_8U, 1 );
	cvSplit(image, red, blue, green, NULL);
	
	IplImage* clone1 = cvCloneImage(green);
	IplImage* clone2 = cvCloneImage(green);
	
	double greenMin = 0, greenMax = 0;
	CvPoint greenMinLoc, greenMaxLoc;
	cvMinMaxLoc(green, &greenMin, &greenMax, &greenMinLoc, &greenMaxLoc, NULL);
	
	printf("Green plane min value %f at (%d, %d), max value %f at (%d, %d)\n", greenMin, greenMinLoc.x, greenMinLoc.y, greenMax, greenMaxLoc.x, greenMaxLoc.y);
	
	unsigned char thresh = (greenMax - greenMin) / 2.0;
	cvSet(clone1, cvScalarAll(thresh), NULL);
	cvSet(clone2, cvScalarAll(0), NULL);
	
	cvCmp(green, clone1, clone2, CV_CMP_GE);
	
	cvSubS(green, cvScalarAll(thresh / 2), green, clone2);
	
	cvNamedWindow("Image", CV_WINDOW_AUTOSIZE);
	cvShowImage("Image", image);
	
	cvNamedWindow("Green", CV_WINDOW_AUTOSIZE);
	cvShowImage("Green", green);
	
	//cvNamedWindow("Clone1", CV_WINDOW_AUTOSIZE);
	//cvShowImage("Clone1", clone1);
	
	//cvNamedWindow("Clone2", CV_WINDOW_AUTOSIZE);
	//cvShowImage("Clone2", clone2);
	cvWaitKey(0);
	
	cvReleaseImage(&image);
	cvReleaseImage(&red);
	cvReleaseImage(&green);
	cvReleaseImage(&blue);

    return 0;
}

