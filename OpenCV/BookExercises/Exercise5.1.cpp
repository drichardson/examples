//
//  Exercise5.1.cpp
//  Load an image with interesting textures. Smooth the image in several ways using cvSmooth() with
// smoothtype=CV_GAUSSIAN
// a) Use a symmetric 3-by-3, 5-by-5, 9-by-9, and 11-by-11 smoothing window size and display the results
// b) Are the output results nearly the same by smoothing the image twice with a 5-by-5 Gaussian
// filter as when you smooth once with two 11-by-11 filters? Why or why not?
//

#include <stdio.h>
#include <opencv2/highgui/highgui_c.h>
#include <opencv2/imgproc/imgproc_c.h>
#include <opencv2/core/core_c.h>

int main (int argc, const char * argv[])
{
	if ( argc != 2 )
	{
		fprintf(stderr, "Specify an image filename.\n");
		exit(1);
	}
	
	IplImage* image = cvLoadImage(argv[1]);
	
	if ( image == NULL )
	{
		fprintf(stderr, "Error loading image file %s\n", argv[1]);
		exit(1);
	}
	
	IplImage* image3x3 = cvCreateImage( cvGetSize(image), image->depth, image->nChannels);
	IplImage* image5x5 = cvCreateImage( cvGetSize(image), image->depth, image->nChannels);
	IplImage* image9x9 = cvCreateImage( cvGetSize(image), image->depth, image->nChannels);
	IplImage* image11x11 = cvCreateImage( cvGetSize(image), image->depth, image->nChannels);
	IplImage* image5x5_twice = cvCreateImage( cvGetSize(image), image->depth, image->nChannels);
	//IplImage* image11x11 = cvCreateImage( cvGetSize(image), image->depth, image->nChannels);
	
	cvSmooth(image, image3x3, CV_GAUSSIAN, 3, 3);
	cvSmooth(image, image5x5, CV_GAUSSIAN, 5, 5);
	cvSmooth(image, image9x9, CV_GAUSSIAN, 9, 9);
	cvSmooth(image, image11x11, CV_GAUSSIAN, 11, 11);
	
	cvSmooth(image, image5x5_twice, CV_GAUSSIAN, 5, 5);
	cvSmooth(image5x5_twice, image5x5_twice, CV_GAUSSIAN, 5, 5);

	cvNamedWindow("original");
	cvShowImage("original", image);

    cvNamedWindow("3-by-3");
	cvShowImage("3-by-3", image3x3);
	
	cvNamedWindow("5-by-5");
	cvShowImage("5-by-5", image5x5);
	
	cvNamedWindow("9-by-9");
	cvShowImage("9-by-9", image9x9);
	
	cvNamedWindow("11-by-11");
	cvShowImage("11-by-11", image11x11);
	
	cvNamedWindow("5-by-5 Twice");
	cvShowImage("5-by-5 Twice", image5x5_twice);
	
    cvWaitKey();
	
    // Should call cvReleaseImage/cvDestroyWindow if we were going to do this as part of an ongoing program.
    
    return 0;
}

