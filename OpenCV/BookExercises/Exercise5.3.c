//
//  Exercise5.3.c

#include <stdio.h>
#include <opencv2/highgui/highgui_c.h>
#include <opencv2/imgproc/imgproc_c.h>
#include <opencv2/core/core_c.h>

int main (int argc, const char * argv[])
{
	if ( argc != 2 ) {
		fprintf(stderr, "Expected image filename.\n");
		exit(1);
	}
	
	IplImage* image = cvLoadImage(argv[1], CV_LOAD_IMAGE_COLOR);
	
	if ( image == NULL ) {
		fprintf(stderr, "Couldn't load file %s\n", argv[1]);
		exit(1);
	}
	
	// part a
	IplImage* sigma1 = cvCreateImage(cvSize(image->width, image->height), image->depth, image->nChannels);
	IplImage* sigma4 = cvCreateImage(cvSize(image->width, image->height), image->depth, image->nChannels);
	IplImage* sigma6 = cvCreateImage(cvSize(image->width, image->height), image->depth, image->nChannels);
	
	cvSmooth(image, sigma1, CV_GAUSSIAN, 9, 9, 1, 0);
	cvSmooth(image, sigma4, CV_GAUSSIAN, 9, 9, 4, 0);
	cvSmooth(image, sigma6, CV_GAUSSIAN, 9, 9, 6, 0);
	
	cvNamedWindow("Original", CV_WINDOW_NORMAL);
	cvShowImage("Original", image);
	cvNamedWindow("asigma1", CV_WINDOW_NORMAL);
	cvShowImage("asigma1", sigma1);
	cvNamedWindow("asigma4", CV_WINDOW_NORMAL);
	cvShowImage("asigma4", sigma4);
	cvNamedWindow("asigma6", CV_WINDOW_NORMAL);
	cvShowImage("asigma6", sigma6);
	cvWaitKey(0);
	
	// part b
	IplImage* sigma1_2 = cvCreateImage(cvSize(image->width, image->height), image->depth, image->nChannels);
	IplImage* sigma4_2 = cvCreateImage(cvSize(image->width, image->height), image->depth, image->nChannels);
	IplImage* sigma6_2 = cvCreateImage(cvSize(image->width, image->height), image->depth, image->nChannels);
	cvSmooth(image, sigma1_2, CV_GAUSSIAN, 0, 0, 1, 0);
	cvSmooth(image, sigma4_2, CV_GAUSSIAN, 0, 0, 4, 0);
	cvSmooth(image, sigma6_2, CV_GAUSSIAN, 0, 0, 6, 0);
	cvNamedWindow("bsigma12", CV_WINDOW_NORMAL);
	cvShowImage("bsigma12", sigma1_2);
	cvNamedWindow("bsigma42", CV_WINDOW_NORMAL);
	cvShowImage("bsigma42", sigma4_2);
	cvNamedWindow("bsigma62", CV_WINDOW_NORMAL);
	cvShowImage("bsigma62", sigma6_2);
	cvWaitKey(0);
	
	// part c
	IplImage* csigma1_9 = cvCreateImage(cvSize(image->width, image->height), image->depth, image->nChannels);
	cvSmooth(image, csigma1_9, CV_GAUSSIAN, 0, 0, 1, 9);
	cvNamedWindow("csigma1_9", CV_WINDOW_NORMAL);
	cvShowImage("csigma1_9", csigma1_9);
	cvWaitKey(0);
	
	// part d
	IplImage* csigma9_1 = cvCreateImage(cvSize(image->width, image->height), image->depth, image->nChannels);
	cvSmooth(image, csigma9_1, CV_GAUSSIAN, 0, 0, 9, 1);
	cvNamedWindow("csigma9_1", CV_WINDOW_NORMAL);
	cvShowImage("csigma9_1", csigma9_1);
	cvWaitKey(0);
	
	// part e
	IplImage* esigma = cvCreateImage(cvSize(image->width, image->height), image->depth, image->nChannels);
	cvSmooth(image, esigma, CV_GAUSSIAN, 0, 0, 1, 9);
	cvSmooth(esigma, esigma, CV_GAUSSIAN, 0, 0, 9, 1);
	cvNamedWindow("esigma", CV_WINDOW_NORMAL);
	cvShowImage("esigma", esigma);
	cvWaitKey(0);
	
	// part f
	IplImage* fsigma = cvCreateImage(cvSize(image->width, image->height), image->depth, image->nChannels);
	cvSmooth(image, fsigma, CV_GAUSSIAN, 9, 9, 0, 0);
	cvNamedWindow("fsigma", CV_WINDOW_NORMAL);
	cvShowImage("fsigma", fsigma);
	cvWaitKey(0);

	// Should call cvReleaseImage if this wasn't going to exit.
	
    return 0;
}

