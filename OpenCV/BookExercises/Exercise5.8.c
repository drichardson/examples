//
//  Exercise5.8.c

#include <stdio.h>
#include <opencv2/highgui/highgui_c.h>
#include <opencv2/imgproc/imgproc_c.h>
#include <opencv2/core/core_c.h>

static void generateImage(const char* filename);
static void filterImage(const char* filename);

int main (int argc, const char * argv[])
{
	if ( argc != 3 )
	{
		fprintf(stderr, "Usage: Exercise5.8 <command> <filename>\nCommand is generate or filter.\n");
		exit(1);
	}
	
	const char* command = argv[1];
	const char* filename = argv[2];
	
	if ( strcmp(command, "generate") == 0 )
	{
		generateImage(filename);
	}
	else if ( strcmp(command, "filter") == 0 )
	{
		filterImage(filename);
	}
	else
	{
		fprintf(stderr, "Invalid command: %s\n", command);
		exit(1);
	}
	
    return 0;
}

static void generateImage(const char* filename)
{
	IplImage* image = cvCreateImage(cvSize(500, 500), IPL_DEPTH_8U, 3);
	
	CvRNG rng = cvRNG(arc4random());
	
	cvRandArr(&rng, image, CV_RAND_UNI, cvScalarAll(0), cvScalarAll(3));
	
	int rc = cvSaveImage(filename, image, NULL);
	
	if ( rc == 0 )
	{
		fprintf(stderr, "Error saving filename %s\n", filename);
		exit(1);
	}
	
	cvNamedWindow("noise", CV_WINDOW_NORMAL);
	cvShowImage("noise", image);
	cvWaitKey(0);
}

static void filterImage(const char* filename)
{
	IplImage* image = cvLoadImage(filename, CV_LOAD_IMAGE_UNCHANGED);
	
	if ( image == NULL ) {
		fprintf(stderr, "Couldn't load image %s\n", filename);
		exit(1);
	}
	
	IplImage* dst = cvCreateImage(cvSize(image->width, image->height), image->depth, image->nChannels);
	cvSmooth(image, dst, CV_BILATERAL, 9, 9, 3, 3);
	
	cvNamedWindow("filtered", CV_WINDOW_NORMAL);
	cvShowImage("filtered", dst);
	cvWaitKey(0);
}
