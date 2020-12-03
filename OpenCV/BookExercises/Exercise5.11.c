//
//  Exercise5.11.c

#include <stdio.h>
#include <opencv2/highgui/highgui_c.h>
#include <opencv2/imgproc/imgproc_c.h>
#include <opencv2/core/core_c.h>

static IplImage* CreateMatchingImage(IplImage* image)
{
	return cvCreateImage(cvSize(image->width, image->height), image->depth, image->nChannels);
}

int main (int argc, const char * argv[])
{
	if ( argc != 2 )
	{
		fprintf(stderr, "Usage: <image>\n");
		exit(1);
	}
	
	IplImage* image = cvLoadImage(argv[1], CV_LOAD_IMAGE_UNCHANGED);
	IplImage* scratchImage = CreateMatchingImage(image);
	CvMemStorage* storage = cvCreateMemStorage(0);
	CvSeq* comp = NULL;
	
	int level = 1; // for a 512x512 image, the highest level you can use is 9, because 2^9=512.
	cvPyrSegmentation(image, scratchImage, storage, &comp, level, 150, 50);
	
	int i;
	for(i = 0; i < comp->total; ++i)
	{
		CvConnectedComp* cc = (CvConnectedComp*)cvGetSeqElem(comp, i);
		printf("cc #%d: area: %f, color: (%f, %f, %f), roi: (%d,%d,%d,%d)\n", i, cc->area, cc->value.val[0], cc->value.val[1], cc->value.val[2], cc->rect.x, cc->rect.y, cc->rect.width, cc->rect.height);
	}
		
	cvNamedWindow("image", CV_WINDOW_NORMAL);
	cvShowImage("image", image);
	cvWaitKey(0);
	
	cvNamedWindow("scratchImage", CV_WINDOW_NORMAL);
	cvShowImage("scratchImage", scratchImage);
	cvWaitKey(0);	
	
    return 0;
}

