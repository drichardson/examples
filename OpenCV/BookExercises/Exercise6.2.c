//
//  Exercise6.2.c

#include <stdio.h>
#include <opencv2/highgui/highgui_c.h>
#include <opencv2/imgproc/imgproc_c.h>
#include <opencv2/core/core_c.h>

static void PrintMat(const char* name, const CvMat* mat)
{	
	printf("%s: {\n", name);
	
	for(int r = 0; r < mat->rows; ++r)
	{
		float* rowFloats = (float*)(mat->data.ptr + mat->step * r);
		for(int c = 0; c < mat->cols; ++c)
		{
			if ( c == 0 )
			{
				printf("\n  ");
			}
			else
			{
				printf(", ");
			}
			
			printf("%f", rowFloats[c]);
		}
	}
	
	printf("\n}\n");
}

int main (int argc, const char * argv[])
{
	if ( argc != 2 )
	{
		fprintf(stderr, "Usage: <image>\n");
		exit(1);
	}
	
	IplImage* image = cvLoadImage(argv[1], CV_LOAD_IMAGE_GRAYSCALE);
	IplImage* dstGaussian = cvCloneImage(image);
	IplImage* dstAcross = cvCloneImage(image);
	IplImage* dstDown = cvCloneImage(image);
	IplImage* dstBoth = cvCloneImage(image);
	
	
	// Gaussian
	CvMat* gaussianKernel = cvCreateMat(3,3,CV_32FC1);
	float* p = gaussianKernel->data.fl;
	p[0] = 1./16.; p[1] = 2./16.; p[2] = 1./16.;
	p[3] = 2./16.; p[4] = 4./16.; p[5] = 2./16.;
	p[6] = 1./16.; p[7] = 2./16.; p[8] = 1./16.;
	cvFilter2D(image, dstGaussian, gaussianKernel, cvPoint(-1,-1));
	
	// Across
	CvMat* acrossKernel = cvCreateMat(1,3,CV_32FC1);
	p = acrossKernel->data.fl;
	p[0] = 1./4.; p[1] = 2./4.; p[2] = 1./4.;
	cvFilter2D(image, dstAcross, acrossKernel, cvPoint(-1,-1));
	
	// Down
	CvMat* downKernel = cvCreateMat(3,1,CV_32FC1);
	p = downKernel->data.fl;
	p[0] = 1./4.; p[1] = 2./4.; p[2] = 1./4.;
	cvFilter2D(image, dstDown, downKernel, cvPoint(-1,-1));
	cvFilter2D(dstAcross, dstBoth, downKernel, cvPoint(-1,-1));
	
	
	cvNamedWindow("main", CV_WINDOW_NORMAL);
	cvShowImage("main", image);
	cvWaitKey(0);
	
	cvShowImage("main", dstGaussian);
	cvWaitKey(0);
	
	cvShowImage("main", dstAcross);
	cvWaitKey(0);
	
	cvShowImage("main", dstDown);
	cvWaitKey(0);
	
	cvShowImage("main", dstBoth);
	cvWaitKey(0);
	
    return 0;
}
