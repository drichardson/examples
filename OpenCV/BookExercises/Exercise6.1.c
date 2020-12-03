//
//  Exercise6.1.c

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
	
	if ( image == NULL )
	{
		fprintf(stderr, "Couldn't load image %s\n", argv[1]);
		exit(1);
	}
	
	IplImage* dst = cvCloneImage(image);
	//cvSetZero(dst);
	
	CvMat* rotation = cvCreateMat(2, 3, CV_32FC1);
	
#if 0
	// Optimized for a finding 3 pixel wide lines.
	float zeroDegreeLineData[] = {
		-10, -10, -10, -10, -10,
		3, 3, 3, 3, 3,
		14, 14, 14, 14, 14,
		3, 3, 3, 3, 3,
		-10, -10, -10, -10, -10
	};
	#if 0
	float zeroDegreeLineData[] = {
		10, 10, 10, 10, 10,
		-3, -3, -3, -3, -3,
		-14, -14, -14, -14, -14,
		-3, -3, -3, -3, -3,
		10, 10, 10, 10, 10
	};
	#endif
	
	CvMat zeroDegreeLine = cvMat(5, 5, CV_32FC1, zeroDegreeLineData);
	PrintMat("Zero Degree Line", &zeroDegreeLine);
	
	cv2DRotationMatrix(cvPoint2D32f(2,2), 60.0, 1.0, rotation);
	
	CvMat* kernel = cvCreateMat(5, 5, CV_32FC1);
	
#else
	// Optimized for finding 1 pixel wide lines. The sum of all co-efficients is 0, so this kernel has
	// the tendency to send pixels towards zero
	#if 0
	float zeroDegreeLineData[] = {
		10, 10, 10,
		-20, -20, -20,
		10, 10, 10
	};
	#elif 0
	float zeroDegreeLineData[] = {
		-10, -10, -10,
		20, 20, 20,
		-10, -10, -10
	};
	#else
	// Line detector optimized to find a horizontal line 1 pixel wide that is darker (smaller value) than it’s surrounding pixels. This works because darker (smaller value) horizontal 1 pixel wide lines will have a smaller magnitude negative
	// component, which means their convoluted value will be higher than surrounding pixels. See Convolution.numbers
	// for a simple example how this works.
	float zeroDegreeLineData[] = {
		1, 1, 1,
		-2, -2, -2,
		1, 1, 1
	};
	#endif
	
	CvMat zeroDegreeLine = cvMat(3, 3, CV_32FC1, zeroDegreeLineData);
	PrintMat("Zero Degree Line", &zeroDegreeLine);
	
	// Going to rotate the horizontal line detecting kernel by 60 degrees to that it will detect 60 degree lines.
	cv2DRotationMatrix(cvPoint2D32f(1,1), 60.0, 1.0, rotation);
	
	CvMat* kernel = cvCreateMat(3, 3, CV_32FC1);
	
#endif
	
	PrintMat("Rotation", rotation);
	
	cvWarpAffine(&zeroDegreeLine, kernel, rotation, CV_INTER_LINEAR+CV_WARP_FILL_OUTLIERS, cvScalarAll(0));
	PrintMat("Kernel", kernel);
	
	cvFilter2D( image, dst, kernel, cvPoint(-1,-1));
	
	cvNamedWindow("main", CV_WINDOW_NORMAL);
	cvShowImage("main", image);
	cvWaitKey(0);
	
	cvShowImage("main", dst);
	cvWaitKey(0);
	
    return 0;
}
