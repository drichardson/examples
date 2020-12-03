//
//  Exercise6.5.c

#include <stdio.h>
#include <opencv2/highgui/highgui_c.h>
#include <opencv2/imgproc/imgproc_c.h>
#include <opencv2/core/core_c.h>

static void Magnitutde(IplImage* dx, IplImage* dy, IplImage* dst)
{
	assert(dy && dy && dst &&
		dx->width == dy->width && dy->width == dst->width &&
		dx->height == dy->height && dy->height == dst->height &&
		dx->depth == dy->depth &&  dy->depth == dst->depth && dst->depth == IPL_DEPTH_32F);
	
	for(int j = 0; j < dy->height; ++j)
	{
		float* xRow = (float*)(dx->imageData + dx->widthStep * j);
		float* yRow = (float*)(dy->imageData + dy->widthStep * j);
		float* dstRow = (float*)(dst->imageData + dst->widthStep * j);
		
		for(int i = 0; i < dy->width; ++i)
		{
			float x = xRow[i];
			float y = yRow[i];
			float mag = sqrtf(x*x + y*y);
			dstRow[i] = mag;
		}
	}
}

static float MaxValue(IplImage* image)
{
	float result = -1000000;
	
	for(int j = 0; j < image->height; ++j)
	{
		float* row = (float*)(image->imageData + image->widthStep * j);
		
		for(int i = 0; i < image->width; ++i)
		{
			float val = row[i];
			if ( val > result )
			{
				result = val;
			}
		}
	}
	
	return result;
}

static float ComputeAngle(IplImage* magnitudes, IplImage* dx, IplImage *dy, float threshold)
{
	float accumulator = 0;
	int points = 0;
	
	for(int j = 0; j < magnitudes->height; ++j)
	{
		float* magRow = (float*)(magnitudes->imageData + magnitudes->widthStep * j);
		float* dxRow = (float*)(dx->imageData + dx->widthStep * j);
		float* dyRow = (float*)(dy->imageData + dy->widthStep * j);
		
		for(int i = 0; i < magnitudes->width; ++i)
		{
			float mag = magRow[i];
			if ( mag > threshold )
			{
				float dx = dxRow[i];
				float dy = dyRow[i];
				float angle = atanf(dx * dy);
				
				printf("Noting angle %f for dx=%f, dy=%f, i=%d,j=%d\n", angle, dx, dy, i, j);
				
				accumulator += angle;
				points++;
			}
		}
	}
	
	float averageAngle = 0;
	
	if ( points != 0 )
	{
		averageAngle = accumulator / ((float)points);
	}
	
	return averageAngle;
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
	
	IplImage* dx = cvCreateImage(cvSize(image->width, image->height), IPL_DEPTH_32F, 1);
	IplImage* dy = cvCloneImage(dx);
	IplImage* magnitudes = cvCloneImage(dx);
	
	cvNamedWindow("main", CV_WINDOW_NORMAL);
	cvShowImage("main", image);
	cvWaitKey(0);
	
	const int aperatureSizes[] = { 3, 5, 9 };
	
	for(int i = 0; i < sizeof(aperatureSizes)/sizeof(aperatureSizes[0]); ++i )
	{
		printf("Using aperature size %d\n", aperatureSizes[i]);
		cvSobel(image, dx, 1, 0, aperatureSizes[i]);
		cvSobel(image, dy, 0, 1, aperatureSizes[i]);
		
		Magnitutde(dx, dy, magnitudes);
		
		// Find the max magnitude
		float maxMagnitude = MaxValue(magnitudes);
		const float kThreshold = 10.0;
		float angle = ComputeAngle(magnitudes, dx, dy, maxMagnitude - kThreshold);

		printf("Max value is %f, angle is %f radians (%f degrees)\n", maxMagnitude, angle, angle * 180.0 / M_PI);
		
		cvShowImage("main", magnitudes);
		cvWaitKey(0);
	}	
	
    return 0;
}
