//
//  Exercise3.1.c

#include <stdio.h>
#include <opencv2/highgui/highgui_c.h>
#include <opencv2/imgproc/imgproc_c.h>
#include <opencv2/core/core_c.h>

int main (int argc, const char * argv[])
{	
	// Part (a)
	float x = -10.4325;
	printf("Abs is %f\n", fabs(x));
	printf("Rounded is %d\n", cvRound(x));
	printf("Ceiling is %d\n", cvCeil(x));
	printf("Floor is %d\n", cvFloor(x));
	
	
	// Part (b)
	CvRNG rng = cvRNG(-1);
	printf("Random numbers: %d, %f\n", cvRandInt(&rng), cvRandReal(&rng));
	
	// Part (c)
	CvPoint2D32f fPoint = cvPoint2D32f(1.25, 5.35);
	CvPoint iPoint = cvPointFrom32f(fPoint);
	printf("iPoint %d, %d\n", iPoint.x, iPoint.y);
	
	// Part (d)
	iPoint.y = 25;
	fPoint = cvPointTo32f(iPoint);
	printf("fPoint %f, %f\n", fPoint.x, fPoint.y);
    
    return 0;
}

