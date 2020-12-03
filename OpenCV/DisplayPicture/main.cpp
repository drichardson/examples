//
//  main.c
//  DisplayPicture
//
//  Created by Doug Richardson on 4/2/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#include <stdio.h>
#include <opencv2/highgui/highgui_c.h>

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

    cvNamedWindow("Example 1 - Load Image");
    cvShowImage("Example1", image);
    cvWaitKey();
    cvReleaseImage(&image);
    cvDestroyWindow("Example1");
    
    return 0;
}

