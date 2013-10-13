//
//  main.m
//  playground
//
//  Created by Douglas Richardson on 10/10/13.
//  Copyright (c) 2013 Doug Richardson. All rights reserved.
//

#include <opencv2/imgproc/imgproc.hpp>
#include <opencv2/highgui/highgui.hpp>

using namespace cv;

int main(int argc, const char * argv[])
{
    Mat image = imread("/Users/doug/Desktop/test.jpg", CV_LOAD_IMAGE_COLOR);
    Mat sub(image, Rect(100, 0, image.size().width / 10, image.size().height));
    GaussianBlur(sub, sub, Size(3,3), 20);
    //sub = Scalar(0, 255, 0);
    
    namedWindow("hi", CV_WINDOW_AUTOSIZE);
    
    imshow("hi", image);
    
    waitKey(0);
    
    return 0;
}

