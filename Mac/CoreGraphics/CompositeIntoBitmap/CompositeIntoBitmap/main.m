//
//  main.m
//  CompositeIntoBitmap
//
//  Created by Doug Richardson on 5/10/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <ApplicationServices/ApplicationServices.h>

static CGImageRef CreateImageFromFilename(const char* filename);

int main (int argc, const char * argv[])
{
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];
    
    if ( argc != 6 )
    {
        fprintf(stderr, "Expected 4 arguments but got %d\n. Usage: CompositeIntoBitmap <background_image.jpg> <overlay_image.png> <overlay_origin_x> <overlay_origin_y> <output.jpg>\n", argc);
        exit(1);
    }
    
    const char* backgroundImageFilename = argv[1];
    const char* overlayImageFilename = argv[2];
    const char* overlayOriginX = argv[3];
    const char* overlayOriginY = argv[4];
    const char* outputImageFilename = argv[5];
    
    CGPoint overlayOrigin;
    overlayOrigin.x = atof(overlayOriginX);
    overlayOrigin.y = atof(overlayOriginY);
    
    //
    // Read the background image
    //
    CGImageRef backgroundImageRef = CreateImageFromFilename(backgroundImageFilename);
    if ( backgroundImageFilename == NULL )
        exit(1);
    
    size_t bgWidth = CGImageGetWidth(backgroundImageRef);
    size_t bgHeight = CGImageGetHeight(backgroundImageRef);
    
    printf("Background is %zdx%zd\n", bgWidth, bgHeight);
    
    
    //
    // Read the overlay image
    //
    
    CGImageRef overlayImageRef = CreateImageFromFilename(overlayImageFilename);
    if ( overlayImageFilename == NULL )
        exit(1);
    
    size_t overlayWidth = CGImageGetWidth(overlayImageRef);
    size_t overlayHeight = CGImageGetHeight(overlayImageRef);
    
    
    //
    // Create the drawing context
    //
    CGColorSpaceRef colorSpace = CGColorSpaceCreateDeviceRGB();
    CGBitmapInfo bitmapInfo = kCGBitmapByteOrderDefault | kCGImageAlphaNoneSkipLast;
    
    CGContextRef context = CGBitmapContextCreate(NULL, bgWidth, bgHeight, 8, bgWidth * 4, colorSpace, bitmapInfo);
    
    if ( context == NULL )
    {
        fprintf(stderr, "Couldn't create context\n");
        exit(2);
    }
    
    //
    // Draw the output image
    //
    CGContextDrawImage(context, CGRectMake(0, 0, bgWidth, bgHeight), backgroundImageRef);
    CGContextDrawImage(context, CGRectMake(overlayOrigin.x, overlayOrigin.y, overlayWidth, overlayHeight), overlayImageRef);
    
    
    // 
    // Create a CGImageRef of the bitmap context
    //
    
    CGImageRef outputImage = CGBitmapContextCreateImage(context);
    
    if ( outputImage == NULL )
    {
        fprintf(stderr, "Error creating output image\n");
        exit(6);
    }
    
    
    //
    // Write the output image to disk
    //
    
    CFStringRef outputType = kUTTypeJPEG;
    NSString* ext = [[NSString stringWithUTF8String:outputImageFilename] pathExtension];
    
    if ( [ext isEqualToString:@"png"] )
    {
        outputType = kUTTypePNG;
    }
    else if ( [ext isEqualToString:@"pdf"] )
    {
        outputType = kUTTypePDF;
    }
    
    printf("Using output type of %s\n", [(NSString*)outputType UTF8String]);
    
    NSURL* url = [NSURL fileURLWithPath:[NSString stringWithUTF8String:outputImageFilename]];
    CGImageDestinationRef destination = CGImageDestinationCreateWithURL((CFURLRef)url, outputType, 1, NULL);
    
    if ( destination == NULL )
    {
        fprintf(stderr, "Error creating image destination.\n");
        exit(5);
    }
    
    CGImageDestinationAddImage(destination, outputImage, NULL);
    CGImageDestinationFinalize(destination);
    
    CGImageRelease(backgroundImageRef);
    CGImageRelease(overlayImageRef);
    
    [pool drain];
    return 0;
}

static CGImageRef CreateImageFromFilename(const char* filename)
{
    CGDataProviderRef provider = CGDataProviderCreateWithFilename(filename);
    
    if ( provider == NULL )
    {
        fprintf(stderr, "Couldn't create data provider for image %s\n", filename);
        return NULL;
    }
    
    BOOL shouldInterpolate = NO;
    CGImageRef imageRef = CGImageCreateWithPNGDataProvider(provider, NULL, shouldInterpolate, kCGRenderingIntentDefault);
    
    CFRelease(provider);
    
    if ( imageRef == NULL )
    {
        fprintf(stderr, "Couldn't create image from file %s.\n", filename);
        return NULL;
    }
    
    return imageRef;
}