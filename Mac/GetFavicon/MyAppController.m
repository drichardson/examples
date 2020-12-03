//
//  MyAppController.m
//  GetFavicon
//
//  Created by Doug on 4/20/08.
//  Copyright 2008 Douglas Richardson. All rights reserved.
//

#import "MyAppController.h"
#import <ApplicationServices/ApplicationServices.h>

@implementation MyAppController

/* Create a CGImageSourceRef from raw data */
static CGImageRef CreateCGImageFromURL(NSURL* url)
{
    CGImageRef        imageRef = NULL;
    CGImageSourceRef  sourceRef;
	
	sourceRef = CGImageSourceCreateWithURL((CFURLRef)url, NULL);
    if(sourceRef) {
        imageRef = CGImageSourceCreateImageAtIndex(sourceRef, 0, NULL);
        CFRelease(sourceRef);
    }
	
	return imageRef;
}

static CGImageRef ResizeFaviconToStandardSize(CGImageRef faviconImageRef)
{
	CGColorSpaceRef rgbRef = NULL;
	CGContextRef contextRef = NULL;
	CGImageRef resultRef = NULL;
	const CGRect standardRect = CGRectMake(0, 0, 16, 16);
	const size_t kBytesPerPixel = 4;
	const size_t kBitsPerComponent = 8;
	
	rgbRef = CGColorSpaceCreateDeviceRGB();
	if(rgbRef == NULL)
		goto bail;
	
	contextRef = CGBitmapContextCreate(NULL,
									   standardRect.size.width,
									   standardRect.size.height,
									   kBitsPerComponent,
									   standardRect.size.width * kBytesPerPixel,
									   rgbRef,
									   kCGImageAlphaPremultipliedLast);
	if(contextRef == NULL)
		goto bail;
	
	CGContextDrawImage(contextRef, standardRect, faviconImageRef);
	
	resultRef = CGBitmapContextCreateImage(contextRef);
	
bail:
	if(rgbRef)
		CGColorSpaceRelease(rgbRef);
	
	if(contextRef)
		CGContextRelease(contextRef);
	
	return resultRef;
}

static NSData* FaviconDataFromURL(NSURL* url)
{	
	NSMutableData *imageData = nil;
	CGImageDestinationRef destination = NULL;
	CGImageRef faviconImageRef = CreateCGImageFromURL(url);
	if(faviconImageRef == NULL)
		goto bail;
	
	imageData = [NSMutableData data];
	destination = CGImageDestinationCreateWithData((CFMutableDataRef)imageData, kUTTypePNG, 1, NULL);
	if(destination == NULL)
		goto bail;
	
	CGImageRef resizedFaviconImageRef = ResizeFaviconToStandardSize(faviconImageRef);
	if(resizedFaviconImageRef == NULL)
		goto bail;
	
	CGImageDestinationAddImage(destination, resizedFaviconImageRef, NULL);
	CGImageRelease(resizedFaviconImageRef);
	CGImageDestinationFinalize(destination);
	
bail:
	
	if(faviconImageRef)
		CFRelease(faviconImageRef);
	
	if(destination)
		CFRelease(destination);
	
	return imageData;
}

// Using ImageIO framework.
-(IBAction)getFavicon:(id)sender
{	
	NSURL *url = [NSURL URLWithString:[mDomain stringValue]];
	NSLog(@"URL Host is: %@", url.host);
	
	NSData *faviconData = FaviconDataFromURL(url);
	
	if(faviconData == nil)
		goto bail;
	
	NSImage *image = [[[NSImage alloc] initWithData:faviconData] autorelease];
	if(image != nil)
	{
		mImageView.image = image;
		[faviconData writeToFile:[@"~/Desktop/favicon.png" stringByExpandingTildeInPath] atomically:NO];
	}
	
bail:
	;
}

@end
