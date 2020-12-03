//
//  PosterLayer.m
//  AnimatedPoster
//
//  Created by Doug on 11/4/10.
//  Copyright 2010 Douglas Richardson. All rights reserved.
//

#import "PosterLayer.h"

#define kTextLayerHeight 20.0
#define kTextLayerTopPadding 5.0

@implementation PosterLayer

- (id) init
{
	self = [super init];
	if (self != nil)
	{
		//self.backgroundColor = CGColorCreateGenericRGB(1, 0, 0, 1);
		
		imageLayer = [CALayer new];
		//imageLayer.backgroundColor = CGColorCreateGenericRGB(0, 1, 0, 1);
		
		titleLayer = [CATextLayer new];
		titleLayer.alignmentMode = kCAAlignmentCenter;
		titleLayer.font = @"Helvetica";
		titleLayer.fontSize = 14;
		titleLayer.foregroundColor = CGColorCreateGenericGray(0, 1);
		CGColorRelease(titleLayer.foregroundColor);
		//titleLayer.backgroundColor = CGColorCreateGenericRGB(0, 0, 1, 1);
		
		[self addSublayer:imageLayer];
		[self addSublayer:titleLayer];
	}
	return self;
}


- (void)setTitle:(NSString *)title
{
	titleLayer.string = title;
}

- (NSString*)title
{
	return titleLayer.string;
}

- (void)setImage:(CGImageRef)image
{
	imageLayer.contents = (id)image;
	[self setNeedsLayout];
}

- (CGImageRef)image
{
	return (CGImageRef)imageLayer.contents;
}

- (CGSize)_imageSize
{
	CGImageRef image = [self image];
	CGSize imageSize = CGSizeZero;
	
	if ( image )
	{
		imageSize = CGSizeMake(CGImageGetWidth(image), CGImageGetHeight(image));
	}
	
	return imageSize;
}

- (void)layoutSublayers
{
	CGSize imageSize = [self _imageSize];
	titleLayer.frame = CGRectMake(0, 0, imageSize.width, kTextLayerHeight);
	imageLayer.frame = CGRectMake(0, kTextLayerHeight + kTextLayerTopPadding, imageSize.width, imageSize.height);
}

- (void)sizeToFit
{
	CGSize imageSize = [self _imageSize];
	self.frame = CGRectMake(0, 0, imageSize.width, imageSize.height + kTextLayerHeight + kTextLayerTopPadding);
}

@end
