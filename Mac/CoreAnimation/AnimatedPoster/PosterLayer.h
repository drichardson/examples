//
//  PosterLayer.h
//  AnimatedPoster
//
//  Created by Doug on 11/4/10.
//  Copyright 2010 Douglas Richardson. All rights reserved.
//

#import <QuartzCore/QuartzCore.h>
#import <Foundation/Foundation.h>

@interface PosterLayer : CALayer
{
	CALayer* imageLayer;
	CATextLayer* titleLayer;
}

@property (copy) NSString* title;
@property CGImageRef image; // The main poster image

- (void)sizeToFit;

@end
