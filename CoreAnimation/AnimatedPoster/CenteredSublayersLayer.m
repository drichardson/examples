//
//  CenteredSublayersLayer.m
//  AnimatedPoster
//
//  Created by Doug on 11/4/10.
//  Copyright 2010 Douglas Richardson. All rights reserved.
//

#import "CenteredSublayersLayer.h"


@implementation CenteredSublayersLayer

- (void)layoutSublayers
{
	CGFloat myYMidpoint = CGRectGetMidY(self.bounds);
	
	[CATransaction begin];
	[CATransaction setValue:(id)kCFBooleanTrue forKey:kCATransactionDisableActions];
	
	// Center sub-layers vertically.
	for (CALayer* layer in self.sublayers)
	{		
		CGRect frame = layer.frame;
		layer.position = CGPointMake(CGRectGetMidX(frame), myYMidpoint);
	}
	
	[CATransaction commit];
}

@end
