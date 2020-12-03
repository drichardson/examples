//
//  RFBView.m
//  Flash Beep Prototype
//
//  Created by Doug on 5/29/10.
//  Copyright 2010 Douglas Richardson. All rights reserved.
//

#import "RFBView.h"


@implementation RFBView

- (void)awakeFromNib
{
	CGImageRef imageRef = [[NSImage imageNamed:@"Screen.png"] CGImageForProposedRect:NULL context:nil hints:nil];
	
	rfbLayer = [CALayer layer];
	rfbLayer.frame = self.bounds;
	rfbLayer.autoresizingMask = kCALayerWidthSizable | kCALayerHeightSizable;
	rfbLayer.contents = (id)imageRef;
	rfbLayer.opacity = 1.0;
	
	CGImageRelease(imageRef);
	
	
	flashLayer = [CALayer layer];
	flashLayer.frame = rfbLayer.bounds;
	flashLayer.autoresizingMask = kCALayerWidthSizable | kCALayerHeightSizable;
	flashLayer.backgroundColor = CGColorCreateGenericRGB(1, 1, 1, 1);
	flashLayer.opacity = 0.0;
	
	[rfbLayer addSublayer:flashLayer];
	
	self.layer = rfbLayer;
	[self setWantsLayer:YES];
}

- (void)mouseUp:(NSEvent *)theEvent
{
	[self flash];
}

- (void)flash
{
	CAMediaTimingFunction *easeInEaseOut = [CAMediaTimingFunction functionWithName:kCAMediaTimingFunctionEaseInEaseOut];
	
	CAKeyframeAnimation *a = [CAKeyframeAnimation animationWithKeyPath:@"opacity"];
	a.duration = 0.5;
	a.keyTimes = [NSArray arrayWithObjects:[NSNumber numberWithFloat:0], [NSNumber numberWithFloat:0.3], [NSNumber numberWithFloat:1.0], nil];
	a.values = [NSArray arrayWithObjects:[NSNumber numberWithFloat:0], [NSNumber numberWithFloat:0.36], [NSNumber numberWithFloat:0], nil];
	a.timingFunctions = [NSArray arrayWithObjects:easeInEaseOut, easeInEaseOut, nil];
	
	[flashLayer addAnimation:a forKey:@"flashAnimation"];
}

@end
