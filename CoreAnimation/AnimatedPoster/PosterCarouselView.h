//
//  PosterCarouselView.h
//  AnimatedPoster
//
//  Created by Doug on 11/4/10.
//  Copyright 2010 Douglas Richardson. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "CenteredSublayersLayer.h"

@interface PosterCarouselView : NSView
{
	CenteredSublayersLayer* mainLayer;
	NSTrackingArea* trackingArea;
	NSEvent* hoverEvent;
	CALayer* jigglingLayer;
}

@end
