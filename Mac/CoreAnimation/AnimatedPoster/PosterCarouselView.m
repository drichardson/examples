//
//  PosterCarouselView.m
//  AnimatedPoster
//
//  Created by Doug on 11/4/10.
//  Copyright 2010 Douglas Richardson. All rights reserved.
//

#import "PosterCarouselView.h"
#import "PosterLayer.h"

#define kHoverEventDelay 0.4

@implementation PosterCarouselView

- (void)dealloc
{
	[trackingArea release];
	[super dealloc];
}

- (void)_setupMyTrackingRect
{
	trackingArea = [[NSTrackingArea alloc] initWithRect:self.bounds options:NSTrackingMouseMoved | NSTrackingActiveWhenFirstResponder owner:self userInfo:nil];
	[self addTrackingArea:trackingArea];
}

- (BOOL)acceptsFirstResponder
{
	return YES;
}

- (void)awakeFromNib
{
	mainLayer = [CenteredSublayersLayer layer];
	//mainLayer.backgroundColor = CGColorCreateGenericRGB(1, 1, 0, 1);
	
	[self setLayer:mainLayer];
	[self setWantsLayer:YES];
	
	CATextLayer* instructions = [CATextLayer layer];
	instructions.string = NSLocalizedString(@"Drag image files here.", nil);
	instructions.font = [NSFont systemFontOfSize:20.0];
	instructions.foregroundColor = CGColorCreateGenericGray(0, 1);
	CGColorRelease(instructions.foregroundColor);
	instructions.bounds = CGRectMake(0, 0, mainLayer.bounds.size.width, 80);
	instructions.position = CGPointMake(CGRectGetMidX(mainLayer.bounds), CGRectGetMidY(mainLayer.bounds));
	instructions.backgroundColor = CGColorCreateGenericRGB(1, 0, 0, 1);
	instructions.autoresizingMask = kCALayerWidthSizable;
	instructions.alignmentMode = kCAAlignmentCenter;
	[mainLayer addSublayer:instructions];
	
	[self registerForDraggedTypes:[NSArray arrayWithObjects:NSFilenamesPboardType, nil]];
	
	[self _setupMyTrackingRect];
}

- (void)updateTrackingAreas
{	
	[self removeTrackingArea:trackingArea];
	[trackingArea release];
	trackingArea = nil;
	[self _setupMyTrackingRect];
	
	[super updateTrackingAreas];
}

- (PosterLayer*)_posterHitTest:(NSEvent*)theEvent
{
	NSPoint locationInView = [self convertPoint:[theEvent locationInWindow] fromView:nil];
	CALayer* hitLayer = [mainLayer hitTest:NSPointToCGPoint(locationInView)];
	
	// I want one of my direct sub-layers, so go up the hierarchy until the superlayer is mainLayer.
	while (hitLayer && hitLayer.superlayer != mainLayer)
	{
		hitLayer = hitLayer.superlayer;
	}
	
	return (PosterLayer*) ([hitLayer isKindOfClass:[PosterLayer class]] ? hitLayer : nil);
}

- (void)_hoverEvent:(NSEvent*)theEvent
{
	hoverEvent = nil;
	
	CALayer* hitLayer = [self _posterHitTest:theEvent];
	
	if ( hitLayer )
	{
		CATransform3D transform = CATransform3DConcat(CATransform3DMakeScale(1.05, 1.05, 1), CATransform3DMakeRotation(M_PI / 48.0, 0, 0, 1));
		
		CABasicAnimation* animation = [CABasicAnimation animationWithKeyPath:@"transform"];
		animation.toValue = [NSValue valueWithCATransform3D:transform];
		//animation.autoreverses = YES;
		animation.duration = 0.25;
		animation.timingFunction = [CAMediaTimingFunction functionWithName:kCAMediaTimingFunctionEaseInEaseOut];
		//animation.repeatCount = HUGE_VAL;
		
		CGPoint pos = hitLayer.position;
		
		CAKeyframeAnimation* key = [CAKeyframeAnimation animationWithKeyPath:@"position"];
		key.values = [NSArray arrayWithObjects:
					  [NSValue valueWithPoint:NSMakePoint(pos.x + 2.0, pos.y + 1.0)],
					  [NSValue valueWithPoint:NSMakePoint(pos.x - 1.0, pos.y + 2.0)],
					  [NSValue valueWithPoint:NSMakePoint(pos.x + 1.0, pos.y - 1.0)],
					  nil];
		//key.repeatCount = HUGE_VAL;
		key.duration = 0.15;
		
		CAAnimationGroup* group = [CAAnimationGroup animation];
		group.animations = [NSArray arrayWithObjects:animation, key, nil];
		group.repeatCount = HUGE_VAL;
		group.autoreverses = YES;
		[hitLayer addAnimation:group forKey:@"jiggle"];
		
		jigglingLayer = hitLayer;
	}
}

- (void)mouseMoved:(NSEvent *)theEvent
{
	if ( jigglingLayer )
	{
		CALayer* hitLayer = [self _posterHitTest:theEvent];
		
		if ( hitLayer != jigglingLayer )
		{
			[jigglingLayer removeAnimationForKey:@"jiggle"];
			
			// Get the presentation layer.
			CATransform3D transform = ((CALayer*)[jigglingLayer presentationLayer]).transform;
			CGPoint position = ((CALayer*)[jigglingLayer presentationLayer]).position;
			
			CGPoint savedPosition = jigglingLayer.position;
			CATransform3D savedTransform = jigglingLayer.transform;
			
			[CATransaction begin];
			[CATransaction setValue:(id)kCFBooleanTrue forKey:kCATransactionDisableActions];
			jigglingLayer.position = position;
			jigglingLayer.transform = transform;
			[CATransaction commit];
			
			jigglingLayer.position = savedPosition;
			jigglingLayer.transform = savedTransform;
			
			
			jigglingLayer = nil;
		}
		else
		{
			// Still over jiggling layer.
			return;
		}

	}
	
	SEL hoverEventSelector = @selector(_hoverEvent:);
	
	if ( hoverEvent )
	{
		[[self class] cancelPreviousPerformRequestsWithTarget:self selector:hoverEventSelector object:hoverEvent];
	}
	
	hoverEvent = theEvent;
	[self performSelector:hoverEventSelector withObject:theEvent afterDelay:kHoverEventDelay];
}

- (void)setNewPosters:(NSArray*)posters
{
	CGFloat currentX = 0;
	
	[posters makeObjectsPerformSelector:@selector(sizeToFit)];
	
	CGFloat totalWidth = 0;
	for (PosterLayer* poster in posters)
	{
		totalWidth += poster.frame.size.width;
	}
	
	NSRect bounds = self.bounds;
	CGFloat xPadding = 0;
	
	if ( totalWidth <= bounds.size.width )
	{
		xPadding = (bounds.size.width - totalWidth) / ((CGFloat)([posters count]+1));
	}
	
	currentX = xPadding;
	
	for (PosterLayer* poster in posters)
	{		
		CGRect frame = poster.frame;
		frame.origin = CGPointMake(currentX, frame.origin.y);
		poster.frame = frame;
		//NSLog(@"Frame for %@ is %@: currentX %f", poster.title, NSStringFromRect(NSRectFromCGRect(poster.frame)), currentX);
		
		currentX += frame.size.width + xPadding;
	}
	
	mainLayer.sublayers = posters;
}

- (NSDragOperation)draggingEntered:(id <NSDraggingInfo>)sender
{
	NSPasteboard* pboard = [sender draggingPasteboard];
    NSDragOperation sourceDragMask = [sender draggingSourceOperationMask];
	
	if ( [[pboard types] containsObject:NSFilenamesPboardType] )
	{
		if (sourceDragMask & NSDragOperationLink)
		{
            return NSDragOperationLink;
        }
	}
	
	return NSDragOperationNone;
}

- (BOOL)performDragOperation:(id <NSDraggingInfo>)sender
{
	NSPasteboard *pboard;
    NSDragOperation sourceDragMask;
	
    sourceDragMask = [sender draggingSourceOperationMask];
    pboard = [sender draggingPasteboard];
	
	if ( [[pboard types] containsObject:NSFilenamesPboardType] ) {
        NSArray *files = [pboard propertyListForType:NSFilenamesPboardType];
		
        // Depending on the dragging source and modifier keys,
        // the file data may be copied or linked
        if (sourceDragMask & NSDragOperationLink)
		{
			NSMutableArray* newPosters = [NSMutableArray arrayWithCapacity:[files count]];
			
			for (NSString* file in files)
			{
				NSLog(@"File is %@", file);
				NSImage* image = [[[NSImage alloc] initWithContentsOfFile:file] autorelease];
				if ( image )
				{
					PosterLayer* posterLayer = [PosterLayer layer];
					posterLayer.title = [file lastPathComponent];
					posterLayer.image = [image CGImageForProposedRect:NULL context:nil hints:nil];
					
					[newPosters addObject:posterLayer];
				}
			}
			
			[self setNewPosters:newPosters];
			return YES;
        }
	}
	
	return NO;
}

@end
