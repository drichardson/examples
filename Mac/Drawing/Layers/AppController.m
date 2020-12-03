//
//  AppController.m
//  Layers
//
//  Created by Doug on 7/11/09.
//  Copyright 2009 Douglas Richardson. All rights reserved.
//

#import "AppController.h"

@interface AppController ()
- (void)setupSolidLayer;
- (void)setupTextLayer;
- (void)setupOpenGLLayer;
@end

@implementation AppController

- (void)awakeFromNib
{
	_rootLayer = [CALayer layer];
	_rootLayer.delegate = self;
	[_rootLayer setNeedsDisplay];
	
	_mainView.layer = _rootLayer;
	
	[self setupSolidLayer];
	[self setupTextLayer];
	[self setupOpenGLLayer];
}

- (void)setupSolidLayer
{
	_solidLayer = [CALayer layer];
	_solidLayer.backgroundColor = [_solidLayerBackgroundColorWell cgColorRef];
	_solidLayer.position = CGPointZero;
	_solidLayer.anchorPoint = CGPointZero;
	CGRect bounds = CGRectMake(0, 0, 100, 200);
	_solidLayer.bounds = bounds;
	
	[_solidLayerBoundsOriginX setObjectValue:[NSNumber numberWithFloat:bounds.origin.x]];
	[_solidLayerBoundsOriginY setObjectValue:[NSNumber numberWithFloat:bounds.origin.y]];
	[_solidLayerBoundsWidth setObjectValue:[NSNumber numberWithFloat:bounds.size.width]];
	[_solidLayerBoundsHeight setObjectValue:[NSNumber numberWithFloat:bounds.size.height]];
	
	[_solidLayer setNeedsDisplay];
	[_rootLayer addSublayer:_solidLayer];
}

- (void)setupTextLayer
{	
	_textLayer = [CATextLayer layer];
	_textLayer.truncationMode = kCATruncationEnd;
	_textLayer.font = [NSFont fontWithName:@"Times-Roman" size:24];
	_textLayer.fontSize = 24.0;
	//_textLayer.backgroundColor = CGColorGetConstantColor(kCGColorClear);
	_textLayer.foregroundColor = CGColorGetConstantColor(kCGColorWhite);
	_textLayer.string = [_textLayerText stringValue];
	_textLayer.bounds = CGRectMake(0, 0, 350, 150);
	_textLayer.position = CGPointMake(230, 100);
	_textLayer.anchorPoint = CGPointMake(0.5, 0.5);
	[_textLayer setNeedsDisplay];
	
	[_textLayerFontName addItemsWithTitles:[[NSFontManager sharedFontManager] availableFonts]];
	[_textLayerFontName selectItemWithTitle:@"Times-Roman"];
	[_textLayerFontSize setObjectValue:[NSNumber numberWithFloat:24]];
	
	[_rootLayer addSublayer:_textLayer];
}

- (void)setupOpenGLLayer
{
	_openGLLayer = [MyOpenGLLayer layer];
	_openGLLayer.position = CGPointMake(200, 20);
	_openGLLayer.bounds = CGRectMake(0, 0, 200, 200);
	_openGLLayer.anchorPoint = CGPointZero;
	_openGLLayer.asynchronous = YES;
	
	[_openGLLayerIsAsynchronous setState:NSOnState];
	
	[_openGLLayer setNeedsDisplay];
	[_rootLayer addSublayer:_openGLLayer];
}

#pragma mark CALayer Delegates
-(void)drawLayer:(CALayer *)layer inContext:(CGContextRef)context
{	
	if(layer == _rootLayer)
	{
		CGRect bounds = CGContextGetClipBoundingBox(context);
		CGContextSetRGBFillColor(context, 1.0, 0, 0, 1.0);
		CGContextFillRect(context, bounds);
	}
}


#pragma mark NSWindow Delegates
- (void)windowWillClose:(NSNotification *)notification
{
	[NSApp terminate:self];
}

#pragma mark Actions
- (IBAction)updateSolidLayer:(id)sender
{	
	_solidLayer.backgroundColor = [_solidLayerBackgroundColorWell cgColorRef];
	
	_solidLayer.position = CGPointMake([[_solidLayerPositionX objectValue] floatValue], [[_solidLayerPositionY objectValue] floatValue]);
	
	_solidLayer.anchorPoint = CGPointMake([[_solidLayerAnchorX objectValue] floatValue], [[_solidLayerAnchorY objectValue] floatValue]);
	
	_solidLayer.bounds = CGRectMake([[_solidLayerBoundsOriginX objectValue] floatValue], 
									[[_solidLayerBoundsOriginY objectValue] floatValue],
									[[_solidLayerBoundsWidth objectValue] floatValue],
									[[_solidLayerBoundsHeight objectValue] floatValue]);
	
	float rotationRadians = [[_solidLayerRotationDegrees objectValue] floatValue] * M_PI / 180.0;
	_solidLayer.transform = CATransform3DMakeRotation(rotationRadians, 0, 0, -1);
	
	[_solidLayer setNeedsDisplay];
}

- (IBAction)updateSolidLayerAnimations:(id)sender
{
    [_solidLayer removeAllAnimations];
    
    if ( [_solidLayerAnimatePosition state] == NSOnState )
    {
        CABasicAnimation* a = [CABasicAnimation animationWithKeyPath:@"position"];
        a.duration = 1.0;
        a.repeatCount = HUGE_VALF;
        a.autoreverses = YES;
        a.toValue = [NSValue valueWithPoint:NSMakePoint(10, 40)];
        [_solidLayer addAnimation:a forKey:@"myPositionAnimation"];
    }
    
    if ( [_solidLayerAnimateRotation state] == NSOnState )
    {
        CABasicAnimation* a = [CABasicAnimation animationWithKeyPath:@"transform"];
        a.duration = 3.0;
        a.repeatCount = HUGE_VALF;
        a.cumulative = YES;
        a.toValue = [NSValue valueWithCATransform3D:CATransform3DMakeRotation(M_PI, 0, 0, 1)];
        [_solidLayer addAnimation:a forKey:@"myRotationAnimation"];
    }
}

- (IBAction)updateTextLayer:(id)sender
{
	_textLayer.string = [_textLayerText stringValue];
	_textLayer.fontSize = [[_textLayerFontSize objectValue] floatValue];
	_textLayer.font = [_textLayerFontName titleOfSelectedItem];
}

- (IBAction)updateOpenGLLayer:(id)sender
{
	_openGLLayer.asynchronous = [_openGLLayerIsAsynchronous state] == NSOnState;
}

@end
