//
//  AppDelegate.m
//  CoreAnimationblending
//
//  Created by Douglas Richardson on 4/23/12.
//  Copyright (c) 2012 Doug Richardson. All rights reserved.
//

#import "AppDelegate.h"
#import <QuartzCore/QuartzCore.h>
#import <ApplicationServices/ApplicationServices.h>
#import <AVFoundation/AVFoundation.h>

@implementation AppDelegate

@synthesize window = _window;

- (CALayer*)layer
{
    return ((NSView*)self.window.contentView).layer;
}

- (void)setContents:(CALayer*)layer
{
    // Disable actions so changing the contents don't animate.
    [CATransaction begin];
    [CATransaction setDisableActions:YES];
    self.layer.sublayers = [NSArray arrayWithObject:layer];
    [CATransaction commit];
}

- (CALayer*)testImageLayer
{
    NSImage* backgroundImage = [NSImage imageNamed:@"background.jpg"];
    CALayer* background = [CALayer layer];
    background.frame = self.layer.bounds;
    background.contents = (id)[backgroundImage CGImageForProposedRect:NULL context:NULL hints:nil];
    return background;
}

- (void)applicationDidFinishLaunching:(NSNotification *)aNotification
{
    // Insert code here to initialize your application
    CALayer* mainLayer = self.layer;
    CGColorRef red = CGColorCreateGenericRGB(1, 0, 0, 1);
    mainLayer.backgroundColor = red;
    CGColorRelease(red);
}

- (IBAction)image:(id)sender
{
    [self setContents:self.testImageLayer];
}

- (IBAction)fastBlur:(id)sender
{
    CALayer* mainLayer = self.layer;
    
    // Apply the filter to the scaled down value so you don't have
    // to blur as many pixels. Can be used in situations when you want
    // to keep the frame rate high.
    
    CALayer* scaleUp = [CALayer layer];
    scaleUp.frame = mainLayer.bounds;
    scaleUp.transform = CATransform3DMakeScale(10, 10, 1);
    scaleUp.shouldRasterize = YES;
    
    CIFilter* filter = [CIFilter filterWithName:@"CIGaussianBlur"];
    [filter setValue:[NSNumber numberWithFloat:2] forKey:kCIInputRadiusKey];
    scaleUp.filters = [NSArray arrayWithObject:filter];
    
    CALayer* scaleDown = [CALayer layer];
    scaleDown.frame = mainLayer.bounds;
    scaleDown.transform = CATransform3DMakeScale(0.1, 0.1, 1);
    
    [scaleUp addSublayer:scaleDown];
    [scaleDown addSublayer:self.testImageLayer];
    [self setContents:scaleUp];
}

- (IBAction)coreImageFilter:(id)sender
{
    CIFilter* filter = [CIFilter filterWithName:@"CIGaussianBlur"];
    [filter setValue:[NSNumber numberWithFloat:20] forKey:kCIInputRadiusKey];
    
    CALayer* image = self.testImageLayer;
    image.filters = [NSArray arrayWithObject:filter];
    
    [self setContents:image];
}

- (IBAction)overlay:(id)sender
{
    CALayer* image = self.testImageLayer;
    
    CALayer* overlay = [CALayer layer];
    overlay.frame = CGRectMake(10, 10, 200, 600);
    overlay.backgroundColor = CGColorCreateGenericRGB(1, 1, 1, 0.3);
    
    [image addSublayer:overlay];
    
    [self setContents:image];
}

- (IBAction)partialBlurRect:(id)sender
{
    CALayer* container = [CALayer layer];
    
    CALayer* image = self.testImageLayer;
    
    CALayer* partialBlur = [CALayer layer];
    CALayer* image2 = self.testImageLayer;
    [partialBlur addSublayer:image2];
    CIFilter* filter = [CIFilter filterWithName:@"CIGaussianBlur"];
    [filter setValue:[NSNumber numberWithFloat:4] forKey:kCIInputRadiusKey];
    image2.filters = [NSArray arrayWithObject:filter];
    CALayer* mask = [CALayer layer];
    mask.backgroundColor = CGColorCreateGenericRGB(1, 0, 0, 1);
    mask.frame = CGRectMake(40, 40, 100, 200);
    mask.cornerRadius = 20;
    partialBlur.mask = mask;
    CALayer* overlay = [CALayer layer];
    overlay.backgroundColor = CGColorCreateGenericRGB(0, 0, 0, 0.3);
    overlay.frame = mask.frame;
    overlay.borderColor = CGColorCreateGenericGray(1, 0.6);
    overlay.borderWidth = 2.0;
    overlay.cornerRadius = mask.cornerRadius;
    [partialBlur addSublayer:overlay];
    
    [container addSublayer:image];
    [container addSublayer:partialBlur];
    
    [self setContents:container];
}

- (IBAction)fastPartialBlurRect:(id)sender
{
    CALayer* container = [CALayer layer];
    
    CALayer* image = self.testImageLayer;
    
    CALayer* partialBlur = [CALayer layer];
    CALayer* image2 = self.testImageLayer;
    [partialBlur addSublayer:image2];
    CIFilter* filter = [CIFilter filterWithName:@"CIGaussianBlur"];
    [filter setValue:[NSNumber numberWithFloat:4] forKey:kCIInputRadiusKey];
    image2.filters = [NSArray arrayWithObject:filter];
    CALayer* mask = [CALayer layer];
    mask.backgroundColor = CGColorCreateGenericRGB(1, 0, 0, 1);
    mask.frame = CGRectMake(40, 40, 100, 200);
    mask.cornerRadius = 20;
    partialBlur.mask = mask;
    CALayer* overlay = [CALayer layer];
    overlay.backgroundColor = CGColorCreateGenericRGB(0, 0, 0, 0.3);
    overlay.frame = mask.frame;
    overlay.borderColor = CGColorCreateGenericGray(1, 0.6);
    overlay.borderWidth = 2.0;
    overlay.cornerRadius = mask.cornerRadius;
    [partialBlur addSublayer:overlay];
    
    [container addSublayer:image];
    [container addSublayer:partialBlur];
    
    [self setContents:container];
}

- (IBAction)mask:(id)sender
{    
    CALayer* image = self.testImageLayer;
    
    CAShapeLayer* mask = [CAShapeLayer layer];
    mask.frame = CGRectMake(100, 50, 200, 300);
    
    CGMutablePathRef path = CGPathCreateMutable();
    CGPathAddEllipseInRect(path, NULL, CGRectMake(50, 50, 100, 30));
    mask.path = path;
    CGPathRelease(path);
    
    image.mask = mask;
    
    [self setContents:image];
}

- (IBAction)partialBlurOnVideo:(id)sender
{
    CALayer* container = [CALayer layer];
    
    NSURL* videoURL = [NSURL URLWithString:@"http://devimages.apple.com/iphone/samples/bipbop/bipbopall.m3u8"];
    
    AVPlayer* player = [AVPlayer playerWithURL:videoURL];
    AVPlayerLayer* playerLayer = [AVPlayerLayer playerLayerWithPlayer:player];
    [player play];
    
    // Note: Background filters are very inefficient.
    CALayer* partialBlur = [CALayer layer];
    CIFilter* filter = [CIFilter filterWithName:@"CIGaussianBlur"];
    [filter setValue:[NSNumber numberWithFloat:30] forKey:kCIInputRadiusKey];
    partialBlur.backgroundFilters = [NSArray arrayWithObject:filter];
    CALayer* mask = [CALayer layer];
    mask.backgroundColor = CGColorCreateGenericRGB(1, 0, 0, 1);
    mask.frame = CGRectMake(40, 40, 100, 200);
    mask.cornerRadius = 20;
    partialBlur.mask = mask;
    CALayer* overlay = [CALayer layer];
    overlay.backgroundColor = CGColorCreateGenericRGB(0, 0, 0, 0.3);
    overlay.frame = mask.frame;
    overlay.borderColor = CGColorCreateGenericGray(1, 0.6);
    overlay.borderWidth = 2.0;
    overlay.cornerRadius = mask.cornerRadius;
    [partialBlur addSublayer:overlay];
    
    CABasicAnimation* animation = [CABasicAnimation animationWithKeyPath:@"position"];
    animation.fromValue = [NSValue valueWithPoint:(NSPoint)partialBlur.position];
    animation.toValue = [NSValue valueWithPoint:NSMakePoint(300, partialBlur.position.y)];
    animation.duration = 3;
    animation.autoreverses = YES;
    animation.repeatCount = HUGE_VAL;
    [partialBlur addAnimation:animation forKey:@"Howdy"];
    
    playerLayer.frame = self.layer.bounds;
    [container addSublayer:playerLayer];
    [container addSublayer:partialBlur];
    
    [self setContents:container];
}

@end
