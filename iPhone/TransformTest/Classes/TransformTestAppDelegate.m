//
//  TransformTestAppDelegate.m
//  TransformTest
//
//  Created by Doug Richardson on 7/19/08.
//  Copyright __MyCompanyName__ 2008. All rights reserved.
//

#import "TransformTestAppDelegate.h"
#import <QuartzCore/QuartzCore.h>

@interface MyView : UIView
@end

static CGColorRef CreateComplementaryColor(CGColorRef originalColor)
{
	size_t componentCount = CGColorGetNumberOfComponents(originalColor);
	const CGFloat *components = CGColorGetComponents(originalColor);
	CGFloat newComponents[10];
	for(size_t i = 0; i < componentCount - 1; ++i)
	{
		newComponents[i] = 1.0 - components[i];
		
		//NSLog(@"Component %d value is %f - complement is %f", i, components[i], newComponents[i]);
	}
	
	newComponents[componentCount - 1] = components[componentCount - 1]; // Copy the alpha as is.
	
	return CGColorCreate(CGColorGetColorSpace(originalColor), newComponents);
}

@implementation MyView

-(id)initWithFrame:(CGRect)frame
{
	self = [super initWithFrame:frame];
	
	NSLog(@"Init with frame");
	
	if(self)
	{
		UITextField *tf = [[UITextField alloc] initWithFrame:CGRectZero];
		tf.center = self.center;
		tf.bounds = CGRectMake(0, 0, frame.size.width / 2.0, 30);
		//tf.autoresizingMask = UIViewAutoresizingFlexibleTopMargin | UIViewAutoresizingFlexibleBottomMargin |
		//	UIViewAutoresizingFlexibleLeftMargin | UIViewAutoresizingFlexibleRightMargin;
		tf.autoresizingMask = UIViewAutoresizingFlexibleWidth | UIViewAutoresizingFlexibleTopMargin | UIViewAutoresizingFlexibleBottomMargin;
		tf.borderStyle = UITextBorderStyleRoundedRect;
		tf.placeholder = @"text field placeholder";
		
		[self addSubview:tf];
		[tf release];
		
		self.autoresizesSubviews = YES;
	}
	
	return self;
}

-(void)drawRect:(CGRect)rect
{	
	CGColorRef colorRef = self.backgroundColor.CGColor;
	
	CGContextRef context = UIGraphicsGetCurrentContext();
	
	CGColorRef complementaryColorRef = CreateComplementaryColor(colorRef);
	CGContextSetStrokeColorWithColor(context, complementaryColorRef);
	CGColorRelease(complementaryColorRef);
	
	CGContextSetLineWidth(context, 5.0);
	CGContextStrokeRect(context, self.bounds);
}

@end

@implementation TransformTestAppDelegate

@synthesize window;

- (void)applicationDidFinishLaunching:(UIApplication *)application {
	
	window.backgroundColor = [UIColor purpleColor];
	
	myView = [[MyView alloc] initWithFrame:window.bounds];
	myView.backgroundColor = [UIColor blueColor];
	
	[window addSubview:myView];
	
	animationTimer = [[NSTimer scheduledTimerWithTimeInterval:2.0 target:self selector:@selector(nextAnimationStep) userInfo:nil repeats:YES] retain];
	
	// Override point for customization after app launch	
    [window makeKeyAndVisible];
	
	[[UIApplication sharedApplication] setStatusBarHidden:YES animated:YES];
}

-(void)nextAnimationStep
{
	[UIView beginAnimations:nil context:NULL];
	[UIView setAnimationDuration:0.5];
	[UIView setAnimationCurve:UIViewAnimationCurveEaseInOut];
	
	NSLog(@"%@", NSStringFromCGAffineTransform(myView.transform));
	
	switch(animationStep)
	{
		case 0:
			myView.transform = CGAffineTransformIdentity;
			myView.backgroundColor = [UIColor blackColor];
			//myView.frame = CGRectMake(40, 30, 100, 200);
			myView.center = CGPointMake(70, 115);
			myView.bounds = CGRectMake(0, 0, 100, 200);
			break;
			
		case 1:
			myView.transform = CGAffineTransformMakeRotation(M_PI_2);
			myView.backgroundColor = [UIColor orangeColor];
			//myView.frame = CGRectMake(100, 20, 100, 50);
			myView.center = CGPointMake(150, 45);
			myView.bounds = CGRectMake(0, 0, 100, 50);
			break;
			
		case 2:
			myView.transform = CGAffineTransformMakeRotation(M_PI_4);
			myView.backgroundColor = [UIColor redColor];
			//myView.frame = [window.layer visibleRect];
			CGRect f = [window.layer visibleRect];
			myView.center = CGPointMake((f.origin.x + f.size.width) / 2.0, (f.origin.y + f.size.height) / 2.0);
			myView.bounds = CGRectMake(0, 0, f.size.width, f.size.height);
			break;
	}
	
	[UIView commitAnimations];
	
	animationStep++;
	if(animationStep > 2)
		animationStep = 0;
	
}


- (void)dealloc {
	[window release];
	[super dealloc];
}


@end
