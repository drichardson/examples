//
//  Test1AppDelegate.m
//  Test1
//
//  Created by Doug on 3/6/08.
//  Copyright __MyCompanyName__ 2008. All rights reserved.
//

#import "Test1AppDelegate.h"

#import "MyViewController.h"

UIImage *toolbarImageWithColor(CGSize imageSize, UIColor *color);

@implementation Test1AppDelegate

@synthesize window;
@synthesize toolbarController;

- (void)applicationDidFinishLaunching:(UIApplication *)application {
    // Create window
    self.window = [[[UIWindow alloc] initWithFrame:[[UIScreen mainScreen] bounds]] autorelease];
    
	 // Create a toolbar controller and an array to contain the view controllers
	toolbarController = [[UIToolbarController alloc] init];
	NSMutableArray *viewControllers = [[NSMutableArray alloc] initWithCapacity:3];
	
	/*
	 Create and configure the view controllers
	 For simplicity, in this case, each is an instance of the same class but each has a different color;
	 typically you'd use a number of different controller classes.
	 */
	MyViewController *viewController;
	CGSize imageSize = CGSizeMake(100, 46);
	
	viewController = [[MyViewController alloc] init];	
	viewController.title = @"Red";
	viewController.color = [UIColor redColor];
	viewController.toolbarItem.image = toolbarImageWithColor(imageSize, viewController.color);

	[viewControllers addObject:viewController];
	[viewController release];
	
	viewController = [[MyViewController alloc] init];	
	viewController.title = @"Green";
	viewController.color = [UIColor greenColor];
	viewController.toolbarItem.image = toolbarImageWithColor(imageSize, viewController.color);
	[viewControllers addObject:viewController];
	[viewController release];
	
	viewController = [[MyViewController alloc] init];	
	viewController.title = @"Blue";
	viewController.color = [UIColor blueColor];
	viewController.toolbarItem.image = toolbarImageWithColor(imageSize, viewController.color);
	[viewControllers addObject:viewController];
	[viewController release];
	
	viewController = [[MyViewController alloc] init];	
	viewController.title = @"Brown";
	viewController.color = [UIColor brownColor];
	viewController.toolbarItem.image = toolbarImageWithColor(imageSize, viewController.color);
	[viewControllers addObject:viewController];
	[viewController release];
	
	viewController = [[MyViewController alloc] init];	
	viewController.title = @"Gray";
	viewController.color = [UIColor grayColor];
	viewController.toolbarItem.image = toolbarImageWithColor(imageSize, viewController.color);
	[viewControllers addObject:viewController];
	[viewController release];
	
	// Add the view controllers to the toolbar controller
	toolbarController.viewControllers = viewControllers;
	[viewControllers release];
	
	// Add the toolbar controller's current view as a subview of the window, then display the window
	[window addSubview:toolbarController.view];
    [window makeKeyAndVisible];
}

- (void)dealloc {
    [toolbarController release];
    [window release];
	[super dealloc];
}

@end

// Returns a transparent image of the given size containing the text in displayString drawn in the specified color.
UIImage *toolbarImageWithColor(CGSize imageSize, UIColor *color) {
	void *bitmapData;
	int bitmapBytesPerRow = (imageSize.width * 4);
	bitmapData = malloc(bitmapBytesPerRow * imageSize.height);
	if (bitmapData == NULL) {
 		return nil;
	}
	
	CGContextRef context = NULL;
	CGColorSpaceRef colorSpace = CGColorSpaceCreateDeviceRGB();
	context = CGBitmapContextCreate(bitmapData, imageSize.width, imageSize.height, 8, bitmapBytesPerRow, colorSpace, kCGImageAlphaPremultipliedLast);
	if (context== NULL) {
		free(bitmapData);
 		return nil;
	}
	CGColorSpaceRelease(colorSpace);

	UIGraphicsPushContext(context);
	[color set];
	// Inset the color rect (custom CGContext coordinate system is flipped with respect to UIView)
	UIRectFill(CGRectMake(20, 12, imageSize.width-40, imageSize.height-20));
	CGImageRef cgImage = CGBitmapContextCreateImage(context);	
	UIImage *uiImage = [UIImage imageWithCGImage:cgImage];
    CGContextRelease(context);
	CGImageRelease(cgImage);
	return uiImage;
}
