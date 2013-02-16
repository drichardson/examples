//
//  TransformTestAppDelegate.h
//  TransformTest
//
//  Created by Doug Richardson on 7/19/08.
//  Copyright __MyCompanyName__ 2008. All rights reserved.
//

#import <UIKit/UIKit.h>

@class TransformTestViewController;

@interface TransformTestAppDelegate : NSObject <UIApplicationDelegate> {
	IBOutlet UIWindow *window;
	
	UIView *myView;
	int animationStep;
	NSTimer *animationTimer;
}

@property (nonatomic, retain) UIWindow *window;

@end

