//
//  TuningForkAppDelegate.h
//  TuningFork
//
//  Created by Doug on 3/11/08.
//  Copyright Douglas Richardson 2008. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface TuningForkAppDelegate : NSObject <UIApplicationDelegate> {
    UIWindow *window;
    UIToolbarController *toolbarController;
}

@property (nonatomic, retain) UIWindow *window;
@property (nonatomic, retain) UIToolbarController *toolbarController;

- (void)applicationDidFinishLaunching:(UIApplication *)application;

@end
