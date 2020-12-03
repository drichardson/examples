//
//  Test1AppDelegate.h
//  Test1
//
//  Created by Doug on 3/6/08.
//  Copyright __MyCompanyName__ 2008. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface Test1AppDelegate : NSObject <UIApplicationDelegate> {
    UIWindow *window;
    UIToolbarController *toolbarController;
}

@property (nonatomic, retain) UIWindow *window;
@property (nonatomic, retain) UIToolbarController *toolbarController;

- (void)applicationDidFinishLaunching:(UIApplication *)application;

@end
