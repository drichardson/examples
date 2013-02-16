//
//  SimplePlayerAppDelegate.h
//  SimplePlayer
//
//  Created by Doug on 11/7/10.
//  Copyright 2010 Douglas Richardson. All rights reserved.
//

#import <UIKit/UIKit.h>

@class SimplePlayerViewController;

@interface SimplePlayerAppDelegate : NSObject <UIApplicationDelegate> {
    UIWindow *window;
    SimplePlayerViewController *viewController;
}

@property (nonatomic, retain) IBOutlet UIWindow *window;
@property (nonatomic, retain) IBOutlet SimplePlayerViewController *viewController;

@end

