//
//  WalkieTalkieAppDelegate.h
//  WalkieTalkie
//
//  Created by Doug on 1/22/09.
//  Copyright Douglas Richardson 2009. All rights reserved.
//

#import <UIKit/UIKit.h>

@class WalkieTalkieViewController;

@interface WalkieTalkieAppDelegate : NSObject <UIApplicationDelegate> {
    UIWindow *window;
    WalkieTalkieViewController *viewController;
}

@property (nonatomic, retain) IBOutlet UIWindow *window;
@property (nonatomic, retain) IBOutlet WalkieTalkieViewController *viewController;

@end

