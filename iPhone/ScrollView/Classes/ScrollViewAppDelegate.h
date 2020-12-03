//
//  ScrollViewAppDelegate.h
//  ScrollView
//
//  Created by Doug on 8/1/08.
//

#import <UIKit/UIKit.h>

@class ScrollViewViewController;

@interface ScrollViewAppDelegate : NSObject <UIApplicationDelegate> {
	IBOutlet UIWindow *window;
	IBOutlet ScrollViewViewController *viewController;
}

@property (nonatomic, retain) UIWindow *window;
@property (nonatomic, retain) ScrollViewViewController *viewController;

@end

