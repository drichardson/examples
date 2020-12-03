//
//  FromScratchAppDelegate.h
//  FromScratch
//
//  Created by Doug on 3/11/08.
//  Copyright __MyCompanyName__ 2008. All rights reserved.
//

#import <UIKit/UIKit.h>

@class MyView;

@interface FromScratchAppDelegate : NSObject {
    UIWindow *window;
	UIToolbarController *toolbarController;
}

@property (nonatomic, retain) UIWindow *window;

@end
